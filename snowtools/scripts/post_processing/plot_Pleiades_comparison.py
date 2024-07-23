#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 18 march 2024

@author: Vernay

WORK IN PROGRESS

example:
--------
'''

import os
import numpy as np
import pandas as pd
import xarray as xr
import argparse

from snowtools.scripts.extract.vortex import vortexIO as io
from snowtools.scores import clusters
from snowtools.plots.boxplots import violinplot
import snowtools.tools.xarray_preprocess as xrp

from snowtools.scripts.post_processing import common_dict

members_map = common_dict.members_map
product_map = common_dict.product_map
xpid_map    = common_dict.xpid_map
colors_map  = common_dict.colors_map

geometry_map = {
    '2021080106': 'Huez250m',
    '2019080106': 'Huez250m',
    '2018080106': 'Lautaret250m',
}


def parse_command_line():
    description = "Plot figures comparing snow depth simulation(s) to a Pleiade observation"

    parser = argparse.ArgumentParser(description=description)

    parser.add_argument('-b', '--datebegin', type=str,
                        help="First date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-e', '--dateend', type=str,
                        help="Last date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-d', '--date', type=str, required=True,
                        help="Date of the reference Pleiade observation."
                             "Format YYYYMMDDHH")

    parser.add_argument('-x', '--xpids', nargs='+', type=str,
                        help="XPID(s) of the simulation(s) format XP_NAME@username")

    parser.add_argument('-a', '--vapp', type=str, default='edelweiss', choices=['s2m', 'edelweiss'],
                        help="Application that produced the target file")

    parser.add_argument('-u', '--uenv', type=str, default="uenv:edelweiss.1@vernaym",
                        help="User environment for static resources (format 'uenv:name@user')")

    parser.add_argument('-z', '--elevation_bands', nargs='+', type=int, default=np.arange(1800, 3500, 400),
                        help='Define elevation bands for clustering')

    parser.add_argument('-w', '--workdir', type=str, default=f'{os.environ["HOME"]}/workdir/EDELWEISS/plot/Pleiades',
                        help='Working directory')

    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    args = parser.parse_args()
    return args


def violin_plot(xpids, obs, var, date, mask=True, member=None):

    mnt = xr.open_dataarray('TARGET_RELIEF.nc')  # Target domain's Digital Elevation Model
    mnt = xrp.preprocess(mnt, decode_time=False)

    # Construct *dataplot* DataFrame with elevation bands as index and 1 column per product
    filtered_obs = clusters.by_slices(obs[var], mnt, elevation_bands)
    dataplot = filtered_obs.to_dataframe(name='obs').dropna().reset_index()
    if 'time' in dataplot.columns:
        dataplot = dataplot.drop(columns=['time'])
    try:
        dataplot = dataplot.drop(columns=['xx', 'yy'])
    except KeyError:
        try:
            dataplot = dataplot.drop(columns=['x', 'y'])
        except KeyError:
            try:
                dataplot = dataplot.drop(columns=['lon', 'lat'])
            except KeyError:
                dataplot = dataplot.drop(columns=['longitude', 'lattitude'])

    for xpid in xpids:
        print(xpid)
        shortid = xpid.split('@')[0]
        if member is None:
            subdir = ''
            df = filter_simu(shortid, obs, subdir, var, date, mnt)
        else:
            df = None
            for mb in range(member):
                print(mb)
                subdir = f'mb{mb:03d}'
                dfm = filter_simu(shortid, obs, subdir, var, date, mnt)
                if df is not None:
                    df = pd.concat([df, dfm], ignore_index=True)
                else:
                    df = dfm

        # Concatenate datasets into the *dataplot* DataFrame
        dataplot = pd.concat([dataplot, df])

    dataplot.columns = dataplot.columns.str.replace('slices', 'Elevation Bands (m)')
    dataplot = dataplot.melt('Elevation Bands (m)', var_name='experiment', value_name=var)

    title = f'Pleiades, {geometry}, {date[:8]}\n'
    violinplot.plot_ange(dataplot, 'Snow depth (m)', figname=f'{var}_{date}_' + '_'.join(xpids),
            title=title, violinplot=False, xmax=3, colors=colors_map, hatchid='assim')


def filter_simu(xpid, obs, subdir, var, date, mnt):

    proname = os.path.join(subdir, f'PRO_{xpid}.nc')
    simu = xr.open_dataset(proname, decode_times=False)
    simu = xrp.preprocess(simu)
    simu = simu.sel({'xx': obs.xx.data, 'yy': obs.yy.data, 'time': pd.to_datetime(date[:8], format='%Y%m%d')})
    simu = xr.where(~obs.isnull(), simu, np.nan)
    filtered_simu = clusters.slices(simu[var], mnt, elevation_bands)

    df = filtered_simu.to_dataframe(name=xpid).dropna().reset_index().drop(columns=['xx', 'yy'])
    return df


def decode_time(pro):
    """
    Manually decode time variable since other variables can not be decoded automatically
    """
    ds = xr.Dataset({"time": pro.time})
    ds = xr.decode_cf(ds)
    pro['time'] = ds.time
    return pro


if __name__ == '__main__':

    args = parse_command_line()
    datebegin       = args.datebegin
    dateend         = args.dateend
    date            = args.date
    xpids           = args.xpids
    workdir         = args.workdir
    geometry        = args.geometry
    vapp            = args.vapp
    uenv            = args.uenv
    elevation_bands = args.elevation_bands

    if not os.path.exists(workdir):
        os.makedirs(workdir)
    os.chdir(workdir)

    # 1. Get all input data

    # a) Pleiades observations
    kw = dict(date=datebegin, vapp=vapp)
    obsname = f'PLEIADES_{date}.nc'
    io.get_snow_obs_date(xpid=xpid_map[datebegin], geometry=geometry_map[datebegin], date=date, vapp='Pleiades', filename=obsname)
    obs = xr.open_dataset(obsname)
    obs = xrp.preprocess(obs.DSN_T_ISBA, decode_time=False)

    # b) Domain's DEM
    io.get_const(uenv, 'relief', geometry, filename='TARGET_RELIEF.nc', gvar='RELIEF_GRANDESROUSSES250M_L93')

    # c) Mask
    io.get_const(uenv, 'mask', geometry, filename='MASK.nc')

    # d) Simulations
    for xpid in xpids:
        if '@' not in xpid:
            user = os.environ["USER"]
            xpid = f'{xpid}@{user}'
        shortid = xpid.split('@')[0]

        # TODO : à gérer autrement pour être flexible
        if shortid in ['SAFRAN', 'ANTILOPE', 'SAFRAN_pappus', 'ANTILOPE_pappus']:
            member = None
        else:
            member = 0

        if datebegin == '2021080106' and shortid in ['RS27_pappus', 'EnKF36_pappus', 'RS27_sorted_pappus', 'PF32_pappus']:
            deb = '2021080207'
        else:
            deb = datebegin

        # Get (filtered) PRO files with Vortex
        kw = dict(datebegin=deb, dateend=dateend, vapp=vapp, member=member, namebuild=None,
                filename=f'PRO_{shortid}.nc')
        io.get_pro(xpid=xpid, geometry=geometry, **kw)

    # TODO : à gérer autrement pour être flexible
    member = None

    violin_plot(xpids, obs, 'DSN_T_ISBA', date, member=member)  # Violinplots by elevation range

    # 3. Clean data
    for xpid in xpids:
        shortid = xpid.split('@')[0]
        if member is None:
            os.remove(f'PRO_{shortid}.nc')
        else:
            for member in range(member):
                os.remove(f'mb{member:03d}/PRO_{shortid}.nc')
