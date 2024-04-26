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
import scipy as scp

from snowtools.scripts.extract.vortex import vortexIO as io
from snowtools.scores import clusters
from snowtools.scripts.post_processing import common_tools as ct


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

    parser.add_argument('-z', '--elevation_bands', nargs='+', type=int, default=np.arange(1900, 3600, 300),
                        help='Define elevation bands for clustering')

    parser.add_argument('-w', '--workdir', type=str, default=f'{os.environ["HOME"]}/workdir/EDELWEISS/scores',
                        help='Working directory')

    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    parser.add_argument('-v', '--variable', type=str, default='DSN_T_ISBA',
                        help='Variable of interest (default : SnowDepth)')

    args = parser.parse_args()
    return args


def execute(xpids, obsname, var, date, mask=True, member=None):

    mnt = xr.open_dataarray('TARGET_RELIEF.nc')  # Target domain's Digital Elevation Model
    if 'x' in mnt.dims:
        mnt = mnt.rename({'x': 'xx', 'y': 'yy'})

    obs = xr.open_dataarray(obsname)
    if 'x' in obs.dims:
        obs = obs.rename({'x': 'xx', 'y': 'yy'})

    listfiles = list()  # List of simulation PRO files
    products  = list()  # List of simulation (products) names
    for xpid in xpids:
        shortid = xpid.split('@')[0]
        proname = f'PRO_{shortid}.nc'
        if member is None:
            listfiles.append(proname)
            products.append(shortid)
        else:
            for mb in member:
                listfiles.append(f'mb{mb:03d}/{proname}')
                products.append(f'{shortid}{mb:03d}')

    # Open all simulation PRO files at once
    simu = xr.open_mfdataset(listfiles, concat_dim='xpid', combine='nested').compute()
    # Get variable's DataArray
    simu = simu.sel({'time': pd.to_datetime(date[:8], format='%Y%m%d')})[var]
    # Set the 'xpid' dimension for simulation identification
    simu['xpid'] = products

    # Select common domains
    obs  = obs.sel({'xx': np.intersect1d(obs.xx, simu.xx), 'yy': np.intersect1d(obs.yy, simu.yy)})
    simu = simu.sel({'xx': np.intersect1d(obs.xx, simu.xx), 'yy': np.intersect1d(obs.yy, simu.yy)})
    mnt  = mnt.sel({'xx': np.intersect1d(obs.xx, simu.xx), 'yy': np.intersect1d(obs.yy, simu.yy)})

    for zmin, zmax in zip(elevation_bands[:-1], elevation_bands[1:]):
        # TODO : mask NaN values from obs dataset in simu dataset
        cluster = (mnt >= zmin) & (mnt < zmax)
        tmpobs  = obs.where(cluster)
        tmpsimu = simu.where(cluster)
        diff = tmpsimu - tmpobs
        #bias = diff.mean(["xx", "yy"], skipna=True)
        bias = diff.mean(["xx", "yy"])

        import pdb
        pdb.set_trace()

    data = clusters.per_alt(obs[var], elevation_bands, mnt)

    for xpid in xpids:
        print(xpid)
        shortid = xpid.split('@')[0]
        proname = f'PRO_{shortid}.nc'
        if member is None:
            simu = xr.open_dataset(proname, decode_times=False)
        else:
            simu = xr.open_dataset(f'mb*/{proname}', decode_times=False)
        ds = simu[var]
        ds = ct.decode_time(ds)
        ds = ds.sel({'time': pd.to_datetime(date[:8], format='%Y%m%d')})
        ds = ct.maskgf(ds)
        ds = clusters.per_alt(ds, elevation_bands, mnt)

    scores(data)


def scores(data):

    # Prepare the data for calculations
    data = data.reset_index().set_index(['level_0', 'Elevation Bands (m)']).drop(['level_1'], axis=1)

    # Initialize scores dataframe for bias, std ratio, and CRPS
    out = pd.DataFrame()
    synthb_pleiade = pd.DataFrame()
    synthd_pleiade = pd.DataFrame()
    synthcrps_pleiade = pd.DataFrame()

    # Loop through simulations except Pleiade (reference)
    for sim in data.index.levels[0].drop('Obs'):

        # Calculate bias for each elevation band
        bias = data.loc[sim].groupby('Elevation Bands (m)').mean() -\
            data.loc['Obs'].groupby('Elevation Bands (m)').mean()

        # Append bias data to dataframe
        synthb_pleiade = pd.concat([bias, synthb_pleiade])

        # Calculate standard deviation ratio for each elevation band
        std_ratio = data.loc[sim].groupby('Elevation Bands (m)').std() / \
            data.loc['Pleiade'].groupby('Elevation Bands (m)').std()

        # Append std ratio data to dataframe
        synthd_pleiade = pd.concat([std_ratio, synthd_pleiade])

        # Calculate Continuous Ranked Probability Score (CRPS) for each elevation band
        for elev in data.loc[sim].index.drop_duplicates():
            # Sort snow height values for both simulations and reference
            sim_data = data.loc[sim].loc[elev].sort_values('Snow Height (m)').values.squeeze()
            ref_data = data.loc["Pleiade"].loc[elev].sort_values('Snow Height (m)').values.squeeze()

            # Interpolate simulation data to match reference data size
            interp_sim = scp.interpolate.interp1d(sim_data, np.linspace(0, 1, num=len(sim_data)), kind='nearest', fill_value="extrapolate")
            ss = np.arange(0, 400, .5)

            # Interpolate reference data to match reference data size
            interp_ref = scp.interpolate.interp1d(ref_data, np.linspace(0, 1, num=len(ref_data)), kind='nearest', fill_value="extrapolate")

            # Add 'Precipitation forcing' and 'Transport mode' columns based on simulation name
            if '_nopap' in sim:
                dic = {'Elevation Bands (m)': [elev],
                       'SPS of Snow Height (m)': scp.integrate.trapezoid((interp_sim(ss) - interp_ref(ss))**2, ss),
                       'Transport mode': 'without transport',
                       'Precipitation forcing': sim[:-6]}
            else:
                dic = {'Elevation Bands (m)': [elev],
                       'SPS of Snow Height (m)': scp.integrate.trapezoid((interp_sim(ss) - interp_ref(ss))**2, ss),
                       'Transport mode': 'with transport',
                       'Precipitation forcing': sim[:-4]}

            # Append CRPS data to dataframe
            synthcrps_pleiade = pd.concat([pd.DataFrame(dic), synthcrps_pleiade])

    synthb_pleiade = synthb_pleiade.rename(columns={'Snow Height (m)':'Bias of Snow Height (m)'})
    synthd_pleiade = synthd_pleiade.rename(columns={'Snow Height (m)':'Standard deviation (std) ratio'})

    # Merge dataframes and set index
    out = pd.merge(synthb_pleiade,synthd_pleiade.reset_index()).merge(synthcrps_pleiade).replace({'Safran_30m':'Safran HR'}).set_index(['Elevation Bands (m)','Transport mode','Precipitation forcing'])

    return out


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
    variable        = args.variable
    elevation_bands = args.elevation_bands

    if not os.path.exists(workdir):
        os.makedirs(workdir)
    os.chdir(workdir)

    # 1. Get all input data

    # a) Pleiades observations
    kw = dict(date=datebegin, vapp=vapp)
    obsname = f'PLEIADES_{date}.nc'
    io.get_snow_obs_date(xpid='CesarDB_AngeH', geometry='Lautaret250m', date=date, vapp='Pleiades', filename=obsname)

    # b) Domain's DEM
    io.get_const(uenv, 'relief', geometry, filename='TARGET_RELIEF.nc', gvar='RELIEF_GRANDESROUSSES250M_L93')

    # c) Mask
    io.get_const(uenv, 'mask', geometry, filename='MASK.nc')

    # d) Simulations
    for xpid in xpids:
        # TODO : gérer ça plus proprement
        if '@' not in xpid:
            user = os.environ["USER"]
            xpid = f'{xpid}@{user}'
        shortid = xpid.split('@')[0]
        # VERRUE pour gérer le décallage d'un jour en attendant de combler les données
#        if shortid.startswith('safran'):
#            deb = '2021080106'
#        else:
#            deb = datebegin  # 2021080207

        # TODO : à gérer autrement pour être flexible
        if shortid in ['SAFRAN', 'ANTILOPE', 'SAFRAN_pappus', 'ANTILOPE_pappus']:
            member = None
        else:
            member = 0

        # Get (filtered) PRO files with Vortex
        kw = dict(datebegin=datebegin, dateend=dateend, vapp=vapp, member=member, namebuild=None,
                filename=f'PRO_{shortid}.nc')
        io.get_pro(xpid, geometry, **kw)

    # TODO : à gérer autrement pour être flexible
    member = None

    execute(xpids, obsname, variable, date, member=member)  # Violinplots by elevation range

    # 3. Clean data
    for xpid in xpids:
        shortid = xpid.split('@')[0]
        if member is None:
            os.remove(f'PRO_{shortid}.nc')
        else:
            for member in range(member):
                os.remove(f'mb{member:03d}/PRO_{shortid}.nc')
