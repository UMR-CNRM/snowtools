#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 18 march 2024

@author: Vernay. M (core code comes from Haddjeri. A)

WORK IN PROGRESS

example:
--------
>>> p plot_sentinel2_diagnostics.py -b 2021080207 -e 2022080106
    -x SAFRAN@vernaym SAFRAN_pappus@vernaym ANTILOPE@vernaym ANTILOPE_pappus@vernaym RS27@vernaym RS27_pappus@vernaym

'''

import os
import numpy as np
import pandas as pd
import xarray as xr
import argparse
import matplotlib.pyplot as plt

from snowtools.scripts.extract.vortex import vortexIO as io
from snowtools.scores import clusters
from snowtools.plots.maps import plot2D
from snowtools.plots.boxplots import violinplot
from snowtools.tools import common_tools as ct
import snowtools.tools.xarray_preprocess as xrp

members_map = dict(
    safran         = None,
    safran_pappus  = None,
    SAFRAN         = None,
    SAFRAN_pappus  = None,
    RawData        = None,
    RawData_pappus = None,
    RS27           = 1,  # post-processed ANTILOPE only
    RS27_pappus    = 1,  # post-processed ANTILOPE only
    # RS27           = 17,  # All members
    # RS27_pappus    = 17,  # All members
    EnKF36         = range(1, 17),  # TODO : syntaxe à implémenter
    EnKF36_pappus  = range(1, 17),  # TODO : syntaxe à implémenter
)

# Retrieve dictionnary to map clustering type to a proper label
label_map = clusters.label_map


def parse_command_line():
    description = "Computation of Sentinel2-like diagnostics (snow melt-out date, snow cover duration) associated \
                   to a SURFEX simulation"
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-b', '--datebegin', type=str,
                        help="First date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-e', '--dateend', type=str,
                        help="Last date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-x', '--xpids', nargs='+', type=str,
                        help="XPID(s) of the simulation(s) format XP_NAME@username")

    parser.add_argument('-a', '--vapp', type=str, default='edelweiss', choices=['s2m', 'edelweiss'],
                        help="Application that produced the target file")

    parser.add_argument('-t', '--thresholds', nargs='+', default=np.arange(2, 30, 6),
                        help='Define bands for clustering (default for uncertainty clustering)')

    parser.add_argument('-c', '--clustering', type=str, default='uncertainty', choices=label_map.keys(),
                        help='Define clustering type')

    parser.add_argument('-w', '--workdir', type=str, default=f'{os.environ["HOME"]}/workdir/EDELWEISS/diag',
                        help='Working directory')

    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    parser.add_argument('--mask', type=str, default=None,
                        help="Absolute path to the mask file (if any)"
                             "WARNING : bad practice")

    parser.add_argument('-u', '--uenv', type=str, default="uenv:edelweiss.2@vernaym",
                        help="User environment for static resources (format 'uenv:name@user')")

    parser.add_argument('-f', '--plot_fields', action='store_true',
                        help="Wether to plot fields or not (avoid duplicates)")

    args = parser.parse_args()
    return args


def main():

    try:
        # Try to get the MASK file with vortexIO
        mask = io.get_const(uenv, 'mask', geometry)
        mask = True
    except (NameError, ModuleNotFoundError):
        if mask is not None:
            # If a mask file was provided in argument, use it
            # Get mask file
            # import shutil
            # shutil.copyfile(mask, 'mask.nc')
            if not os.path.exists('MASK.nc'):  # TODO remplacer le lien par sécurité ?
                os.symlink(mask, 'MASK.nc')
            mask = True

    cluster = cluster_criterion(clustering)

    # 1. Get all input data
    for xpid in xpids:
        # TODO : gérer ça plus proprement
        if '@' not in xpid:
            user = os.environ["USER"]
            xpid = f'{xpid}@{user}'
        shortid = xpid.split('@')[0]
        # VERRUE pour gérer le décallage d'un jour en attendant de combler les données
        if shortid.startswith('safran') and datebegin == '2021080207':
            deb = '2021080106'
        else:
            deb = datebegin  # 2021080207

        # TODO : à gérer autrement pour être flexible
        if shortid in ['safran', 'ANTILOPE', 'safran_pappus', 'ANTILOPE_pappus', 'SAFRAN', 'SAFRAN_pappus']:
            member = None
        else:
            member = 0

        # Get DIAG files with Vortex
        kw = dict(datebegin=deb, dateend=dateend, vapp=vapp, member=member, filename=f'DIAG_{shortid}.nc')
        io.get_diag(xpid=xpid, geometry=geometry, **kw)

    # TODO : à gérer autrement pour être flexible
    member = None

    # 2. Compare simulated data with Sentinel2 data
    # TODO : concaténer les 2 variables dans 1 seul fichier
    # TODO : put/get Sentinel2 data from hendrix (already implemented in vortexIO)
    for var in ['scd_concurent', 'mod']:
        # open Sentinel2 data
        if var == 'mod':
            obs = xr.open_dataset('/home/vernaym/These/DATA/Sentinel2/SMOD_20210901.nc')
        elif var == 'scd_concurent':
            obs = xr.open_dataset('/home/vernaym/These/DATA/Sentinel2/SCD_20210901.nc')
        obs = xrp.preprocess(obs.Band1, decode_time=False)
        cluster = cluster.interp({'xx': obs.xx, 'yy': obs.yy}, method='nearest')

        if mask:
            # mask glacier/forest covered pixels
            obs = ct.maskgf(obs)

        # compare(obs, var=var)
        violin_plot(xpids, obs, var, cluster, member=member)  # Violinplots by elevation range
        if plot_fields:
            plot_all_fields(xpids, obs, var, member=member)  # Field difference

    # 3. Clean data
    for xpid in xpids:
        shortid = xpid.split('@')[0]
        if member is None:
            os.remove(f'DIAG_{shortid}.nc')
        else:
            for member in range(member):
                os.remove(f'mb{member:03d}/DIAG_{shortid}.nc')


def cluster_criterion(clustering):

    if clustering == 'elevation':
        # Get Domain's DEM in case ZS not in simulation file
        io.get_const(uenv, 'relief', geometry, filename='TARGET_RELIEF.nc', gvar='RELIEF_GRANDESROUSSES250M_L93')
        mnt = xr.open_dataset('TARGET_RELIEF.nc')  # Target domain's Digital Elevation Model
        mnt = xrp.preprocess(mnt, decode_time=False)
        mask = mnt.ZS
    elif clustering == 'uncertainty':
        ds = xr.open_dataset('/home/vernaym/workdir/ASSIMILATION/mask/GrandesRousses/Observation_error_L93.nc')
        ds = xrp.preprocess(ds, decode_time=False)
        mask = ds.Uncertainty
    elif clustering == 'landforms':
        # Get Domain's DEM in case ZS not in simulation file
        io.get_const(uenv=uenv, kind='geomorph', geometry=geometry, filename='GEOMORPH.nc')
        geomorph = xr.open_dataset('GEOMORPH.nc')  # Target domain's Geomorphons mask
        mask = xrp.preprocess(geomorph.Band1, decode_time=False)
    mask = mask.rename(clustering)

    return mask


def plot_all_fields(xpids, obs, var, member=None):

    vmin = obs.min()
    vmax = obs.max()

    # Plot observation field
    cmap = plt.cm.Greens  # TODO : chose a better colormap ?
    savename = f'{var}_Sentinel2_{datebegin}_{dateend}.pdf'
    plot2D.plot_field(obs, savename, cmap=cmap, vmin=vmin, vmax=vmax)

    for xpid in xpids:
        shortid = xpid.split('@')[0]
        if member is None:
            simu = xr.open_dataset(f'DIAG_{shortid}.nc', decode_times=False)
        else:
            # Verrue : trouver une solution standard plus propre
            if member == 1:
                # "Deterministic" member=0 by default (WARNING : different from the current 's2m oper' convention)
                simu = xr.open_dataset(f'DIAG_{shortid}.nc', decode_times=False)
            else:
                simu = xr.open_mfdataset([f'mb{mb:03d}/DIAG_{shortid}.nc' for mb in range(member)],
                                         combine='nested', concat_dim='member', decode_times=False)
        simu = xrp.preprocess(simu, decode_time=False)

        if member is not None and member > 1:
            simu['member'] = range(member)
            tmp = simu.mean(dim='member')
        else:
            tmp = simu
        tmp = tmp.compute()
        savename = f'{var}_{shortid}_{datebegin}_{dateend}.pdf'
        cmap = plt.cm.Greens  # TODO : chose a better colormap ?
        plot2D.plot_field(tmp[var], savename, cmap=cmap, vmin=vmin, vmax=vmax)
        savename = f'diff_{var}_{shortid}_{datebegin}_{dateend}.pdf'
        diff = tmp[var] - obs
        plot2D.plot_field(diff, savename, cmap=plt.cm.RdBu, vmin=-100, vmax=100)


def violin_plot(xpids, obs, var, mask, member=None):

    if clustering in ['elevation', 'uncertainty']:
        filtered_obs = clusters.by_slices(obs, mask, thresholds)
    elif clustering == 'landforms':
        filtered_obs = clusters.per_landform_types(obs, mask)
    dataplot = filtered_obs.to_dataframe(name='obs').dropna().reset_index().drop(columns=['xx', 'yy'])

    for xpid in xpids:
        print(xpid)
        shortid = xpid.split('@')[0]
        if member is None:
            subdir = ''
            df = filter_simu(shortid, subdir, mask, var)
        else:
            df = None
            for mb in range(member):
                print(mb)
                subdir = f'mb{mb:03d}'
                dfm = filter_simu(shortid, subdir, mask, var)
                if df is not None:
                    df = pd.concat([df, dfm], ignore_index=True)
                else:
                    df = dfm

        dataplot = pd.concat([dataplot, df])

    dataplot = dataplot.rename(columns={'slices': label_map[clustering], clustering: label_map[clustering]})
    dataplot = dataplot.melt(label_map[clustering], var_name='experiment', value_name=var)

    figname = f'{var}_by_{clustering}_{datebegin}_{dateend}.pdf'
    violinplot.plot_ange(dataplot, var, figname, yaxis=label_map[clustering], violinplot=False, xmax=375)


def filter_simu(xpid, subdir, mask, var):
    diagname = os.path.join(subdir, f'DIAG_{xpid}.nc')
    simu = xr.open_dataset(diagname, decode_times=False)
    simu = xrp.preprocess(simu[var], decode_time=False)
    if clustering in ['elevation', 'uncertainty']:
        filtered_simu = clusters.by_slices(simu, mask, thresholds)
    elif clustering == 'landforms':
        filtered_simu = clusters.per_landform_types(simu, mask)
    df = filtered_simu.to_dataframe(name=xpid).dropna().reset_index().drop(columns=['xx', 'yy'])
    return df


if __name__ == '__main__':

    mntdir = '/home/vernaym/These/DATA'

    args = parse_command_line()
    datebegin       = args.datebegin
    dateend         = args.dateend
    xpids           = args.xpids
    workdir         = args.workdir
    geometry        = args.geometry
    vapp            = args.vapp
    mask            = args.mask
    uenv            = args.uenv
    clustering      = args.clustering
    thresholds      = args.thresholds
    plot_fields     = args.plot_fields

    if not os.path.exists(workdir):
        os.makedirs(workdir)
    os.chdir(workdir)

    main()
    ct.execution_info(workdir)
