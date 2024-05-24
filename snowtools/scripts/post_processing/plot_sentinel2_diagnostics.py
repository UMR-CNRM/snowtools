#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 18 march 2024

@author: Vernay. M (core code comes from Haddjeri. A)

WORK IN PROGRESS

example:
--------
>>> p plot_sentinel2_diagnostics.py -b 2021080207 -e 2022080106
    -x safran@vernaym safran_pappus@vernaym ANTILOPE@vernaym ANTILOPE_pappus@vernaym RS27@vernaym RS27_pappus@vernaym

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

members_map = dict(
    safran         = None,
    safran_pappus  = None,
    RawData        = None,
    RawData_pappus = None,
    RS27           = 1,  # post-processed ANTILOPE only
    RS27_pappus    = 1,  # post-processed ANTILOPE only
    # RS27           = 17,  # All members
    # RS27_pappus    = 17,  # All members
    EnKF36         = range(1, 17),  # TODO : syntaxe à implémenter
    EnKF36_pappus  = range(1, 17),  # TODO : syntaxe à implémenter
)


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

    parser.add_argument('-z', '--elevation_bands', nargs='+', type=int, default=np.arange(1900, 3600, 300),
                        help='Define elevation bands for clustering')

    parser.add_argument('-w', '--workdir', type=str, default=f'{os.environ["HOME"]}/workdir/EDELWEISS/diag',
                        help='Working directory')

    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    parser.add_argument('--mask', type=str,
                        help="Absolute path to the mask file (if any)"
                             "WARNING : bad practice")

    parser.add_argument('-u', '--uenv', type=str, default="uenv:edelweiss.1@vernaym",
                        help="User environment for static resources (format 'uenv:name@user')")

    args = parser.parse_args()
    return args


def read_mnt():
    mnt = xr.open_dataset(os.path.join(mntdir, "MNTLouisGRoussecorrected.nc"))
    return mnt


def plot_fields(xpids, obs, var, member=None):

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
        if 'x' in simu.keys():
            simu = simu.rename({'x': 'xx', 'y': 'yy'})
        # TODO : résoudre le problème de décallage des coordonnées en amont (dans OPTIONS.nam)
        simu['xx'] = obs['xx']
        simu['yy'] = obs['yy']

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


def violin_plot(xpids, obs, var, member=None, mask=True):
    mnt = read_mnt()

    filtered_obs = clusters.per_alt(obs, elevation_bands, mnt.ZS)
    dataplot = filtered_obs.to_dataframe(name='obs').dropna().reset_index().drop(columns=['xx', 'yy'])

    for xpid in xpids:
        print(xpid)
        shortid = xpid.split('@')[0]
        if member is None:
            subdir = ''
            df = filter_simu(shortid, subdir, mnt)
        else:
            df = None
            for mb in range(member):
                print(mb)
                subdir = f'mb{mb:03d}'
                dfm = filter_simu(shortid, subdir, mnt)
                if df is not None:
                    df = pd.concat([df, dfm], ignore_index=True)
                else:
                    df = dfm

        dataplot = pd.concat([dataplot, df])

    dataplot.columns = dataplot.columns.str.replace('middle_slices_ZS', 'Elevation Bands (m)')
    dataplot = dataplot.melt('Elevation Bands (m)', var_name='experiment', value_name=var)

    figname = f'{var}_{datebegin}_{dateend}.pdf'
    violinplot.plot_ange(dataplot, var, figname, xmax=375, violinplot=False)


def filter_simu(xpid, subdir, mnt):
    diagname = os.path.join(subdir, f'DIAG_{xpid}.nc')
    simu = xr.open_dataset(diagname, decode_times=False)
    # TODO : gérer le problème de coordonnées pour éviter les "rename" très lents !
    if 'x' in simu.keys():
        simu = simu.rename({'x': 'xx', 'y': 'yy'})
    # TODO : résoudre le problème de décallage des coordonnées en amont
    simu['xx'] = mnt['xx']
    simu['yy'] = mnt['yy']
    filtered_simu = clusters.per_alt(simu[var], elevation_bands, mnt.ZS)
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
    elevation_bands = args.elevation_bands
    mask            = args.mask
    uenv            = args.uenv

    if not os.path.exists(workdir):
        os.makedirs(workdir)
    os.chdir(workdir)

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
        if shortid in ['safran', 'ANTILOPE', 'safran_pappus', 'ANTILOPE_pappus']:
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

        if mask:
            from snowtools.scripts.post_processing import common_tools as ct
            # mask glacier/forest covered pixels
            obs = ct.maskgf(obs)

        # compare(obs, var=var)
        violin_plot(xpids, obs['Band1'], var, member=member)  # Violinplots by elevation range
        plot_fields(xpids, obs['Band1'], var, member=member)  # Field difference

    # 3. Clean data
    for xpid in xpids:
        shortid = xpid.split('@')[0]
        if member is None:
            os.remove(f'DIAG_{shortid}.nc')
        else:
            for member in range(member):
                os.remove(f'mb{member:03d}/DIAG_{shortid}.nc')
