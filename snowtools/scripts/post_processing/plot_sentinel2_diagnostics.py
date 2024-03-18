#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 18 march 2024

@author: Vernay

WORK IN PROGRESS

example:
--------
>>> p plot_sentinel2_diagnostics.py -b 2021080207 -e 2022080106
    -x safran@vernaym safran_pappus@vernaym RawData@vernaym RawData_pappus@vernaym RS27@vernaym RS27_pappus@vernaym

'''

import os
import numpy as np
import pandas as pd
import xarray as xr
import argparse

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
    parser.add_argument('-z', '--elevation_bands', nargs='+', type=int, default=np.arange(1900, 3600, 300),
                        help='Define elevation bands for clustering')
    parser.add_argument('-w', '--workdir', type=str, default=f'{os.environ["HOME"]}/workdir/EDELWEISS/diag',
                        help='Working directory')
    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    args = parser.parse_args()
    return args


def read_mnt():
    mnt = xr.open_dataset(os.path.join(mntdir, "MNTLouisGRoussecorrected.nc"))
    return mnt


def plot_error_fields(xpids, obs, var):
    for xpid in xpids:
        shortid = xpid.split('@')[0]
        members = members_map[shortid]
        if members is None:
            simu = xr.open_dataset(f'DIAG_{shortid}.nc', decode_times=False)
        else:
            # Verrue : trouver une solution standard plus propre
            if members == 1:
                # "Deterministic" member=0 by default (WARNING : different from the current 's2m oper' convention)
                simu = xr.open_dataset(f'mb000/DIAG_{shortid}.nc', decode_times=False)
            else:
                simu = xr.open_mfdataset([f'mb{member:03d}/DIAG_{shortid}.nc' for member in range(members)],
                                         combine='nested', concat_dim='member', decode_times=False)
        if 'x' in simu.keys():
            simu = simu.rename({'x': 'xx', 'y': 'yy'})
        # TODO : résoudre le problème de décallage des coordonnées en amont (dans OPTIONS.nam)
        simu['xx'] = obs['xx']
        simu['yy'] = obs['yy']

        if members is not None and members > 1:
            simu['member'] = range(members)
            tmp = simu.mean(dim='member')
        else:
            tmp = simu
        tmp = tmp.compute()
        savename = f'diff_{var}_{shortid}.pdf'
        vmax = 100
        plot2D.plot_error_fields(tmp[var], obs['Band1'], var, savename, vmax=vmax)


def violin_plot(xpids, obs, var, mask=True):
    mnt = read_mnt()

    filtered_obs = clusters.per_alt(obs.Band1, elevation_bands, mnt)
    dataplot = filtered_obs.to_dataframe(name='obs').dropna().reset_index().drop(columns=['xx', 'yy'])

    for xpid in xpids:
        print(xpid)
        shortid = xpid.split('@')[0]
        members = members_map[shortid]
        if members is None:
            subdir = ''
            df = filter_simu(shortid, subdir, mnt)
        else:
            df = None
            for member in range(members):
                print(member)
                subdir = f'mb{member:03d}'
                dfm = filter_simu(shortid, subdir, mnt)
                if df is not None:
                    df = pd.concat([df, dfm], ignore_index=True)
                else:
                    df = dfm

        dataplot = pd.concat([dataplot, df])

    dataplot.columns = dataplot.columns.str.replace('middle_slices_ZS', 'Elevation Bands (m)')
    dataplot = dataplot.melt('Elevation Bands (m)', var_name='experiment', value_name=var)

    figname = f'{var}.pdf'
    violinplot.plot_ange(dataplot, var, figname)


def filter_simu(xpid, subdir, mnt):
    diagname = os.path.join(subdir, f'DIAG_{xpid}.nc')
    simu = xr.open_dataset(diagname, decode_times=False)
    # TODO : gérer le problème de coordonnées pour éviter les "rename" très lents !
    if 'x' in simu.keys():
        simu = simu.rename({'x': 'xx', 'y': 'yy'})
    # TODO : résoudre le problème de décallage des coordonnées en amont
    simu['xx'] = mnt['xx']
    simu['yy'] = mnt['yy']
    filtered_simu = clusters.per_alt(simu[var], elevation_bands, mnt)
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
    elevation_bands = args.elevation_bands

    if not os.path.exists(workdir):
        os.makedirs(workdir)
    os.chdir(workdir)

    # 1. Get all input data
    for xpid in xpids:
        # TODO : gérer ça plus proprement
        if '@' not in xpid:
            user = os.environ["USER"]
            xpid = f'{xpid}@{user}'
        shortid = xpid.split('@')[0]
        # VERRUE pour gérer le décallage d'un jour en attendant de combler les données
        if shortid.startswith('safran'):
            deb = '2021080106'
        else:
            deb = datebegin  # 2021080207
        members = members_map[shortid]
        # Get DIAG files with Vortex
        io.get_diag(deb, dateend, xpid, geometry, vapp='edelweiss', members=members, filename=f'DIAG_{shortid}.nc')

    # 2. Compare simulated data with Sentinel2 data
    # TODO : concaténer les 2 variables dans 1 seul fichier
    # TODO : put/get Sentinel2 data from hendrix (already implemented in vortexIO)
    for var in ['scd_concurent', 'mod']:
        # open Sentinel2 data
        if var == 'mod':
            obs = xr.open_dataset('/home/vernaym/These/DATA/Sentinel2/SMOD_20210901.nc')
        elif var == 'scd_concurent':
            obs = xr.open_dataset('/home/vernaym/These/DATA/Sentinel2/SCD_20210901.nc')

        # compare(obs, var=var)
        violin_plot(xpids, obs, var)  # Violinplots by elevation range
        plot_error_fields(xpids, obs, var)  # Field difference

    # 3. Clean data
    for xpid in xpids:
        shortid = xpid.split('@')[0]
        members = members_map[shortid]
        if members is None:
            os.remove(f'DIAG_{shortid}.nc')
        else:
            for member in range(members):
                os.remove(f'mb{member:03d}/DIAG_{shortid}.nc')
