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
import matplotlib.pyplot as plt

# Following installation required on sxcen :
# - Install xskillscore : pip install xskillscore
# - Upgarde numba       : pip install numba --upgrade
import xskillscore  # Requires an installation : pip install xskillscore
# https://xskillscore.readthedocs.io/en/stable/api/xskillscore.crps_ensemble.html

import snowtools.tools.xarray_preprocess as xrp
from snowtools.scripts.extract.vortex import vortexIO as io
from snowtools.plots.maps import plot2D
from snowtools.scores import clusters
from snowtools.plots.boxplots import violinplot


members_map = dict(
    RS27_pappus        = [mb for mb in range(17)],
    EnKF36_pappus      = [mb for mb in range(17)],
    PF32_pappus        = [mb for mb in range(17)],
    RS27_sorted_pappus = [mb for mb in range(17)],
    ANTILOPE_pappus    = None,
    SAFRAN_pappus      = None,
)

xpid_map = {
    '2018012312': 'CesarDB_AngeH',
    '2018031612': 'CesarDB_AngeH',
    '2018031612': 'CesarDB_AngeH',
    '2019051312': 'CesarDB_AngeH',
    '2022022612': 'CesarDB',
    '2022050112': 'CesarDB',
}


# Retrieve dictionnary to map clustering type to a proper label
label_map = clusters.label_map


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

    parser.add_argument('-u', '--uenv', type=str, default="uenv:edelweiss.2@vernaym",
                        help="User environment for static resources (format 'uenv:name@user')")

    parser.add_argument('-c', '--clustering', type=str, default='uncertainty', choices=label_map.keys(),
                        help='Define clustering type')

    parser.add_argument('-t', '--thresholds', nargs='+', default=np.arange(2, 30, 6),
                        help='Define bands for clustering (default for uncertainty clustering)')

    parser.add_argument('-w', '--workdir', type=str, default=f'{os.environ["HOME"]}/workdir/EDELWEISS/scores',
                        help='Working directory')

    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    parser.add_argument('-v', '--variable', type=str, default='DSN_T_ISBA',
                        help='Variable of interest (default : SnowDepth)')

    parser.add_argument('-o', '--obs_geometry', type=str, choices=['Lautaret250m', 'Huez250m'], required=True,
                        help='Geometry of the observation')

    parser.add_argument('-m', '--members', action='store_true',
                        help="To activate ensemble simulations")

    args = parser.parse_args()
    return args


def execute():

    # 1. Get all input data

    # a) Pleiades observations
    kw = dict(date=datebegin, vapp=vapp)
    obsname = f'PLEIADES_{date}.nc'
    io.get_snow_obs_date(xpid=xpid_map[date], geometry=obs_geometry, date=date, vapp='Pleiades', filename=obsname)
    # Open observation file as DataArray
    obs = xr.open_dataset(obsname)
    obs = xrp.preprocess(obs, decode_time=False, rename={'Band1': 'DSN_T_ISBA'})

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
    mask = mask.interp({'xx': obs.xx, 'yy': obs.yy}, method='nearest')
    mask = mask.rename(clustering)

    dataplot = pd.DataFrame()
    # c) Simulations
    pearson = dict()
    for xpid in xpids:
        # TODO : gérer ça plus proprement
        if '@' not in xpid:
            user = os.environ["USER"]
            xpid = f'{xpid}@{user}'
        shortid = xpid.split('@')[0]

        # Get (filtered) PRO files with Vortex
        if members:
            member = members_map[shortid]
        else:
            if shortid in ['safran', 'ANTILOPE', 'safran_pappus', 'ANTILOPE_pappus', 'SAFRAN', 'SAFRAN_pappus']:
                member = None
            else:
                member = [0]

        # VERRUE pour gérer le décallage d'un jour en attendant de combler les données
        if (shortid.startswith('SAFRAN') or shortid.startswith('ANTILOPE')) and datebegin == '2021080207':
            deb = '2021080106'
        else:
            deb = datebegin  # 2021080207

        kw = dict(datebegin=deb, dateend=dateend, vapp=vapp, member=member, namebuild=None,
                filename=f'PRO_{shortid}.nc', xpid=xpid, geometry=geometry)
        io.get_pro(**kw)

        simu = read_simu(xpid, member, date)

        if member is not None and len(member) > 1 and clustering in 'elevation':
            plot_ensemble(simu, obs.DSN_T_ISBA, shortid, date)

        pearson[shortid], crps = compute_scores(simu, obs)

        savename = f'CRPS_{shortid}_{date}.pdf'
        vmin = 0
        vmax = 3
        print(f'plot crps {xpid}')
        fig, ax = plot2D.plot_field(crps, vmin=vmin, vmax=vmax, cmap=plt.cm.Reds)
        print(f'save crps {xpid}')
        plot2D.save_fig(fig, savename)

        if clustering in ['elevation', 'uncertainty']:
            tmp = clusters.by_slices(crps, mask, thresholds)
        elif clustering == 'landforms':
            tmp = clusters.per_landform_types(crps, mask)
        df = tmp.to_dataframe(name=shortid).dropna().reset_index().drop(columns=['xx', 'yy', 'time'], errors='ignore')
        dataplot = pd.concat([dataplot, df])

        clean(shortid, member)

    dataplot = dataplot.rename(columns={'slices': label_map[clustering], clustering: label_map[clustering]})
    dataplot = dataplot.melt(label_map[clustering], var_name='experiment', value_name='CRPS (m)')

    title = f'Pleiades, {geometry}, {date[:8]}\n'
    violinplot.plot_ange(dataplot, 'CRPS (m)', figname=f'CRPS_by_{clustering}_{date}_' + '_'.join(xpids),
            title=title, yaxis=label_map[clustering], violinplot=False, xmax=3)

    print()
    if member is not None and len(member) > 1:
        fig, ax = plt.subplots()
        ax.boxplot(pearson.values(), notch=True, labels=pearson.keys())
        ax.legend()
        fig.savefig(f'PearsonCoeff_{date[:8]}.pdf')
    else:
        with open(f'PearsonCoeff_{date[:8]}.csv', 'a') as f:
            for shortid, pearson_corr in pearson.items():
                f.write(f'{shortid};{pearson_corr.data[0]}\n')


def read_simu(xpid, members, date):
    listfiles = list()  # List of simulation PRO files
    shortid = xpid.split('@')[0]
    proname = f'PRO_{shortid}.nc'
    if members is not None and len(members) > 1:
        for mb in members:
            listfiles.append(f'mb{mb:03d}/{proname}')
    else:
        listfiles.append(f'{proname}')

    # Open all simulation PRO files at once
    simu = xr.open_mfdataset(listfiles, concat_dim='member', combine='nested').compute()
    simu = xrp.preprocess(simu, decode_time=False)
    # <xarray.Dataset>
    # Dimensions:     (time: 3, xx: 143, yy: 101, member: 16)
    # Coordinates:
    #   * time        (time) datetime64[ns] 2018-01-23 2018-03-16
    #   * xx          (xx) float64 9.379e+05 9.381e+05 ... 9.731e+05 9.734e+05
    #   * yy          (yy) float64 6.439e+06 6.439e+06 ... 6.464e+06 6.464e+06
    # Dimensions without coordinates: xpid
    # Data variables:
    #     DSN_T_ISBA  (member, time, yy, xx) float64 1.997 1.918 ... 0.0001194 0.2854
    # Get variable's DataArray
    simu = simu.sel({'time': pd.to_datetime(date[:8], format='%Y%m%d')})
    # <xarray.Dataset>
    # Dimensions:     (xx: 143, yy: 101, member: 16)
    # Coordinates:
    #   time        datetime64[ns] 2018-01-23
    #   * xx        (xx) float64 9.379e+05 9.381e+05 ... 9.731e+05 9.734e+05
    #   * yy        (yy) float64 6.439e+06 6.439e+06 ... 6.464e+06 6.464e+06
    # Dimensions without coordinates: xpid
    # Data variables:
    #     DSN_T_ISBA  (member, time, yy, xx) float64 1.997 1.918 ... 0.0001194 0.2854

    # Set the 'xpid' dimension for simulation identification
    if members is not None:
        simu['member'] = members
    else:
        simu['member'] = [0]
    # <xarray.Dataset>
    # Dimensions:     (xx: 143, yy: 101, member: 16)
    # Coordinates:
    #   time        datetime64[ns] 2018-01-23
    #   * xx        (xx) float64 9.379e+05 9.381e+05 ... 9.731e+05 9.734e+05
    #   * yy        (yy) float64 6.439e+06 6.439e+06 ... 6.464e+06 6.464e+06
    #   * member    (member) int64 1 2 3 ... 16
    # Dimensions without coordinates: xpid
    # Data variables:
    #     DSN_T_ISBA  (member, time, yy, xx) float64 1.997 1.918 ... 0.0001194 0.2854

    return simu


def plot_ensemble(simu, obs, xpid, date):
    mean = simu.DSN_T_ISBA.mean(dim='member')
    savename = f'Mean_HTN_{xpid}_{date}.pdf'
    vmin = 0
    vmax = 3
    print(f'plot mean {xpid}')
    fig, ax = plot2D.plot_field(mean, vmin=vmin, vmax=vmax, cmap=plt.cm.Blues)
    print(f'save mean {xpid}')
    plot2D.save_fig(fig, savename)

    spread = simu.DSN_T_ISBA.std(dim='member')
    savename = f'Spread_HTN_{xpid}_{date}.pdf'
    vmin = 0
    vmax = 1
    print(f'plot spread {xpid}')
    fig, ax = plot2D.plot_field(spread, vmin=vmin, vmax=vmax, cmap=plt.cm.Purples)
    print(f'save spread {xpid}')
    plot2D.save_fig(fig, savename)

    error = mean - obs
    savename = f'Error_HTN_{xpid}_{date}.pdf'
    print(f'plot error {xpid}')
    fig, ax = plot2D.plot_field(error, cmap=plt.cm.RdBu)
    print(f'save error {xpid}')
    plot2D.save_fig(fig, savename)


def compute_scores(simu, obs):

    # Select common domains
    obs  = obs.sel({'xx': np.intersect1d(obs.xx, simu.xx), 'yy': np.intersect1d(obs.yy, simu.yy)})['DSN_T_ISBA']
    simu = simu.sel({'xx': np.intersect1d(obs.xx, simu.xx), 'yy': np.intersect1d(obs.yy, simu.yy)})['DSN_T_ISBA']
    # Mask missing values from the observatin dataset in the simulation dataset
    simu = simu.where(~np.isnan(obs))
    # <xarray.Dataset>
    # Dimensions:     (member: 16, yy: 79, xx: 63)
    # Coordinates:
    #  * xx          (xx) float64 9.579e+05 9.581e+05 ... 9.731e+05 9.734e+05
    #  * yy          (yy) float64 6.439e+06 6.439e+06 ... 6.458e+06 6.459e+06
    # Dimensions without coordinates: xpid
    # Data variables:
    #    DSN_T_ISBA  (member, yy, xx) float64 nan nan 1.132 1.234 ... 1.918 1.997  nan nan

    # xskillscore.crps_ensemble only allows to compute the mean CRPS along 1 or several dimensions.
    # We want a CRPS for each pixel of the domain, so we add a "fake" dimension to cmpute
    # the CRPS along this dimension and get 1 value per pixel

    # control_member = simu.sel({'member': 0})
    pearson = xr.corr(simu, obs, dim=['xx', 'yy'])
    pearson = pearson[~np.isnan(pearson)]  # Remove nan values (all 0s simulations)

    simu = simu.expand_dims(dim="time")
    obs  = obs.expand_dims(dim="time")
    crps = xskillscore.crps_ensemble(obs, simu, dim='time')

    return pearson, crps


def clean(xpid, members):
    if members is not None and len(members) > 1:
        for member in members:
            os.remove(f'mb{member:03d}/PRO_{xpid}.nc')
    else:
        os.remove(f'PRO_{xpid}.nc')


def execution_info(workdir):
    print()
    print("===========================================================================================")
    print("                                     Execution result                                      ")
    print("===========================================================================================")
    print()
    print(f"Produced figures are available here : {workdir}")
    print()


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
    obs_geometry    = args.obs_geometry
    clustering      = args.clustering
    thresholds      = args.thresholds
    members         = args.members

#    if ':' in args.members:
#        first_mb, last_mb = args.members.split(':')
#        members         = [mb for mb in range(int(first_mb), int(last_mb) + 1)]
#    else:
#        members = None

    if not os.path.exists(workdir):
        os.makedirs(workdir)
    os.chdir(workdir)

    execute()
    execution_info(workdir)
