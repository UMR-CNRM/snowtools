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
from snowtools.scripts.post_processing import common_dict
from snowtools.tools import common_tools as ct

members_map = common_dict.members_map
product_map = common_dict.product_map
xpid_map    = common_dict.xpid_map
colors_map  = common_dict.colors_map
vmax_map    = common_dict.vmax_map

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

    parser.add_argument('-u', '--uenv', type=str, default="uenv:edelweiss.3@vernaym",
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

    parser.add_argument('-o', '--obs_geometry', type=str, choices=['Huez250m', 'GrandesRousses250m'],
                        required=True, help='Geometry of the observation')

    parser.add_argument('-m', '--members', action='store_true',
                        help="To activate ensemble simulations")

    args = parser.parse_args()
    return args


def execute():

    # 1. Get all input data

    # a) Pleiades observations
    obsname = f'PLEIADES_{date}.nc'
    io.get_snow_obs_date(xpid=xpid_map[date], geometry=obs_geometry, date=date, vapp='Pleiades', filename=obsname)
    # Open observation file as DataArray
    try:
        obs = xr.open_dataarray(obsname)
        obs = xrp.preprocess(obs, decode_time=False)
    except ValueError:
        obs = xr.open_dataset(obsname)
        obs = xrp.preprocess(obs, decode_time=False, mapping={'Band1': 'HTN', 'DEP': 'HTN', 'DSN_T_ISBA': 'HTN'})
        obs = obs['HTN']

    # b) DEM
    # io.get_const(uenv, 'relief', geometry, filename='TARGET_RELIEF.nc', gvar='RELIEF_GRANDESROUSSES250M_L93')
    # High-resolution (25m) DEM for fancy figures (set shade=True in plot_field calls)
    io.get_const('uenv:dem.2@vernaym', 'relief', geometry, filename='TARGET_RELIEF.nc',
            gvar='DEM_GRANDESROUSSES25M_L93')

    # Get Domain's DEM in case ZS not in simulation file
    mnt = xr.open_dataset('TARGET_RELIEF.nc')  # Target domain's Digital Elevation Model
    mnt = xrp.preprocess(mnt, decode_time=False)
    mnt = mnt['ZS']

    if clustering == 'elevation':
        mask = mnt
    elif clustering == 'uncertainty':
        io.get_const(uenv=uenv, kind='geomorph', geometry=geometry, filename='Estimated_error.nc',
                gvar='ANTILOPE_ERROR_ALP1KM_RS27')
        ds = xr.open_dataset('Estimated_error.nc')
        ds = ct.proj_array(ds)
        ds = xrp.preprocess(ds, decode_time=False)
        mask = ds.Uncertainty
    elif clustering == 'ratio':
        io.get_const(uenv=uenv, kind='geomorph', geometry=geometry, filename='Estimated_ratio.nc',
                gvar='ANTILOPE_RATIO_ALP1KM_RS27')
        ds = xr.open_dataset('Estimated_ratio.nc')
        ds = ct.proj_array(ds)
        ds = xrp.preprocess(ds, decode_time=False)
        mask = ds.Ratio
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

    if clustering in 'elevation':
        safran_elevations = [z for z in reversed(range(1800, 3000, 300))]
        fig0, ax0 = plt.subplots(len(safran_elevations), 1, figsize=(14, 14))
        im = list()  # List of products for common legend
        #im.append(plt.plot([], [], color='k', label='Observation', linewidth=3))
        for i, elevation in enumerate(safran_elevations):
            tmp = obs.where((mnt > elevation - 150) & (mnt <= elevation + 150), drop=True)
            tmp.mean('yy').plot(ax=ax0[i], color='k', linewidth=3)
            # Add empty plot for common legend

    for xpid in xpids:
        # TODO : gérer ça plus proprement
        if '@' not in xpid:
            user = os.environ["USER"]
            xpid = f'{xpid}@{user}'
        shortid = xpid.split('@')[0]

        # Get (filtered) PRO files with Vortex
        if members:
            member = members_map(shortid)
        else:
            if shortid in ['ANTILOPE', 'safran_pappus', 'ANTILOPE_pappus', 'SAFRAN', 'SAFRAN_pappus', 'AROME_pappus']:
                member = None
            elif shortid in ['EnKF36_pappus', 'PF32_pappus']:
                member = [1]
            else:
                member = [members_map(shortid)[0]]
        name = product_map(shortid)

        # VERRUE pour gérer le décallage d'un jour en attendant de combler les données
        if (shortid.split('_')[0] in ['SAFRAN', 'ANTILOPE', 'KRIGING', 'AROME']) and datebegin == '2021080207':
            deb = '2021080106'
        else:
            deb = datebegin  # 2021080207
        kw = dict(datebegin=deb, dateend=dateend, vapp=vapp, member=member, namebuild=None,
                filename=f'PRO_{shortid}.nc', xpid=xpid, geometry=geometry)
        io.get_pro(**kw)

        simu = read_simu(xpid, member, date)

        if clustering in 'elevation' and (member is None or len(member) == 1):
            # Add empty plot for common legend
            #im.append(plt.plot([], [], color=colors_map[name], label=name, linewidth=3))
            for i, elevation in enumerate(safran_elevations):
                tmpobs = obs.where((obs.notnull()) & (mnt > elevation - 150) & (mnt <= elevation + 150))
                tmp = simu.where(tmpobs.notnull()).squeeze()
                #tmp = tmp.where((mnt > elevation - 150) & (mnt <= elevation + 150))
                #tmp = tmp.where(~np.isnan(obs))
                # Compute Pearson correlation for this elevation band
                p = xr.corr(tmp, tmpobs, dim=['xx', 'yy'])
                label = f'Pearson={np.round(p.data, 2):.2f}'
                tmp.mean('yy').plot(ax=ax0[i], color=colors_map[name], label=label, linewidth=3)

            if member is not None and len(member) > 1:
                plot_ensemble(simu, obs, shortid, date, dem=mnt)
            else:
                plot_deterministe(simu, obs, shortid, date, dem=mnt, member=member)

        if clustering in 'elevation' and (member is None or len(member) == 1):
            plot_HTN = True
        else:
            plot_HTN = False

        if (member is None or len(member) == 1) and clustering != 'uncertainty':
            # Uncertainty is a measure of the absolute error
            pearson[shortid], crps = compute_scores(simu, obs, shortid, date, plot_HTN, dem=mnt, deterministic=True)
            vmax = vmax_map[date]  # set colobar extend in CRPS/Error plot
            vmin = -vmax
            xmax = vmax + 2  # xlim in violinplot (Add margin for legend)
            cmap = plt.cm.RdBu
            label = 'Error (m)'
        else:
            pearson[shortid], crps = compute_scores(simu, obs, shortid, date, plot_HTN, dem=mnt)
            vmin = 0
            vmax = vmax_map[date]
            xmax = vmax + 2  # xlim in violinplot (Add margin for legend)
            cmap = plt.cm.Reds
            label = 'CRPS (m)'
        crps = crps.rename(label)

        # if False:
        # if True:
        if member is not None and len(member) > 1 and clustering in ['elevation']:
            savename = f'CRPS_{shortid}_{date}.pdf'
            print(f'plot crps {xpid}')
            # To plot data by elevation cluster :
            # tmp = mnt.interp({'xx': crps.xx, 'yy': crps.yy})
            # plot2D.plot_field(crps.where((tmp.data>2000) & (tmp.data<=2500)), vmin=vmin, vmax=vmax, cmap=cmap,
            #    dem=mnt, shade=False)
            plot2D.plot_field(crps, vmin=vmin, vmax=vmax, cmap=cmap, dem=mnt, shade=False)
            print(f'save crps {xpid}')
            plot2D.save_fig(savename)

        if clustering in ['elevation', 'uncertainty', 'ratio']:
            tmp = clusters.by_slices(crps, mask, thresholds)
        elif clustering == 'landforms':
            tmp = clusters.per_landform_types(crps, mask)
        if member is None or len(member) == 1:  # Deterministic case
            name = name.split('_')[0]
        df = tmp.to_dataframe(name=name).dropna().reset_index().drop(
            columns=['xx', 'yy', 'time', 'band', 'spatial_ref'], errors='ignore')
        dataplot = pd.concat([dataplot, df])

        clean(shortid, member)

    if clustering in 'elevation':
        for i, elevation in enumerate(safran_elevations):
            ax0[i].legend(loc='upper right', ncol=3, fontsize=10)
            ax0[i].set_title(f'{elevation-150}m - {elevation+150}m')
            ax0[i].set_ylabel('')
            if elevation != safran_elevations[-1]:
                ax0[i].set_xlabel('')
                ax0[i].set_xticklabels([])
        fig0.supylabel("Mean snow depth (m)")
        # ax0[0].legend(loc='upper center', bbox_to_anchor=(0.5, 1.5), ncol=len(xpids) + 1)
        #fig0.legend(im, loc='upper center', bbox_to_anchor=(0.5, 1.1), ncol=3)
        plt.tight_layout()
        fig0.savefig(f'Gradient_HTN_WE_{date}_' + '_'.join(xpids) + '.pdf')
        plt.close('all')

    dataplot = dataplot.rename(columns={'slices': label_map[clustering], clustering: label_map[clustering]})
    dataplot = dataplot.melt(label_map[clustering], var_name='experiment', value_name=label)

    title = f'Pleiades, {geometry}, {date[:8]}\n'
    violinplot.plot_ange(dataplot, label, figname=f'{label.split(" ")[0]}_by_{clustering}_{date}_' + '_'.join(xpids),
            title=title, yaxis=label_map[clustering], violinplot=False, xmin=vmin, xmax=xmax, hatchid='assim',
            colors=colors_map)

    print()
    if member is not None and len(member) > 1:
        plt.figure()
        plt.boxplot(pearson.values(), notch=True, labels=pearson.keys())
        plt.legend()
        plt.ylim(0, 1)
        plt.savefig(f'PearsonCoeff_{date[:8]}.pdf')
        plt.close()
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
    simu = simu.sel({'time': pd.to_datetime(date[:8], format='%Y%m%d')}, method='nearest')
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

    simu = simu.DSN_T_ISBA.rename('Snow depth (m)')

    return simu


def plot_ensemble(simu, obs, xpid, date, dem=None):
    savename = f'HTN_{xpid}_{date}.pdf'

    fig, ax = plt.subplots(1, 3, figsize=(36 * len(simu.xx) / len(simu.yy), 10), sharey=True)

    mean = simu.mean(dim='member').rename('Ensemble mean snow depth (m)')
    vmin = 0
    vmax = vmax_map[date]
    print(f'plot mean {xpid}')
    plot2D.plot_field(mean, ax=ax[0], vmin=vmin, vmax=vmax, cmap=plt.cm.Blues, dem=dem, shade=False,
            isolevels=thresholds)
    #        shade=True,)
    # ax[0].set_title('Ensemble mean snow depth (m)')

    spread = simu.std(dim='member').rename('Ensemble spread(m)')
    smin = 0
    smax = 1
    print(f'plot spread {xpid}')
    plot2D.plot_field(spread, ax=ax[1], vmin=smin, vmax=smax, cmap=plt.cm.Purples, dem=dem, shade=False,
            isolevels=thresholds)
    #        shade=True,)
    # ax[1].set_title('Ensemble spread (m)')

    error = (mean - obs).rename('Mean snow depth error (m)')
    vmin = -vmax
    print(f'plot error {xpid}')
    plot2D.plot_field(error, ax=ax[2], vmin=vmin, vmax=vmax, cmap=plt.cm.RdBu, dem=dem, shade=False,
            isolevels=thresholds)
    #        shade=True,)
    # ax[2].set_title('Ensemble mean error (m)')

    plot2D.save_fig(savename, fig)


def plot_deterministe(simu, obs, xpid, date, dem=None, member=None):

    if member is not None:
        savename = f'HTN_error_{xpid}_mb{member}_{date}.pdf'
    else:
        savename = f'HTN_error_{xpid}_{date}.pdf'

    fig, ax = plt.subplots(1, 2, figsize=(24 * len(simu.xx) / len(simu.yy), 10), sharey=True)

    field = simu.rename('Snow depth (m)')
    vmin = 0
    vmax = vmax_map[date]
    print(f'plot HTN {xpid}')
    plot2D.plot_field(field, ax=ax[0], vmin=vmin, vmax=vmax, cmap=plt.cm.Blues, dem=dem, shade=False,
            isolevels=thresholds)
    #        shade=True,)
    # ax[0].set_title('Snow depth (m)')

    error = (field - obs).rename('Snow depth error (m)')
    vmin = -vmax
    print(f'plot error {xpid}')
    plot2D.plot_field(error, ax=ax[1], vmin=vmin, vmax=vmax, cmap=plt.cm.RdBu, dem=dem, shade=False,
            isolevels=thresholds)
    #        shade=True,)
    # ax[1].set_title('Error (m)')

    plot2D.save_fig(savename, fig)


def compute_scores(simu, obs, xpid, date, plot_HTN, dem=None, deterministic=False):

    # Select common domains
    obs  = obs.sel({'xx': np.intersect1d(obs.xx, simu.xx), 'yy': np.intersect1d(obs.yy, simu.yy)})
    simu = simu.sel({'xx': np.intersect1d(obs.xx, simu.xx), 'yy': np.intersect1d(obs.yy, simu.yy)})

    if plot_HTN:
        savename = f'HTN_{xpid}_{date}.pdf'
        plt.figure(figsize=(12 * len(simu.xx) / len(simu.yy), 10))
        vmin = 0
        vmax = vmax_map[date]
        plot2D.plot_field(simu.squeeze(), vmin=vmin, vmax=vmax, cmap=plt.cm.Blues, dem=dem, shade=False,
                isolevels=thresholds)
        #        shade=True,)
        plot2D.save_fig(savename)

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
    pearson = xr.corr(simu.where(obs > 0, drop=True), obs.where(obs > 0, drop=True), dim=['xx', 'yy'])
    pearson = pearson[~np.isnan(pearson)]  # Remove nan values (all 0s simulations)

    # TODO : check https://scikit-image.org/docs/stable/auto_examples/transform/plot_ssim.html

    # simu = simu.expand_dims(dim="time")
    # obs  = obs.expand_dims(dim="time")
    if deterministic:
        if 'member' in simu.dims:
            simu = simu.squeeze().drop('member')
        crps = simu - obs
    else:
        crps = xskillscore.crps_ensemble(obs, simu, dim=[])

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
