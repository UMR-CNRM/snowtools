#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 5 april 2024

@author: Vernay

WORK IN PROGRESS

example:
--------

To plot yearly accumulations of Rainf, Snowf and the sum of the 2 (total precipitation):

>>> p plot_variable.py -b 2017080106 -e 2018080106 -x Safran_tc@haddjeria -g gr250ls -a s2m -k FORCING
           -v Rainf+Snowf Rainf Snowf

>>> p plot_variable.py -b 2017080106 -e 2018080106 -x RS27@vernaym -g GrandesRousses250m -k FORCING -m
           -v Rainf+Snowf Rainf Snowf

Plot simulated snow depths

>>> p plot_variable.py -b 2017080106 -e 2018080106 -x SAFRAN SAFRAN_pappus ANTILOPE ANTILOPE_pappus -v DSN_T_ISBA
           -d 20180123

Plot observed snow depth (Pleiade)

>>> p plot_variable.py -b 2017080106 -e 2018080106 -x CesarDB_AngeH -g Lautaret250m -a Pleiades -v DSN_T_ISBA
           -k snow_obs_date -d 2018012312

'''

import os
import xarray as xr
import argparse

from bronx.stdtypes.date import Date

import footprints

from snowtools.scripts.extract.vortex import vortexIO as io
from snowtools.plots.maps import plot2D
from snowtools.tools import common_tools as ct
import snowtools.tools.xarray_preprocess as xrp

import matplotlib.pyplot as plt


cmap = dict(
    DSN_T_ISBA = plt.cm.Blues,
)

vmax_map = dict(
    # WARNING : values defined for 2017/2018
    DSN_T_ISBA  = 3,  # m
    # DSN_T_ISBA  = 6,  # m
    Snowf       = 1200,  # kg/m² [mm]
    Rainf       = 1400,  # kg/m² [mm]
    RainfSnowf  = 1400,  # kg/m² [mm]
    SnowfRainf  = 1400,  # kg/m² [mm]
)

vmin_map = dict(
    # WARNING : values defined for 2017/2018
    DSN_T_ISBA  = 0,  # m
    Snowf       = 0,  # kg/m² [mm]
    Rainf       = 400,  # kg/m² [mm]
    RainfSnowf  = 400,  # kg/m² [mm]
    SnowfRainf  = 400,  # kg/m² [mm]
)

domain_map = dict(
    Lautaret = dict(xmin=957875., xmax=973375., ymin=6439125., ymax=6458625.),
    Huez2019 = dict(xmin=941625., xmax=951875., ymin=6441125., ymax=6459625.),
    Huez2022 = dict(xmin=937875., xmax=973375., ymin=6464125., ymax=6439125.),
)

reference_point = dict(
    Lautaret = (965767.64, 6445415.30),  # Nivose Galibier
    Huez2019     = (944584.42, 6452410.74),  # Nivose col du lac Blanc
    Huez2022 = (942705.64, 6447916.82),  # Poste nivometeo Huez (1860m)
)


def parse_command_line():
    description = "Computation of Sentinel2-like diagnostics (snow melt-out date, snow cover duration) associated \
                   to a SURFEX simulation"
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-b', '--datebegin', type=str, default=None,
                        help="First date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-e', '--dateend', type=str, default=None,
                        help="Last date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-x', '--xpids', nargs='+', type=str, required=True,
                        help="XPID(s) of the simulation(s) format XP_NAME@username")

    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    parser.add_argument('-k', '--kind', type=str, default='PRO', choices=['FORCING', 'PRO', 'snow_obs_date'],
                        help='kind file containing the variable(s) to plot')

    parser.add_argument('-m', '--ensemble', action='store', default=None, choices=['mean', 'first'],
                        help="If the file comes from an ensemble, either plot the ensemble mean or only"
                             "the first member (the control member by convention)")

    parser.add_argument('-a', '--vapp', type=str, default='edelweiss', choices=['s2m', 'edelweiss', 'Pleiades'],
                        help="Application that produced the target file")

    parser.add_argument('-u', '--uenv', type=str, default="uenv:edelweiss.1@vernaym",
                        help="User environment for static resources (format 'uenv:name@user')")

    parser.add_argument('-n', '--uenv_dem', type=str, default="uenv:dem.1@vernaym",
                        help="User environment for static resources (format 'uenv:name@user')")

    parser.add_argument('-v', '--variables', nargs='+', required=True,
                        help="Variable(s) name to plot (see list for each specific file)"
                             "It is possible to plot a combination of variables, for example :"
                             "-v Rainf+Snowf will plot the total precipitation from a FORCING file")

    parser.add_argument('-d', '--date', type=str, default=None,
                        help="Plot the variable for a specific date."
                             "If no *date* is provided, plot the variable's accumulation over the simulation period")

    parser.add_argument('--domain', type=str, default=None, choices=domain_map.keys(),
                        help="Plot a specific sub-domain.")

    parser.add_argument('-w', '--workdir', type=str, default=f'{os.environ["HOME"]}/workdir/EDELWEISS/plot',
                        help='Working directory')

    parser.add_argument('--mask', type=str, default=None,
                        help="Absolute path to the mask file (if any)"
                             "WARNING : bad practice")

    args = parser.parse_args()
    return args


def plot_var(ds, variables, xpid, date=None, mask=True):

    if mask:
        # mask glacier/forest covered pixels
        ds = ct.maskgf(ds)

    if any(['Rainf' in var for var in variables]):
        ds['Rainf'] = ds['Rainf'] * 3600.
    if any(['Snowf' in var for var in variables]):
        ds['Snowf'] = ds['Snowf'] * 3600.

    for var in variables:

        if '+' in var:
            # Compute the sum of all variables in 'var'
            # The solution to that comes from :
            # https://stackoverflow.com/questions/69537081/sum-data-variables-of-dataset
            sumvar = var.split('+')
            extract = ds[sumvar]  # ex : ['Rainf', 'Snowf']
            tmp = extract.to_array().sum("variable")
            var = var.replace('+', '')
            tmp.name = var
        else:
            tmp = ds[var]

        if date is not None:
            date = Date(date)
            if 'time' in tmp.dims:
                tmp = tmp.sel({'time': date})
            savename = f'{var}_{date.ymdh}_{xpid}.pdf'
        else:
            if 'member' in list(tmp.dims):
                tmp = tmp.sum('time').mean('member')
                tmp = tmp.compute()
                savename = f'{var}_mean_cumul_{xpid}_{datebegin}_{dateend}.pdf'
            else:
                tmp = tmp.sum(dim='time')
                savename = f'{var}_cumul_{xpid}_{datebegin}_{dateend}.pdf'

        vmax = vmax_map[var] if var in vmax_map.keys() else tmp.max()
        vmin = vmin_map[var] if var in vmin_map.keys() else tmp.min()

        if domain is not None:
            addpoint = reference_point[domain]
        else:
            addpoint = None

        # fig, ax = plt.subplots(figsize=(12 * len(tmp.xx) / len(tmp.yy), 10))

        # Add relief
        # dem = xr.open_dataset('TARGET_RELIEF.tif', engine='rasterio')  # Target domain's Digital Elevation Model
        dem = xr.open_dataset('TARGET_RELIEF.tif')  # Target domain's Digital Elevation Model
        dem = xrp.preprocess(dem, decode_time=False)
        dem = dem.squeeze()

        if var in cmap.keys():
            plot2D.plot_field(tmp, vmin=vmin, vmax=vmax, cmap=cmap[var], addpoint=addpoint, dem=dem.band_data)
        else:
            # plot2D.plot_field(tmp, ax=ax, vmin=vmin, vmax=vmax, addpoint=addpoint)
            plot2D.plot_field(tmp, vmin=vmin, vmax=vmax, addpoint=addpoint, dem=dem.band_data)

        # Add relief
        # plot2D.add_iso_elevation(ax, dem.ZS)

        plot2D.save_fig(savename)


if __name__ == '__main__':

    mntdir = '/home/vernaym/These/DATA'

    args = parse_command_line()
    datebegin       = args.datebegin
    dateend         = args.dateend
    date            = args.date
    xpids           = args.xpids
    geometry        = args.geometry
    kind            = args.kind
    ensemble        = args.ensemble
    if ensemble == 'mean':
        member = footprints.util.rangex('0-16-1')
    elif ensemble == 'first':
        member = 0
    else:
        member = None
    uenv            = args.uenv
    uenv_dem        = args.uenv_dem
    vapp            = args.vapp
    variables       = args.variables
    workdir         = args.workdir
    mask            = args.mask
    domain          = args.domain

    if not os.path.exists(workdir):
        os.makedirs(workdir)
    os.chdir(workdir)

    io.get_const(uenv_dem, 'relief', geometry, filename='TARGET_RELIEF.tif', gvar='DEM_GRANDESROUSSES25M_L93')

    # 1. Get all input data
    if mask is not None:
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
    else:
        mask = False

    for xpid in xpids:
        # TODO : gérer ça plus proprement
        if '@' not in xpid:
            user = os.environ["USER"]
            xpid = f'{xpid}@{user}'
        shortid = xpid.split('@')[0]

        # Get DIAG files with Vortex
        filename = f'{kind}_{shortid}.nc'
        kw = dict(datebegin=datebegin, dateend=dateend, vapp=vapp, member=member, filename=filename)
        if date is not None:
            if vapp == 'Pleiades':
                kw.update(date=date)
                getattr(io, f'get_{kind.lower()}')(xpid=xpid, geometry=geometry, **kw)
            else:
                from vortex.layout.dataflow import SectionFatalError
                try:
                    # If a simulation file with extracted dates already exists (nambuild=None), take it
                    # TODO :  find a better way to store these files
                    kw.update(date=dateend, namebuild=None)
                    getattr(io, f'get_{kind.lower()}')(xpid=xpid, geometry=geometry, **kw)
                except SectionFatalError:
                    # Else get the entire file
                    kw.pop('namebuild')
                    getattr(io, f'get_{kind.lower()}')(xpid=xpid, geometry=geometry, **kw)
        else:
            getattr(io, f'get_{kind.lower()}')(xpid=xpid, geometry=geometry, **kw)

        if ensemble == 'mean':
            ds = xr.open_mfdataset([f'mb{member:03d}/{filename}' for member in range(17)],
                    concat_dim='member', combine='nested', chunks='auto')
        else:
            ds = xr.open_dataset(filename)
        ds = xrp.preprocess(ds)

        if domain is not None:
            dom = domain_map[domain]
            ds = ds.where((ds.xx >= dom['xmin']) & (ds.xx <= dom['xmax']) & (ds.yy >= dom['ymin']) &
                    (ds.yy <= dom['ymax']), drop=True)

        plot_var(ds, variables, shortid, date=date, mask=mask)

    # 3. Clean data
    os.remove('TARGET_RELIEF.tif')
    for xpid in xpids:
        shortid = xpid.split('@')[0]
        if ensemble == 'mean':
            for member in range(17):
                os.remove(f'mb{member:03d}/{kind}_{shortid}.nc')
        else:
            os.remove(f'{kind}_{shortid}.nc')

    print()
    print("===========================================================================================")
    print("                                     Execution result                                      ")
    print("===========================================================================================")
    print()
    print(f"Produced figures are available here : {workdir}")
    print()