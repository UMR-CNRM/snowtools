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

from snowtools.scripts.extract.vortex import vortexIO as io
from snowtools.plots.maps import plot2D
import matplotlib.pyplot as plt


cmap = dict(
    DSN_T_ISBA = plt.cm.Blues,
)

vmax_map = dict(
    # WARNING : values defined for 2017/2018
    DSN_T_ISBA = 6,  # m
    Snowf      = 1500,  # kg/m² [mm]
    Rainf      = 1400,  # kg/m² [mm]
)


def parse_command_line():
    description = "Computation of Sentinel2-like diagnostics (snow melt-out date, snow cover duration) associated \
                   to a SURFEX simulation"
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-b', '--datebegin', type=str, required=True,
                        help="First date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-e', '--dateend', type=str, required=True,
                        help="Last date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-x', '--xpids', nargs='+', type=str, required=True,
                        help="XPID(s) of the simulation(s) format XP_NAME@username")

    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    parser.add_argument('-k', '--kind', type=str, default='PRO', choices=['FORCING', 'PRO', 'snow_obs_date'],
                        help='kind file containing the variable(s) to plot')

    parser.add_argument('-m', '--member', action='store_true',
                        help="If the file comes from an ensemble, take the first member (the control member"
                             "by convention)")

    parser.add_argument('-a', '--vapp', type=str, default='edelweiss', choices=['s2m', 'edelweiss', 'Pleiades'],
                        help="Application that produced the target file")

    parser.add_argument('-v', '--variables', nargs='+', required=True,
                        help="Variable(s) name to plot (see list for each specific file)"
                             "It is possible to plot a combination of variables, for example :"
                             "-v Rainf+Snowf will plot the total precipitation from a FORCING file")

    parser.add_argument('-d', '--date', type=str, default=None,
                        help="Plot the variable for a specific date."
                             "If no *date* is provided, plot the variable's accumulation over the simulation period")

    parser.add_argument('-w', '--workdir', type=str, default=f'{os.environ["HOME"]}/workdir/EDELWEISS/plot',
                        help='Working directory')

    parser.add_argument('--mask', type=str,
                        help="Absolute path to the mask file (if any)"
                             "WARNING : bad practice")

    args = parser.parse_args()
    return args


def maskgf(arr, method='nearest'):
    """
    Masks an input array (arr) using a reference mask dataset.

    Args:
        arr    : The input array to be masked.
        method : The interpolation method to use when resampling the glacier mask
                  to the same resolution as the input array. Valid options are
                  'nearest', 'linear', 'cubic', etc. (default: 'nearest').

    Returns:
        A new array with the same shape as the input array, where values are masked
        out based on the glacier mask. Masked values are set to NaN.
    """

    # Load the glacier mask dataset
    mask = xr.open_dataset('MASK.nc')['Band1']
    # TODO la commande rename est très lente
    # --> faire le renomage directement dans le fichier pour éviter de le faire à chaque exécution
    # mask = mask.rename({'x': 'xx', 'y': 'yy'})

    # Interpolate the glacier mask to the same resolution as the input array
    mask = mask.interp_like(arr, method=method)

    # Mask the input array based on the glacier mask
    return arr.where(mask == 0)


def plot_var(ds, variables, xpid, date=None, mask=True):

    if mask:
        # mask glacier/forest covered pixels
        ds = maskgf(ds)

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
        else:
            tmp = ds[var]

        if date is not None:
            date = Date(date)
            if 'time' in tmp.dims:
                tmp = tmp.sel({'time': date})
            savename = f'{var}_{date.ymdh}_{xpid}.pdf'
        else:
            tmp = tmp.sum(dim='time')
            savename = f'{var}_cumul_{xpid}.pdf'

        if var in vmax_map.keys():
            vmax = vmax_map[var]
            vmin = 0
        else:
            vmax = tmp.max()
            vmin = tmp.min()

        if var in cmap.keys():
            plot2D.plot_field(tmp, savename, vmin=vmin, vmax=vmax, cmap=cmap[var])
        else:
            plot2D.plot_field(tmp, savename, vmin=vmin, vmax=vmax)


if __name__ == '__main__':

    mntdir = '/home/vernaym/These/DATA'

    args = parse_command_line()
    datebegin       = args.datebegin
    dateend         = args.dateend
    date            = args.date
    xpids           = args.xpids
    geometry        = args.geometry
    kind            = args.kind
    member          = 0 if args.member else None
    vapp            = args.vapp
    variables       = args.variables
    workdir         = args.workdir
    mask            = args.mask

    if not os.path.exists(workdir):
        os.makedirs(workdir)
    os.chdir(workdir)

    # 1. Get all input data

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
                getattr(io, f'get_{kind.lower()}')(xpid, geometry, **kw)
            else:
                from vortex.layout.dataflow import SectionFatalError
                try:
                    # If a simulation file with extracted dates already exists (nambuild=None), take it
                    # TODO :  find a better way to store these files
                    kw.update(date=dateend, namebuild=None)
                    getattr(io, f'get_{kind.lower()}')(xpid, geometry, **kw)
                except SectionFatalError:
                    # Else get the entire file
                    kw.pop('namebuild')
                    getattr(io, f'get_{kind.lower()}')(xpid, geometry, **kw)
        else:
            getattr(io, f'get_{kind.lower()}')(xpid, geometry, **kw)

        ds = xr.open_dataset(filename)

        plot_var(ds, variables, shortid, date=date, mask=mask)

    # 3. Clean data
    for xpid in xpids:
        shortid = xpid.split('@')[0]
        os.remove(f'{kind}_{shortid}.nc')

    print()
    print("===========================================================================================")
    print("                                     Execution result                                      ")
    print("===========================================================================================")
    print()
    print(f"Produced figures are available here : {workdir}")
    print()
