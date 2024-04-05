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

'''

import os
import xarray as xr
import argparse

from bronx.stdtypes.date import Date

from snowtools.scripts.extract.vortex import vortexIO as io
from snowtools.plots.maps import plot2D


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

    parser.add_argument('-k', '--kind', type=str, default='PRO', choices=['FORCING', 'PRO'],
                        help='kind file containing the variable(s) to plot')

    parser.add_argument('-m', '--member', action='store_true',
                        help="If the file comes from an ensemble, take the first member (the control member"
                             "by convention)")

    parser.add_argument('-a', '--vapp', type=str, default='edelweiss', choices=['s2m', 'edelweiss'],
                        help="Application that produced the target file")

    parser.add_argument('-v', '--variable', nargs='+', type=str, default=None,
                        help="Variable(s) name to plot (see list for each specific file"
                             "It is possible to plot a combination of variables, for example :"
                             "-v Rainf+Snowf will plot the total precipitation from a FORCING file")

    parser.add_argument('-d', '--date', type=str, default=None,
                        help="Plot the variable for a specific date."
                             "If no *date* is provided, plot the variable's accumulation over the simulation period")

    parser.add_argument('-w', '--workdir', type=str, default=f'{os.environ["HOME"]}/workdir/EDELWEISS/plot',
                        help='Working directory')

    args = parser.parse_args()
    return args


def plot_var(ds, variables, xpid, date=None):

    for var in variables:

        if 'Rainf' in var:
            ds['Rainf'] = ds['Rainf'] * 3600.
        if 'Snowf' in var:
            ds['Snowf'] = ds['Snowf'] * 3600.

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
            tmp = tmp.sel({'time': date})
            savename = f'{var}_{date}_{xpid}.pdf'
        else:
            tmp = tmp.sum(dim='time')
            savename = f'{var}_cumul_{xpid}.pdf'
        plot2D.plot_field(tmp, savename)


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
    variable        = args.variable
    workdir         = args.workdir

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

        # Get DIAG files with Vortex
        filename = f'{kind}_{shortid}.nc'
        kw = dict(datebegin=datebegin, dateend=dateend, vapp=vapp, member=member)
        getattr(io, f'get_{kind.lower()}')(xpid, geometry, filename=filename, **kw)

        ds = xr.open_dataset(filename)

        plot_var(ds, variable, shortid, date=date)

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
