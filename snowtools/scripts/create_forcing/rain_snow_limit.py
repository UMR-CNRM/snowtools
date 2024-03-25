#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 25 march 2024

@author: Vernay

Script designed to produce 'Rainf' and 'Snowf' variables from a precipitation analysis and AROME
WETBT profiles (using V.Vionnet's Method) or ISO-0/1°C.

'''

import os
import shutil
import xarray as xr
import numpy as np
import argparse

DEFAULT_NETCDF_FORMAT = 'NETCDF4_CLASSIC'


def parse_command_line():
    description = "Merge meteorological variables from different sources into 1 FORCING file"
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-b', '--datebegin', type=str, required=True,
                        help="First date covered by the simulation file, format YYYYMMDDHH.")
    parser.add_argument('-e', '--dateend', type=str,
                        help="Last date covered by the simulation file, format YYYYMMDDHH.")
    parser.add_argument('-m', '--members', type=int, default=None,
                        help="Number of members associated to the experiment")
    parser.add_argument('-p', '--precipitation', type=str, required=True,
                        help="XPID (format xpid@username) OR abspath of the file containing  precipitation variables")
    parser.add_argument('-w', '--workdir', type=str, required=True,
                        help="Working directory")
    args = parser.parse_args()
    return args


def update_precipitation(forcing, subdir=None):
    """
    Compute Rainf and Snowf variables.
    """
    precipitation = xr.open_dataset('PRECIPITATION.nc', drop_variables=['Precipitation', 'snowfrac_ds', 'z_snowlim_ds'],
                                    chunks='auto')

    # Output file on common dates only
    dates = np.intersect1d(forcing.time, precipitation.time)
    forcing = forcing.sel({'time': dates})
    # Set time variable attributes
    forcing.time.encoding['units'] = f'hours since {forcing.time.data[0]}'

    precipitation = precipitation.sel({'time': dates})
    precipitation = precipitation.rename({'xx': 'x', 'yy': 'y'})

    # Replace Rainf/Snowf variables by the ones from the ensemble analysis
    forcing['Rainf'] = precipitation['Rainf_ds'] / 3600.
    forcing['Snowf'] = precipitation['Snowf_ds'] / 3600.

    return forcing


def write(ds, outname):
    datedeb = ds.time[0]
    dateend = ds.time[-1]
    ds.load().to_netcdf(outname, unlimited_dims={'time': True}, format=DEFAULT_NETCDF_FORMAT)
    return datedeb, dateend


def compute_phase(members):
    outname = 'OUT.nc'
    if members is not None:
        for member in range(members):
            print(f'Member {member}')
            subdir = f'mb{member}'
            update_precipitation(subdir=subdir)
    else:
        update_precipitation()
    datebegin, dateend = write(forcing, outname)


def clean(members):
    for member in range(members):
        shutil.rmtree(os.path.join(f'mb{member}', 'PRECIPITATION.nc'))


def open_dataset(filename):
    return xr.open_dataset(filename, chunks='auto')


if __name__ == '__main__':

    args = parse_command_line()

    datebegin     = args.datebegin
    dateend       = args.dateend
    xpid          = args.xpid
    members       = args.members
    workdir       = args.workdir
    precipitation = args.precipitation
    diroutput     = args.diroutput
    geometry      = 'GrandesRousses250m'

    os.chdir(workdir)

    try:
        # Retrieve input files with Vortex
        from snowtools.scripts.extract.vortex import vortexIO
        vortexIO.get_precipitation(datebegin, dateend, precipitation, geometry, members=members)
        vortexIO.get_iso_wetbt(datebegin, dateend, precipitation, geometry, members=members)
        vortex = True
    except (ImportError, ModuleNotFoundError):
        vortex = False
        # Retrieve input files without Vortex
        for member in range(members):
            os.makedirs(f'mb{member}')
            os.symlink(f'{precipitation}/mb{member}/PRECIPITATION.nc', f'mb{member}/PRECIPITATION.nc')

    datedeb, datefin = compute_phase(members)

    if datedeb != datebegin:
        print(f'WARNING : begin date of the produced FORCING {datedeb} does not match the one prescribed {datebegin}')
    if datefin != dateend:
        print(f'WARNING : end date of the produced FORCING {datefin} does not match the one prescribed {dateend}')

    if vortex:
        # TODO : Trouver une façon de différencier les resource de type "precipitation totale" avec les celles prêtes
        # à intégrer un FORCING (contenant les variables 'Rainf' et 'Snowf')
        vortexIO.put_precipitation(datebegin, dateend, precipitation, geometry, members=members)
        clean()
    else:
        # Save output files without Vortex
        if diroutput is not None:
            shutil.copyfile('OUT.nc', diroutput)
        else:
            print(f'No {diroutput} argument provided, output files are left under {workdir}')
