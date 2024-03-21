#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 8 march 2024

@author: Vernay

Script designed to merge together FORCING variables from different sources into a single FORCING file.

At this stage of the EDELWEISS development, most meteorological variables still come from a single
re-gridded SAFRAN reanalysis FORCING file except for :
* Wind variables (Wind/Wind_DIR) that come from a single AROME-DEVINE downscaling
* Precipitation (Snowf/Rainf) that come from an ANTILOPE/[PEAROME] ensemble precipitation analysis

The current version of this script (march 2024) follows this workflow :
    1. Start from a SINGLE FORCING.nc file (SAFRAN reanalysis)
    2. Replace Wind/Wind_DIR variables from a SINGLE WIND.nc file (AROME/-DEVINE)
    3. For each ensemble member: copy the modified FORCING.nc and replace Rainf/Snowf variables by the ones
      in the corresponding PRECIPITATION.nc file.
'''

import os
import shutil
import xarray as xr
import numpy as np
import pandas as pd
import argparse

DEFAULT_NETCDF_FORMAT = 'NETCDF3_CLASSIC'


def parse_command_line():
    description = "Merge meteorological variables from different sources into 1 FORCING file"
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-b', '--datebegin', type=str, required=True,
                        help="First date covered by the simulation file, format YYYYMMDDHH.")
    parser.add_argument('-e', '--dateend', type=str,
                        help="Last date covered by the simulation file, format YYYYMMDDHH.")
    parser.add_argument('-m', '--members', type=int, default=None,
                        help="Number of members associated to the experiment")
    parser.add_argument('-s', '--safran', type=str, required=True,
                        help="XPID (format xpid@username) OR abspath of the SAFRAN base FORCING")
    parser.add_argument('-w', '--wind', type=str, required=True,
                        help="XPID (format xpid@username) OR abspath of the file containing wind variables")
    parser.add_argument('-p', '--precipitation', type=str, required=True,
                        help="XPID (format xpid@username) OR abspath of the file containing  precipitation variables")
    parser.add_argument('-w', '--workdir', type=str, required=True,
                        help="Working directory")
    args = parser.parse_args()
    return args


def update_wind(forcing):
    """
    Replace Wind and Wind_DIR variables in a FORCING file by the ones coming from a DEVINE execution.
    """
    print('Update wind')
    # 1 Open wind produced by HM with LLT method
    wind = xr.open_dataset('WIND.nc')
    dates = np.intersect1d(forcing.time, wind.time)
    forcing = forcing.sel({'time': dates})
    wind = wind.sel({'time': dates})
    forcing['Wind'].data = wind['Wind'].data
    forcing['Wind_DIR'].data = wind['Wind_dir'].data
    wind.close()
    forcing.time.encoding['dtype'] = 'int32'

    return forcing


def update_precipitation(forcing, subdir=None):
    """
    Replace Rainf and Snowf variables in a FORCING file by the ones coming from a precipitation analysis
    The subdir keyword argument is used when the FORCING file is part of an ensemble.
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
    datedeb = pd.to_datetime(str(ds.time[0]))
    dateend = pd.to_datetime(str(ds.time[-1]))
    ds.to_netcdf(outname, unlimited_dims={'time': True}, format=DEFAULT_NETCDF_FORMAT)
    return datedeb, dateend


def update(forcing, members):
    outname = 'FORCING_OUT.nc'
    forcing = update_wind(forcing)
    if members is not None:
        for member in range(1, members + 1):
            subdir = f'mb{member}'
            forcing = update_precipitation(forcing, subdir=subdir)
    else:
        forcing = update_precipitation(forcing)
    datebegin, dateend = write(forcing, outname)


def clean(members):
    shutil.rmtree('FORCING_IN.nc')
    shutil.rmtree('WIND.nc')
    for member in range(1, members + 1):
        shutil.rmtree(os.path.join(f'mb{member}', 'PRECIPITATION.nc'))


def open(filename):
    return xr.open_dataset(filename)


if __name__ == '__main__':

    args = parse_command_line()

    datebegin     = args.datebegin
    dateend       = args.dateend
    xpid          = args.xpid
    members       = args.members
    workdir       = args.workdir
    safran        = args.safran
    wind          = args.wind
    precipitation = args.precipitation
    geometry      = 'GrandesRousses250m'

    os.chdir(workdir)

    try:
        # Retrieve input files with Vortex
        from snowtools.scripts.extract.vortex import vortexIO
        vortexIO.get_forcing(datebegin, dateend, safran, geometry, filename='FORCING_IN.nc')
        vortexIO.get_wind(datebegin, dateend, wind, geometry)
        vortexIO.get_precipitation(datebegin, dateend, precipitation, geometry, members=members)
    except (ImportError, ModuleNotFoundError):
        # Retrieve input files without Vortex
        os.symlink(safran, 'FORCING_IN.nc')
        os.symlink(wind, 'WIND.nc')
        for member in range(1, members + 1):
            os.makedirs(f'mb{member}')
            os.symlink(f'{precipitation}/mb{member}/PRECIPITATION.nc', f'mb{member}/PRECIPITATION.nc')

    forcing = xr.open_dataset('FORCING_IN.nc')  # Read input forcing file
    datedeb, datefin = update(forcing, members)  # Single SAFRAN FORCING file --> 16 FORCINGs

    if datedeb != datebegin:
        print(f'WARNING : begin date of the produced FORCING {datedeb} does not match the one prescribed {datebegin}')
    if datefin != dateend:
        print(f'WARNING : end date of the produced FORCING {datefin} does not match the one prescribed {dateend}')

    vortexIO.put_forcing(datedeb, datefin, xpid, geometry, members=members, filename='FORCING_OUT.nc')

    clean()
