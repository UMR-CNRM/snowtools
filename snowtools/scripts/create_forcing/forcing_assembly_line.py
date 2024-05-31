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
import argparse

import snowtools.tools.xarray_preprocess as xrp

# DEFAULT_NETCDF_FORMAT = 'NETCDF3_CLASSIC'  # WARNING : to_netcdf command very slow (>15')
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
    wind = xr.open_dataset('WIND.nc', chunks='auto')
    dates = np.intersect1d(forcing.time, wind.time)
    forcing = forcing.sel({'time': dates})
    wind = wind.sel({'time': dates}).transpose('time', 'y', 'x')
    forcing['Wind'].data = wind['Wind'].data
    forcing['Wind_DIR'].data = wind['Wind_dir'].data
    wind.close()

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
    precipitation = xrp.update_varname(precipitation)
    precipitation = precipitation.transpose('time', 'y', 'x')

    forcing['Rainf'].data = precipitation['Rainf'].data / 3600.
    forcing['Snowf'].data = precipitation['Snowf'].data / 3600.

    return forcing


def write(ds, outname):
    ds.time.encoding['dtype'] = 'int32'
    datedeb = ds.time[0]
    dateend = ds.time[-1]
    ds.load().to_netcdf(outname, unlimited_dims={'time': True}, format=DEFAULT_NETCDF_FORMAT)
    return datedeb, dateend


def check_date(datebegin_file, datebegin_arg, dateend_file, dateend_arg):
    if datebegin_file != datebegin_arg:
        print(f'WARNING : Begin date of the produced FORCING {datebegin_file} does not match the one prescribed'
              '{datebegin_arg}')
    if datebegin_file != datebegin_arg:
        print(f'WARNING : End date of the produced FORCING {dateend_file} does not match the one prescribed'
              '{dateend_arg}')


def update(forcing, members, datebegin, dateend, wind=True, precipitation=True):
    outname = 'FORCING_OUT.nc'
    if wind:
        forcing = update_wind(forcing)

    if precipitation:
        if members is not None:
            for member in range(members):
                print(f'Member {member}')
                subdir = f'mb{member}'
                forcing = update_precipitation(forcing, subdir=subdir)
                datedeb, datefin = write(forcing, outname)
                check_date(datedeb, datebegin, datefin, dateend)
        else:
            forcing = update_precipitation(forcing)
            datebegin, dateend = write(forcing, outname)
            check_date(datedeb, datebegin, datefin, dateend)

    return datebegin, dateend


def clean(members):
    shutil.rmtree('FORCING_IN.nc')
    shutil.rmtree('WIND.nc')
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
    safran        = args.safran
    wind          = args.wind
    precipitation = args.precipitation
    diroutput     = args.diroutput
    geometry      = 'GrandesRousses250m'

    os.chdir(workdir)

    try:
        # Retrieve input files with Vortex
        from snowtools.scripts.extract.vortex import vortexIO
        vortexIO.get_forcing(datebegin, dateend, safran, geometry, filename='FORCING_IN.nc')
        kw = dict(datebegin=datebegin, dateend=dateend)
        wind = vortexIO.get_wind(wind, geometry, **kw)
        precipitation = vortexIO.get_precipitation(precipitation, geometry, members=members, **kw)
        vortex = True
    except (ImportError, ModuleNotFoundError):
        vortex = False
        # Retrieve input files without Vortex
        os.symlink(safran, 'FORCING_IN.nc')
        os.symlink(wind, 'WIND.nc')
        for member in range(members):
            os.makedirs(f'mb{member}')
            os.symlink(f'{precipitation}/mb{member}/PRECIPITATION.nc', f'mb{member}/PRECIPITATION.nc')

    forcing = open_dataset('FORCING_IN.nc')  # Read input forcing file
    # Update default FORCING file with wind and precipitation variables (if any)
    datedeb, datefin = update(forcing, members, datebegin, dateend, wind=wind[0], precipitation=precipitation[0])

    if vortex:
        vortexIO.put_forcing(xpid, geometry, members=members, filename='FORCING_OUT.nc', **kw)
        clean()
    else:
        # Save output files without Vortex
        if diroutput is not None:
            shutil.copyfile('FORCING_OUT.nc', diroutput)
        else:
            print(f'No {diroutput} argument provided, output files are left under {workdir}')
