#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Create a new forcing file from two forcing (forcing1 and forcing2), with data from
*forcing1*  until the specified *date* and data from *forcing2* after *date*.
'''

import os
import shutil
import xarray as xr
import argparse


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

    parser.add_argument('-x', '--xpid', type=str, required=True,
                        help="XPID (format xpid@username) OR abspath of the output FORCING")

    parser.add_argument('--xpid1', type=str, required=True,
                        help="XPID (format xpid@username) of the FORCING over period 1")

    parser.add_argument('--xpid2', type=str, required=True,
                        help="XPID (format xpid@username) of the FORCING over period 2")

    parser.add_argument('-d', '--date', type=str, default=None,
                        help="Date to switch from forcing 1 to forcing2, format YYYYMMDDHH")

    parser.add_argument('-w', '--workdir', type=str, required=True,
                        help="Working directory")

    args = parser.parse_args()
    return args


def execute(date):
    """
    """

    forcing1 = xr.open_dataset('FORCING1.nc', chunks={'time': 24})
    forcing1 = forcing1.sel(time=slice(forcing1.time[0], date))
    forcing2 = xr.open_dataset('FORCING2.nc', chunks={'time': 24})
    forcing2 = forcing2.sel(time=slice(date, forcing2.time[-1]))
    # Solution to remove time duplicates
    out = xr.concat([forcing1, forcing2], dim='new_dim').max(dim='new_dim')
    out.to_netcdf('FORCING_OUT.nc')


def write(ds, outname):
    # ds.time.encoding['dtype'] = 'int32'
    datedeb = ds.time[0]
    dateend = ds.time[-1]
    ds.load().to_netcdf(outname, unlimited_dims={'time': True}, format=DEFAULT_NETCDF_FORMAT,
            encoding={'time': {'dtype': 'int32'}})
    return datedeb, dateend


def check_date(datebegin_file, datebegin_arg, dateend_file, dateend_arg):
    if datebegin_file != datebegin_arg:
        print(f'WARNING : Begin date of the produced FORCING {datebegin_file} does not match the one prescribed'
              '{datebegin_arg}')
    if datebegin_file != datebegin_arg:
        print(f'WARNING : End date of the produced FORCING {dateend_file} does not match the one prescribed'
              '{dateend_arg}')


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
    date          = args.date
    xpid_out      = args.xpid_out
    xpid1         = args.xpid1
    xpid2         = args.xpid2
    members       = args.members
    workdir       = args.workdir
    geometry      = 'GrandesRousses250m'

    os.chdir(workdir)

    # Retrieve input files with Vortex
    from snowtools.scripts.extract.vortex import vortex_get
    vortex_get.get(
        datebegin      = datebegin,
        dateend        = dateend,
        xpid           = xpid1,
        geometry       = geometry,
        member         = members,
        filename       = '[member:03d]/FORCING_1.nc'
    )
    vortex_get.get(
        datebegin      = datebegin,
        dateend        = dateend,
        xpid           = xpid2,
        geometry       = geometry,
        member         = members,
        filename       = '[member:03d]/FORCING_2.nc'
    )

    date_str = f'{date[:4]}-{date[4-6]}-{date[6-8]}'
    workdir = os.getcwd()
    for mb in range(members):
        os.chdir(f'{mb:03d}')
        execute(date_str)
        os.chdir(workdir)

    vortex_get.put(
        xpid           = xpid_out,
        geometry       = geometry,
        members        = members,
        filename       = '[member:03d]/FORCING_OUT.nc'
    )
    clean()
