#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 25 march 2024

@author: Vernay

Script designed to produce downsale a precipitation variable onto a target grid.

It can also produce 'Rainf' and 'Snowf' variables from a precipitation analysis and AROME
Wet-bulb's iso 1°C (or 0°C alternatively).

WARNING :
AROME's wet-bulb temperature definition changed on 01/07/2019. Before that date, it was named 'TPW'.

'''

import os
import shutil
import xarray as xr
import argparse

from snowtools.utils import xarray_snowtools

DEFAULT_NETCDF_FORMAT = 'NETCDF4_CLASSIC'

home = os.environ['HOME']


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
    parser.add_argument('-i', '--iso_wetbt', type=str, required=False, default=None,
                        help="XPID (format xpid@username)/abspath of the file containing iso wet-buld temperatures")
    parser.add_argument('-u', '--uenv', type=str, default="uenv:edelweiss.1@vernaym",
                        help="User environment for static resources (format 'uenv:name@user')")
    parser.add_argument('-w', '--workdir', type=str, help="Working directory",
                        default=os.path.join(home, 'EDELWEISS', 'meteo'))
    parser.add_argument('-g', '--iso_grid', type=str, help="Grid of the iso_wetbt file", required=False, default=None,
                        choices=['FRANGP0025', 'EURW1S40'])
    args = parser.parse_args()
    return args


def goto(path):
    if not os.path.exists(path):
        os.makedirs(path)
    os.chdir(path)


def read_target_relief():

    target_relief = xr.open_dataarray('TARGET_RELIEF.nc')  # Target domain's Digital Elevation Model
    target_relief = xarray_snowtools.preprocess(target_relief, decode_time=False)

    return target_relief


def compute_phase_from_iso_wetbt1(precipitation, target_relief=None, subdir=None):
    """
    Compute Rainf and Snowf variables from the iso-wetbt (or iso-tpw before 7/12/2019) elevation.
    """
    # Open  Wet-bulb temperature iso-0/1°C dataset, and rename time as in the 'precipitation' ds
    isowetbt  = xr.open_dataset('ISO_TPW.nc')
    if 'valid_time' in isowetbt.keys():
        isowetbt = isowetbt.drop('time').rename({'valid_time': 'time'})
    isowetbt = xarray_snowtools.preprocess(isowetbt, decode_time=False)
    # Fill missing dates with nearest value :
    isowetbt  = isowetbt.reindex({'time': precipitation.time}, method='nearest')
    isowetbt1 = isowetbt.sel({'ISO_TPW': 27415.})  # Wet-bulb temperature iso-1°C
    # source_relief = xr.open_dataarray('SOURCE_RELIEF.nc')  # Iso-TPW grid's Digital Elevation Model
    # Drop useless 'valid_time' dimension (len=1) added by xarray when converting grid files to netcdf
    # TODO : do it at the netcdf generation (in the Extraction_BDAP.py script)
    # iso_elevation = isowetbt1.sro + source_relief
    iso_elevation = isowetbt1.sro

    # Interpolation of all data to the target geometry
    iso = iso_elevation.interp({'yy': precipitation.yy, 'xx': precipitation.xx})

    if target_relief is None and 'ZS' not in precipitation.keys():

        precipitation['ZS'] = read_target_relief()

    # Creation of the Rainf / Snowf variables
    precipitation['Rainf'] = precipitation.where(iso > precipitation.ZS, 0)
    precipitation['Snowf'] = precipitation.where(iso <= precipitation.ZS, 0)

    return precipitation


def downscale_precipitation(subdir=None):
    """
    Downscale precipitation data with a linear interpolation on the target grid
    """
    precipitation = xr.open_dataset(os.path.join(subdir or '', 'PRECIPITATION.nc'))
    precipitation = xarray_snowtools.preprocess(precipitation, decode_time=False)
    precipitation = precipitation[['Precipitation']]
    # Fill potentially missing dates from the BDAP with 0
    precipitation = precipitation.fillna(0)

    target_relief = read_target_relief()

    # Interpolation to the target geometry
    # Default interpolation method = 'linear'
    output = precipitation.interp({'yy': target_relief.yy, 'xx': target_relief.xx})

    # Remove small precipitation to avoid problems in Crocus
    output = output.where(output > 0.01, 0)

    output['ZS'] = target_relief

    return output


def write(ds, outname):
    datedeb = ds.time[0]
    dateend = ds.time[-1]
    ds.load().to_netcdf(outname, unlimited_dims={'time': True}, format=DEFAULT_NETCDF_FORMAT,
            encoding={'time': {'dtype': 'int32'}})
    return datedeb, dateend


def execute(members=None, downscale=True, compute_phase=False):
    """
    Main function to be called in "script mode"
    """
    outname = 'PRECIPITATION_OUT.nc'
    if members is not None:
        for member in range(members):
            print(f'Member {member}')
            subdir = f'mb{member:03d}'
            if downscale:
                output = downscale_precipitation(subdir=subdir)
            if compute_phase:
                output = compute_phase_from_iso_wetbt1(subdir=subdir)
            datedeb, dateend = write(output, os.path.join(subdir, outname))

    else:
        if downscale:
            output = downscale_precipitation()
        if compute_phase:
            output = compute_phase_from_iso_wetbt1()
        datedeb, dateend = write(output, outname)

    return datedeb, dateend


def clean(members):
    for member in range(members):
        shutil.rmtree(os.path.join(f'mb{member}', 'PRECIPITATION.nc'))


def open_dataset(filename):
    return xr.open_dataset(filename, chunks='auto')


if __name__ == '__main__':

    args = parse_command_line()

    datebegin     = args.datebegin
    dateend       = args.dateend
    uenv          = args.uenv
    members       = args.members
    precipitation = args.precipitation
    iso_wetbt     = args.iso_wetbt
    iso_grid      = args.iso_grid
    geometry      = 'GrandesRousses250m'

    goto(args.workdir)

    try:
        # Retrieve input files with Vortex
        # --------------------------------
        from snowtools.scripts.extract.vortex import vortexIO as io
        period = dict(datebegin=datebegin, dateend=dateend)
        # Hourly total precipitation at 1km resolution --> use get_meteo since it is not FORCING-ready
        io.get_meteo(
            kind     = 'Precipitation',
            geometry = 'GrandesRousses1km',
            xpid     = precipitation,
            members  = members,
            vapp     = 'edelweiss',
            block    = 'hourly',
            **period
        )
        if iso_wetbt is not None:
            # Hourly iso Wet-bulb temperatures 0°C, 1°C [, 1.5°C] --> use get_meteo (not FORCING-ready)
            io.get_meteo(kind='ISO_TPW', geometry=iso_grid, xpid=iso_wetbt, **period)
        # Iso-TPW's grid relief
        # io.get_const(uenv, 'relief', 'FRANGP0025', filename='SOURCE_RELIEF.nc')
        # Domain's DEM
        io.get_const(uenv, 'relief', geometry, filename='TARGET_RELIEF.nc', gvar='RELIEF_GRANDESROUSSES250M_4326')
        vortex = True
    except (ImportError, ModuleNotFoundError):
        vortex = False
        # Retrieve input files without Vortex
        for member in range(members):
            os.makedirs(f'mb{member:03d}')
            os.symlink(f'{precipitation}/mb{member:03d}/PRECIPITATION.nc', f'mb{member:03d}/PRECIPITATION.nc')

    # Call main function
    datedeb, datefin = execute(members, compute_phase=iso_wetbt is not None)

    if datedeb != datebegin:
        print(f'WARNING : begin date of the produced FORCING {datedeb} does not match the one prescribed {datebegin}')
    if datefin != dateend:
        print(f'WARNING : end date of the produced FORCING {datefin} does not match the one prescribed {dateend}')

    if vortex:
        # FORCING-ready resource --> use specific the vortexIO method
        io.put_precipitation(precipitation, geometry, members=members, filename='PRECIPITATION_OUT.nc', **period)
        clean()
