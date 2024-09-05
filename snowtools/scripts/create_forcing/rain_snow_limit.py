#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 25 march 2024

@author: Vernay

Script designed to produce 'Rainf' and 'Snowf' variables from a precipitation analysis and AROME
Wet-bulb's iso 1°C (or 0°C alternatively).

WARNING :
AROME's wet-bulb temperature definition changed on 01/07/2019. Before that date, it was named 'TPW'.

'''

import os
import shutil
import xarray as xr
import argparse

import snowtools.tools.xarray_preprocess as xrp

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
    parser.add_argument('-i', '--iso_wetbt', type=str, default='ExtractionBDAP@vernaym',
                        help="XPID (format xpid@username)/abspath of the file containing iso wet-buld temperatures")
    parser.add_argument('-u', '--uenv', type=str, default="uenv:edelweiss.1@vernaym",
                        help="User environment for static resources (format 'uenv:name@user')")
    parser.add_argument('-w', '--workdir', type=str, help="Working directory",
                        default=os.path.join(home, 'EDELWEISS', 'meteo'))
    parser.add_argument('-g', '--iso_grid', type=str, help="Grid of the iso_wetbt file",
                        default='FRANGP0025', choices=['FRANGP0025', 'EURW1S40'])
    args = parser.parse_args()
    return args


def goto(path):
    if not os.path.exists(path):
        os.makedirs(path)
    os.chdir(path)


def compute_phase_from_iso_wetbt1(subdir=None):
    """
    Compute Rainf and Snowf variables from the iso-wetbt (or iso-tpw before 7/12/2019) elevation.
    """
    try:
        precipitation = xr.open_dataarray(os.path.join(subdir or '', 'PRECIPITATION.nc'))  # Hourly precipitation
        precipitation = xrp.preprocess(precipitation, decode_time=False)
    except ValueError:
        precipitation = xr.open_dataset(os.path.join(subdir or '', 'PRECIPITATION.nc'))
        precipitation = xrp.preprocess(precipitation, decode_time=False)
        precipitation = precipitation.Precipitation
    # Fill potentially missing dates from the BDAP with 0
    precipitation = precipitation.fillna(0)

    # Open  Wet-bulb temperature iso-0/1°C dataset, and rename time as in the 'precipitation' ds
    isowetbt  = xr.open_dataset('ISO_TPW.nc')
    if 'valid_time' in isowetbt.keys():
        isowetbt = isowetbt.drop('time').rename({'valid_time': 'time'})
    isowetbt = xrp.preprocess(isowetbt, decode_time=False)
    # Fill missing dates with nearest value :
    isowetbt  = isowetbt.reindex({'time': precipitation.time}, method='nearest')
    isowetbt1 = isowetbt.sel({'ISO_TPW': 27415.})  # Wet-bulb temperature iso-1°C
    # source_relief = xr.open_dataarray('SOURCE_RELIEF.nc')  # Iso-TPW grid's Digital Elevation Model
    # Drop useless 'valid_time' dimension (len=1) added by xarray when converting grid files to netcdf
    # TODO : do it at the netcdf generation (in the Extraction_BDAP.py script)
    # iso_elevation = isowetbt1.sro + source_relief
    iso_elevation = isowetbt1.sro
    target_relief = xr.open_dataarray('TARGET_RELIEF.nc')  # Target domain's Digital Elevation Model
    target_relief = xrp.preprocess(target_relief, decode_time=False)

    # Interpolation of all data to the target geometry
    iso250m = iso_elevation.interp({'yy': target_relief.yy, 'xx': target_relief.xx})
    precipitation250m = precipitation.interp({'yy': target_relief.yy, 'xx': target_relief.xx})

    # Remove small precipitation to avoid problems in Crocus
    precipitation250m = precipitation250m.where(precipitation250m > 0.01, 0)

    # Creation of the Rainf / Snowf variables
    rain = precipitation250m.where(iso250m > target_relief, 0)
    snow = precipitation250m.where(iso250m <= target_relief, 0)
    rain = rain.rename('Rainf')
    snow = snow.rename('Snowf')
    output = rain.to_dataset().merge(snow).drop(['step', 'ISO_TPW'])

    return output


def write(ds, outname):
    datedeb = ds.time[0]
    dateend = ds.time[-1]
    ds.load().to_netcdf(outname, unlimited_dims={'time': True}, format=DEFAULT_NETCDF_FORMAT,
            encoding={'time': {'dtype': 'int32'}})
    return datedeb, dateend


def compute_precipitation_phase(members):
    """
    Main function to be called in "script mode"
    """
    outname = 'PRECIPITATION_OUT.nc'
    if members is not None:
        for member in range(members):
            print(f'Member {member}')
            subdir = f'mb{member:03d}'
            output = compute_phase_from_iso_wetbt1(subdir=subdir)
            datedeb, dateend = write(output, os.path.join(subdir, outname))

    else:
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
    datedeb, datefin = compute_precipitation_phase(members)

    if datedeb != datebegin:
        print(f'WARNING : begin date of the produced FORCING {datedeb} does not match the one prescribed {datebegin}')
    if datefin != dateend:
        print(f'WARNING : end date of the produced FORCING {datefin} does not match the one prescribed {dateend}')

    if vortex:
        # FORCING-ready resource --> use specific the vortexIO method
        io.put_precipitation(precipitation, geometry, members=members, filename='PRECIPITATION_OUT.nc', **period)
        clean()
