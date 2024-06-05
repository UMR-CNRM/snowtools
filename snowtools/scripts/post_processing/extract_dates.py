#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 9 april 2024

@author: Vernay

Script for the extraction of a specific date from a PRO file for compraison with a Satellite (Pleiades) observation.
This script can either be called directly or throught the "extract_date" Vortex task via the
"extract_date" Vortex algo component (cen/algo/postprocessing.py).
'''

import os
import xarray as xr
import pandas as pd
import argparse

import snowtools.tools.xarray_preprocess as xrp
from snowtools.utils.dates import get_standad_nivology_season
from bronx.stdtypes.date import Date
try:
    # Retrieve input files with Vortex
    from snowtools.scripts.extract.vortex import vortexIO as io
except ImportError:
    print('Vortex not available, input files must be defined by their absolute path')


def parse_command_line():
    description = "Computation of Sentinel2-like diagnostics (snow melt-out date, snow cover duration) associated \
                   to a SURFEX simulation"
    parser = argparse.ArgumentParser(description=description)

#    parser.add_argument('-b', '--datebegin', type=str, default=None,
#                        help="First date covered by the simulation file, format YYYYMMDDHH.")
#
#    parser.add_argument('-e', '--dateend', type=str, default=None,
#                        help="Last date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-x', '--xpid', type=str, default=None,
                        help="XPID of the simulation format XP_NAME@username")

    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    parser.add_argument('-m', '--members', type=int, default=None,
                        help="Number of members associated to the experiment")

    parser.add_argument('-d', '--date', '--dates' '--extractdate', '--extractdates',
                        dest='dates', type=str, required=True, nargs='+',
                        help="Date(s) of extraction, format YYYYMMDDHH or YYYYMMDD.")

    parser.add_argument('-w', '--workdir', type=str, default=None,
                        help="Working directory")

    parser.add_argument('-u', '--uenv', type=str, default='uenv:edelweiss.2024.1@vernaym',
                        help="Uenv containing the mask file (if any)"
                             "Authorised formats :"
                             " * 'uenv:{uenv_name}@{username}' (recomended)"
                             " * '{uenv_name}@{username}'"
                             " * '{uenv_name}' --> only if the uenv is yours")

    parser.add_argument('-p', '--pro', type=str,
                        help="Absolute path to the pro file."
                             "WARNING : bad practice")

    args = parser.parse_args()

    # Retrieve *datebegin* and *dateend* from *date*
    dates = [Date(date) for date in args.dates]
    list_datebegin, list_dateend = zip(*[get_standad_nivology_season(date) for date in dates])
    if len(list_datebegin) == 1:
        args.datebegin = list_datebegin[0].ymdh
        args.dateend   = list_dateend[0].ymdh
    else:
        raise ValueError("The current state of this script does not manage several PRO files")

    # Check and convert *uenv* syntax
    if not args.uenv.startswith('uenv'):
        args.uenv = f'uenv:{args.uenv}'
    if '@' not in args.uenv:
        args.uenv = f'{uenv}@{os.environ["USER"]}'

    return args


def decode_time(pro):
    """
    Manually decode time variable since other variables can not be decoded automatically
    """
    ds = xr.Dataset({"time": pro.time})
    ds = xr.decode_cf(ds)
    pro['time'] = ds.time
    return pro


def execute(subdir, dates):
    """
    Main method
    """
    proname = os.path.join(subdir, 'PRO.nc')
    pro     = xr.open_dataset(proname, decode_times=False)
    pro     = xrp.preprocess(pro)
    if 'ZS' in pro.keys():
        pro     = pro[['DSN_T_ISBA', 'ZS']]
    else:
        pro = pro.DSN_T_ISBA
    out     = extract(dates, pro)

    # Write DIAG file and remove PRO
    suffix = '_'.join(dates)
    out.to_netcdf(os.path.join(subdir, f'PRO_{suffix}.nc'))


def extract(dates, pro):
    """
    Method to extract a list of dates from a pro file
    """
    try:
        # Prescribed date(s) can include hour or not
        # WARNING : this is the standard case when called by a Vortex task / algo
        # --> ensure that hours match (the 'resample' method set hour at 0)
        # Convert dates into pd.datetime objects (idem as in the pro 'time' dimension)
        dates = [pd.to_datetime(date, format='%Y%m%d') for date in dates]
        pro     = pro.resample(time='1D').mean()
    except ValueError:
        try:
            # if prescribed dates don't include a specific hour, resample PRO data
            # Convert dates into pd.datetime objects (idem as in the pro 'time' dimension)
            dates = [pd.to_datetime(date, format='%Y%m%d%H') for date in dates]
            # Resample data
        except ValueError:
            raise

    out = pro.sel({'time': dates})
    return out


if __name__ == '__main__':

    args = parse_command_line()
    datebegin = args.datebegin
    dateend   = args.dateend
    dates     = args.dates
    xpid      = args.xpid
    members   = args.members
    workdir   = args.workdir
    pro       = args.pro
    geometry  = args.geometry
    uenv      = args.uenv

    # 1. Move in a (clean) working directory
    # --------------------------------------
    if workdir is not None:
        os.chdir(workdir)

    # 2. Fill working directory with input files
    # -------------------------------------------

    try:
        # Try to get the PRO file with vortexIO
        io.get_pro(datebegin=datebegin, dateend=dateend, xpid=xpid, geometry=geometry, members=members)
    except (ImportError, NameError, ModuleNotFoundError):
        # Otherwise a PRO file should be provided
        # TODO : Le code actuel ne gère qu'un unique fichier PRO
        # TODO : il reste à gérer les simulations d'ensemble avec 1 PRO/membre
        # shutil.copyfile(pro, 'PRO.nc')
        os.symlink(pro, 'PRO.nc')

    # 3. Execute the core algorithm and clean up working directory
    # ------------------------------------------------------------
    if members is None:
        subdir = ''
        execute(subdir, dates)
        os.remove('PRO.nc')
    else:
        for member in range(members):
            print(f'Member {member}')
            subdir = f'mb{member:03d}'
            execute(subdir, dates)
            os.remove(os.path.join(subdir, 'PRO.nc'))
