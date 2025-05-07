#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 8 march 2024

@author: Vernay

Script for the computation of Sentinel2-like diagnostics (snow melt-out date, snow cover duration)
from a SURFEX-Crocus simulation.
This script can either be called directly or throught the "diag_sentinel2" Vortex task via the
"Sentinel2_Diagnostics" Vortex algo component (cen/algo/postprocessing.py).
'''

import os
import sys
import xarray as xr
import argparse

import snowtools.tools.xarray_preprocess as xrp
from snowtools.tools.SnowCoverDuration import lcscd


def parse_command_line():
    description = "Computation of Sentinel2-like diagnostics (snow melt-out date, snow cover duration) associated \
                   to a SURFEX simulation"
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-b', '--datebegin', type=str, default=None,
                        help="First date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-e', '--dateend', type=str, default=None,
                        help="Last date covered by the simulation file, format YYYYMMDDHH.")

    parser.add_argument('-x', '--xpid', type=str, default=None,
                        help="XPID of the simulation format XP_NAME@username")

    parser.add_argument('-g', '--geometry', type=str, default='GrandesRousses250m',
                        help='Geometry of the simulation(s) / observation')

    parser.add_argument('-m', '--members', type=int, default=None,
                        help="Number of members associated to the experiment")

    parser.add_argument('-w', '--workdir', type=str, default=None,
                        help="Working directory")

    parser.add_argument('-p', '--pro', type=str, required=not any(arg in sys.argv for arg in ['-x', '--xpid']),
                        help="Absolute path to the pro file")

    parser.add_argument('-t', '--threshold', type=str, default=0.2,
                        help="Threshold to apply to the simulated snow depth in order to consider that the pixel"
                             "is covered by snow.")

    args = parser.parse_args()
    return args


def decode_time(pro):
    """
    Manually decode time variable since other variables can not be decoded automatically
    """
    ds = xr.Dataset({"time": pro.time})
    ds = xr.decode_cf(ds)
    pro['time'] = ds.time
    return pro


def execute(subdir='', threshold=0.2):
    """
    Main method to compute Sentinel2-like diagnostics from a SURFEX simulation
    """
    proname = os.path.join(subdir, 'PRO.nc')
    pro = xr.open_dataset(proname, decode_times=False, engine='netcdf4')
    pro = xrp.preprocess(pro)
    pro = decode_time(pro)
    diag = lcscd(pro.DSN_T_ISBA.resample(time='1D').mean(), threshold)

    # Add 'ZS' (DEM) variable
    if 'ZS' in pro.keys():
        diag = xr.merge([diag, pro['ZS']])

    # Write DIAG file and remove PRO
    diag.to_netcdf(os.path.join(subdir, 'DIAG.nc'))


if __name__ == '__main__':

    args = parse_command_line()
    datebegin = args.datebegin
    dateend   = args.dateend
    xpid      = args.xpid
    members   = args.members
    workdir   = args.workdir
    pro       = args.pro
    threshold = args.threshold
    geometry  = 'GrandesRousses250m'

    if workdir is not None:
        os.chdir(workdir)

    if pro is not None:
        # TODO : Le code actuel ne gère qu'un unique fichier PRO
        # TODO : il reste à gérer les simulations d'ensemble avec 1 PRO/membre
        # --> open_mfdataset(list_pro, concat_dim='member')
        if os.path.islink('PRO.nc'):
            os.remove('PRO.nc')
        if not os.path.exists('PRO.nc'):
            os.symlink(pro, 'PRO.nc')
    else:
        # Retrieve PRO files with Vortex
        from snowtools.scripts.extract.vortex import vortexIO
        vortexIO.get_pro(xpid, geometry, datebegin=datebegin, dateend=dateend, members=members)

    if members is None:
        subdir = ''
        # Call main method
        execute(subdir=subdir, threshold=threshold)
    else:
        for member in range(members):
            print(f'Member {member}')
            subdir = f'mb{member:03d}'
            # Call main method
            execute(subdir=subdir, threshold=threshold)
