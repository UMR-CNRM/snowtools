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
import numpy as np
import xarray as xr
import pandas as pd
import argparse


def parse_command_line():
    description = "Computation of Sentinel2-like diagnostics (snow melt-out date, snow cover duration) associated \
                   to a SURFEX simulation"
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-b', '--datebegin', type=str, required=True,
                        help="First date covered by the simulation file, format YYYYMMDDHH.")
    parser.add_argument('-e', '--dateend', type=str,
                        help="Last date covered by the simulation file, format YYYYMMDDHH.")
    parser.add_argument('-x', '--xpid', type=str, required=True,
                        help="XPID of the simulation format XP_NAME@username")
    parser.add_argument('-m', '--members', type=int, default=None,
                        help="Number of members associated to the experiment")
    parser.add_argument('-w', '--workdir', type=str, required=True,
                        help="Working directory")
    parser.add_argument('-k', '--mask', type=str,
                        help="Absolute path to the mask file")
    parser.add_argument('-p', '--pro', type=str,
                        help="Absolute path to the pro file")
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
    mask = xr.open_dataset('mask.nc')['Band1']
    mask = mask.rename({'x': 'xx', 'y': 'yy'})

    # Interpolate the glacier mask to the same resolution as the input array
    mask = mask.interp_like(arr, method=method)

    # Mask the input array based on the glacier mask
    return arr.where(mask == 0)


def lcscd(data, threshold=.2):
    """
    Compute the following diagnostic variables from PRO DSN_T_ISBA (snow depth) variable :
    * LCSCD  : Longest Concurent Snow Cover Duration period
    * LCSMOD : Snow Melt Out Date of the Longest Concurent snow cover period
    * LCSOD  : Snow Cover Onset date of the Longest Concurent snow cover period
    """

    data = xr.where(data > threshold, True, False)
    cumulative = data.cumsum(dim='time') - data.cumsum(dim='time').where(data.values == 0).ffill(dim='time').fillna(0)
    scd = (cumulative.max(dim = 'time')).rename('scd_concurent')
    mod = (cumulative.argmax(dim = 'time') + 1).rename('mod')
    sod = (mod - scd).rename('sod')
    sd = data.where(data, np.nan).count(dim = 'time').rename('sd')
    return xr.merge([scd, mod, sod, sd])


def decode_time(pro):
    """
    Manually decode time variable since other variables can not be decoded automatically
    """
    ds = xr.Dataset({"time": pro.time})
    ds = xr.decode_cf(ds)
    pro['time'] = ds.time
    return pro


def filter_dates(pro, datebegin):
    """
    Sentinel 2 SCD/SMOD are defined as days since sept. 1 and PRO files start on aug. 1.
    This method ensures that the unit diagnostics computed from the PRO files is
    'days since sept. 1'.
    WARNING : Pixels detected by Sentinel2 as 'snow covered' after aug. 1 of the next year must also be filtered
    out since SMOD/SCD can not be computed from the simulation in this case.
    """
    deb = pd.to_datetime(datebegin, format='%Y%m%d%H%M')
    deb = deb.replace(month=9, day=1)
    pro = pro.sel({'time': pro.time.data > deb})
    return pro


def diag(subdir, datebegin, mask=True):
    """
    Main method to compute Sentinel2-like diagnostics from a SURFEX simulation
    """
    proname = os.path.join(subdir, 'PRO.nc')
    pro = xr.open_dataset(proname, decode_times=False)
    pro = decode_time(pro)
    pro = filter_dates(pro, datebegin)
    diag = lcscd(pro.DSN_T_ISBA.resample(time='1D').mean())

    if mask:
        # mask glacier/forest covered pixels
        diag = maskgf(diag)

    # Write DIAG file and remove PRO
    diag.to_netcdf(os.path.join(subdir, 'DIAG.nc'))


if __name__ == '__main__':

    args = parse_command_line()
    datebegin = args.datebegin
    dateend   = args.dateend
    xpid      = args.xpid
    members   = args.members
    workdir   = args.workdir
    mask      = args.mask
    pro       = args.pro
    geometry  = 'GrandesRousses250m'

    os.chdir(workdir)

    if mask is not None:
        # Get mask file
        # import shutil
        # shutil.copyfile(mask, 'mask.nc')
        os.symlink(mask, 'mask.nc')
        mask = True

    try:
        # Retrieve PRO files with Vortex
        from snowtools.scripts.extract.vortex import vortexIO
        vortexIO.get_pro(datebegin, dateend, xpid, geometry, members=members)
    except (ImportError, ModuleNotFoundError):
        # TODO : Le code actuel ne gère qu'un unique fichier PRO
        # TODO : il reste à gérer les simulations d'ensemble avec 1 PRO/membre
        # shutil.copyfile(pro, 'PRO.nc')
        os.symlink(pro, 'PRO.nc')

    if members is None:
        subdir = ''
        diag(subdir, datebegin, mask=mask)
        os.remove('PRO.nc')
    else:
        for member in range(1, members + 1):
            print(f'Member {member}')
            subdir = f'mb{member:03d}'
            diag(subdir, datebegin, mask=mask)
            os.remove(os.path.join(subdir, 'PRO.nc'))
