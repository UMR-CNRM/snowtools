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
    parser.add_argument('-e', '--dateend', type=str, default=None,
                        help="Last date covered by the simulation file, format YYYYMMDDHH.")
    parser.add_argument('-x', '--xpid', type=str, default=None,
                        help="XPID of the simulation format XP_NAME@username")
    parser.add_argument('-m', '--members', type=int, default=None,
                        help="Number of members associated to the experiment")
    parser.add_argument('-w', '--workdir', type=str, default=None,
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
    # TODO la commande rename est très lente
    # --> faire le renomage directement dans le fichier pour éviter de le faire à chaque exécution
    # mask = mask.rename({'x': 'xx', 'y': 'yy'})

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
    # The goal of the following is to compute an array containing the number of concurent snow covered days :
    # * 0 for snow free days
    # * else : number of days since the begining of the current snow covered period
    # Example :
    # data = np.array([True,False,True,True,True,False,True,True])
    # the expected output is :
    # cumul = [1, 0, 1, 2, 3, 0, 1, 1]
    # then :
    # * scd = 3 (max(cumul))
    # * mod = 6 (max(cumul) on position 4 --> first snow free day on position 5 = 6th day since the begining)
    # * sod = 3 (3th day since the begining of the season)

    try:
        # 1. Smart solution to use on local computer or sxcen
        # WARNING : ffill does not work on Meteo-France's HPC dur to a dependency to the "bottleneck" module,
        # which is not currently installed on belneos/taranis
        # data.cumsum(dim='time') = array([1, 1, 2, 3, 4, 4, 5, 6]) (numpy syntax : data.cumsum())
        # data.cumsum(dim='time').where(data.values == 0) = array([1, 4]) (numpy : data.cumsum()[np.where(data==0)])
        # data.cumsum(dim='time').where(data.values == 0).ffill(dim='time') = array([nan, 1, 1, 1, 1, 4, 4, 4])
        # data.cumsum(dim='time').where(data.values == 0).ffill(dim='time').fillna(0) = [0, 1, 1, 1, 1, 4, 4, 4]
        # cumul = array([1, 0, 1, 2, 3, 0, 1, 2])
        cumul = data.cumsum(dim='time') - data.cumsum(dim='time').where(data.values == 0).ffill(dim='time').fillna(0)
    except ModuleNotFoundError:
        # This workaround is 10 times slower than the (better) solution above but it works on Meteo-France's HPC and
        # it returns the same result.
        def scd_periods_np(array):
            # This method is appplied to each 1D array (implicit loop over the domain's pixels) of the
            # data.cumsum(dim='time').values numpy array.
            # In the previous example, array would be array([1, 1, 2, 3, 4, 4, 5, 6])
            out = np.zeros(np.shape(array))  # array([0, 0, 0, 0, 0, 0, 0, 0])
            out[0] = array[0]  # Initialisation of the first date --> array([1, 0, 0, 0, 0, 0, 0, 0])
            for idx in range(1, len(array)):
                if array[idx] == array[idx - 1]:
                    out[idx] = 0  # snow free date --> reset the counter (array([1, 0,...]))
                else:
                    out[idx] = out[idx - 1] + 1  # increment the current concurent snow cover period duration
                    # array([1, 0, 1,...]) ; array([1, 0, 1, 2,...]) ; array([1, 0, 1, 2, 3,...])
            # out = array([1, 0, 1, 2, 3, 0, 1, 2])
            return out
        cumul = data.copy()
        cumul.values = np.apply_along_axis(scd_periods_np, 0, data.cumsum(dim='time').values)
    scd = (cumul.max(dim = 'time')).rename('scd_concurent')
    mod = (cumul.argmax(dim = 'time') + 2).rename('mod')
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

    # Add 'ZS' (DEM) variable
    if 'ZS' in pro.keys():
        diag = xr.merge([diag, pro['ZS']])

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

    if workdir is not None:
        os.chdir(workdir)

    if mask is not None:
        # Get mask file
        # import shutil
        # shutil.copyfile(mask, 'mask.nc')
        if not os.path.exists('mask.nc'):  # TODO remplacer le lien par sécurité ?
            os.symlink(mask, 'mask.nc')
        mask = True

    if xpid is not None:
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
        for member in range(members):
            print(f'Member {member}')
            subdir = f'mb{member:03d}'
            diag(subdir, datebegin, mask=mask)
            os.remove(os.path.join(subdir, 'PRO.nc'))
