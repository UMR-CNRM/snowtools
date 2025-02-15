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
import numpy as np
import xarray as xr
import argparse

import snowtools.tools.xarray_preprocess as xrp


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


def xr_ffill(da, dim='time', fillna=False):
    """
    Solution inspired from :
    https://stackoverflow.com/questions/41190852/most-efficient-way-to-forward-fill-nan-values-in-numpy-array

    Arguments :
    -----------
    * da     : DataArray.
    * dim    : string. Name of the dimension along which the forward-fill should be applied
    * fillna : bool. If True, fill remaining NaN values with 0
    """
    axis = np.where(np.array([dimension for dimension in da.dims]) == dim)[0][0]
    arr = da.data
    idx_shape = tuple([slice(None)] + [np.newaxis] * (len(arr.shape) - axis - 1))
    idx = np.where(~np.isnan(arr), np.arange(arr.shape[axis])[idx_shape], 0)
    np.maximum.accumulate(idx, axis=axis, out=idx)
    slc = [np.arange(k)[tuple([slice(None) if dim == i else np.newaxis
        for dim in range(len(arr.shape))])]
        for i, k in enumerate(arr.shape)]
    slc[axis] = idx
    outdata = arr[tuple(slc)]
    if fillna:
        outdata[np.isnan(outdata)] = 0
    da.data = outdata

    return da


def lcscd(data, threshold):
    """
    Arguments :
    -----------
    * Data (xarray.DataArray) :  Daily snow depths (can cover several years : in this case the diagnostics
                  are computed for every 01/09 --> 31/08 time period in *data*)
    * threshold (float)       : Snow depth threshold to consider a pixel as "snow covered"

    Output :
    --------
    * xarray.Dataset containing the following variables :
    - LCSCD  : Longest Concurent Snow Cover Duration period
    - LCSMOD : Snow Melt Out Date of the Longest Concurent snow cover period
    - LCSOD  : Snow Cover Onset date of the Longest Concurent snow cover period
    - SD     : Snow duration : total number of snow coverage

    Method :
    --------

    1. Classify each date/day as "snow covered" or "snow free" using the prescribed threshold

    2. Associate to each time/day of *data* the starting date of the corresponding season (the previous 1 septembre)
        - Get indices of all dates corresponding to september 1st
        - "forward fill" (see warning below) these values to associate each date to its season's starting date
        - Remove dates before the first 1st september and force all 1 september as snow free dates to match Sentinel2
          convention

    3. Use the xarray "groupby" method to compute the diagnostics for each season
        - Create a time serie containing the number of snow covered dates since the begining of the seaon with
          the xarray's "cumsum" method (see warning below)
        - Create a time serie containing the number of snow free days since the begining of the season after
          each snow cover period
        --> the difference between these 2 time series is a time serie containing the number of days since the
            apparition of snow for each snow covered period of the season

    4. Comute all diagnotics from the time serie obtained in step 3
        - SCD = maximum value (higher number of concurent snow covered days)
        - MOD = index (number of days since 1 september) of the SCD value
        - SOD = MOD - SCD (retract the number of snow days to get the index of the begining of the period)
        - SD  = Total number of days counted as snow covered


    WARNINGs :
    ----------

    * The standard solution to compute the longest continuous snow cover duration relies on the use of the
      xarray "ffill" method that uses the "bottleneck" module which is not available on MF's HPC.
      --> a custom "xr_ffill" method based on numpy only is used in this case
      TODO : compare the performances of both methods

    * The "cumsum" method as be added to xarray objects after the current version 2023.3.0 installed by default
     --> This script requires an upgrade of xarray (or the developpement of an alternative numpy-based solution)

     NB :
     ----
    The use of xarray.DataArray object instead of numpy arrays is motivated by several reasons :
    * The "resample" method used to resample hourly HTN values to daily one has not equivalent in prosimu / numpy
    * The multi-season situation is managed using the "groupby" method that does not exist in numpy
    * The "cumsum" method exists in numpy (with the same core than the xarray's one), but with xarray
      the dimension order management is explicit (parsing a dimension name instead of an index number that can vary
      between numpy arrays)
    * The "ffill" method has no equivalent in numpy
    * The final goal is to write a netcdf file, which is straightforward with a DataArray but requires
      (a bit of) formatting with numpy
    """

    # Select snow covered pixels
    data = xr.where(data > threshold, True, False)
    # Add "startseason" information
    startseason = data.time.where((data.time.dt.month == 9) & (data.time.dt.day == 1))
    try:
        # Try to use xarray's native "ffill" method based on bottleneck (not available on MF's HPC)
        # Return a "startseason" variable containing the preceding 1 septembre of each date
        startseason = startseason.ffill(dim='time')
    except ModuleNotFoundError:
        # If "bottleneck" not available, use a custom ffill function
        startseason = xr_ffill(startseason)
    data['startseason'] = startseason
    data = data[~np.isnan(startseason)]  # Remove dates before the first 1 september in data
    data = xr.where(data.time == startseason, 0, data)  # Force 1 september as snow free day
    # Group data by seasons (from 1 septembre of each year)
    gp = data.groupby('startseason')

    # The goal of the following is to compute an array containing the number of concurent snow covered days :
    # * 0 for snow free days
    # * else : number of days since the begining of the current snow covered period
    # Example :
    # data = np.array([True,False,True,True,True,False,True,True])
    # the expected output is :
    # cumul = [1, 0, 1, 2, 3, 0, 1, 2]
    # then :
    # * scd = 3 (max(cumul))
    # * mod = 6 (max(cumul) on position 4 --> first snow free day on position 5 = 6th day since the begining)
    # * sod = 3 (3th day since the begining of the season)
    try:
        # Try to use xarray's native "ffill" method based on bottleneck (not available on MF's HPC)
        # 1. Smart solution to use on local computer or sxcen
        # WARNING : ffill does not work on Meteo-France's HPC dur to a dependency to the "bottleneck" module,
        # which is not currently installed on belneos/taranis
        # data.cumsum(dim='time') --> array([1, 1, 2, 3, 4, 4, 5, 6]) --> Snow days since the begining of the season
        # data.cumsum(dim='time').where(data.values == 0) --> array([1, 4]) --> Snow free days
        # data.cumsum(dim='time').where(data.values == 0).ffill(dim='time') --> array([nan, 1, 1, 1, 1, 4, 4, 4])
        # --> Number of snow days occurences before each snow free day
        # data.cumsum(dim='time').where(data.values == 0).ffill(dim='time').fillna(0) --> [0, 1, 1, 1, 1, 4, 4, 4]
        # fillna(0) deal with snow days starting before 1 september
        # cumul --> array([1, 0, 1, 2, 3, 0, 1, 2])
        # WARNING : applying cumsum method on a *DataArrayGroupBy* method requires version 2024.3.0 of xarray
        cumul = gp.cumsum(dim='time') - gp.cumsum(dim='time').where(data.values == 0).ffill(dim='time').fillna(0)
    except (ModuleNotFoundError, AttributeError):
        # If "bottleneck" not available, use a custom ffill function
        cumul = data.copy()  # Construct an array similar to data
        # cumsum method not available on xarray's version 0.16.0 (MF HPC's valid install on 27/05/2024)
        # --> replace by an "apply" function untill the next version update
        try:
            cs = gp.cumsum(dim='time')
        except AttributeError:
            cs = gp.apply(lambda x: x.cumsum(dim='time'))
        # cs --> array([1, 1, 2, 3, 4, 4, 5, 6]) (numpy syntax : data.cumsum())
        # Apply custom "xr_ffill" function that does not depend on bottleneck
        npfill = xr_ffill(cs.where(data.values == 0), fillna=True)
        cumul = cs - npfill

    # Compute longest snow cover duration for each group/season as the maximum value along the time dimension
    scd = cumul.groupby('startseason').max(dim='time').rename('scd_concurent')
    # Compute the snow melt out date for each group/season as the index of the maximum value along the time dimension
    mod = (cumul.groupby('startseason').apply(lambda c: c.argmax(dim="time")) + 1).rename('mod')
    # Return Nan when snow cover duration is 0 (no day with snow depth above threshold)
    mod = xr.where(scd.data == 0, np.nan, mod)
    # scd = (cumul.max(dim = 'time')).rename('scd_concurent')
    # mod = (cumul.argmax(dim = 'time') + 1).rename('mod')
    sod = (mod - scd).rename('sod')
    sd  = data.where(data, np.nan).count(dim = 'time').rename('sd')
    return xr.merge([scd, mod, sod, sd])


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
