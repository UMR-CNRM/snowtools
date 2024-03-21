import os
import xarray as xr
import pandas as pd

from snowtools.scores import clusters


def maskgf(arr, method='nearest', maskfile='MASK.nc'):
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
    mask = xr.open_dataset(maskfile)['Band1']
    # TODO la commande rename est très lente
    # --> faire le renomage directement dans le fichier pour éviter de le faire à chaque exécution
    # mask = mask.rename({'x': 'xx', 'y': 'yy'})

    # Interpolate the glacier mask to the same resolution as the input array
    mask = mask.interp_like(arr, method=method)

    # Mask the input array based on the glacier mask
    return arr.where(mask == 0)


def elevation_string(band):
    """
    Formats an array of elevation values into a string representation with line breaks.

    Args:
        band (list): A list of numerical elevation values.

    Returns:
        list: A new list containing formatted strings representing the elevation ranges.
    """

    out = []
    for i in range(len(band) - 1):
        # Format each string with elevation range and a newline character
        formatted_string = f"{band[i]} to {band[i+1]}"
        out.append(formatted_string)

    return out


def filter_dataset(ds, date, mnt, elevation_bands):

    # TODO : gérer le problème de coordonnées pour éviter les "rename" très lents !
    ds = decode_time(ds)
    ds = ds.sel({'time': pd.to_datetime(date[:8], format='%Y%m%d')})
    mnt = mnt.rename({'x': 'xx', 'y': 'yy'})
    ds = maskgf(ds)
    filtered_ds = clusters.per_alt(ds, elevation_bands, mnt)

    return filtered_ds

    # df = filtered_simu.to_dataframe(name=xpid).dropna().reset_index().drop(columns=['xx', 'yy', 'time'])
    #df = filtered_simu.to_dataframe(name=xpid).dropna().drop(columns=['time'])
    #return df


def decode_time(pro):
    """
    Manually decode time variable since other variables can not be decoded automatically
    """
    ds = xr.Dataset({"time": pro.time})
    ds = xr.decode_cf(ds)
    pro['time'] = ds.time
    return pro