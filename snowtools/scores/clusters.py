#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 8 march 2024

@author: Vernay

Collection of functions to clusterize data.
'''

import xarray as xr


def per_alt(data, ls_alt, mnt, elevation_label=None):
    """
      Groups data into slices based on altitude ranges.

      Args:
          data: The input dataset containing the data to be grouped.
          ls_alt: A list of altitude values defining the boundaries of each slice.
                  Values should be in ascending order.
          elevation_label: The name of the variable in the dataset containing
                  elevation values (default: 'ZS').

      Returns:
          A new dataset with the same variables as the input data, but with an
          additional dimension 'middle_slices_ZS' corresponding to the mean altitude
          slices. Each element along this dimension represents data within a
          specific altitude range.
    """
    data_per_alt = []
    for i in range(0, len(ls_alt) - 1):
        # data_per_alt.append(data.where((mnt['ZS'] >= ls_alt[i]) & (mnt['ZS'] < ls_alt[i + 1])))
        data_per_alt.append(data.where((mnt >= ls_alt[i]) & (mnt < ls_alt[i + 1])))
    data_per_alt = xr.concat(data_per_alt, dim='middle_slices_ZS')
    data_per_alt['middle_slices_ZS'] = ls_alt[1:] - (ls_alt[1] - ls_alt[0]) / 2

    return data_per_alt


def per_uncertainty(data, uncertainty, thresholds):
    out = []
    for i in range(0, len(thresholds) - 1):
        out.append(data.where((uncertainty >= thresholds[i]) & (uncertainty < thresholds[i + 1])))
    out = xr.concat(out, dim='middle_slices')
    out['middle_slices'] = thresholds[1:] - (thresholds[1] - thresholds[0]) / 2

    return out


def groupby_elevation(ds, elevations, mnt=None, elevation_label=None):
    """
    Add an elevation band dimension to a dataset to ienable the use of "groupby" method
    to compute diagnostics by elevation bands.

    It is assumed that "ds" contains a 'ZS' (relief elevation) variable.
    """
    ds['elevation_band'] = ds.ZS.copy()
    gb = ds.groupby_bins('ZS', elevations, labels=elevations[1:], restore_coord_dims=True, include_lowest=True)
    gb.var(keep_attrs=True)
    gb.groups
