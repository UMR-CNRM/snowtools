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


def by_slices(data, criterion, thresholds):
    """
      Groups data into slices according to a given criterion

      Args:
          data      : The input dataset containing the data to be grouped.
          criterion : Dataarray of the variable to use for clustering
          threshold : A list of values defining the boundaries of each slice.
                  Values should be in ascending order.

      Returns:
          A new dataset with the same variables as the input data, but with an
          additional dimension 'middle_slices' corresponding to the mean value of each
          slice. Each element along this dimension represents data within a
          specific range.

      THIS METHOD SHOULD BE USED INSTEAD OF THE "per_alt" ONE
    """
    thresholds = [float(value) for value in thresholds]
    out    = []
    labels = []
    for i in range(len(thresholds) - 1):
        out.append(data.where((criterion >= thresholds[i]) & (criterion < thresholds[i + 1])))
        labels.append(f'{thresholds[i]}-{thresholds[i+1]}')
    out = xr.concat(out, dim='slices')
    out['slices'] = labels

    return out
