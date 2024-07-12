#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 8 March 2024

@author: Vernay, Haddjeri

Collection of functions to clusterize data.
"""

import xarray as xr
import numpy as np


def per_alt(data, ls_alt, mnt):
    """
    Groups data into slices based on altitude ranges.

    :param data: The input dataset containing the data to be grouped.
    :type data: xarray dataset
    :param ls_alt: A list of altitude values defining the boundaries of each slice.
                  Values should be in ascending order.
    :param mnt: The data variable containing elevation values
    :type mnt: xarray data array

    :returns: A new dataset with the same variables as the input data, but with an
          additional dimension 'middle_slices_ZS' corresponding to the mean altitude
          slices. Each element along this dimension represents data within a
          specific altitude range.
    :rtype: xarray dataset
    """
    data_per_alt = []
    for i in range(0, len(ls_alt) - 1):
        # data_per_alt.append(data.where((mnt['ZS'] >= ls_alt[i]) & (mnt['ZS'] < ls_alt[i + 1])))
        data_per_alt.append(data.where((mnt >= ls_alt[i]) & (mnt < ls_alt[i + 1])))
    data_per_alt = xr.concat(data_per_alt, dim='middle_slices_ZS')
    data_per_alt['middle_slices_ZS'] = ls_alt[1:] - (ls_alt[1] - ls_alt[0]) / 2

    return data_per_alt


def per_landform_types(data, landform, test_coords=False):
    """
    Groups data into landform types as defined by https://doi.org/10.1016/j.geomorph.2012.11.00

    :param data: The input dataset containing the data to be grouped.
    :type data: xarray dataset
    :param landform: The DEM landform data array. SHOULD BE ON THE SAME GRID AS DATA AND COMPUTED BEFORE
            (doc : http://intra.cnrm.meteo.fr/cen/snowtools/extra_documentation/Masks.html#geomorphons-masks)
    :type landform: xarray data array
    :param test_coords: The grouping fail if landform and data are not on the same grid (does not share coordinates).
            This option test if common values exist between the two inputs.
    :type test_coords: bool

    :returns: A new dataset with the same variables as the input data, but with an additional dimension and
        coordinate named 'landforms' with value corresponding to each pixels respective landform type.
    :rtype: xarray dataset
    """
    if test_coords:
        print('Testing coordinates')
        for k in data.coords.keys():
            for m in landform.coords.keys():
                try:
                    np.intersect1d(landform[m], data[k])
                except:
                    print('No common coordinate between '+str(m)+' and '+str(k))
                else:
                    print('Common coordinate found between '+str(m)+' and '+str(k))

    ii = 0
    for i in np.unique(landform.values)[~np.isnan(np.unique(landform.values))]:
        if ii == 0:
            dd = xr.where(landform.values == i, data, np.nan).assign_coords({'landforms': geomorpho_switch(i)
                                                                             }).expand_dims('landforms')
            ii = ii+1
        else:
            dd = xr.concat((dd,
                            xr.where(landform.values == i, data, np.nan).assign_coords({'landforms': geomorpho_switch(i)
                                                                                        }).expand_dims('landforms')),
                           'landforms')

    return dd


def geomorpho_switch(i):
    """
    Function to convert the numeral code to the name of the 10 most common
    landforms types defined by https://doi.org/10.1016/j.geomorph.2012.11.005

    :param i: the numeral landform code

    :returns:
        The litteral translation of the code according to paper convention.
    :rtype: str
    """

    if i == 1.:
        return 'Flat'
    if i == 2.:
        return 'Peak (summit)'
    if i == 3.:
        return 'Ridge'
    if i == 4.:
        return 'Shoulder'
    if i == 5.:
        return 'Spur (Convex)'
    if i == 6.:
        return 'Slope'
    if i == 7.:
        return 'Hollow (concave)'
    if i == 8.:
        return 'Footslope'
    if i == 9.:
        return 'Valley'
    if i == 10.:
        return 'Pit'
