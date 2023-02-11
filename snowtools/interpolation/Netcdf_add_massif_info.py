#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul  9 16:44:24 2019

:Authors:
    reveilletm
    SGD team

This script is adding massif info into a netcdf forcing where dimension of ZS is (latitude, longitude)
"""

from functools import partial
import argparse
import os

import numpy as np
from netCDF4 import Dataset
import pyproj
import shapefile
from shapely.geometry import shape, Point
from shapely.ops import transform
import matplotlib.pyplot as plt

from snowtools.DATA import SNOWTOOLS_DATA


latitude_names = ['latitude', 'lat', 'LAT']
longitude_names = ['longitude', 'lon', 'LON']

# Create argument parser
parser = argparse.ArgumentParser(description="""
Script to add massif info into the netcdf forcing 2D with a variable ZS. Dimension of ZS = (latitude, longitude). 

Example of use:
python3 Netcdf_add_massif_info /home/reveilletm/alpha4.nc -p
python3 Netcdf_add_massif_info.py input_4_test_add_massif_info.nc -p -v -o output_4_test_add_massif_info.nc
""")

# Mandatory argument
parser.add_argument("path_forcing", help="Path to forcing file (mandatory)", type=str)

# Optional argument
parser.add_argument("-s", "--shape", help="Path to shapefile file", type=str,
                    default=os.path.join(SNOWTOOLS_DATA, 'massifs'), dest='path_shape')
parser.add_argument("-p", "--plot", help="Make a plot to check", action='store_true', dest='bool_plot')
parser.add_argument("-v", "--verbose", help="Print index in loop", action='store_true', dest='bool_print')
parser.add_argument("-o", "--output", help="Path to output file", type=str, default='alpha_massifs.nc',
                    dest='path_output')

args = parser.parse_args()

# 1 - load forcing
with Dataset(args.path_forcing) as ftot:
    for lat_name in latitude_names:
        if lat_name in ftot.variables:
            lats = ftot.variables[lat_name][Ellipsis]
            # we want to get latitude in 1d-array -> 2 possibilities
            # - latitude is 2d-array (standard 2d_forcing)
            # - latitude is 1d-array (original code)
            if len(lats.shape) == 2:
                lats = ftot.variables[lat_name][:, 0]
            else:
                lats = ftot.variables[lat_name][:]
            break
    else:
        raise ValueError('latitude not found')
    for lon_name in longitude_names:
        if lon_name in ftot.variables:
            lons = ftot.variables[lon_name][Ellipsis]
            # Idem as latitude.
            if len(lons.shape) == 2:
                lons = ftot.variables[lon_name][0, :]
            else:
                lons = ftot.variables[lon_name][:]
            break
    else:
        raise ValueError('longitude not found')
    ZS = ftot.variables['ZS'][:, :]

# 2- load Shapefile
r = shapefile.Reader(args.path_shape)
shapes = r.shapes()  # get the shapes
if args.bool_print:
    print(len(shapes))

# prepare coordinate transformation from Lambert 93 to lon/lat
project = partial(pyproj.transform, pyproj.Proj(init='epsg:2154'), pyproj.Proj(init='epsg:4326'))

# 3- Create variable and fill in with massif number
massif_num = np.zeros((lons.shape[0], lats.shape[0]))
#max_alps = 23
#max_pyr = 22
#max_cor = 2

for nb in range(0, len(shapes)):
    if args.bool_print:
        print(nb)
    
    # Massif and information
    polygon = shape(shapes[nb])  # build a shapely polygon from your shape
    geomet = r.shapeRecords()  # will store the geometry separately
    first = geomet[nb]  # will extract the first polygon to a new object
    poly1_info = first.record  # will show you the attributes
    M_nb = poly1_info[0]
    polygon2 = transform(project, polygon)
    # print(M_nb)
    # print(polygon2)
    # raise RuntimeError

    # Select point into the polygone and add massif info
    for i in range(0, lons.shape[0]):
        # print(i)
        for j in range(0, lats.shape[0]):
            point = Point(lons[i], lats[j])
            # print(point)
            R = polygon2.contains(point)
            # print(R)
            if R:
                # print(M_nb)
                massif_num[i, j] = M_nb
          
            
# 4- Check results if you want
results = np.transpose(massif_num)
if args.bool_plot:
    plt.imshow(results)
    plt.colorbar()
    plt.show()
   
    plt.imshow(ZS)
    plt.colorbar()
    plt.show()


# %% 5- Save netcdf file
with Dataset(args.path_output, 'w', format='NETCDF4') as outputs:
    outputs.createDimension('lat', lats.shape[0])
    outputs.createDimension('lon', lons.shape[0])
    A = outputs.createVariable('latitude', np.float64, ('lat', ), fill_value=-9999)
    B = outputs.createVariable('longitude', np.float64, ('lon', ), fill_value=-9999)
    C = outputs.createVariable('massif_num', np.float64, ('lat', 'lon'), fill_value=-9999)
    D = outputs.createVariable('ZS', np.float64, ('lat', 'lon'), fill_value=-9999)

    num = np.flipud(results)
    #Z = np.flipud(ZS)
    outputs["latitude"][:, ] = lats
    outputs["longitude"][:, ] = lons
    outputs["massif_num"][:, :] = num #results
    outputs["ZS"][:, :] = ZS  # Z

    A.setncatts({'long_name': u"latitude", 'units': u"degrees_north"})
    B.setncatts({'long_name': u"longitude", 'units': u"degrees_east"})
    C.setncatts({'long_name': u"massif Number"})
    D.setncatts({'long_name': u"Geometrical height", 'units': u"m", 'cell_method': u"time: mean"})
