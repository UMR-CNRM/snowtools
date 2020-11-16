#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Jul  9 16:44:24 2019

@author: reveilletm
"""

# LOAD LIBRARIES
import netCDF4 as nc
from netCDF4 import Dataset
import pandas as pd
import numpy as np

import shapefile
from shapely.geometry import shape, Point, Polygon, MultiPolygon

import matplotlib.pyplot as plt


# #############################################################################
#
#               Script to add massif info into the netcdf forcing
# 
###############################################################################

## 1 - load forcing
ftot = Dataset("../../../manto/lafaysse/alphagrid/alpha.nc")
lats = ftot.variables['latitude'][:] 
lons = ftot.variables['longitude'][:]

# and select point only for the massif 
# ALPS
#lat_bnds, lon_bnds = [43.90, 46.43], [5.18, 7.78]
# PYRENNEES
#lat_bnds, lon_bnds = [42.07, 43.18], [-1.64, 2.71]
# CORSE
lat_bnds, lon_bnds = [41.69, 42.56], [8.77, 9.28]
lat_inds = np.where((lats > lat_bnds[0]) & (lats < lat_bnds[1]))
lon_inds = np.where((lons > lon_bnds[0]) & (lons < lon_bnds[1]))
lat_alps = ftot.variables['latitude'][lat_inds]
lon_alps = ftot.variables['longitude'][lon_inds]
ZS_alps = ftot.variables['ZS'][np.min(lat_inds):np.max(lat_inds)+1,np.min(lon_inds):np.max(lon_inds)+1]


## 2- load Shapefile
# ALPS:
#r = shapefile.Reader('/home/reveilletm/Data/Map/massifs_Safran/shp_2154/massifs_alpes_4326')
# PYRENNES:
#r = shapefile.Reader('/home/reveilletm/Data/Map/massifs_Safran/shp_2154/massifs_pyrenees_4326')
# CORSE:
r = shapefile.Reader('/home/reveilletm/Data/Map/massifs_Safran/shp_2154/massifs_corse_4326')
shapes = r.shapes() # get the shapes


## 3- Create variable and fill in with massif number
massif_num=np.zeros((lon_alps.shape[0],lat_alps.shape[0]))
max_alps=23
max_pyr=22
max_cor=2

for nb in range(0,max_cor):  
    print(nb)
    
    #Massif and information
    polygon = shape(shapes[nb]) # build a shapely polygon from your shape 
    geomet = r.shapeRecords() #will store the geometry separately
    first = geomet[nb] #will extract the first polygon to a new object
    poly1_info = first.record #will show you the attributes
    M_nb=poly1_info[0]
    
    #Select point into the polygone and add massif info
    for i in range(0,lon_alps.shape[0]):
        for j in range(0,lat_alps.shape[0]):
            point = Point(lon_alps[i], lat_alps[j])
            R = polygon.contains(point)
            if R:
                massif_num[i,j]=M_nb
          
            
## 4- Check results if you want            
results=np.transpose(massif_num)        
plt.imshow(results);
plt.colorbar()
plt.show()   
   
plt.imshow(ZS_alps);
plt.colorbar()
plt.show()  


#%% 5- Save netcdf file
# ALPS:
#outputs = Dataset('ALPS.nc', 'w', format='NETCDF4')     
# PYRENNES:
#outputs = Dataset('PYRENNES.nc', 'w', format='NETCDF4') 
# CORSE:
outputs = Dataset('CORSE.nc', 'w', format='NETCDF4')        
outputs.createDimension('lat', lat_alps.shape[0])
outputs.createDimension('lon', lon_alps.shape[0])
A=outputs.createVariable('latitude', np.float64,('lat',), fill_value=-9999)
B=outputs.createVariable('longitude', np.float64,('lon',), fill_value=-9999)
C=outputs.createVariable('massif_num', np.float64, ('lat', 'lon'), fill_value=-9999)
D=outputs.createVariable('ZS', np.float64, ('lat', 'lon'), fill_value=-9999)

num=np.flip(results,0)
Z=np.flip(ZS_alps,0)
outputs["latitude"][:,] = lat_alps
outputs["longitude"][:,] = lon_alps
outputs["massif_num"][:,:] = num
outputs["ZS"][:,:] = Z

A.setncatts({'long_name': u"latitude",\
                'units': u"degrees_north"})    
B.setncatts({'long_name': u"longitude",\
                'units': u"degrees_east"})
C.setncatts({'long_name': u"massif Number"})
D.setncatts({'long_name': u"Geometrical height",\
                'units': u"m",\
                'cell_method': u"time: mean"})                                  
    
outputs.close()
