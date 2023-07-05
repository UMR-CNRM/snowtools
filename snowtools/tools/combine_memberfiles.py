#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jul  5 16:04:43 2023

@author: Jari-Pekka Nousu

"""

import glob
from netCDF4 import Dataset
import re
import os

def combine_memberfiles(parentfolder, keep_open=False):
    ''' 
    Combining ensemble of SURFEX outputs into a single outputfile 
    with additional dimension for ensemble members 
    IN: 
        - parentfolder hosting output folders for each member
    OUT: 
        - creates output folder e.g. 'mb0001_mb0005'
        - adds new profile file
    NOTE:
        - currently ensemble dimensions added for all variables, including constant outputs..
        - couple of work arounds to avoid AttributeErrors, fix better later
        - only tested for outputs for one point
    '''
    
    # find the folders under parentfolder
    folders = glob.glob(f'{parentfolder}/mb*')
    # sorting to have members in order
    folders.sort()
    # finding pro files inside folders
    files = []
    for f in folders:
        files.append(glob.glob(f'{f}/pro/*.nc')[0])
    files.sort()

    # opening first file to derive information on dimensions, variables etc.
    data = Dataset(files[0], 'r')
    
    # defining outputfilename according to first and last member
    outname = re.split('(/)', files[0])[-1]
    firstmember = re.split('(/)', folders[0])[-1]
    lastmember = re.split('(/)', folders[-1])[-1]
    outputfolder = f'{parentfolder}/{firstmember}_{lastmember}'
    outfilefull = f'{outputfolder}/{outname}'

    # new repo for output if does not exist
    if not os.path.exists(outputfolder):
        os.makedirs(outputfolder)

    # creating the new output netcdf file
    new_output = Dataset(outfilefull, 'w', format='NETCDF4_CLASSIC')

    # dimensions as in the example file
    for d in data.dimensions.keys():
        new_output.createDimension(d, data.dimensions[d].size)
    
    # plus additional dimension for ensemble members
    new_output.createDimension('Ensemble_members', len(files))

    # list of key information
    list_of_variables = list(data.variables.keys())
    list_of_variable_units = []
    list_of_variable_longnames = []

    # create list of units
    for v in data.variables.keys():
        try:
            list_of_variable_longnames.append(data[v].long_name)
        except AttributeError:
            list_of_variable_longnames.append('')        
        try:
            list_of_variable_units.append(data[v].units)
        except AttributeError:
            list_of_variable_units.append('')

    # creating variables for netcdf file
    for i in range(len(list_of_variables)):
        variable = list_of_variables[i]
        try:
            units = data[variable].units
        except AttributeError:
            units = False
        dtype = data[variable].dtype
        if variable == 'time':
            dimensions = data[variable].dimensions
        else:
            dimensions = data[variable].dimensions + ('Ensemble_members',)
        longname = list_of_variable_longnames[i]
        try:
            fill_value = data[variable]._FillValue
        except AttributeError: # going around AttributeError, at least 'Projection_Type'... fix this better later
            fill_value = -2147483647
    
        output_var = new_output.createVariable(variable, dtype, dimensions,
                                        fill_value=fill_value)
        if units != False:
            output_var.units = units
            output_var.long_name = longname

    # looping over the files to read data into right member dimension
    for member in range(len(files)):
        file = files[member]
        print('Filling ensemble member:', member)
        tempfile = Dataset(file, 'r')        
        for key in list_of_variables:
            if tempfile[key].dimensions != ():
                dimensionlen = len(list(tempfile[key].dimensions))
            else:
                dimensionlen = 0
            if dimensionlen == 4:
                new_output[key][:,:,:,:,member] = tempfile[key][:,:,:,:]
            elif dimensionlen == 3:
                new_output[key][:,:,:,member] = tempfile[key][:,:,:]            
            elif dimensionlen == 2:
                new_output[key][:,:,member] = tempfile[key][:,:]       
            elif dimensionlen == 1:
                new_output[key][:] = tempfile[key][:]
        tempfile.close()
    
    if keep_open == True:
        return new_output
    else:
        new_output.close()