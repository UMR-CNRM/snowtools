#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 15 may 2025

@author: Diego Monteiro

Create a new forcing file from an initial one (containing at least specific humidity, air temperature and total precipitation) 
with recalculated solid and liquid precipitation based on Froidurot et al., (2014) method. 
The method separate total precipitation, either given as one variable (PTOT) or two (Rainf and Snowf), 
using a logistic function that take near surface air temperature (°C) and relative humidity (%)
as predictors.
Note : Some parameters of the logistic function (alpha, beta and gamma) can be site adjusted,
see Froidurot et al., (2014) for further details.
'''

import os
from typing import List, Tuple
import numpy as np
import xarray as xr

DEFAULT_NETCDF_FORMAT = 'NETCDF4_CLASSIC'

def qair2rh(qair : np.array, tair : np.array, psurf : np.array = 1013.25): 
    """
    Function to convert specific humidity (kg/kg) into relative humidity (%).

    tair : air temperature (°C)
    qair : specific humidity (kg/kg) 
    psurf : atmospheric pressure at surface (mb or hPa)
    """
    
    es =  6.112 * np.exp((17.67 * tair)/(tair + 243.5))
    e = qair * psurf / (0.378 * qair + 0.622)
    rh = e / es
    rh[rh > 1] = 1
    rh[rh < 0] = 0
    
    return rh*100
    
def splitting_prectot(prectot: np.array,tair: np.array,rh: np.array,alpha:float = 22, beta:float = -2.7, gamma:float = -0.2):
    
    """
    Function used to partition total precipitation into liquid and solid part 
    using air temperature and relative humidity.
    Alpha, beta and gamma are parameters of logistic values that can be adjusted 
    depending on the site considered following Froidurot et al., (2014) method
    (default values are taken from Koistinen and Saltikoff (1998))
    
    prectot : total precipitation (units in is unit out)
    tair : near surface air temperature (°C)
    rh : near surface relative humidity (%)
    alpha, beta, gamma : parameters or logistic function 
    """

    # Compute probability of rain and snow using air temperature and relative humidity
    prain = 1 / (1 + np.exp(alpha + beta*tair + gamma*rh))
    psnow = 1 - prain
    
    ## Create output liquid and solid precipitation amount
    # precliq : liquid precipitation (units in is unit out)
    # precsol : solid precipitation (units in is unit out)
    
    precliq = prectot * prain
    precsol = prectot * psnow
    
    return precliq, precsol

def main_splitprectot(workdir: str,forc_file: str, prectot: List[str], output_forc: str):
        
    """
    Input
    workdir : working directory
    forc_file : name of the forcing file
    output_forc : name of the output forcing file
    prectot : list [,] of string : variable name composing total precipitation either one or multiple variables.

    units of precipitation (prectot) : units in is unit out
    units of air temperature (Tair) : K
    units of surface pressure (Psurf) : Pa
    units of specific humidity (qair) : kg/kg
    """
    
    ds = xr.open_dataset(workdir+forc_file,format=DEFAULT_NETCDF_FORMAT)
    
    if len(prectot) == 1 :
        prectot = ds[prectot[0]].values
    elif len(prectot) > 1 :
        prectot = np.array([ds[prectot[i]].values for i in range(0,len(prectot))])
        prectot = prectot.sum(axis = 0)
        
    # Get requires variable from file
    tair = ds['Tair'].values
    qair = ds['Qair'].values
    psurf = ds['PSurf'].values

    # Compute relative humidity from specific humidity
    rh = qair2rh(qair,tair - 273.15, psurf/100)

    # Compute new solid and liquid precipitation
    prec = splitting_prectot(prectot, tair - 273.15, rh)

    # Create new variable variable
    Rainf = xr.Variable(dims = ds['Tair'].dims, data = prec[0], 
            attrs = dict(long_name="Rainfall Rate", units ='kg/m2/s', _FillValue = -9999999.))
    Snowf = xr.Variable(dims = ds['Tair'].dims, data = prec[1], 
            attrs = dict(long_name="Snowfall Rate", units ='kg/m2/s', _FillValue = -9999999.))

    ds['Rainf'] = Rainf
    ds['Snowf'] = Snowf
    
    ds.to_netcdf(workdir+output_forc, format=DEFAULT_NETCDF_FORMAT)

if __name__ == '__main__':

    workdir = '/home/monteirod/'
    forc_file = 'FORCING.nc'
    prectot = ['Rainf', 'Snowf']
    output_forc = 'FORCING_out.nc'

    main_splitprectot(workdir=workdir, forc_file=forc_file, prectot=prectot, output_forc=output_forc)