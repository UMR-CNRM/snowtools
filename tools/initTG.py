#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 5 Sept. 2017

@author: lafaysse
'''

# General python modules
import os
import numpy as np
import netCDF4

# snowtools modules
from utils.prosimu import prosimu
from utils.resources import get_file_period

def get_meteo_for_clim(forcingpath,datebegin,dateend,list_forcing=[]):
    
    
    dateforcbegin,dateforcend = get_file_period("FORCING",forcingpath,datebegin,dateend)
    forcing="FORCING_"+dateforcbegin.strftime('%Y%m%d%H')+"_"+dateforcend.strftime('%Y%m%d%H')+".nc"
    
    os.rename("FORCING.nc",forcing)
    list_forcing.append(forcing)
    
    datebegin=dateforcend
    
    if dateforcend<dateend:
        get_meteo_for_clim(forcingpath,datebegin,dateend,list_forcing=list_forcing)
        
    return list_forcing

def generate_clim(list_forcing):
       
    DataMeteo=prosimu(list_forcing)
    Tair=DataMeteo.read("Tair")
    spatialdim=DataMeteo.getdimvar("Tair")[1:]
    tclim=np.mean(Tair,axis=0)
    
    initTGfile=netCDF4.Dataset("init_TG.nc","w")
    
    for i,dim in enumerate(spatialdim):
        initTGfile.createDimension(dim,Tair.shape[i+1])
    
    var=initTGfile.createVariable("TG",float,spatialdim)
    
    var[:]=tclim

def clim(forcingpath,datebegin,dateend):
    list_forcing=get_meteo_for_clim(forcingpath,datebegin,dateend)
    generate_clim(list_forcing)
    for forcing in list_forcing:
        os.remove(forcing)

if __name__ == "__main__":
    get_meteo_for_clim
 



