# -*- coding: utf-8 -*-
"""
Created on Wed Oct 13 15:04:27 2021

@author: Jari-Pekka Nousu


# SCRIPT TO CREATE FORCING FILE FROM TXT OR CSV FILE
# FEEL FREE TO BE INSPIRED BY THIS TO CREATE YOUR OWN FORCING FILE
# 
# TXT/CSV --> PD.DATAFRAME --> NCF-FILE
#
# The FORCING File has mandatory columns.
# Please note time column needs to be first and timezone has to be UTC
# [['time', 'CO2air', 'DIR_SWdown', 'HUMREL', 'LWdown', 'NEB', 'PSurf', 'Qair',
# 'Rainf', 'SCA_SWdown', 'Snowf','Tair', 'Wind', 'Wind_DIR']]

"""

#########################
# Import
#########################
import pandas as pd
from netCDF4 import Dataset 
import netCDF4
from datetime import datetime
from bronx.meteo.thermo import Thermo

###########################################################################################
# Path for your obs file
###########################################################################################
infile = "Path_to_meteo_file/meteo.csv"  #!!! TO BE CHANGED

###########################################################################################
# Path for the FORCING you want to create
###########################################################################################
outfile = "/home/The_path_you_want/FORCING_from_my_obs.nc"  #!!! TO BE CHANGED

###########################################################################################
# dictionary for station data: information on your obs site
###########################################################################################
station_data = {'LAT': 67.983333, 'LON': 24.216667, 'UREF': 2, 'ZREF': 2,  #!!! TO BE CHANGED
                'ZS': 347, 'aspect': 0, 'slope': 0, 'FRC_TIME_STP': 3600}  #!!! TO BE CHANGED
# TIME STEP in seconds, ZS is the altitude, ZREF is the height Temperature sensor, 
# UREF is the height of Wind sensor

###########################################################################################
# Processing raw meteo file into pd dataframe
###########################################################################################
# reading the file
meteo_data = pd.read_csv(infile, sep=";", index_col=0, parse_dates=True)
# assigning fill value
fill_value = -9999999

# calculating Qair (specific humidity) from Pressure, relative humidity and Temperature
meteo_data["Qair"] = Thermo(['v', 'c'], dict(P=meteo_data["PSurf"], Huw=meteo_data["HUMREL"],
                        T=meteo_data["Tair"], rc=0)).get('qv')

# saving only the necessary variables in a new df
croc_data = meteo_data[['CO2air', 'DIR_SWdown', 'HUMREL', 
                        'LWdown', 'NEB', 'PSurf', 'Qair', 'Rainf',
                        'SCA_SWdown', 'Snowf','Tair', 'Wind', 'Wind_DIR']]

# in case nan, fill with fill_value=-9999999 
# some variables do not need to be perfect, some do
croc_data = croc_data.fillna(fill_value)

# in case resampling is wanted
#croc_data = croc_data.resample('3H').mean()

###########################################################################################
# NETCDF FILE CREATION
###########################################################################################
# first date
first_date = croc_data.index[0]

# list of dates in right format
List_dates = []
for i in range(len(croc_data)):
    List_dates.append(datetime.utcfromtimestamp(datetime.timestamp(croc_data.index.tz_localize('UTC')[i])))


# creating the netcdf file: dataset
fic_forcing = Dataset(outfile, 'w', format='NETCDF4_CLASSIC')

# creating the netcdf file: dimensions
fic_forcing.createDimension('time', None)
fic_forcing.createDimension('Number_of_points', 1) 

# defining time dimension
unit_time = 'seconds since ' + str(first_date)
time = netCDF4.date2num(List_dates, unit_time)
time_nc = fic_forcing.createVariable('time', 'f', ('time',), fill_value=fill_value)
time_nc.units = unit_time
time_nc[:] = time

# creation and definition of Time Step
frc_nc = fic_forcing.createVariable('FRC_TIME_STP', 'f', fill_value=fill_value)
frc_nc.units = 's'
frc_nc[:] = station_data["FRC_TIME_STP"]

# variables with 2 dimensions and their metadata
List_nom_time_nbpoint = ['CO2air', 'DIR_SWdown', 'HUMREL', 'LWdown', 'NEB', 'PSurf', 'Qair', 'Rainf',
                              'SCA_SWdown', 'Snowf', 'Tair', 'Wind', 'Wind_DIR']

List_unite_time_nbpoint = ['kg/m3', 'W/m2', '%', 'W/m2', 'between 0 and 1', 'Pa', 'Kg/Kg', 'kg/m2/s',
                                'W/m2', 'kg/m2/s', 'K', 'm/s', 'deg']

List_longname_time_nbpoint = ['Near Surface CO2 Concentration', 'Surface Incident Direct Shortwave Radiation',
                                   'Relative Humidity', 'Surface Incident Longwave Radiation', 'Nebulosity',
                                   'Surface Pressure', 'Near Surface Specific Humidity',
                                   'Rainfall Rate', 'Surface Incident Diffuse Shortwave Radiation', 'Snowfall Rate',
                                   'Near Surface Air Temperature',
                                   'Wind Speed', 'Wind Direction']

# variables with 1 dimension and their metadata
List_nom_nbpoint = ['LAT', 'LON', 'UREF', 'ZREF', 'ZS', 'aspect', 'slope']

List_unite_nbpoint = ['degrees_north', 'degrees_east', 'm', 'm', 'm', 'degrees from north',
                           'degrees from horizontal']

List_longname_nbpoint = ['latitude', 'longitude', 'Reference_Height_for_Wind', 'Reference_Height', 'altitude',
                              'slope aspect', 'slope angle']

# Creating 2 dimensions variables
for i in range(len(List_nom_time_nbpoint)):
    fic_nc = fic_forcing.createVariable(List_nom_time_nbpoint[i], 'f', ('time', 'Number_of_points'),
                                        fill_value=fill_value)
    fic_nc.units = List_unite_time_nbpoint[i]
    fic_nc.long_name = List_longname_time_nbpoint[i]

# Creating 1 dimension variables    
for i in range(len(List_nom_nbpoint)):
    fic_nc = fic_forcing.createVariable(List_nom_nbpoint[i], 'f', ('Number_of_points',), fill_value=fill_value)
    fic_nc.units = List_unite_nbpoint[i]
    fic_nc.long_name = List_longname_nbpoint[i]


# Filling the variables with pd dataframe (2 dimensions)
for i in range(len(List_nom_time_nbpoint)):
    vari = List_nom_time_nbpoint[i]
    try:
        fic_forcing[vari][:,0] = croc_data[vari]
    except:
        print("there is no", List_nom_time_nbpoint[i], "data available")

# Filling the variables with pd dataframe (1 dimension)
for i in range(len(List_nom_nbpoint)):
    vari = List_nom_nbpoint[i]
    try:
        fic_forcing[vari][0] = station_data[vari]
    except:
        print("there is no", List_nom_nbpoint[i], "data available")

# Others Metadata #!!! TO BE CHANGED
fic_nc.title = 'FORCING TITLE'
fic_nc.summary = 'This file is bla bla bla FORCING from bla bla bla Observation Site'
fic_nc.id = 'FORCING ID'
#fic_nc.references = 'https://doi.org/; references from publications...'
#fic_nc.platform = ''
#fic_nc.platform_vocabulary = ''
fic_nc.contributor_name = 'People 1, People 2, People 3'
fic_nc.contributor_role = 'People 1 collected the observation; People 2 made some modelisation; People 3 made some coffee (huge contribution)'

fic_nc.date_created = datetime.datetime.today().replace(second=0, microsecond=0).isoformat()

fic_forcing.close()
