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
import datetime

# snowtools modules
from utils.prosimu import prosimu
from utils.resources import get_file_period, save_file_const
from tools.change_forcing import forcinput_select
from utils.FileException import DirFileException


def create_env(diroutput):
    """Create working directory and directories to save outputs"""

    # Note that it is not necessary to remove any existing working directory as the working directory is always new
    # (date in microseconds in the directory name)

    if os.path.isfile(diroutput):
        raise DirFileException(diroutput)
    dirprep = diroutput + "/prep"
    dirwork = diroutput + "/workClim" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f")
    # Create all directories
    for directory in [dirprep, dirwork]:
        if not os.path.isdir(directory):
            os.makedirs(directory)

    # Change current directory to working directory
    os.chdir(dirwork)


def get_meteo_for_clim(forcingpath, datebegin, dateend, geolist, list_forcing=[]):

    dateforcbegin, dateforcend = get_file_period("FORCING", forcingpath, datebegin, dateend)
    forcing = "FORCING_" + dateforcbegin.strftime('%Y%m%d%H') + "_" + dateforcend.strftime('%Y%m%d%H') + ".nc"

    if geolist:
        os.rename("FORCING.nc", "FORCING_base.nc")
        forcinput_select("FORCING_base.nc", "FORCING.nc", *geolist)

    os.rename("FORCING.nc", forcing)
    list_forcing.append(forcing)

    datebegin = dateforcend

    if dateforcend < dateend:
        get_meteo_for_clim(forcingpath, datebegin, dateend, geolist, list_forcing = list_forcing)

    return list_forcing


def generate_clim(list_forcing):

    DataMeteo = prosimu(list_forcing)
    Tair = DataMeteo.read("Tair", keepfillvalue=True)
    spatialdim = DataMeteo.getdimvar("Tair")[1:]
    tclim = np.mean(Tair, axis=0)

    initTGfile = netCDF4.Dataset("init_TG.nc", "w", format='NETCDF3_CLASSIC')

    for i, dim in enumerate(spatialdim):
        initTGfile.createDimension(dim, Tair.shape[i + 1])

    var = initTGfile.createVariable("TG", float, spatialdim)

    var[:] = tclim

    initTGfile.close()


def clim(options):

    create_env(options.diroutput)

    if options.region or options.slopes or options.aspects or options.minlevel or options.maxlevel:
        geolist = [options.region, options.minlevel, options.maxlevel, options.slopes, options.aspects]
    else:
        geolist = None

    list_forcing = get_meteo_for_clim(options.forcing, options.datedeb, options.datefin, geolist)
    generate_clim(list_forcing)

    save_file_const(options.diroutput + "/prep", "init_TG.nc")

    for forcing in list_forcing:
        os.remove(forcing)
