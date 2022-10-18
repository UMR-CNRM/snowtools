# -*- coding: utf-8 -*-

'''
Created on 5 Sept. 2017

@author: lafaysse
'''

# General python modules
import os
import datetime

import numpy as np
import netCDF4

# snowtools modules
from snowtools.utils.prosimu import prosimu_auto
from snowtools.utils.resources import get_file_period, save_file_const
from snowtools.tools.change_forcing import forcinput_select, forcinput_applymask
from snowtools.utils.FileException import DirFileException
from snowtools.tools.execute import callSurfexOrDie
from snowtools.DATA import SNOWTOOLS_DIR


def create_env(diroutput):
    """Create working directory and directories to save outputs

    :param diroutput: output directory
    :type diroutput: str
    """

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
    """Get the meteorological forcing files required to compute a climatology
    This is a recursive function until all files are found.

    :param forcingpath: directory where are stored the forcing files
    :type forcingpath: str
    :param datebegin: First date of the climatology period
    :type datebegin: class:`datetime.datetime`
    :param dateend: Last date of the climatology period
    :type dateend: class:`datetime.datetime`
    :param geolist: list defining the domain
    :type geolist: list
    :param list_forcing: list of forcing files previously found by previous iterations (should not be set by the user)
    :type list_forcing: list, optional
    """
    if type(forcingpath) is str:
        forcingpath = [forcingpath]
    list_forcing_tomerge = []

    for i, path in enumerate(forcingpath):
        dateforcbegin, dateforcend = get_file_period("FORCING", path, datebegin, dateend)
        os.rename("FORCING.nc", "FORCING_" + str(i) + ".nc")
        list_forcing_tomerge.append("FORCING_" + str(i) + ".nc")

    if len(forcingpath) > 1:
        forcinput_applymask(list_forcing_tomerge, "FORCING.nc")
    else:
        os.rename("FORCING_0.nc", "FORCING.nc")

    if geolist:
        if type(geolist[0]) is str and os.path.isfile(geolist[0]):
            os.rename("FORCING.nc", "input.nc")
            if not os.path.islink('GRID.nc'):
                os.symlink(geolist[0], "GRID.nc")
            callSurfexOrDie(SNOWTOOLS_DIR + "/interpolation/interpol", moderun='MPIRUN', nproc=4)
            os.rename("output.nc", "FORCING.nc")
        else:
            os.rename("FORCING.nc", "FORCING_base.nc")
            forcinput_select("FORCING_base.nc", "FORCING.nc", *geolist)

    forcing = ("FORCING_" + str(i) + "_" + dateforcbegin.strftime('%Y%m%d%H') +
               "_" + dateforcend.strftime('%Y%m%d%H') + ".nc")
    os.rename("FORCING.nc", forcing)
    list_forcing.append(forcing)

    datebegin = dateforcend

    if dateforcend < dateend:
        get_meteo_for_clim(forcingpath, datebegin, dateend, geolist, list_forcing=list_forcing)

    return list_forcing


def generate_clim(list_forcing):
    """Generate a temperature climatology from meteorological forcing files

    :param list_forcing: List of forcing files addresses
    :type list_forcing: list of str
    """
    DataMeteo = prosimu_auto(list_forcing)
    Tair = DataMeteo.read("Tair", keepfillvalue=True)
    spatialdim = DataMeteo.getdimvar("Tair")[1:]
    tclim = np.mean(Tair, axis=0)

    initTGfile = netCDF4.Dataset("init_TG.nc", "w", format='NETCDF3_CLASSIC')

    for i, dim in enumerate(spatialdim):
        initTGfile.createDimension(dim, Tair.shape[i + 1])

    var = initTGfile.createVariable("TG", float, spatialdim)

    var[:] = tclim

    DataMeteo.close()
    initTGfile.close()


def clim(options):
    """Generate a temperature climatology from S2M options

    :param options: S2M options
    :type options: dict
    """
    # Save initial directory to go back at the end
    initcurrentdirectory = os.getcwd()

    create_env(options.diroutput)

    if options.region or options.slopes or options.aspects or options.minlevel or options.maxlevel:
        geolist = [options.region, options.minlevel, options.maxlevel, options.slopes, options.aspects]
    else:
        geolist = None

    list_forcing = get_meteo_for_clim(options.forcing, options.datedeb, options.datefin, geolist, list_forcing=[])
    generate_clim(list_forcing)

    save_file_const(options.diroutput + "/prep", "init_TG.nc")

    for forcing in list_forcing:
        os.remove(forcing)

    # Back to initial directory
    os.chdir(initcurrentdirectory)
