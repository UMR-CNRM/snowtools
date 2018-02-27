#! /usr/bin/env python
# -*- coding: utf-8 -*-
# Author: M. Lafaysse 24/05/2017
# Recoding of functionalities of faitNAMetPGD in snowtools1 for projects snowtools2 and vortex


# General python modules
import numpy as np
import os

# Snowtools modules
from utils.prosimu import prosimu
from utils.dates import checkdatebefore, checkdateafter
from utils.FileException import FileNameException

from bronx.datagrip.namelist import NamelistParser


def update_surfex_namelist_file(datebegin, namelistfile="OPTIONS.nam", forcing="FORCING.nc", dateend=None, updateloc=True):
    '''This function updates a namelist file through the bronx module.'''
    if not os.path.isfile(namelistfile):
        raise FileNameException(os.getcwd() + "/OPTIONS.nam")

    os.rename(namelistfile, "OPTIONS_base.nam")
    n = NamelistParser()
    N = n.parse("OPTIONS_base.nam")
    update_surfex_namelist_object(N, datebegin=datebegin, forcing=forcing, dateend=dateend, updateloc=updateloc)
    namSURFEX = open(namelistfile, 'w')
    namSURFEX.write(N.dumps())
    namSURFEX.close()


def update_surfex_namelist_object(NamelistObject, datebegin, forcing="FORCING.nc", dateend=None, updateloc=True):
    '''This function updates a NamelistSet object of the bronx module or a NamelistContents object of the vortex module.'''

    NamelistObject = update_mandatory_settings(NamelistObject)
    NamelistObject = update_dates(NamelistObject, datebegin)
    if updateloc:
        NamelistObject = update_loc(NamelistObject, forcing)
    NamelistObject = update_forcingdates(NamelistObject, datebegin, dateend, forcing=forcing)

    return NamelistObject


def update_mandatory_settings(NamelistObject):
    '''Force some options whose values are mandatory to be compatible with snowtools_git'''

    NamelistObject["NAM_IO_OFFLINE"].CSURF_FILETYPE = "NC "
    NamelistObject["NAM_IO_OFFLINE"].CFORCING_FILETYPE = "NETCDF"
    NamelistObject["NAM_IO_OFFLINE"].CTIMESERIES_FILETYPE = "NETCDF"
    NamelistObject["NAM_IO_OFFLINE"].LWRITE_COORD = False
    NamelistObject["NAM_IO_OFFLINE"].LWRITE_TOPO = True
    NamelistObject["NAM_IO_OFFLINE"].LRESTART = True

    return NamelistObject


def update_dates(NamelistObject, datebegin):
    """Modify SURFEX namelist for defining the beginning of the simulation."""

    NamelistObject["NAM_PREP_SURF_ATM"].NYEAR = datebegin.year
    NamelistObject["NAM_PREP_SURF_ATM"].NMONTH = datebegin.month
    NamelistObject["NAM_PREP_SURF_ATM"].NDAY = datebegin.day
    NamelistObject["NAM_PREP_SURF_ATM"].XTIME = datebegin.hour * 3600.

    return NamelistObject


def update_loc(NamelistObject, forcing):
    """modify SURFEX namelist for defining the coordinates of the simulation points."""

    # Read coordinates in FORCING file
    forc = prosimu(forcing)
    latitudes1d = forc.read("LAT")
    longitudes1d = forc.read("LON")
    forc.close()

    # Constant dlat/dlon
    dlat1d = np.zeros_like(latitudes1d) + 0.5
    dlon1d = np.zeros_like(longitudes1d) + 0.5

    NamelistObject["NAM_LONLATVAL"].XY = list(latitudes1d)
    NamelistObject["NAM_LONLATVAL"].XX = list(longitudes1d)
    NamelistObject["NAM_LONLATVAL"].XDY = list(dlat1d)
    NamelistObject["NAM_LONLATVAL"].XDX = list(dlon1d)
    NamelistObject["NAM_LONLATVAL"].NPOINTS = len(longitudes1d)

    return NamelistObject


def update_forcingdates(NamelistObject, datebegin, dateend, forcing="FORCING.nc"):

    """modify SURFEX namelist for limiting the dates to read in a FORCING file longer than the simulation."""

    forc = prosimu(forcing)
    timeforc = forc.readtime()
    forc.close()

    dateforcbegin = timeforc[0]
    dateforcend = timeforc[-1]

    checkdateafter(datebegin, dateforcbegin)
    if dateend:
#         checkdatebefore(dateend, dateforcend)
        checkdateafter(dateend, dateforcbegin)
    else:
        dateend = dateforcend

    if datebegin > dateforcbegin or dateend < dateforcend:
        NamelistObject["NAM_IO_OFFLINE"].LDELAYEDSTART_NC = True

#     if dateend < dateforcend:
    NamelistObject["NAM_IO_OFFLINE"].NDATESTOP = [dateend.year, dateend.month, dateend.day, dateend.hour * 3600]

    return NamelistObject
