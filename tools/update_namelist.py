#! /usr/bin/env python
# -*- coding: utf-8 -*-
# Author: M. Lafaysse 24/05/2017
# Recoding of functionalities of faitNAMetPGD in snowtools1 for projects snowtools2 and vortex


# General python modules
import numpy as np
import os
import six

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


def update_surfex_namelist_object(NamelistObject, datebegin, forcing="FORCING.nc", dateend=None, updateloc=True, physicaloptions={}, snowparameters={}, nmembers=None):
    '''This function updates a NamelistSet object of the bronx module or a NamelistContents object of the vortex module.'''

    NamelistObject = update_mandatory_settings(NamelistObject)
    NamelistObject = update_dates(NamelistObject, datebegin)
    if updateloc:
        NamelistObject = update_loc(NamelistObject, forcing)
    NamelistObject = update_forcingdates(NamelistObject, datebegin, dateend, forcing=forcing)

    print(type(physicaloptions), type(snowparameters))

    print(physicaloptions)

    NamelistObject = update_physicaloptions(NamelistObject, **physicaloptions)
    NamelistObject = update_snowparameters(NamelistObject, **snowparameters)
    NamelistObject = update_nmembers(NamelistObject, nmembers)
    return NamelistObject


def update_mandatory_settings(NamelistObject):
    '''Force some options whose values are mandatory to be compatible with snowtools_git'''

    NamelistObject["NAM_IO_OFFLINE"].CSURF_FILETYPE = "NC "
    NamelistObject["NAM_IO_OFFLINE"].CFORCING_FILETYPE = "NETCDF"
    NamelistObject["NAM_IO_OFFLINE"].CTIMESERIES_FILETYPE = "NETCDF"
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

    NamelistObject["NAM_IO_OFFLINE"].LWRITE_COORD = False
    NamelistObject["NAM_IO_OFFLINE"].LWRITE_TOPO = True


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
    print ("Ndatestop set to {0}{1}{2}{3}".format(dateend.year, dateend.month, dateend.day, dateend.hour))

    return NamelistObject


def update_physicaloptions(NamelistObject, **kwargs):

    for key, value in six.iteritems(kwargs):
        if key.upper() in ["CSNOWDRIFT", "LSNOWDRIFT_SUBLIM", "LSNOW_ABS_ZENITH", "CSNOWMETAMO", "CSNOWRAD", "CSNOWFALL", "CSNOWCOND", "CSNOWHOLD", "CSNOWCOMP", "CSNOWZREF"]:
            setattr(NamelistObject["NAM_ISBA_SNOWn"], key.upper(), value)

    return NamelistObject


def update_snowparameters(NamelistObject, **kwargs):

    for key, value in six.iteritems(kwargs):
        if key.upper() in ["XZ0SN", "XZ0HSN", "XTAU_LW"]:
            setattr(NamelistObject["NAM_SURF_CSTS"], key.upper(), value)
        elif key.upper() in ["XALBICE1", "XALBICE2", "XALBICE3", "XRHOTHRESHOLD_ICE", "XZ0ICEZ0SNOW", "XVAGING_GLACIER", "XVAGING_NOGLACIER", "XVVISC3", "X_RI_MAX"]:
            setattr(NamelistObject["NAM_SURF_SNOW_CSTS"], key.upper(), value)
        elif key.upper() in ["XCVHEATF"]:
            setattr(NamelistObject["NAM_ISBAn"], key.upper(), value)
        else:
            print("IGNORE FIELD " + key + " : not in namelist.")

    return NamelistObject


def update_nmembers(NamelistObject, nmembers):

    if nmembers is not None:
        setattr(NamelistObject["NAM_ENS"], 'NENS', nmembers)
        print ("NENS set to {}".format(nmembers))
    else:
            print ("IGNORE FIELD NENS : not in namelist.")

    return NamelistObject
