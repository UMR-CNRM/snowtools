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
from utils.dates import checkdateafter
from utils.FileException import FileNameException

from bronx.datagrip.namelist import NamelistParser


def update_surfex_namelist_file(datebegin, namelistfile="OPTIONS.nam", forcing="FORCING.nc", dateend=None, updateloc=True, nmembers = None):
    '''This function updates a namelist file through the bronx module.'''
    if not os.path.isfile(namelistfile):
        raise FileNameException(os.getcwd() + "/OPTIONS.nam")
    n = NamelistParser()

    if nmembers is None:  # this doesn't work on beaufix.
        os.rename(namelistfile, "OPTIONS_base.nam")
        N = n.parse("OPTIONS_base.nam")
        update_surfex_namelist_object(N, datebegin=datebegin, forcing=forcing, dateend=dateend, updateloc=updateloc)
    else:
        N = n.parse(namelistfile)
        update_namelist_object_nmembers(N, nmembers)
    namSURFEX = open(namelistfile, 'w')
    namSURFEX.write(N.dumps())
    namSURFEX.close()


def update_surfex_namelist_object(NamelistObject, datebegin, forcing="FORCING.nc", dateend=None, updateloc=True, physicaloptions={}, snowparameters={}):
    '''This function updates a NamelistSet object of the bronx module or a NamelistContents object of the vortex module.'''
    NamelistObject = update_mandatory_settings(NamelistObject)
    NamelistObject = update_dates(NamelistObject, datebegin)
    if updateloc:
        NamelistObject = update_loc(NamelistObject, forcing)

    NamelistObject = update_forcingdates(NamelistObject, datebegin, dateend, forcing=forcing)

    NamelistObject = update_physicaloptions(NamelistObject, **physicaloptions)
    NamelistObject = update_snowparameters(NamelistObject, **snowparameters)
    return NamelistObject


def check_or_create_block(NamelistObject, blockname):
    if blockname not in NamelistObject.keys():
        NamelistObject.newblock(blockname)


def update_mandatory_settings(NamelistObject):
    '''Force some options whose values are mandatory to be compatible with snowtools_git'''
    check_or_create_block(NamelistObject, "NAM_IO_OFFLINE")
    NamelistObject["NAM_IO_OFFLINE"].CSURF_FILETYPE = "NC "
    NamelistObject["NAM_IO_OFFLINE"].CFORCING_FILETYPE = "NETCDF"
    NamelistObject["NAM_IO_OFFLINE"].CTIMESERIES_FILETYPE = "NETCDF"
    NamelistObject["NAM_IO_OFFLINE"].LRESTART = True

    '''The following options are necessary to output homogeneous fluxes among the Crocus community'''
    '''(fluxes averaged over the output time step)'''
    NamelistObject["NAM_DIAG_SURFn"].LSURF_BUDGET = True
    NamelistObject["NAM_DIAG_SURFn"].LSURF_BUDGETC = True
    NamelistObject["NAM_DIAG_SURFn"].LRESET_BUDGETC = True
    NamelistObject["NAM_WRITE_DIAG_SURFn"].LRESETCUMUL = True

    return NamelistObject


def update_dates(NamelistObject, datebegin):
    """Modify SURFEX namelist for defining the beginning of the simulation."""
    check_or_create_block(NamelistObject, "NAM_PREP_SURF_ATM")
    NamelistObject["NAM_PREP_SURF_ATM"].NYEAR = datebegin.year
    NamelistObject["NAM_PREP_SURF_ATM"].NMONTH = datebegin.month
    NamelistObject["NAM_PREP_SURF_ATM"].NDAY = datebegin.day
    NamelistObject["NAM_PREP_SURF_ATM"].XTIME = datebegin.hour * 3600.

    return NamelistObject


def update_loc(NamelistObject, forcing):
    """modify SURFEX namelist for defining the coordinates of the simulation points."""

    if NamelistObject["NAM_PGD_GRID"].CGRID == "LONLATVAL":
        check_or_create_block(NamelistObject, "NAM_LONLATVAL")
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

    # another special patch/bugfix for distributed simulations (then CGRID==IGN)
    if NamelistObject["NAM_PGD_GRID"].CGRID == "IGN":
        NamelistObject["NAM_IO_OFFLINE"].LWRITE_TOPO = False
    else:
        NamelistObject["NAM_IO_OFFLINE"].LWRITE_TOPO = True

    return NamelistObject


def update_forcingdates(NamelistObject, datebegin, dateend, forcing="FORCING.nc"):

    """modify SURFEX namelist for limiting the dates to read in a FORCING file longer than the simulation."""

    forc = prosimu(forcing)
    timeforc = forc.readtime()
    forc.close()

    dateforcbegin = timeforc[0]
    dateforcend = timeforc[-1]

    print ("DATES OF THE FORCING FILE:", dateforcbegin, dateforcend)
    print ("PRESCRIBED SIMULATION DATES:", datebegin, dateend)

    checkdateafter(datebegin, dateforcbegin)
    if dateend:
        checkdateafter(dateend, dateforcbegin)
    else:
        dateend = dateforcend

    if datebegin > dateforcbegin or dateend < dateforcend:
        NamelistObject["NAM_IO_OFFLINE"].LDELAYEDSTART_NC = True

#     if dateend < dateforcend:
    NamelistObject["NAM_IO_OFFLINE"].NDATESTOP = [dateend.year, dateend.month, dateend.day, dateend.hour * 3600]

    return NamelistObject


def update_physicaloptions(NamelistObject, **kwargs):
    check_or_create_block(NamelistObject, "NAM_ISBA_SNOWn")
    for key, value in six.iteritems(kwargs):
        if key.upper() in ["CSNOWDRIFT", "LSNOWDRIFT_SUBLIM", "LSNOW_ABS_ZENITH", "CSNOWMETAMO", "CSNOWRAD", "CSNOWFALL", "CSNOWCOND", "CSNOWHOLD", "CSNOWCOMP", "CSNOWZREF", "LSNOWSYTRON"]:
            setattr(NamelistObject["NAM_ISBA_SNOWn"], key.upper(), value)

    return NamelistObject


def update_snowparameters(NamelistObject, **kwargs):
    check_or_create_block(NamelistObject, "NAM_SURF_CSTS")
    check_or_create_block(NamelistObject, "NAM_SURF_SNOW_CSTS")
    check_or_create_block(NamelistObject, "NAM_ISBAn")
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


def update_namelist_object_nmembers(NamelistObject, nmembers):

    if nmembers is not None:
        check_or_create_block(NamelistObject, "NAM_ENS")
        setattr(NamelistObject["NAM_ENS"], 'NENS', nmembers)
        print ("NENS set to {}".format(nmembers))

    return NamelistObject


def update_namelist_var(namelist_file, data_file):

    ''' This function was implemented by C. Carmagnola in December 2018 (PROSNOW project).
    It reads "water consumption data for snowmaking" from an external file (data_file) and updates a namelist (namelist_file) accordingly.'''

    # 1) Read data from an external file

    if not os.path.isfile(data_file):
        raise FileNameException(data_file)

    mdat = open(data_file)
    var_tbc = []

    for line in mdat:
        maliste = line.split()
        var_tbc = var_tbc + [maliste[2]]

    print var_tbc

    for i in range(len(var_tbc)):
        var_tbc[i] = float(var_tbc[i])

#     print "Water consumption for snowmaking (kg/m2) - In external file:"
#     print var_tbc
#     print "--------------"

    # 2) Put data from an external file into the namelist

    if not os.path.isfile(namelist_file):
        raise FileNameException(namelist_file)

    n = NamelistParser()
    NamelistObject = n.parse(namelist_file)

    check_or_create_block(NamelistObject, "XPROD_SCHEME")
    NamelistObject["NAM_SURF_SNOW_CSTS"].XPROD_SCHEME = var_tbc

#     print "Water consumption for snowmaking (kg/m2) - In namelist:"
#     print NamelistObject["NAM_SURF_SNOW_CSTS"].XPROD_SCHEME

    namSURFEX = open(namelist_file, 'w')
    namSURFEX.write(NamelistObject.dumps())
    namSURFEX.close()

    os.system('cp ' + namelist_file + ' OPTIONS.nam')


# Test

# if __name__ == "__main__":
#     update_namelist_var("/home/carmagnolac/CMC/CEN/4_SIMUL/Vortex_3_SD/2_namelists/OPTIONS_V8.1_0.nam", "/home/carmagnolac/CMC/CEN/4_SIMUL/Vortex_3_SD/4_measurements/Technoalpin_saisies.txt")

