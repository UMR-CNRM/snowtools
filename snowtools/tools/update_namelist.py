# -*- coding: utf-8 -*-

# Author: M. Lafaysse 24/05/2017
# Recoding of functionalities of faitNAMetPGD in snowtools1 for projects snowtools2 and vortex


# General python modules
import numpy as np
import os

# Snowtools modules
from snowtools.utils.prosimu import prosimu_base
from snowtools.utils.dates import checkdateafter
from snowtools.utils.FileException import FileNameException, VarDimensionException

from bronx.datagrip.namelist import NamelistParser


def update_surfex_namelist_file(datebegin, namelistfile="OPTIONS.nam",
                                forcing="FORCING.nc", dateend=None, updateloc=True,
                                physicaloptions={}, snowparameters={}, nmembers=None,
                                no_caution=False, cselect=None):
    """This function updates a namelist file through the bronx module. Called by standalone S2M but not by vortex

    :param datebegin: Initial date of simulation
    :type datebegin: class:`bronx.stdtypes.date.Date`
    :param namelistfile: Address of namelist to modified. Defaults to "OPTIONS.nam"
    :type namelistfile: str, optional
    :param forcing: Address of associated forcing file. Defaults to "FORCING.nc"
    :type forcing: str, optional
    :param dateend: Last date of simulation (only necessary if different from the end of the FORCING file)
    :type dateend: class:`bronx.stdtypes.date.Date`, optional
    :param updateloc: Modify coordinates in the namelist from the forcing file. Defaults to True
    :param physicaloptions: ESCROC physical options. Defaults to {}.
    :type physicaloptions: dict, optional
    :param snowparameters: ESCROC physical parameters. Defaults to {}.
    :type snowparameters: dict, optional
    :type updateloc: bool, optional
    :param nmembers: number of members for mutiphysics simulations. Defaults to None
    :type nmembers: int, optional
    :param no_caution: do not open the forcing to reduce computation time
    :type no_caution: boolean, optional
    """
    if not os.path.isfile(namelistfile):
        raise FileNameException(os.getcwd() + "/OPTIONS.nam")
    n = NamelistParser()

    if nmembers is None:  # this doesn't work on beaufix.
        os.rename(namelistfile, "OPTIONS_base.nam")
        N = n.parse("OPTIONS_base.nam")
        update_surfex_namelist_object(N, datebegin=datebegin, forcing=forcing, dateend=dateend, updateloc=updateloc,
                                      physicaloptions=physicaloptions, snowparameters=snowparameters,
                                      no_caution=no_caution, cselect=cselect)
    else:
        N = n.parse(namelistfile)
        update_namelist_object_nmembers(N, nmembers)
    namSURFEX = open(namelistfile, 'w')
    namSURFEX.write(N.dumps())
    namSURFEX.close()


def update_surfex_namelist_object(NamelistObject, datebegin, forcing="FORCING.nc",
                                  dateend=None, updateloc=True, physicaloptions={}, snowparameters={},
                                  no_caution=False, cselect = None):
    """This function updates a class:`bronx.datagrip.namelist.NamelistSet` object. Called directly by vortex algos.

    :param NamelistObject: Namelist to modified.
    :type NamelistObject: class:`bronx.datagrip.namelist.NamelistSet`
    :param datebegin: Initial date of simulation
    :type datebegin: class:`bronx.stdtypes.date.Date`
    :param forcing: Address of associated forcing file. Defaults to "FORCING.nc"
    :type forcing: str, optional
    :param dateend: Last date of simulation (only necessary if different from the end of the FORCING file)
    :type dateend: class:`bronx.stdtypes.date.Date`, optional
    :param updateloc: Modify coordinates in the namelist from the forcing file. Defaults to True
    :type updateloc: bool, optional
    :param nmembers: number of members for SODA simulations. Defaults to None
    :type nmembers: int, optional
    :param physicaloptions: ESCROC physical options. Defaults to {}.
    :type physicaloptions: dict, optional
    :param snowparameters: ESCROC physical parameters. Defaults to {}.
    :type snowparameters: dict, optional
    :param no_caution: do not open the forcing to reduce computation time
    :type no_caution: boolean, optional
    :param cselect: do not open the forcing to reduce computation time
    :type no_caution: boolean, optional
    :param cselect: set a list of diagnostic variables
    :type cselect: list
    """
    NamelistObject = update_mandatory_settings(NamelistObject)
    NamelistObject = update_dates(NamelistObject, datebegin)
    if updateloc:
        NamelistObject = update_loc(NamelistObject, forcing)

    NamelistObject = update_forcingdates(NamelistObject, datebegin, dateend, forcing=forcing, no_caution=no_caution)

    NamelistObject = update_physicaloptions(NamelistObject, **physicaloptions)
    NamelistObject = update_snowparameters(NamelistObject, **snowparameters)

    if cselect:
        NamelistObject = update_cselect(NamelistObject, cselect)
    return NamelistObject


def check_or_create_block(NamelistObject, blockname):
    """Create a new namelist block only if it does not already exist.

    :param NamelistObject: Namelist to modified
    :type NamelistObject: class:`bronx.datagrip.namelist.NamelistSet`
    :param blockname: name of new block
    :type blockname: str
    """
    if blockname not in NamelistObject.keys():
        NamelistObject.newblock(blockname)


def update_mandatory_settings(NamelistObject):
    """Force some options whose values are mandatory to be compatible with snowtools_git

    :param NamelistObject: Namelist to modified
    :type NamelistObject: class:`bronx.datagrip.namelist.NamelistSet`
    """
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
    """Modify SURFEX namelist for defining the beginning of the simulation.

    :param NamelistObject: Namelist to modified
    :type NamelistObject: class:`bronx.datagrip.namelist.NamelistSet`
    :param datebegin: Initial date of simulation
    :type datebegin: class:`bronx.stdtypes.date.Date`
    """
    check_or_create_block(NamelistObject, "NAM_PREP_SURF_ATM")
    NamelistObject["NAM_PREP_SURF_ATM"].NYEAR = datebegin.year
    NamelistObject["NAM_PREP_SURF_ATM"].NMONTH = datebegin.month
    NamelistObject["NAM_PREP_SURF_ATM"].NDAY = datebegin.day
    NamelistObject["NAM_PREP_SURF_ATM"].XTIME = datebegin.hour * 3600.

    return NamelistObject


def update_loc(NamelistObject, forcing):
    """Modify SURFEX namelist for defining the coordinates of the simulation points.

    :param NamelistObject: Namelist to modified
    :type NamelistObject: class:`bronx.datagrip.namelist.NamelistSet`
    :param forcing: Address of associated forcing file.
    :type forcing: str
    """
    if NamelistObject["NAM_PGD_GRID"].CGRID == "LONLATVAL":
        check_or_create_block(NamelistObject, "NAM_LONLATVAL")
        # Read coordinates in FORCING file
        forc = prosimu_base(forcing)
        latitudes1d = forc.read("LAT")
        longitudes1d = forc.read("LON")
        forc.close()

        if len(latitudes1d.shape) > 1:
            raise VarDimensionException("LAT", latitudes1d)

        if len(longitudes1d.shape) > 1:
            raise VarDimensionException("LON", longitudes1d)

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


def update_forcingdates(NamelistObject, datebegin, dateend, forcing="FORCING.nc", no_caution=False):
    """Modify SURFEX namelist limiting the dates to read in a FORCING file longer than the simulation.

    :param NamelistObject: Namelist to modified
    :type NamelistObject: class:`bronx.datagrip.namelist.NamelistSet`
    :param datebegin: Initial date of simulation
    :type datebegin: class:`bronx.stdtypes.date.Date`
    :param dateend: Last date of simulation
    :type dateend: class:`bronx.stdtypes.date.Date`
    :param forcing: Address of associated forcing file. Defaults to "FORCING.nc"
    :type forcing: str, optional
    :param no_caution: do not check the forcing dates to reduce computation time (dangerous option, not recommended)
    :type no_caution: boolean, optional
    """
    if no_caution:
        # Assume forcing dates are equal or larger than s2m option
        # Very dangerous option, not compatible with yearly simulations
        dateforcbegin = datebegin  # only used for the final test, ok while real dateforcbegin <= datebegin
        dateforcend = dateend  # only used for the final test, ok while real dateforcend >= dateend
    else:
        # Normal case
        forc = prosimu_base(forcing)
        timeforc = forc.readtime()
        forc.close()

        dateforcbegin = timeforc[0]
        dateforcend = timeforc[-1]

        print("DATES OF THE FORCING FILE:", dateforcbegin, dateforcend)
        print("PRESCRIBED SIMULATION DATES:", datebegin, dateend)

        checkdateafter(datebegin, dateforcbegin)
        if dateend:
            checkdateafter(dateend, dateforcbegin)
        else:
            dateend = dateforcend

    if datebegin > dateforcbegin or dateend < dateforcend or no_caution:
        NamelistObject["NAM_IO_OFFLINE"].LDELAYEDSTART_NC = True

    NamelistObject["NAM_IO_OFFLINE"].NDATESTOP = [dateend.year, dateend.month, dateend.day, dateend.hour * 3600]

    return NamelistObject


def update_physicaloptions(NamelistObject, **kwargs):
    """Modify physical options for ESCROC simulations

    :param NamelistObject: Namelist to modified
    :type NamelistObject: class:`bronx.datagrip.namelist.NamelistSet`
    :param kwargs: ESCROC physical options. Defaults to {}.
    :type kwargs: dict, optional
    """
    check_or_create_block(NamelistObject, "NAM_ISBA_SNOWn")
    check_or_create_block(NamelistObject, "NAM_ISBAn")

    for key, value in kwargs.items():
        if key.upper() in ["CSNOWDRIFT", "LSNOWDRIFT_SUBLIM", "LSNOW_ABS_ZENITH", "CSNOWMETAMO",
                           "CSNOWRAD", "CSNOWFALL", "CSNOWCOND", "CSNOWHOLD", "CSNOWCOMP", "CSNOWZREF", "LSNOWSYTRON"]:
            setattr(NamelistObject["NAM_ISBA_SNOWn"], key.upper(), value)
        elif key.upper() in ["CSNOWRES"]:
            setattr(NamelistObject["NAM_ISBAn"], key.upper(), value)
    return NamelistObject


def update_snowparameters(NamelistObject, **kwargs):
    """Modify physical parameters for ESCROC simulations

    :param NamelistObject: Namelist to modified
    :type NamelistObject: class:`bronx.datagrip.namelist.NamelistSet`
    :param kwargs: ESCROC physical parameters. Defaults to {}.
    :type kwargs: dict, optional
    """
    check_or_create_block(NamelistObject, "NAM_SURF_CSTS")
    check_or_create_block(NamelistObject, "NAM_SURF_SNOW_CSTS")
    check_or_create_block(NamelistObject, "NAM_ISBAn")
    for key, value in kwargs.items():
        if key.upper() in ["XZ0SN", "XZ0HSN", "XTAU_LW"]:
            setattr(NamelistObject["NAM_SURF_CSTS"], key.upper(), value)
        elif key.upper() in ["XALBICE1", "XALBICE2", "XALBICE3", "XRHOTHRESHOLD_ICE", "XZ0ICEZ0SNOW",
                             "XVAGING_GLACIER", "XVAGING_NOGLACIER", "XVVISC3", "X_RI_MAX",
                             "XIMPUR_WET", "XIMPUR_DRY"]:
            setattr(NamelistObject["NAM_SURF_SNOW_CSTS"], key.upper(), value)
        elif key.upper() in ["XCVHEATF"]:
            setattr(NamelistObject["NAM_ISBAn"], key.upper(), value)
        else:
            print("IGNORE FIELD " + key + " : not in namelist.")

    return NamelistObject


def update_cselect(NamelistObject, cselect):
    setattr(NamelistObject['NAM_WRITE_DIAG_SURFN'], 'CSELECT', cselect)


def update_namelist_object_nmembers(NamelistObject, nmembers):
    """Modify number of members for SODA simulations

    :param NamelistObject: Namelist to modified
    :type NamelistObject: class:`bronx.datagrip.namelist.NamelistSet`
    :param nmembers: number of members for SODA simulations.
    :type nmembers: int
    """
    if nmembers is not None:
        check_or_create_block(NamelistObject, "NAM_ENS")
        setattr(NamelistObject["NAM_ENS"], 'NENS', nmembers)
        print("NENS set to {}".format(nmembers))

    return NamelistObject


def update_namelist_var(namelist_file, data_file):
    import shutil
    """ Modify snowmaking parameters.
    The function reads "water consumption data for snowmaking" from an external file (data_file)
    and updates a namelist (namelist_file) accordingly.
    This function was implemented by C. Carmagnola in December 2018 (PROSNOW project).
    Comment Matthieu : the copy at the end is awful and dangerous for use in vortex, can we remove ?


    :param namelist_file: namelist address
    :type namelist_file: str
    :param data_file: file containing water consumption data
    :type data_file: str
    """

    # 1) Read data from an external file

    if not os.path.isfile(data_file):
        raise FileNameException(data_file)

    mdat = open(data_file)
    var_tbc = []

    for line in mdat:
        maliste = line.split()
        var_tbc = var_tbc + [maliste[2]]

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

    shutil.copy(namelist_file, "OPTIONS.nam")
