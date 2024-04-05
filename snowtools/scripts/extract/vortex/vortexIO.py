#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 8 march 2024

@author: Vernay

Extraction / Backup of common Flow resources (date-dependant)  with Vortex

For each resource a "get" and "put" methods are provided with a number of arguments
that can specified by the user.

Mandatory arguments are :
    * `datebegin` and `dateend` of the perdio covered by the resource
    * `xpid` of the experiment that produced the resource
    * `geometry` of the resource

All other arguments are optional and defined by default with standard names.
However it is possible to use this tool in a more advanced way by precising :
    * the `namespace` to precise where to get/put the resource. Possible values are:
        - 'vortex.cache.fr'   : the local Vortex cache
        - 'vortex.archive.fr' : Hendrix Vortex cache
        - 'vortex.multi.fr'   : both the local and Hendrix caches
        - TODO : inclure sxcen
    * the 'filename' of the target resource (its name in the working directory)
    * Either:
       - the number of *members* for ensemble simulations
      OR
       - the explicit *member* list, using the 'X-Y-1' syntax
    * the 'vapp' ('s2m' or 'edelweiss' currently) TODO : ajuster en fonction de la convention choisie
    * an 'abspath' to look for a file not in the current workind directory (TODO : not implemented yet)
    * the 'block' (the last directory(ies) names where the resource is stored on the Vortex cache)
    * the 'source_app' to precise which application produced the resource
    * the 'source_conf' to precise the configuration of the producing application
    * the 'model' to precise the name of the model that produced the resource (TODO : redondant ?)
      NB : the last 3 arguments are only used for resources at the begining of the processing chain


Specificities of 'meteorological' resources :
---------------------------------------------
Meteorological resources are all resources containing one or several meteorological variables.
There are 3 possibilities :

    1. The resource contains all variables necessary to run a SURFEX-Corcus simulation
       --> This is a standard FORCING file, identified by a 'MeteorologicalForcing' *kind*
           and stored in the 'meteo' *block*

    2. The resource contains only some of the variables necessary to run a SURFEX-Corcus simulation
      --> This resource must be called by a specific method identifying which variables are concerned
          and is also store in the 'meteo' *block*

    3. The resource contains meteorological variables that are not used by a SURFEX-Crocus experiment but
       are necessary/used to compute some of these variables (ex: total precipitation, iso-temperature levels,...)
       --> These resources must be called by the generic get[put]_meteo functions and are NOT stored under the
           'meteo' *block* to avoid overwritings

Examples :
==========

>>> from snowtools.scripts.extract.vortex import vortexIO

1. To save a 'PRO.nc' file present in the current working directory
 - covering the period 2024031306 --> 2024031406
 - produced by an experiment "XP00"
 - on a "GrandesRousses250m" geometry (already defined in the .vortexrc/geometries.ini file)

>>> vortexIO.put_pro('XP00', "GrandesRousses250m", datebegin='2024031306', dateend='2024031406')

2. To get the FORCING file generated by the S2M analysis dev chain of 13/03/2024 at 6h in the cor massif geometry:

>>> vortexIO.get_forcing('nouveaux_guess@vernaym', 'cor', datebegin='2024031206', dateend='2024031306', vapp='s2m',
        block='massifs', namebuild=None, date='2024031306', source_app='arpege', source_conf='4dvarfr')

3. Use of alternate resources:
   ---------------------------
   Looking for a PREP file valid at date "2017080206" for an experiment "test@vernaym" in the "GrandesRousses250m"
   geometry of the application "edelweiss".

>>> vortexIO.get_prep('test@vernaym', 'GrandesRousses250m', date='2017080206', vapp='edelweiss')

    --> works but returns
    # ----  Result from get: [False]  ---- #
    # [2024/03/19-16:58:23][vortex.toolbox][add_section:0386][ERROR]: Could not get resource PREP.nc
    because the file doesn't exist

    It is possible to allow for alternate ressource search by providing an *alternate_xpid* (by default,
    a PREP file from a 'spinup' experiment will be searched first) :

>>> vortexIO.get_prep('test@vernaym', 'GrandesRousses250m', date='2017080206', vapp='edelweiss',
        alternate_xpid='reanalysis2020.2@prep_reanalysis_CEN')

    --> works and returns a PREP file

4. [THEORETICAL EXAMPLE] To get a 16-member ensemble of pro files from a user "username"
 - covering the period yyyymmddhh --> YYYYMMDDHH
 - produced by an experiment "xpid"
 - on a "new_geometry" geometry (must be defined in the .vortexrc/geometries.ini file)
and name each PRO file "THIS_IS_A_PRO.nc" :

>>> vortexIO.get_pro('xpid@username', 'new_geometry', datebegin='yyyymmddhh', dateend='YYYYMMDDHH',
        members=16, filename="THIS_IS_A_PRO.nc")

TODO :
    - Ajouter les 2 premiers exemples dans la base de tests
    - Uniformiser la gestion du "role" (fixé pour chaque ressource ou modifiable en argument ?)
    - ajouter des exemples avec alternate (PREP)

'''

import os
import vortex
import cen  # Import necessary to load vortex CEN-specific ressourees
from vortex import toolbox

import footprints

toolbox.active_now = True

t = vortex.ticket()

if 'MTOOLDIR' not in os.environ.keys():
    # Set a default MTOOLDIR to define the local Vortex cache
    os.environ['MTOOLDIR'] = os.path.join(os.environ['HOME'], 'cache')
    if not os.path.exists(os.environ['MTOOLDIR']):
        os.makedirs(os.environ['MTOOLDIR'])

########################
# Some usefull functions
########################


def init():

    return dict(
        role        = None,
        namespace   = 'vortex.multi.fr',  # Get/put resources on both Hendrix and the local cache
        members     = None,
        vapp        = 's2m',
        abspath     = None,
        block       = '',
        namebuild   = 'flat@cen',  # CEN-specific file name builder (use only for CEN produced resources)
        fatal       = True,  # Crash if the resource is not found
        nativefmt   = 'netcdf',  # Most CEN standard files are netcdf files
        source_app  = None,  # Use only for "external" resources (produced by another application)
        source_conf = None,  # Use only for "external" resources (produced by another application)
        scope       = None,
        model       = None,  # TODO : réfléchir à la pertinence de mettre une valeur par défaut
        cutoff      = 'assimilation',  # TODO : réfléchir à la pertinence de mettre une valeur par défaut
        # Leave intent to 'in' for more efficient IOs (symbolic links instead of copies)
        intent      = 'in',  # Rights to give to the file (inout=read/write, in=read-only)
        # intent      = 'inout',  # Rights to give to the file (inout=read/write, in=read-only)
        download    = True,  # Download actual file (default mode, switch to False if you need Vortex object's only)
    )


def function_map():
    """
    Returns a dictionary to map function names to function objects
    """
    return {"get": get, "put": put}


def add_user(xpid):
    if '@' in xpid:
        return xpid
    else:
        user = os.environ['USER']
        return f'{xpid}@{user}'


def log_separator(resource):
    print('---------------------------------------------------------------------------------------------------------')
    print(f'-                                            ALTERNATIVE {resource}                                      ')
    print('---------------------------------------------------------------------------------------------------------')


def title(string):

    print('==========================================================================================================')
    print(f'=                               {string}                                                                =')
    print('==========================================================================================================')


def check_period(resource_name, **kw):
    """
    Check/set for *begindate* and *enddate* footprints for period-related resources (FORCING, PRO,...)

    CEN's convention is to name period footprints *datebegin* and *dateend*, but
    Vortex's convention is to name period footprints *begindate* / *enddate*
    An alias in cen/syntax/stdattrs.py ensures the equivalence

    The notion of "date" of an execution is imported from NWP models but does make much sense
    for snowpack modeling since simulations are instead likend to a period of time (except for PREP files).
    The convention is to set the "date" attribute as the end of the simulation period.
    """

    if 'datebegin' in kw.keys():
        kw['begindate'] = kw.pop('datebegin')
    else:
        raise KeyError(f'Missing mandatory *datebegin* keyword argument for resource {resource_name}')

    if 'dateend' in kw.keys():
        kw['enddate']   = kw.pop('dateend')
    else:
        raise KeyError(f'Missing mandatory *dateend* keyword argument for resource {resource_name}')

    kw['date'] = kw['enddate']

    return kw


def check_date(resource_name, **kw):
    """
    Check for *date* footprint for date-specific resources (PREP) and remove useless period specific
    footprints.
    """

    if 'date' not in kw.keys():
        raise KeyError(f'Missing mandatory *date* keyword argument for resource {resource_name}')

    # Remove useless footprints
    for key in ['datebegin', 'begindate', 'dateend', 'enddate']:
        if key in kw.keys():
            kw.pop(key)

    return kw


def check_datevalidity(resource_name, **kw):
    """
    Check for *datevalidity* footprint for snowpack observations and remove useless period specific
    footprints. If *datevalidity* is not definied, use the *date* footprint.

    TODO : Remplacer *datevalidity* par *date* dans les ressources Vortex (vérifier si la séparation à un sens)
    """

    if 'datevalidity' not in kw.keys() or 'date' not in kw.keys():
        raise KeyError(f'Missing mandatory *date* keyword argument for resource {resource_name}')

    # Remove useless footprints
    for key in ['datebegin', 'begindate', 'dateend', 'enddate']:
        if key in kw.keys():
            kw.pop(key)

    return kw


def check_vapp(resource_name, **kw):
    """
    Check for mandatory *vapp* footprint for snowpack observation resources.
    ex : Sentinel2, Pleiades,...
    """

    if 'vapp' not in kw.keys():
        raise KeyError(f'Missing mandatory *vapp* (application) keyword argument for resource {resource_name}'
                       'ex : Sentinel2, Pleiades')

    return kw


def get_full_description(specific_footprints, user_kw, specific_default_footprints):
    """
    Create full description dictionnary

    In case of keyword duplicates the priority order is :
        1. the ressource's *specific_footprints* (hard written in the method called  by the user)
        2. the user's kw arguments, *user_kw*
        3. the ressource's *specific_default_footprints* that must be defined in the toolbox but can be set by the user

    This means that a resource-specific footprint such as the "kind" can not be overwritten by the user (if the method
    "get_forcing" is called, the kind can not be "SnowpackSimulation", even if given as argument by the user)
    --> priority is given to the function called
    """

    common_default_footprints = init()
    common_default_footprints.update(specific_default_footprints)
    common_default_footprints.update(user_kw)
    common_default_footprints.update(specific_footprints)

    return common_default_footprints


###############################
# Main get/put common functions
###############################


def get(xpid, geometry, **kw):
    """
    Main  method to fetch a Flow resource with Vortex
    """
    # Forward all arguments to the footprints_kitchen function
    description = footprints_kitchen(xpid, geometry, **kw)
    if description['download']:
        tb = toolbox.input(**description)
    else:
        # Only load Vortex's *ResourceHandler* object, not the actual file
        tb = toolbox.rload(**description)
    return tb


def put(xpid, geometry, **kw):
    """
    Main  method to save a Flow resource with Vortex
    """
    # Forward all arguments to the footprints_kitchen function
    description = footprints_kitchen(xpid, geometry, **kw)
    tb = toolbox.output(**description)
    return tb


def footprints_kitchen(xpid, geometry, **kw):

    description = dict(
        experiment     = add_user(xpid),
        geometry       = geometry,
        vconf          = '[geometry:tag]',
    )

    if kw['members'] is not None or 'member' in kw.keys():
        # On peut actuellement passer *members* =  un entier (le nombre de membres)
        # ou un objet *member* convertible en FPLIst (string au format X-Y-1)
        # TODO : gérer ça plus proprement
        # TODO : uniformiser la gestion des membres
        if isinstance(kw['members'], int):
            description['member'] = footprints.util.rangex(0, kw.pop('members') - 1)
        elif isinstance(kw['members'], str):
            description['member'] = footprints.util.rangex(kw.pop('members'))
        # If there is more than 1 member, store the files in a sub-directory:
        if len(description['member']) > 1:
            description['local'] = f'mb[member]/{kw.pop("filename")}'
        else:
            description['local'] = kw.pop("filename")
    else:
        description['local'] = kw.pop("filename")

    if kw['model'] is None:
        kw['model'] = kw['vapp']

    kw.update(description)

    if 'sxcen' in kw['namespace']:
        description['storage'] = 'sxcen.cnrm.meteo.fr'

    return kw


########################################
# Main flow resources specific functions
########################################

def precipitation(action, xpid, geometry, **kw):

    kw = check_period('PRECIPITATION', **kw)

    # Precipitation-specific footprints
    specific_footprints = dict(
        role      = 'Precipitation',
        kind      = 'Precipitation',
        nativefmt = 'netcdf',
    )
    # Precipitation-specific defaults values that can be overwritten by the function's kw
    specific_default_footprints = dict(
        filename = 'PRECIPITATION.nc',
        block    = 'meteo',
    )

    # Create full description dictionnary
    description = get_full_description(specific_footprints, kw, specific_default_footprints)

    precipitation = function_map()[action](xpid, geometry, **description)
    print(t.prompt, 'PRECIPITATION =', precipitation)
    print()

    return precipitation


def meteo(action, xpid, geometry, **kw):
    """
    Generic function for resources containing any non FORCING-ready meteorological variable(s) necessary to construct
    a FORCING file but that can not be directly used as a FORCING variable (ex : total precipitation,
    iso wet-bulb temperature,...)
    """

    if 'kind' not in kw.keys():
        outstr = 'Missing mandatory *kind* keyword argument for meteo resource.\n'
        'Possible values are "Precipitation", "Wind", "ISO_TPW"'  # TODO : == SurfaceForcing's *kind* values
        raise KeyError(outstr)

    kw = check_period(kw['kind'].upper(), **kw)

    # Precipitation-specific footprints
    specific_footprints = dict(
        role      = kw['kind'],
        nativefmt = 'netcdf',
    )
    # Precipitation-specific defaults values that can be overwritten by the function's kw
    specific_default_footprints = dict(
        filename = f'{kw["kind"].upper()}.nc',
        block    = f'{kw["kind"].lower()}'
    )

    # Create full description dictionnary
    description = get_full_description(specific_footprints, kw, specific_default_footprints)

    if description['block'] == 'meteo':
        outstr = "The block of a 'meteo' resource can not be 'meteo' (sorry for the confusion)\n"
        "Use a different block or leave it to vortexIO to chose one"
        raise KeyError(outstr)

    precipitation = function_map()[action](xpid, geometry, **description)
    print(t.prompt, f'{kw["kind"]} =', precipitation)
    print()

    return precipitation


def wind(action, xpid, geometry, **kw):

    kw = check_period('WIND', **kw)

    # Wind-specific footprints
    specific_footprints = dict(
        role      = 'Wind',
        kind      = 'Wind',
        nativefmt = 'netcdf',
    )
    # Wind-specific defaults values that can be overwritten by the function's kw
    specific_default_footprints = dict(
        filename = 'WIND.nc',
        block    = 'meteo',
        source_app='arome',
        source_conf='devine',
        model='devine',
    )

    # Create full description dictionnary
    description = get_full_description(specific_footprints, kw, specific_default_footprints)

    wind = function_map()[action](xpid, geometry, **description)
    print(t.prompt, 'PRECIPITATION =', wind)
    print()

    return wind


def forcing(action, xpid, geometry, **kw):
    """
    Main function for complete FORCING files (can be directly use to run a SURFEX-Crocus simulation)
    """

    kw = check_period('FORCING', **kw)

    # FORCING-specific footprints not to be overwritten
    specific_footprints = dict(
        role      = 'Forcing',
        kind      = 'MeteorologicalForcing',
        nativefmt = 'netcdf',
    )
    # FORCING-specific defaults values that can be overwritten by the function's kw
    specific_default_footprints = dict(
        filename = 'FORCING.nc',
        block    = 'meteo',
        model    = 'safran'
    )

    # Create full description dictionnary
    description = get_full_description(specific_footprints, kw, specific_default_footprints)

    forcing = function_map()[action](xpid, geometry, **description)
    print(t.prompt, 'FORCING =', forcing)
    print()

    return forcing


def prep(action, xpid, geometry, **kw):
    """
    Function to get a PREP file for a SURFEX (OFFLINE) simulation.

    The nominal case is to use a PREP file already available for the simulation's xpid, geometry and initial date.
    If no such file is available, an alternative file coming from a previous "spinup" exepriment can be used.
    The third possibility is to look for a PREP file from the S2M reanalysis (works only for massif geometries).

    IMPORTANT : The user must explicitely switch on the alternate resource search by providing an *alternate_xpid*
    keyword argument.

    If there is no PREP file available, the user should generate one by calling the PREP task before the OFFLINE task.
    """

    kw = check_date('PREP', **kw)

    # PREP-specific footprints not to be overwritten
    specific_footprints = dict(
        role      = 'SnowpackInit',
        kind      = 'PREP',
        nativefmt = 'netcdf',
        fatal     = False,  # Several possibilities, do not crash imediatly !
    )
    # PREP-specific defaults values that can be overwritten by the function's kw
    specific_default_footprints = dict(
        filename = 'PREP.nc',
        block    = 'prep',
        model    = 'surfex'
    )

    # Create full description dictionnary
    description = get_full_description(specific_footprints, kw, specific_default_footprints)

    # Look for a PREP file if already available for this xpid, geometry, and initial date
    prep = function_map()[action](xpid, geometry, **description)
    print(t.prompt, 'PREP (a) =', prep)
    print()

    if action == 'get' and 'alternate_xpid' in kw.keys():

        log_separator('PREP (b)')

        # 1st alternate : look for a PREP file if already available for this geometry,
        # and initial date for the "spinup" xpid
        description['alternate'] = description.pop('role')  # This is an alternative resource
        alternate_xpid = 'spinup@' + t.env.getvar("USER")
        prep.extend(function_map()[action](alternate_xpid, geometry, **description))
        print(t.prompt, 'PREP (b) =', prep)
        print()

        log_separator('PREP (c)')

        # 2nd alternate : look for a PREP file if already available for this geometry,
        # and initial date for the "reanalysis" xpid
        # If not available, crash : the PREP file should be build by calling the PREP task first
        # WARNING : this behaviour is different from the one in the surfex_task
        prep.extend(function_map()[action](kw['alternate_xpid'], geometry, **description))
        print(t.prompt, 'PREP (c) =', prep)
        print()

    return prep


def pro(action, xpid, geometry, **kw):
    """
    Main funciton for PRO files (SURFEX-Crocus) outputs.
    """

    kw = check_period('PRO', **kw)

    # PRO-specific footprints not to be overwritten
    specific_footprints = dict(
        kind      = 'SnowpackSimulation',
        model     = 'surfex',
        nativefmt = 'netcdf',
    )

    # PRO-specific defaults values that can be overwritten by the function's kw
    specific_default_footprints = dict(
        filename = 'PRO.nc',
        block    = 'pro',
    )

    # Create full description dictionnary
    description = get_full_description(specific_footprints, kw, specific_default_footprints)

    # Call the common get/put method
    pro = function_map()[action](xpid, geometry, **description)
    print(t.prompt, 'PRO =', pro)
    print()

    return pro


def diag(action, xpid, geometry, **kw):
    """
    Main function for DIAG (SURFEX-Crocus diagnostics) files.
    """

    kw = check_period('DIAG', **kw)

    # DIAG-specific footprints
    specific_footprints = dict(
        kind      = 'diagnostics',
        scope     = 'SesonalSnowCoverDiagnostic',
        nativefmt = 'netcdf',
        model     = 'surfex',
    )

    # DIAG-specific defaults values that can be overwritten by the function's kw
    specific_default_footprints = dict(
        filename = 'DIAG.nc',
        block    = 'diag',
    )

    # Create full description dictionnary
    description = get_full_description(specific_footprints, kw, specific_default_footprints)

    # Call the common get/put method
    diag = function_map()[action](xpid, geometry, **description)
    print(t.prompt, 'DIAG =', diag)
    print()

    return diag


# TODO : use a decorator
def get_pro(xpid, geometry, **kw):
    return pro('get', xpid, geometry, **kw)


# TODO : use a decorator
def put_pro(xpid, geometry, **kw):
    return pro('put', xpid, geometry, **kw)


# TODO : use a decorator
def get_diag(xpid, geometry, **kw):
    return diag('get', xpid, geometry, **kw)


# TODO : use a decorator
def put_diag(xpid, geometry, **kw):
    return diag('put', xpid, geometry, **kw)


# TODO : use a decorator
def get_forcing(xpid, geometry, **kw):
    return forcing('get', xpid, geometry, **kw)


# TODO : use a decorator
def put_forcing(xpid, geometry, **kw):
    return forcing('put', xpid, geometry, **kw)


# TODO : use a decorator
def get_meteo(xpid, geometry, **kw):
    return meteo('get', xpid, geometry, **kw)


# TODO : use a decorator
def put_meteo(xpid, geometry, **kw):
    return meteo('put', xpid, geometry, **kw)


# TODO : use a decorator
def get_prep(xpid, geometry, **kw):
    return prep('get', xpid, geometry, **kw)


# TODO : use a decorator
def put_prep(xpid, geometry, **kw):
    return prep('put', xpid, geometry, **kw)


# TODO : use a decorator
def get_precipitation(xpid, geometry, **kw):
    return precipitation('get', xpid, geometry, **kw)


# TODO : use a decorator
def put_precipitation(xpid, geometry, **kw):
    return precipitation('put', xpid, geometry, **kw)


# TODO : use a decorator
def get_wind(xpid, geometry, **kw):
    return precipitation('get', xpid, geometry, **kw)


# TODO : use a decorator
def put_wind(xpid, geometry, **kw):
    return precipitation('put', xpid, geometry, **kw)


############################################
# Main "static resources" specific functions
############################################


def pgd(action, xpid, geometry, **kw):
    """
    Function to get a PGD file for a SURFEX (OFFLINE) simulation.

    The nominal case is to use a PGD file already available for the simulation's xpid and geometry.
    If no such file is available, an alternative file coming from a previous "spinup" exepriment can be used.

    If there is no PGD file available, the user should generate one by calling the PGD task before the OFFLINE task.
    """
    # PGD-specific footprints not to be overwritten
    specific_footprints = dict(
        role     = 'SurfexClim',
        kind     = 'pgdnc',  # TODO : à modifier, la classe common.data.surfex.PGDNC est obsolète !
        model    = 'surfex',
        fatal    = False,  # Several possibilities, do not crash imediatly !
    )
    # PGD-specific defaults values that can be overwritten by the function's kw
    specific_default_footprints = dict(
        filename = 'PGD.nc',
        block    = 'pgd',
    )

    # Create full description dictionnary
    description = get_full_description(specific_footprints, kw, specific_default_footprints)

    # Look for a PGD file already available for this xpid and geometry
    pgd_a = function_map()[action](xpid, geometry, **description)
    print(t.prompt, 'PGD input (a) =', pgd_a)
    print()

    if action == 'get':

        log_separator('PGD (b)')
        # Alternate : look for a PGD file if already available for this geometry with the "spinup" xpid
        # If not available, crash : the PGD file should be build by calling the PGD task first
        # WARNING : this behaviour is different from the one in the surfex_task
        description['alternate'] = description.pop('role')  # This is an alternative resource
        description['fatal']     = True  # Last change to get the resource
        alternate_xpid           = 'spinup@' + t.env.getvar("USER")
        pgd_b = get(alternate_xpid, geometry, **description)
        print(t.prompt, 'PGD input (b) =', pgd_b)
        print()

        return pgd_a, pgd_b

    else:

        return pgd_a


def get_pgd(xpid, geometry, **kw):
    return pgd('get', xpid, geometry, **kw)


def put_pgd(xpid, geometry, **kw):
    return pgd('put', xpid, geometry, **kw)


def get_const(uenv, kind, geometry, fmt='nc', intent='in', fatal=False, **kw):

    if 'filename' in kw.keys():
        filename = kw['filename']
    else:
        filename = f'{kind.upper()}.{fmt}'

    if 'gvar' in kw.keys():
        gvar = kw['gvar']
    else:
        gvar = '[kind]_[gdomain]'  # Default value from Vortex

    const = toolbox.input(
        kind           = kind,
        genv           = uenv,
        geometry       = geometry,
        local          = filename,
        gdomain        = '[geometry::area]',  # Default value from Vortex
        gvar           = gvar,
        nativefmt      = fmt,
        intent         = intent,  # 'in' = make a hard link rather than a copy
        fatal          = fatal,
    ),
    print(t.prompt, f'{kind.upper()} =', const)
    print()

    return const


def get_surfex_namelist(uenv, alternate_uenv=None, source='OPTIONS.nam', intent='inout'):
    """
    Specific resource for SURFEX's namelist because a *role* attribute may be definied
    for parallelisation configuration.A
    The namelist file will be modifier by the pre-processing step, so a copy must be made
    instead of a symbolic link (--> intent='inout').

    TODO : stocker toutes les namelists dans un même 'tar' et les identifier avec le
    footprint *source* ?
    """

    namelist = toolbox.input(
        role            = 'Nam_surfex',
        source          = source,
        genv            = uenv,
        kind            = 'namelist',
        model           = 'surfex',
        local           = 'OPTIONS.nam',
        intent          = 'inout',
    )
    print(t.prompt, 'OPTIONS.nam =', namelist)
    print()

    if alternate_uenv is not None and not namelist:

        title('ALTERNATIVE SURFEX NAMELIST (b)')

        namelist = toolbox.input(
            alternate       = 'Nam_surfex',
            source          = source,
            genv            = alternate_uenv,
            kind            = 'namelist',
            model           = 'surfex',
            local           = 'OPTIONS.nam',
            intent          = 'inout',
        )
        print(t.prompt, 'OPTIONS.nam =', namelist)
        print()

    return namelist


def get_const_offline(geometry, uenv, alternate_uenv=None):

    if alternate_uenv is not None:
        fatal = False
    else:
        fatal = True

    title('STATIC OFFLINE INPUTS')

    # Binary ECOCLIMAP I files are mandatory to run SURFEX and taken from the uenv
    ecmap1 = toolbox.input(
        role           = 'Surfex cover parameters',
        kind           = 'coverparams',
        nativefmt      = 'bin',
        local          = 'ecoclimapI_covers_param.bin',
        geometry       = geometry,
        genv           = uenv,
        source         = 'ecoclimap1',
        model          = 'surfex',
        fatal          = fatal,
    ),
    print(t.prompt, 'ecoclimap1 =', ecmap1)
    print()

    # Binary ECOCLIMAP II files are mandatory to run SURFEX and taken from the uenv
    ecmap2 = toolbox.input(
        role           = 'Surfex cover parameters',
        kind           = 'coverparams',
        nativefmt      = 'bin',
        local          = 'ecoclimapII_eu_covers_param.bin',
        geometry       = geometry,
        genv           = uenv,
        source         = 'ecoclimap2',
        model          = 'surfex',
        fatal          = fatal,
    ),
    print(t.prompt, 'ecoclimap2 =', ecmap2)
    print()

    # Crocus metamorphism parameters mandatory to run SURFEX and taken from the uenv
    drdt_bst_fit_60 = toolbox.input(
        role            = 'Parameters for F06 metamorphism',
        kind            = 'ssa_params',
        genv            = uenv,
        nativefmt       = 'netcdf',
        local           = 'drdt_bst_fit_60.nc',
        model           = 'surfex',
        fatal           = fatal,
    )
    print(t.prompt, 'drdt_bst_fit_60 =', drdt_bst_fit_60)
    print()

    if alternate_uenv is not None:

        title('ALTERNATIVE STATIC OFFLINE INPUTS (b)')

        # Binary ECOCLIMAP I files are mandatory to run SURFEX and taken from the uenv
        ecmap1 = toolbox.input(
            alternate      = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapI_covers_param.bin',
            geometry       = geometry,
            genv           = alternate_uenv,
            source         = 'ecoclimap1',
            model          = 'surfex',
        ),
        print(t.prompt, 'ecoclimap1 =', ecmap1)
        print()

        # Binary ECOCLIMAP II files are mandatory to run SURFEX and taken from the uenv
        ecmap2 = toolbox.input(
            alternate      = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapII_eu_covers_param.bin',
            geometry       = geometry,
            genv           = alternate_uenv,
            source         = 'ecoclimap2',
            model          = 'surfex',
        ),
        print(t.prompt, 'ecoclimap2 =', ecmap2)
        print()

        # Crocus metamorphism parameters mandatory to run SURFEX and taken from the uenv
        drdt_bst_fit_60 = toolbox.input(
            alternate       = 'Parameters for F06 metamorphism',
            kind            = 'ssa_params',
            genv            = alternate_uenv,
            nativefmt       = 'netcdf',
            local           = 'drdt_bst_fit_60.nc',
            model           = 'surfex',
        )
        print(t.prompt, 'drdt_bst_fit_60 =', drdt_bst_fit_60)
        print()


def get_init_TG(geometry, genv, xpid):
    # not self.conf.climground means that the user does not allow the climatological file
    # to be built by the system (-g option of the s2m command is not activated)
    # If no prep file has been found, look for a climatological file to initialize the ground in the uenv
    initTG_a = toolbox.input(
        role           = 'initial values of ground temperature',
        kind           = 'climTG',
        nativefmt      = 'netcdf',
        local          = 'init_TG.nc',
        geometry       = geometry,
        genv           = genv,
        gvar           = 'climtg_[geometry::tag]',
        model          = 'surfex',
        fatal          = False
    ),
    print(t.prompt, 'Init_TG (a) =', initTG_a)
    print()

    # Alternate : look for the climatological file if already available for this xpid and geometry
    # Fail if not available because mandatory to build a prep file !
    initTG_b = toolbox.input(
        alternate      = 'initial values of ground temperature',
        kind           = 'climTG',
        nativefmt      = 'netcdf',
        local          = 'init_TG.nc',
        experiment     = xpid,
        geometry       = geometry,
        model          = 'surfex',
        namespace      = 'vortex.multi.fr',
        namebuild      = 'flat@cen',
        block          = 'prep',
    ),

    print(t.prompt, 'Init_TG (b) =', initTG_b)
    print()

#######################################
# Main "executables" specific functions
#######################################


def get_offline_mpi(uenv, alternate_uenv=None, gvar='master_surfex_offline_mpi'):

    if alternate_uenv is not None:
        fatal = False
    else:
        fatal = True

    title('OFFLINE MPI EXECUTABLE')

    offline = toolbox.executable(
        role           = 'Binary',
        kind           = 'offline',
        local          = 'OFFLINE',
        model          = 'surfex',
        genv           = uenv,
        gvar           = gvar,
        fatal          = fatal,
    )
    print(t.prompt, 'Executable =', offline)
    print()

    if alternate_uenv is not None and not offline:

        title('ALTERNATIVE OFFLINE MPI EXECUTABLE (b)')

        offline = toolbox.executable(
            alternate      = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            genv           = alternate_uenv,
            gvar           = gvar,
        )
        print(t.prompt, 'Executable =', offline)
        print()

    return offline


#######################################################################################################################
# WARNING : functions bellow this point are in development
#######################################################################################################################


def snow_obs_date(action, xpid, geometry, **kw):
    """
    Main function for dated snowpack observations.
    """

    kw = check_date('SnowObsDate', **kw)
    kw = check_vapp('SnowObsPeriod', **kw)

    # DIAG-specific footprints
    specific_footprints = dict(
        # Specific footprints
        members   = None,
        kind      = 'SnowObservations',
        model     = 'surfex',
        nativefmt = 'netcdf',
    )

    # DIAG-specific defaults values that can be overwritten by the function's kw
    specific_default_footprints = dict(
        filename = 'SnowObs_[date].nc',
    )

    # Create full description dictionnary
    description = get_full_description(specific_footprints, kw, specific_default_footprints)

    # Call the common get/put method
    obs = function_map()[action](xpid, geometry, **description)
    print(t.prompt, 'SnowObsDate =', obs)
    print()

    return diag


def snow_obs_period(action, xpid, geometry, **kw):
    """
    Main function for dated snowpack observations.
    """

    kw = check_period('SnowObsPeriod', **kw)
    kw = check_vapp('SnowObsPeriod', **kw)

    # DIAG-specific footprints
    specific_footprints = dict(
        # Specific footprints
        members   = None,
        kind      = 'SnowObservations',
        model     = 'surfex',
        nativefmt = 'netcdf',
    )

    # DIAG-specific defaults values that can be overwritten by the function's kw
    specific_default_footprints = dict(
        filename = 'SnowObs_[begindate]_[enddate].nc',
    )

    # Create full description dictionnary
    description = get_full_description(specific_footprints, kw, specific_default_footprints)

    # Call the common get/put method
    obs = function_map()[action](xpid, geometry, **description)
    print(t.prompt, 'SnowObsPerdio =', obs)
    print()

    return diag


def put_snow_obs_date(xpid, geometry, **kw):
    return snow_obs_date('put', xpid, geometry, **kw)


def get_snow_obs_date(xpid, geometry, **kw):
    return snow_obs_date('get', xpid, geometry, **kw)


def put_snow_obs_period(xpid, geometry, **kw):
    return snow_obs_period('put', xpid, geometry, **kw)


def get_snow_obs_period(xpid, geometry, **kw):
    return snow_obs_period('get', xpid, geometry, **kw)
