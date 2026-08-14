# -*- coding: utf-8 -*-
"""
configuration_variables.py
--------------------------

Documentation of most frequently used configuration variables.
See :ref:`dynamic_documentation` for more information on the documentation syntax conventions.

"""

# Standard attributes
member_type = "int, footprints.stdtypes.FPList (ex : 'first-last-step')"
uenv_format = "<uenv_name> or <uenv_name>@<uenv_username>"
namespace_choices = "'vortex.multi.fr' (Hendrix + local cache), 'vortex.cache.fr' (local cache),"
"'vortex.archive.fr' (Hendrix)"

xpid_default = "The simulation's *xpid*"
datebegin_default = "The simulation's *datebegin*"
dateend_default = "The simulation's *dateend*"
geometry_default = "The simulation's *geometry*"

standard_variables = dict(
    datebegin = dict(
        help  = "Begin date of the simulation",
        type  = "str or Date",
    ),
    dateend   = dict(
        help = "End date of the simulation",
        type  = "str or Date",
    ),
    datevalidity = dict(
        help = "The validity date of the PREP file",
        type = "Date",
    ),
    rundate = dict(
        help = "Run date",
        type = "str or Date",
    ),
    date = dict(
        help = "Run date",
        type = "str or Date",
    ),
    xpid   = dict(
        help = "Experiment identifier of the simulation",
        type  = "str",
    ),
    geometry   = dict(
        help = "Geometry of the simulation.",
        type  = "'str', 'list'",
    ),
    member = dict(
        help = "The simulation's member(s) in case it is part of an ensemble",
        type = member_type,
        default = "None",
    ),
    members = dict(
        help = "The list of ensemble members",
        type = member_type,
        default = "None",
    ),
    nmembers = dict(
        help = "Number of ensemble members",
        type = "int",
    ),
    uenv     = dict(
        help = "Name of the User Environment containing constant files",
        type = "str",
        format = uenv_format,
        # TODO : définir un uenv par défaut
    ),
    surfex_uenv = dict(
        help = "Name of the User Environment containing SURFEX executables",
        type = "str",
        format = uenv_format,
        default = "*uenv*",
    ),
    consts_surfex_uenv = dict(
        help = "Name of the User Environment containing all SURFEX constant files (including the namelist)",
        type = "str",
        format = uenv_format,
        default = "*uenv*",
    ),
    forcing = dict(
        metavar = True,
        help = "Footprint description of the FORCING file(s)",
        values = ["forcing_datebegin", "forcing_dateend", "forcing_xpid", "forcing_user", "forcing_geometry",
            "forcing_block", "forcing_vapp", "forcing_vconf", "forcing_member", "forcing_vortex1"],
    ),
    forcing_datebegin = dict(
        help  = "Begin date of the forcing file(s)",
        type  = "str or Date",
        deafult = datebegin_default,
    ),
    forcing_dateend   = dict(
        help = "End date of the forcing file(s)",
        type  = "str or Date",
        default = dateend_default,
    ),
    forcing_xpid   = dict(
        help = "Experiment identifier of the forcing file(s)",
        type  = "str",
        default = xpid_default,
    ),
    forcing_user   = dict(
        help = "Username of the producer of the forcing file",
        type  = "str",
        default = "$USER",
    ),
    forcing_geometry   = dict(
        help    = "Geometry of the forcing file(s).",
        type  = "'str', 'list'",
        default = geometry_default,
    ),
    forcing_member = dict(
        help = "The member(s) of the forcing file(s) in case they come from an ensemble",
        type = member_type,
        default = "None",
    ),
    forcing_vapp = dict(
        help = "The *vapp* level of the forcing file(s)",
        type = "str",
        default = "The simulation's *vapp*",
    ),
    forcing_vconf = dict(
        help = "The *vconf* level of the forcing file(s)",
        type = "str",
        default = "The simulation's *vconf*",
    ),
    forcing_block = dict(
        help = "The *block* level of the forcing file(s)",
        type = "str",
        default = "meteo",
    ),
    forcing_namespace = dict(
        help = "The forcing *namespace* (where to look for the data)",
        type = str,
        default = "vortex.multi.fr",
        choices = namespace_choices,
    ),
    forcing_namebuild = dict(
        help = "The forcing namebuilder (operational use only)",
        type = str,
        default = "flat@cen",
        choices = "None, 'flat@cen'",
    ),
    forcing_intent = dict(
        help = "Intent use of the forcing file(s), defines the forcing file(s) rights (read-write or read-only) "
        "in the working directory",
        type = "str",
        choices = "'in' (read-only), 'inout' (read-write)",
        default = "in",
    ),
    forcing_source_app = dict(
        help = "*vapp* of the original meteorological data used to create the forcing file(s) "
        "(S2M-reanalysis use only)",
        type = "str",
        default = "None"
    ),
    forcing_source_conf = dict(
        help = "*vconf* of the original meteorological data used to create the forcing file(s) "
        "(S2M-reanalysis use only)",
        type = "str",
        default = "None"
    ),
    forcing_source = dict(
        help = "Alias to find 'forcing_source_app' and 'forcing_source_conf' (S2M-reanalysis use only)",
        type = "str",
        default = "None",
        choices = "'era5', 'era40'",
    ),
    forcing_cutoff = dict(
        help = "The *cutoff* of the forcing file(s) (operational use only)",
        type = "str",
        choices = "'assimilation', 'production'",
    ),
    forcing_vortex1 = dict(
        help = "Set this value to 'True' if the target forcing file(s) have been produced "
        "with a version of vortex <2",
        type = "bool",
        default = "False",
    ),
    pgd = dict(
        metavar = True,
        help = "Footprint description of a PGD.nc file stored in a Vortex cache",
        values = ["pgd_xpid", "pgd_user", "pgd_vapp", "pgd_vconf", "pgd_geometry", "pgd_2d", "pgd_vortex1"],
    ),
    pgd_xpid   = dict(
        help = "Experiment identifier of the PGD file",
        type  = "str",
        default = xpid_default,
    ),
    pgd_user   = dict(
        help = "Username of the producer of the PGD file",
        type  = "str",
        default = "$USER",
    ),
    pgd_vapp = dict(
        help = "The *vapp* level of the PGD file",
        type = "str",
        default = "The simulation's *vapp*",
    ),
    pgd_vconf = dict(
        help = "The *vconf* level of the PGD file",
        type = "str",
        default = "The simulation's *vconf*",
    ),
    pgd_geometry   = dict(
        help    = "Geometry of the PGD.nc file.",
        type  = "'str', 'list'",
        default = geometry_default,
    ),
    pgd_2d = dict(
        help= "Set this value to 'True' if a PGD file for a 2D simulation should be produced",
        type="bool",
        default = "False",
    ),
    pgd_vortex1 = dict(
        help = "Set this value to 'True' if the target PGD file have been produced with a version of vortex <2",
        type = "bool",
        default = "False",
    ),
    pgdnc_gvar = dict(
        help = "Key to look up the PGD.nc file in the uenv if the file should come from there.",
        type = "str",
        default = "'pgd_[geometry::tag]'",
    ),
    force_uenv = dict(
        help = "Set this value to 'True' to search for a PGD.nc file only in the uenv and not in the cache or "
               "archive.",
        type = "bool",
        default = "False",
    ),
    pgd_gvar = dict(
        help = "Key to look up the PGD executable in the uenv if it should come from there.",
        type = "str",
        default = "'MASTER_PGD_MPI', or 'MASTER_PGD_NOMPI'",
    ),
    offline_gvar = dict(
        help = "Key to look up the OFFLINE executable in the uenv if it should come from there.",
        type = "str",
        default = "'MASTER_OFFLINE_MPI' or 'MASTER_OFFLINE_NOMPI'",
    ),
    prep_gvar = dict(
        help = "Key to look up the PREP executable in the uenv if it should come from there.",
        type = "str",
        default = "'MASTER_PREP_MPI' or 'MASTER_PREP_NOMPI'",
    ),
    soda_gvar = dict(
        help = "Key to look up the SODA executable in the uenv if it should come from there.",
        type = "str",
        default = "'MASTER_SODA_MPI' or 'MASTER_SODA_NOMPI'",
    ),
    prep = dict(
        metavar = True,
        help = "Footprint description of PREP.nc file(s)",
        values = ["prep_xpid", "prep_user", "prep_vapp", "prep_vconf", "prep_geometry", "prep_vortex1", "prep_member",
            "prep_block", "prep_datevalidity"],
    ),
    prep_xpid   = dict(
        help = "Experiment identifier of the PREP file",
        type  = "str",
        default = xpid_default,
    ),
    prep_user   = dict(
        help = "Username of the producer of the PREP file",
        type  = "str",
        default = "$USER",
    ),
    prep_vapp = dict(
        help = "The *vapp* level of the PREP file",
        type = "str",
        default = "The simulation's *vapp*",
    ),
    prep_vconf = dict(
        help = "The *vconf* level of the PREP file",
        type = "str",
        default = "The simulation's *vconf*",
    ),
    prep_geometry   = dict(
        help    = "Geometry of the PREP.nc file.",
        type  = "'str', 'list'",
        default = geometry_default,
    ),
    prep_vortex1 = dict(
        help = "Set this value to 'True' if the target PREP file have been produced with a version of vortex <2",
        type = "bool",
        default = "False",
    ),
    prep_member = dict(
        help = "The member(s) of the PREP file(s) in case they come from an ensemble (ex: SODA)",
        type = member_type,
        default = "None",
    ),
    prep_block = dict(
        help = "The *block* level of the PREP file(s)",
        type = "str",
        default = "prep",
    ),
    prep_datevalidity = dict(
        help = "Validity date of the PREP file (if different from *datebegin*)",
        type = "str or Date",
        default = datebegin_default,
    ),
    august_threshold = dict(
        help = "Threshold to apply to the snow water equivalent (in kg/m2) each 1st August",
        type = "int",
        default = "-999",
    ),
    drhook = dict(
        help = "Activate / deactivate the profiling with DRHOOK",
        type = "bool",
        default = "False",
    ),
    namespace_out = dict(
        help = "Force specific target namespace(s) for output files",
        type = "str",
        default = "vortex.multi.fr",
        singular = True,
    ),
    io_duration = dict(
        help = "Argument similar to the one of the `get_list_dates_files` method in snowtools/utils/dates.py."
        "Used to retrieve the list of *datebegin* and *dateend* for IO covering sub-periods",
        type = "str",
        default = "yearly",
        choices = "'yearly', 'monthly' or 'full' (look only for IOs covering the full simulation period)",
        singular = True,
    ),
    exesurfex = dict(
        help = "Absolute path to SURFEX executables (WARNING : non-reproductible simulations)",
        type = "str",
    ),
    tg_cache = dict(
        metavar = True,
        help = "Footprint description of the init_TG.nc file stored in a Vortex cache",
        values = ["tg_xpid", "tg_user", "tg_vapp", "tg_vconf", "tg_geometry"]
    ),
    tg_xpid = dict(
        help = "Experiment identifier the init_TG.nc file",
        type = "str",
        default = xpid_default,
    ),
    tg_user = dict(
        help = "Username of the producer of the init_TG.nc file",
        type = "str",
        default = "$USER",
    ),
    tg_vapp = dict(
        help = "Application of the init_TG.nc file",
        type = "str",
        default = "*vapp*",
    ),
    tg_vconf = dict(
        help = "Configuration of the init_TG.nc file",
        type = "str",
        default = "*vconf*",
    ),
    tg_geometry   = dict(
        help    = "Geometry of the init_TG.nc file.",
        type  = "'str', 'list'",
        default = geometry_default,
    ),
    tg_gvar = dict(
        help = "Key to look up the init_TG.nc file in the uenv if the file should come from there.",
        type = "str",
        default = "climtg_[geometry::area]",
    ),
    climground = dict(
        help = "Allow the generation of a ground initialization file by computing a climatological "
        "average of air temperature on the provided period.",
        type = "bool",
        default = "False",
    ),
    namelist_path     = dict(
        help = "Absolute path to the SURFEX namelist (WARNING : non-reproductible simulations)",
        type = "str",
    ),
    namelist_source     = dict(
        help = "Name of the target namelist in the User Environement",
        type = "str",
    ),
    nnodes = dict(
        help = "Number of nodes to allocate to the execution",
        type = "int or dict[geometry]",
    ),
    ntasks = dict(
        help = "Number of parallel tasks to allocate to the execution",
        type = "int or dict[geometry]",
    ),
    nprocs = dict(
        help = "Number of process to allocate to the execution",
        type = "int or dict[geometry]",
    ),
    test = dict(
        help = "Launch an HPC unit test",
        type = "bool",
        default = "False",
        singular = True,
    ),
    localtest = dict(
        help = "Test code outside HPC environment",
        type = "bool",
        default = "False",
        singular = True,
    ),
    debug = dict(
        help = "Force crash at the end of the execution to preserve the working directory",
        type = "bool",
        default = "False",
        singular = True,
    ),
    concat_dim = dict(
        help = "Dimensions along which to concatenate variables, as used by xarray.concat()",
        type = "str",
        default = "Number_of_points",
    ),
    max_ntasks = dict(
        help = "Set a maximum number of parallel tasks to  avoid memory overflow",
        type = "int",
        default = "*ntasks*",
    ),
    out_block = dict(
        help = "Set the output resource's block",
        type = "str",
    ),
    massifs = dict(
        help = "Massif number(s) to be extracted",
        type = "int, list",
    ),
    slopes = dict(
        help = "Slope(s) to be extracted",
        type = "int, list",
    ),
    elevations = dict(
        help = "Elevations(s) to be extracted",
        type = "int, list",
    ),
    aspects = dict(
        help = "Aspects(s) to be extracted",
        type = "int, list",
    ),
    newobs_xpid = dict(
        help = "Experiment identifier of the reconstructed hourly temperature observation dataset",
        type = "str",
        default = xpid_default,
    ),
    newobs_user = dict(
        help = "Username of the producer of the reconstructed hourly temperature observation dataset",
        type  = "str",
        default = "$USER",
    ),
    guess_xpid = dict(
        help = "Experiment identifier of the SAFRAN guess files",
        type = "str",
        default = xpid_default,
    ),
    guess_user = dict(
        help = "Username of the producer of the SAFRAN guess files",
        type  = "str",
        default = "$USER",
    ),
    prv_terms = dict(
        help = "Lead times of the Safran guess files",
        type = "footprints.stdtypes.FPList",
        format = "first-last-step",
    ),
    arpege_geometry = dict(
        help = "Geometry of ARPEGE analyses / forecasts files used to generate Safran guess",
        type = "str",
    ),
    pearp_geometry = dict(
        help = "Geometry of PEARP forecast files used to generate Safran guess",
        type = "str",
    ),
    nwp_xpid = dict(
        help = "Experiment identifier of the NWP models used to generate Safran guess files",
        type = "str",
    ),
    geometries   = dict(
        help = "List of output geometries of the simulation.",
        type  = "'list'",
    ),
    diff_xpid = dict(
        help = "Experiment identifier of the reference file for reproductibility check",
        type = "str",
        default = xpid_default,
    ),
    diff_user = dict(
        help = "Username of the producer of the reference file for reproductibility check",
        type  = "str",
        default = "$USER",
    ),
    diff_block = dict(
        help = "The reference's block for reproductibiliy check",
        type = "str",
    ),
)
