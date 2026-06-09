# -*- coding: utf-8 -*-

# Group variables
forcing = ["forcing_datebegin", "forcing_dateend", "forcing_xpid", "forcing_user", "forcing_geometry",
        "forcing_block", "forcing_vapp", "forcing_vconf", "forcing_member", "forcing_vortex1"]
pgd_cache = ["pgd_xpid", "pgd_user", "pgd_vapp", "pgd_vconf", "pgd_vortex1"]
pgd_uenv = ["pgd_uenv", "pgd_gvar"]
prep = ["prep_xpid", "prep_user", "prep_vapp", "prep_vconf", "prep_vortex1", "prep_member", "prep_block"]
init_tg_cache = ["tg_xpid", "tg_user", "tg_vapp", "tg_vconf"]
init_tg_uenv = ["tg_uenv", "tg_gvar"]

# Standard attributes
member_type = "int, footprints.stdtypes.FPList (ex : 'first-last-step')"
uenv_format = "<uenv_name> or <uenv_name>@<uenv_username>"
namespace_values = "'vortex.multi.fr' (Hendrix + local cache), 'vortex.cache.fr' (local cache),"
"'vortex.archive.fr' (Hendrix)"

standard_variables = dict(
    datebegin = dict(
        help  = "Begin date of the simulation",
        type  = "str or Date",
    ),
    dateend   = dict(
        help = "End date of the simulation",
        type  = "str or Date",
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
        help = "Geometry of the simulation. This must be a valid geometry tag in your"
        "'$HOME/.vortexrc/geometries.ini' file.",
        type  = "str",
    ),
    member = dict(
        help = "The simulation's member(s) in case it is part of an ensemble",
        type = member_type,
        default = "None",
    ),
    namelist_uenv     = dict(
        help = "Name of the User Environment containing the target namelist",
        type = "str",
        format = uenv_format,
        default = "uenv",
        alias = "surfex_uenv",
    ),
    uenv     = dict(
        help = "Name of the User Environment containing constant files",
        type = "str",
        format = uenv_format,
        default = "uenv",
    ),
    surfex_uenv     = dict(
        help = "Name of the User Environment containing SURFEX executables and namelists",
        type = "str",
        format = uenv_format,
        default = "uenv",
    ),
    forcing_datebegin = dict(
        help  = "Begin date of the forcing file(s)",
        type  = "str or Date",
        deafult = "datebegin",
    ),
    forcing_dateend   = dict(
        help = "End date of the forcing file(s)",
        type  = "str or Date",
        default = "dateend",
    ),
    forcing_xpid   = dict(
        help = "Experiment identifier of the forcing file(s)",
        type  = "str",
        default = "xpid",
    ),
    forcing_user   = dict(
        help = "Username of the producer of the forcing file",
        type  = "str",
        default = "$USER",
    ),
    forcing_geometry   = dict(
        help    = "Geometry of the forcing file(s). This must be a valid geometry tag in your"
        "'$HOME/.vortexrc/geometries.ini' file.",
        type    = "str",
        default = "geometry",
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
        choices = namespace_values,
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
        help = "Set this value to 'True' if the target forcing file(s) have been produced with a version of vortex <2",
        type = "bool",
        default = "False",
    ),
    pgd_xpid   = dict(
        help = "Experiment identifier of the PGD file",
        type  = "str",
        default = "xpid",
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
    pgd_vortex1 = dict(
        help = "Set this value to 'True' if the target PGD file have been produced with a version of vortex <2",
        type = "bool",
        default = "False",
    ),
    pgd_uenv = dict(
        help = "Name of the User Environment containing the target PGD file or executable",
        type = "str",
        format = uenv_format,
        default = "uenv",
        alias = "surfex_uenv",
    ),
    pgd_gvar = dict(
        help = "Key to look up the PGD.nc file in the uenv if the file should come from there.",
        type = "str",
        default = "'pgd_[geometry::tag]' or 'master_pgd_mpi', or 'master_pgd_nompi'",
    ),
    prep_xpid   = dict(
        help = "Experiment identifier of the PREP file",
        type  = "str",
        default = "xpid",
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
    prep_date = dict(
        help = "Validity date of the PREP file (if different from *datebegin*)",
        type = "str or Date",
        default = "*datebegin*",
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
    ),
    io_duration = dict(
        help = "Argument similar to the one of the `get_list_dates_files` method in snowtools/utils/dates.py."
        "Used to retrieve the list of *datebegin* and *dateend* for IO covering sub-periods",
        type = "str",
        default = "yearly",
        choices = "'yearly', 'monthly' or 'full' (look only for IOs covering the full simulation period",
    ),
    exesurfex = dict(
        help = "Absolute path to SURFEX executables",
        type = "str",
    ),
    tg_xpid = dict(
        help = "Experiment identifier the init_TG.nc file",
        type = "str",
        default = "*xpid*",
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
    tg_uenv = dict(
        help = "UEnv to look for the init_TG.nc file in case the file should come from an uenv.",
        type = "str",
        format = uenv_format,
        default = "*uenv*",
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
        help = "Absolute path to the SURFEX namelist",
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
    ),
    localtest = dict(
        help = "Test code outside HPC environment",
        type = "bool",
        default = "False",
    ),
    debug = dict(
        help = "Force crash at the end of the execution to preserve the working directory",
        type = "bool",
        default = "False",
    ),


)
