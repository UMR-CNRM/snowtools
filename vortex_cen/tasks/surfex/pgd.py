# -*- coding: utf-8 -*-
"""
"""

import vortex
from vortex.util.helpers import InputCheckerError
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin


class PgdCommonsMixin(SurfexCommonsMixin):
    """
    Mixin methods for PGD binary IOs.

    Configuration variables used by mixin methods:
    ----------------------------------------------

    * ``surfex_uenv`` or ``uenv`` The uenv that holds the soil databases for 2D simulations and the PGD executable.
    * ``pgd_gvar`` optional variable to specify the name of the PGD executable in the uenv.
          Default is ``master_pgd_mpi`` if mpi=*True*
           and `master_pgd_nompi`` if mpi=*False*
    * ``exesurfex`` path to the folder with surfex executables.
    """
    def get_2D_databases(self):
        """
        Get Sand_DB.bin and Sand_DB.hdr, Clay_DB.bin and Clay_DB.hdr,
        ECOCLIMAP_II_EUROP.dir and ECOCLIMAP_II_EUROP.hdr from Uenv

        Configuration variables used:
        -----------------------------
        * ``surfex_uenv`` or ``uenv`` The uenv that holds the soil databases for 2D simulations.
        """
        # Binary Sand files are mandatory to run SURFEX for PGD construction in simu2D
        self.sh.title('Toolbox input sand')
        sand_tbi = vortex.input(
            role           = 'SandDB',
            format         = 'dir/hdr',
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
            model          = 'surfex',
            kind           = 'sand',  # 'database'
            local          = 'sand_DB.tgz',
            source         = 'sand_DB',
            gvar           = 'sand_DB',
        )
        print(self.ticket.prompt, 'sand_tbi =', sand_tbi)
        print()

        # Binary Clay files are mandatory to run SURFEX for PGD construction in simu2D
        self.sh.title('Toolbox input clay')
        clay_tbi = vortex.input(
            role           = 'ClayDB',
            format         = 'dir/hdr',
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
            model          = 'surfex',
            kind           = 'clay',
            local          = 'clay_DB.tgz',
            source         = 'clay_DB',
            gvar           = 'clay_DB',
        )
        print(self.ticket.prompt, 'clay_tbi =', clay_tbi)
        print()

        # EcoclimapII_europ files are mandatory to run SURFEX for PGD construction in simu2D
        self.sh.title('Toolbox input ecoclimap2_europ')
        ecoclimap2_europ_tbi = vortex.input(
            role           = 'EcoclimapIIEurop',
            format         = 'dir/hdr',
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
            model          = 'surfex',
            kind           = 'coverparams',
            local          = 'ECOCLIMAP_II_EUROP.tgz',
            source         = 'ecoclimap2',
            gvar           = 'ECOCLIMAP_II_EUROP',
        )
        print(self.ticket.prompt, 'ecoclimap2_europ_tbi =', ecoclimap2_europ_tbi)
        print()

    def get_pgd_exe_from_uenv(self, mpi=True, fatal=True):
        """
         Method to get a PGD executable from uenv.
        :param mpi: True if an executable with MPI support should be fetched, False otherwise. Default is True.
        :param fatal: True if failing to fetch the executable should cause a fatal error, False otherwise.
            Default is True.

        Configuration variables used:
        -----------------------------
        * ``surfex_uenv`` or ``uenv`` The uenv that holds the PGD executable.
        * ``pgd_gvar`` optional variable to specify the name of the PGD executable in the uenv.
          Default is ``master_pgd_mpi`` if mpi=*True*
           and `master_pgd_nompi`` if mpi=*False*
        """

        if mpi:
            default_gvar = 'master_pgd_mpi'
        else:
            default_gvar = 'master_pgd_nompi'

        self.sh.title('Toolbox input PGD executable from uenv')
        pgd_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
            gvar           = self.conf.get('pgd_gvar', default_gvar),
            fatal          = fatal,
        )
        print(self.ticket.prompt, 'PGD_tbx =', pgd_tbx)
        print()

    def get_pgd_exe_from_local_path(self):
        """
        Fetch the PGD executable from a local path.

        Configuration variables used:
        -----------------------------
        * ``exesurfex`` path to the folder with surfex executables.
        """
        self.sh.title('Toolbox input PGD executable from local path')
        pgd_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PGD"
        )
        print(self.ticket.prompt, 'PGD_tbx =', pgd_tbx)
        print()


class _PgdConstruct(PgdCommonsMixin, _CenResearchTask):
    """
    Task : _PgdConstruct
    =====================

    Abstract task for the generation of ground physiography (PGD.nc file).

   Inputs:
    -------
    - FORCING file
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD executable

    Outputs:
    --------
    - PGD.nc (Ground physiography)

    Mandatory configuration variables:
    ----------------------------------

    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
      type: str
    * ``surfex_unev`` or ``uenv`` User Environment in which the following resources are to be retrieved :
                 - ecoclimapI_covers_param.bin
                 - ecoclimapII_eu_covers_param.bin
                 - drdt_bst_fit_60.nc
                 - PGD executable (unless supplied locally using the ``exesurfex`` variable).
                 Format : uenv:{uenv_name}@{user}
      type: str

    Optionnal configuration variables (other than forcing-specific ones):
    ---------------------------------------------------------------------
    * ``pgd_2d`` *True* if the PGD.nc should be calculated for a 2D domain. Default: *False*
      type: bool
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary. Default: 1
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary. Default: 1
      type: int
    * ``nodes`` Number of nodes to allocate to the execution of the MPI binary. Default: 1
      type: int
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str
     * ``diff_xpid`` Experiment id of the reference file used for reproducibility test.
      type diff_xpid: str
    * ``diff_user`` *user name* associated with the reference file used for reproducibility test
      (only if different from current user). Default: *None*
      type diff_user: str

    Forcing related configuration variables:
    ----------------------------------------

     Mandatory:
     **********

    * ``forcing_datebegin`` *datebegin* footprint, default self.conf.datebegin
      type forcing_datebegin: str, footprints.stdtypes.FPList
    * ``forcing_dateend`` *dateend* footprint, default self.conf.dateend
      type forcing_dateend: str, footprints.stdtypes.FPList
    * ``forcing_xpid`` Experiment identifier, default self.conf.xpid
      type forcing_xpid: str
    * ``forcing_geometry`` *geometry* footprint, default self.conf.geometry
      type forcing_geometry: str, footprints.stdtypes.FPList
    * ``forcing_vapp`` *vapp* footprint, default self.conf.vapp
      type forcing_vapp: str
    * ``forcing_vconf`` *vconf* footprint, default self.conf.vconf
      type forcing_vconf: str
    * ``forcing_block`` *block* footprint, default "meteo"
      type forcing_vconf: str
    * ``forcing_namespace`` *namespace* footprint, default "vortex.multi.fr" (hendrix + local cache)
      type forcing_namespace: str

    * ``forcing_date`` *date* footprint (unsed with the research namebuilders), default to [dateend]
      type forcing_date: str
    * ``forcing_model`` *model* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_model: str

    Optional:
    *********

    * ``forcing_member`` *member* footprint, default None (or *member* if provided)
      type forcing_member: int, footprints.stdtypes.FPList
    * ``forcing_namebuild`` *namebuild* footprint, default "flat@cen" (will change soon)
      type forcing_namebuild: str
    * ``forcing_intent`` *intent* footprint (local file permissions), default "in"
                           Possible values : "in" (read-only), "inout" (read-write)
      type forcing_intent: str
    * ``forcing_source_app`` *source_app* footprint, default None
      type forcing_source_app: str, footprints.stdtypes.FPList
    * ``forcing_source_conf`` *source_conf* footprint, default None
      type forcing_source_conf: str, footprints.stdtypes.FPList
    * ``forcing_source`` Retrieve *source_app* and *source_conf* footrprints dictionnaries for S2M reanalysis
                           Possible values : 'era5', 'era40'
      type forcing_source: str
    * ``forcing_cutoff`` *cutoff* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_cutoff: str
    * ``io_duration`` Argument similar to the one of the `get_list_dates_files` method in
                        snowtools/utils/dates.py.
                        Used to retrieve the list of *datebegin* and *dateend* for inputs covering sub-periods.
                        Possible values : "yearly", "monthly" or "full"
      type io_duration: str
    * ``forcing_vortex1`` Boolean to identify resources produced with vortex1 (filename without geometry)
      type forcing_vortex1: bool
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "geometry",
            "xpid",
            "uenv|surfex_uenv",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "pgd_2d",
            "ntasks",
            "nnodes",
            "nprocs",
            "diff_xpid",
            "diff_user",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get forcing file(s), ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        drdt_bst_fit_60.nc
        """
        simulation2d = self.conf.get("pgd_2d", False)
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                         alternate=self.conf.get("forcing_alternate", True))
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        if simulation2d:
            self.get_2D_databases()
        self.get_pgd_executable()

    def get_pgd_executable(self):
        """
        Call either "get_pgd_exe_from_local_path" or "get_pgd_exe_from_uenv" method.
        """
        raise NotImplementedError("A get_prep_executable method should be implemented, it should call either the "
                                  "get_prep_exe_from_path or the get_prep_exe_from_uenv method.")

    def get_local_inputs(self):
        """
        Get OPTIONS.nam which is always in the user's local cache because it comes
        from a namelist pre-processing task.
        """
        self.get_namelist_from_cache()

    def algo(self):
        """
        Algo component to produce the PGD file
        """
        avail_forcings = self.ticket.context.sequence.effective_inputs(role='Forcing')
        if len(avail_forcings) > 0:
            firstforcing = avail_forcings[0]
        else:
            raise InputCheckerError('No FORCING file present, the task can not run properly')

        self.sh.title('Toolbox algo PGD')
        pgd_tba = vortex.task(
            kind         = 'pgd_from_forcing',
            # Le nom local de la ressource est fourni par le "container"
            forcingname  = firstforcing.rh.container.basename,
        )
        print(self.ticket.prompt, 'Toolbox algo pgd=', pgd_tba)
        print()
        return pgd_tba

    def launch_algo(self, algo, **kwargs):
        """
        Run PGD algo component.

        :param algo: algo component to launch (pgd_tba)
        :param kwargs: not used
        """
        # Pour un exécution de binaire, il faut donner l'objet "exécutable" associé (récupéré par la commande
        # vortex.executable(...))
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        #
        # MV : Il faudra également pouvoir fournir le nombre de process et le nombre de tâches via le fichier de conf
        # TODO : réfléchir à la procédure pour définir des valeurs par défaut en fonction du domaine comme c'est
        # le cas actuellement
        # TODO : S'assurer que ce qui suit fonctionne avec un executable compilé sans MPI,
        # ou prévoir un switch MPI / NOMPI
        self.component_runner(
            algo,
            executable,
            mpiopts=dict(
                nnodes=self.conf.get('nnodes', 1),
                nprocs=self.conf.get('nprocs', 1),
                ntasks=self.conf.get('ntasks', 1),
            )
        )

    def put_outputs(self):
        """
        Save the PGD file
        """
        self.sh.title('Toolbox Output PGD')
        pgd_tbo = vortex.output(
            local      = 'PGD.nc',
            role       = 'SurfexClim',
            experiment = self.conf.xpid,
            geometry   = self.conf.geometry,
            nativefmt  = 'netcdf',
            kind       = 'pgdnc',
            model      = 'surfex',
            namespace  = 'vortex.multi.fr',
            namebuild  = 'flat@cen',  # TODO : passer en variable de configuration
            block      = 'pgd',
        ),
        # MF: in surfex_task.py:       member = self.conf.member if hasattr(self.conf, 'member') else None,
        # MV : c'était un bug introduit par mon commit #21915d5748ec0f80095edced4fc7ee6790a8faa4
        print(self.ticket.prompt, 'pgd_tbo =', pgd_tbo)
        print()

    def diff(self):
        """
        Test output reproductibility [OPTIONAL]
        """
        simulation2d = self.conf.get("pgd_2d", False)
        if simulation2d:
            block = 'pgd2d/pgd'
        else:
            block = 'pgd'
        self.sh.title("Reproductibility check : PGD")
        diff = vortex.diff(
            local      = 'PGD.nc',
            role       = 'SurfexClim',
            experiment = self.conf.diff_xpid,
            username   = self.conf.get('diff_user', None),
            geometry   = self.conf.geometry,
            nativefmt  = 'netcdf',
            kind       = 'pgdnc',
            model      = 'surfex',
            namespace  = 'vortex.multi.fr',
            namebuild  = 'flat@cen',
            block      = block,
        ),
        print(self.ticket.prompt, 'diff =', diff)
        print()


class MakePgd(_PgdConstruct):
    """
    Task : MakePgd
    ==============

    Generation of ground physiography (PGD.nc file).
    Get PGD executable from Uenv

    Inputs:
    -------
    - FORCING file
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD executable

    Outputs:
    --------
    - PGD.nc (Ground physiography)

    Mandatory configuration variables:
    ----------------------------------

    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
      type: str
    * ``surfex_unev`` or ``uenv`` User Environment in which the following resources are to be retrieved :
                 - ecoclimapI_covers_param.bin
                 - ecoclimapII_eu_covers_param.bin
                 - drdt_bst_fit_60.nc
                 - PGD executable (unless supplied locally using the ``exesurfex`` variable).
                 Format : uenv:{uenv_name}@{user}
      type: str

    Optionnal configuration variables (other than forcing-specific ones):
    ---------------------------------------------------------------------
    * ``pgd_2d`` *True* if the PGD.nc should be calculated for a 2D domain. Default: *False*
      type: bool
    * ``exesurfex`` path to the folder with surfex executables (if supplied locally and not via the uenv).
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary. Default: 1
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary. Default: 1
      type: int
    * ``nodes`` Number of nodes to allocate to the execution of the MPI binary. Default: 1
      type: int
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str
     * ``diff_xpid`` Experiment id of the reference file used for reproducibility test.
      type diff_xpid: str
    * ``diff_user`` *user name* associated with the reference file used for reproducibility test
      (only if different from current user). Default: *None*
      type diff_user: str

    Forcing related configuration variables:
    ----------------------------------------

     Mandatory:
     **********

    * ``forcing_datebegin`` *datebegin* footprint, default self.conf.datebegin
      type forcing_datebegin: str, footprints.stdtypes.FPList
    * ``forcing_dateend`` *dateend* footprint, default self.conf.dateend
      type forcing_dateend: str, footprints.stdtypes.FPList
    * ``forcing_xpid`` Experiment identifier, default self.conf.xpid
      type forcing_xpid: str
    * ``forcing_geometry`` *geometry* footprint, default self.conf.geometry
      type forcing_geometry: str, footprints.stdtypes.FPList
    * ``forcing_vapp`` *vapp* footprint, default self.conf.vapp
      type forcing_vapp: str
    * ``forcing_vconf`` *vconf* footprint, default self.conf.vconf
      type forcing_vconf: str
    * ``forcing_block`` *block* footprint, default "meteo"
      type forcing_vconf: str
    * ``forcing_namespace`` *namespace* footprint, default "vortex.multi.fr" (hendrix + local cache)
      type forcing_namespace: str

    * ``forcing_date`` *date* footprint (unsed with the research namebuilders), default to [dateend]
      type forcing_date: str
    * ``forcing_model`` *model* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_model: str

    Optional:
    *********

    * ``forcing_member`` *member* footprint, default None (or *member* if provided)
      type forcing_member: int, footprints.stdtypes.FPList
    * ``forcing_namebuild`` *namebuild* footprint, default "flat@cen" (will change soon)
      type forcing_namebuild: str
    * ``forcing_intent`` *intent* footprint (local file permissions), default "in"
                           Possible values : "in" (read-only), "inout" (read-write)
      type forcing_intent: str
    * ``forcing_source_app`` *source_app* footprint, default None
      type forcing_source_app: str, footprints.stdtypes.FPList
    * ``forcing_source_conf`` *source_conf* footprint, default None
      type forcing_source_conf: str, footprints.stdtypes.FPList
    * ``forcing_source`` Retrieve *source_app* and *source_conf* footrprints dictionnaries for S2M reanalysis
                           Possible values : 'era5', 'era40'
      type forcing_source: str
    * ``forcing_cutoff`` *cutoff* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_cutoff: str
    * ``io_duration`` Argument similar to the one of the `get_list_dates_files` method in
                        snowtools/utils/dates.py.
                        Used to retrieve the list of *datebegin* and *dateend* for inputs covering sub-periods.
                        Possible values : "yearly", "monthly" or "full"
      type io_duration: str
    * ``forcing_vortex1`` Boolean to identify resources produced with vortex1 (filename without geometry)
      type forcing_vortex1: bool
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "uenv|surfex_uenv",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "exesurfex"
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_pgd_executable(self):
        """
        get PREP executable either from local path or from a UEnv
        """
        if hasattr(self.conf, 'exesurfex'):
            self.get_pgd_exe_from_local_path()
        else:
            self.get_pgd_exe_from_uenv()


class FetchPgdOrMake(_PgdConstruct):
    """
    Task : GetPgdOrMake
    ===================

    Generation of ground physiography (PGD.nc file).
    If PGD.nc is available in cache or archive for the current experiment fetch it.
    If not, try to get it from an uenv.
    If not either, generate it by calling the methods from the mother class.

    WARNING : The simulation's reproducibility can not be guaranteed with this task !
            The PGD.nc file is only put to the vortex cache and not archived!

    Inputs:
    -------
    - FORCING file
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD executable

    Outputs:
    --------
    - PGD.nc (Ground physiography)

    Mandatory configuration variables:
    ----------------------------------

    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
      type: str
    * ``surfex_unev`` or ``uenv`` User Environment in which the following resources are to be retrieved :
                 - ecoclimapI_covers_param.bin
                 - ecoclimapII_eu_covers_param.bin
                 - drdt_bst_fit_60.nc
                 - PGD executable (unless supplied locally using the ``exesurfex`` variable).
                 Format : uenv:{uenv_name}@{user}
      type: str


    Optional configuration variables (other than forcing-specific ones):
    ---------------------------------------------------------------------
    * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID. defaults to ``xpid``.
          type: str
    * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*. defaults to ``vapp``.
          type: str
    * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*. defaults to ``vconf``.
          type: str
    * ``pgd_vortex1`` True if the pgd file was produced with vortex1 and uses vortex1 naming conventions.
          default: False
          type: bool
    * ``pgd_user`` Name of the user who produced the PGD file. Default: ``None``.
          type: str
    * ``pgd_geometry`` *geometry* of the pgd file. Default: ``geometry``.
          type: str, footprints.stdtypes.FPList
    * ``pgdnc_gvar`` variable name of the pgd file in the uenv. Default: *pgd_[geometry::tag]*.
    * ``pgd_2d`` *True* if the PGD.nc should be calculated for a 2D domain. Default: *False*
      type: bool
    * ``exesurfex`` path to the folder with surfex executables (if supplied locally and not via the uenv).
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary. Default: 1
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary. Default: 1
      type: int
    * ``nodes`` Number of nodes to allocate to the execution of the MPI binary. Default: 1
      type: int
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str
     * ``diff_xpid`` Experiment id of the reference file used for reproducibility test.
      type diff_xpid: str
    * ``diff_user`` *user name* associated with the reference file used for reproducibility test
      (only if different from current user). Default: *None*
      type diff_user: str

    Forcing related configuration variables:
    ----------------------------------------

     Mandatory:
     **********

    * ``forcing_datebegin`` *datebegin* footprint, default self.conf.datebegin
      type forcing_datebegin: str, footprints.stdtypes.FPList
    * ``forcing_dateend`` *dateend* footprint, default self.conf.dateend
      type forcing_dateend: str, footprints.stdtypes.FPList
    * ``forcing_xpid`` Experiment identifier, default self.conf.xpid
      type forcing_xpid: str
    * ``forcing_geometry`` *geometry* footprint, default self.conf.geometry
      type forcing_geometry: str, footprints.stdtypes.FPList
    * ``forcing_vapp`` *vapp* footprint, default self.conf.vapp
      type forcing_vapp: str
    * ``forcing_vconf`` *vconf* footprint, default self.conf.vconf
      type forcing_vconf: str
    * ``forcing_block`` *block* footprint, default "meteo"
      type forcing_vconf: str
    * ``forcing_namespace`` *namespace* footprint, default "vortex.multi.fr" (hendrix + local cache)
      type forcing_namespace: str

    * ``forcing_date`` *date* footprint (unsed with the research namebuilders), default to [dateend]
      type forcing_date: str
    * ``forcing_model`` *model* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_model: str

    Optional:
    *********

    * ``forcing_member`` *member* footprint, default None (or *member* if provided)
      type forcing_member: int, footprints.stdtypes.FPList
    * ``forcing_namebuild`` *namebuild* footprint, default "flat@cen" (will change soon)
      type forcing_namebuild: str
    * ``forcing_intent`` *intent* footprint (local file permissions), default "in"
                           Possible values : "in" (read-only), "inout" (read-write)
      type forcing_intent: str
    * ``forcing_source_app`` *source_app* footprint, default None
      type forcing_source_app: str, footprints.stdtypes.FPList
    * ``forcing_source_conf`` *source_conf* footprint, default None
      type forcing_source_conf: str, footprints.stdtypes.FPList
    * ``forcing_source`` Retrieve *source_app* and *source_conf* footrprints dictionnaries for S2M reanalysis
                           Possible values : 'era5', 'era40'
      type forcing_source: str
    * ``forcing_cutoff`` *cutoff* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_cutoff: str
    * ``io_duration`` Argument similar to the one of the `get_list_dates_files` method in
                        snowtools/utils/dates.py.
                        Used to retrieve the list of *datebegin* and *dateend* for inputs covering sub-periods.
                        Possible values : "yearly", "monthly" or "full"
      type io_duration: str
    * ``forcing_vortex1`` Boolean to identify resources produced with vortex1 (filename without geometry)
      type forcing_vortex1: bool
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "exesurfex",
            "pgdnc_gvar",
            "pgd_cache",
            "pgd_2d",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def pgd_avail(self):
        """
        Try to get PGD.nc from cache or archive. If not available try to get PGD.nc from uenv.

        :return: True if the PGD.nc file can be fetched from the uenv, cache or archive, False otherwise.
        :rtype: bool
        """
        pgd = self.get_pgd_file_from_cache_or_archive(fatal=False)

        # try to get PGD.nc from uenv
        if not pgd[0]:
            _ = self.get_pgd_file_from_uenv(fatal=False)

        if len(self.ctx.sequence.effective_inputs(role="SurfexClim")) == 0:
            return False
        else:
            return True

    def get_pgd_executable(self):
        """
        get PGD executable from uenv or local path
        """
        if hasattr(self.conf, 'exesurfex'):
            self.get_pgd_exe_from_local_path()
        else:
            self.get_pgd_exe_from_uenv()

    def get_remote_inputs(self):

        input_pgd = self.pgd_avail()
        if not input_pgd:
            super().get_remote_inputs()

    def get_local_inputs(self):
        if len(self.ctx.sequence.effective_inputs(role="SurfexClim")) > 0:
            pass
        else:
            super().get_local_inputs()

    def algo(self):
        if len(self.ctx.sequence.effective_inputs(role="SurfexClim")) > 0:
            pass
        else:
            myalgo = super().algo()
            return myalgo

    def launch_algo(self, algo):
        if len(self.ctx.sequence.effective_inputs(role="SurfexClim")) > 0:
            pass
        else:
            super().launch_algo(algo)

    def put_outputs(self):
        self.sh.title('Toolbox Output PGD')
        pgd_tbo = vortex.output(
            local       = 'PGD.nc',
            role        = 'SurfexClim',
            experiment  = self.conf.xpid,
            geometry    = self.conf.geometry,
            nativefmt   = 'netcdf',
            kind        = 'pgdnc',
            model       = 'surfex',
            namespace   = 'vortex.cache.fr',
            namebuild   = 'flat@cen',  # TODO : passer en variable de configuration
            block       = 'pgd',
        ),
        print(self.ticket.prompt, 'pgd_tbo =', pgd_tbo)
        print()


class FetchPgdOrCrash(FetchPgdOrMake):
    """
    Get a PGD.nc file from an uenv or vortex cache/archive. And put it in the cache of the current experiment.
    Crash if the file does not exist. The ``force_uenv`` configuration variable allows to look for the
    PGD.nc file exclusively in the uenv.

    Outputs:
    --------
    - PGD.nc (Ground physiography)

    Mandatory configuration variables:
    ----------------------------------
    * ``surfex_uenv`` or if not present ``uenv`` User Environment from which the PGD.nc file should be fetched.
                 Format : uenv:{uenv_name}@{user}
    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier

    Mandatory configuration variables unless ``force_uenv`` is *True*:
    ------------------------------------------------------------------
    * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID. defaults to ``xpid``.
          type: str
    * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*. defaults to ``vapp``.
          type: str
    * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*. defaults to ``vconf``.
          type: str
    * ``pgd_vortex1`` True if the pgd file was produced with vortex1 and uses vortex1 naming conventions.
          default: False
          type: bool
    * ``pgd_user`` Name of the user who produced the PGD file. Default: ``None``.
          type: str

    Optional configuration variables:
    ---------------------------------
    * ``force_uenv`` If *True* the PGD.nc file must come from an uenv. Default: *False*
    * ``pgdnc_gvar`` variable name of the pgd file in the uenv. Default: *pgd_[geometry::tag]*.
    * ``pgd_geometry`` *geometry* of the pgd file. Default: ``geometry``.
          type: str, footprints.stdtypes.FPList
    """
    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "force_uenv",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        force_uenv = self.conf.get("force_uenv", False)
        pgd = self.get_pgd_file_from_uenv(fatal=force_uenv)
        if not pgd[0]:
            _ = self.get_pgd_file_from_cache_or_archive(fatal=True)

    def get_local_inputs(self):
        pass

    def algo(self):
        pass

    def launch_algo(self, algo, **kwargs):
        pass

