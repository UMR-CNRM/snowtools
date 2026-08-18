# -*- coding: utf-8 -*-
"""
offline_ensemble.py
-------------------

Tasks designed to launch an OFFLINE executable WITHOUT MPI parallelisation
several time in parallel.

.. inheritance-diagram:: vortex_cen.tasks.surfex.offline_ensemble
   :top-classes: vortex_cen.tasks.research_task_base._CenResearchTask
   :private-bases:
   :parts: 2

.. autoclass:: Escroc
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: CrocO
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: EscrocResearch
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""

import vortex
from bronx.stdtypes.date import Date
from vortex_cen.tasks.surfex.offline import _Offline


class Escroc(_Offline):
    """
    **Task : Escroc**

    Multiple executions of an OFFLINE binary with a single meteorological FORCING but
    different Crocus physics (namelists) and no MPI parallelization.

    Lafaysse et al. (2017) : https://tc.copernicus.org/articles/11/1173/2017/

    **Inputs:**

    - FORCING.nc files(s) (near-surface meteorological conditions during the simulation period)
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD.nc (Ground physiography)
    - PREP.nc (initial conditions)

    **Outputs:**

    - PRO.nc Snowpack simulations covering the entire simulation period
    - PREP.nc SURFEX/Crocus model state variables at the end of the simulation
    - CUMUL.nc TODO   Compléter et CHECKER la doc
    - DIAG.nc TODO    Compléter et CHECKER la doc

    **Mandatory configuration variables:**

    * ``datebegin`` *datebegin* of the forcing file(s). type: str, footprints.stdtypes.FPList
    * ``dateend`` *dateend* of the forcing files(s). type: str, footprints.stdtypes.FPList
    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` User-defined Experiment identifier (WARNING : 4-digit strings prohibited)
      type: str
    * ``surfex_uenv`` or ``uenv`` User Environment in which the following resources are to be retrieved:
        - ecoclimapI_covers_param.bin
        - ecoclimapII_eu_covers_param.bin
        - drdt_bst_fit_60.nc
        - OFFLINE executable

      Format : uenv:{uenv_name}@{user}
      type: str
    * ``nmembers`` number of ensemble members.

    **Optional configuration variables (other than forcing-specific ones):**

    * ``exesurfex`` Path to the executable if it should come from a local path.
    * ``offline_gvar`` specify the name of the offline executable in the uenv. Default is ``master_offline_mpi``
      if the mpi parameter is True and ``master_offline_nompi`` otherwise.
    * ``member`` Simulation member.
      NB : This is a deterministic task, only one single member value can be provided
      type: int
    * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID
      type: str
    * ``pgd_user`` User who produced the target PGD file.
      type: str
    * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*
      type: str
    * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*
      type: str
    * ``prep_xpid`` Experiment Identifier of the PREP file, if different from the task's XPID
      type: str
    * ``prep_user`` User who produced the target PREP file.
      type: str
    * ``prep_member`` Member associated to the PREP file if it comes from an ensemble (after a SODA run)
      NB : This is a deterministic task, only one single member value can be provided
      type: int
    * ``prep_vapp`` *vapp* of the PREP file, if different from the task's *vapp*
      type: str
    * ``prep_vconf`` *vconf* of the PREP file, if different from the task's *vconf*
      type: str
    * ``prep_date`` Validity date of the PREP file (if different from *datebegin*)
      type: str
    * ``prep_block`` *block* of the PREP file (default 'prep', but can be different after an assimilation step)
      type: str
    * ``prep_vortex1`` Boolean to identify resources produced with vortex1 (filename without geometry)
      type: bool
    * ``august_threshold`` Threshold to apply to the snow water equivalent (in kg/m2) each 1st August (default: -999)
      type: int
    * ``dailyprep`` TODO :comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
      type: bool
    * ``drhook`` Activate / deactivate the profiling with DRHOOK (default: False)
      type: bool
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str
    * ``nnodes`` Number of available nodes for MPI parallelisation
      type: int
    * ``nprocs`` Number of available processors for MPI parallelisation
      type: int
    * ``ntasks`` Number of MPI tasks
      type: int
    * ``io_duration`` Argument similar to the one of the `get_list_dates_files` method in
      snowtools/utils/dates.py.
      Used to retrieve the list of *datebegin* and *dateend* for IO covering sub-periods.
      Possible values : "yearly", "monthly" or "full"
      type: str
    * ``diff_xpid`` Experiment id of the reference file used for reproducibility test.
      type diff_xpid: str
    * ``diff_user`` *user name* associated with the reference file used for reproducibility test
      (only if different from current user). Default: *None*
      type diff_user: str
    * ``diff_block`` *block* of the reference file used for reproducibility test
    * ``subensemble`` Name of the predefined escroc sub-ensemble to use. typ: str, default: "E2"
    * ``output_storage`` Name of the archive/server where the output files will be stored. type: str

    **Forcing related configuration variables:**

    **Mandatory**

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

    **Optional**

    * ``forcing_member`` *member* footprint, default None (or *member* if provided)
      type forcing_member: int, footprints.stdtypes.FPList
    * ``forcing_namebuild`` *namebuild* footprint, default "flat@cen" (will change soon)
      type forcing_namebuild: str
    * ``forcing_intent`` *intent* footprint (local file permissions), default "in"
      Possible values: "in" (read-only), "inout" (read-write)
      type forcing_intent: str
    * ``forcing_source_app`` *source_app* footprint, default None
      type forcing_source_app: str, footprints.stdtypes.FPList
    * ``forcing_source_conf`` *source_conf* footprint, default None
      type forcing_source_conf: str, footprints.stdtypes.FPList
    * ``forcing_source`` Retrieve *source_app* and *source_conf* footrprints dictionnaries for S2M reanalysis
      Possible values: 'era5', 'era40'
      type forcing_source: str
    * ``forcing_cutoff`` *cutoff* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_cutoff: str
    * ``io_duration`` Argument similar to the one of the `get_list_dates_files` method in
      snowtools/utils/dates.py.
      Used to retrieve the list of *datebegin* and *dateend* for inputs covering sub-periods.
      Possible values: "yearly", "monthly" or "full"
      type io_duration: str
    * ``forcing_vortex1`` Boolean to identify resources produced with vortex1 (filename without geometry)
      type forcing_vortex1: bool

    """

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = [
            "nmembers",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "drhook",
            "august_threshold",
            "ntasks",
            "nnodes",
            "nprocs",
            "subensemble+help=Name of the predefined escroc sub-ensemble to use;type=str;default=E2",
            "output_storage+help=Name of the archive / server where the output files will be stored;type=str",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_executable(self):
        self.get_executable_from_uenv(mpi=False)

    def algo(self):
        """
        Algo component to execute OFFLINE several time in parallel with different namelists.
        """

        ctx = self.ticket.context

        self.sh.title('Algo OFFLINE-ESCROC')
        algo = vortex.task(
            kind           = "escroc",
            engine         = 'blind',
            # binary         = 'OFFLINE',  # unused
            verbose        = True,
            # MV TODO : gérer la conversion en Date dans l'algo
            datebegin      = Date(self.conf.datebegin),
            dateend        = Date(self.conf.dateend),
            dateinit       = ctx.sequence.effective_inputs(role="SnowpackInit")[0].rh.resource.datevalidity,
            # MV TODO :  La valeur par défaut de "threshold" est à sortir de la tâche
            threshold      = self.conf.get('august_threshold', -999),
            members        = self.get_list_members(),
            geometry_in    = [self.conf.geometry.tag],
            geometry_out   = self.conf.geometry.tag,
            # MV TODO : La valeur par défaut de "subensemble" est à sortir de la tâche
            subensemble    = self.conf.get('subensemble', 'E2'),
            ntasks         = self.conf.get('ntasks', self.conf.nmembers),
            reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, 'Algo =', algo)
        print()
        return algo

    def launch_algo(self, algo, **kw):
        """
        Run OFFLINE algo component without MPI parallelisation.
        """
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        self.component_runner(algo, executable)

    def put_pro(self):

        self.sh.title('Output PRO')
        pro = vortex.output(
            local          = 'mb[member%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datebegin      = self.list_dates_begin_pro,
            dateend        = self.dict_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            # TODO : le storage de sortie devrait être traité à plus haut niveau, en créant une variable de conf
            # dans la methode "defaults" 'research_task_base'
            storage        = self.conf.get('output_storage', None),
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'pro',
            model          = 'surfex',
            member         = self.get_list_members(),
        ),
        print(self.ticket.prompt, 'pro =', pro)
        print()

    def put_prep(self):

        self.sh.title('Output PREP')
        prep_tbo = vortex.output(
            local          = 'mb[member%04d]/PREP_[datevalidity:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datevalidity   = self.list_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'prep',
            model          = 'surfex',
            member         = self.get_list_members(),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()

    def diff(self):
        pass


class CrocO(Escroc):
    """
    **Task : CrocO**

    Multiple executions of an OFFLINE binary with an ensemble of FORCING files
    and potentially different Crocus physics (namelists).

    This task is strongly linked to the croco driver (vortex_cen/Crocus/assim/drivers/croco.py).

    In particular, some configuration variables derived from the list of assimilation dates
    (configuration variable "assimdates" provided in the configuration file by the user) come
    from the internal loop within the croco driver :
    * ``assimdate_prev`` refers to the assimilation date of the last interation
    * ``assimdate`` (singular !) refers to the assimilation date of the current iteration
    * ``assimdate_next`` refers to the assimilation date of the next iteration

    **Mandatory configuration variables:**

    * ``datebegin`` *datebegin* of the forcing file(s). type: str, footprints.stdtypes.FPList
    * ``dateend`` *dateend* of the forcing files(s). type: str, footprints.stdtypes.FPList
    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` User-defined Experiment identifier (WARNING : 4-digit strings prohibited)
      type: str
    * ``surfex_uenv`` or ``uenv`` User Environment in which the following resources are to be retrieved:
        - ecoclimapI_covers_param.bin
        - ecoclimapII_eu_covers_param.bin
        - drdt_bst_fit_60.nc
        - OFFLINE executable

      Format : uenv:{uenv_name}@{user}
      type: str
    * ``nmembers`` number of ensemble members.

    **Optional configuration variables (other than forcing-specific ones):**

    * ``exesurfex`` Path to the executable if it should come from a local path.
    * ``offline_gvar`` specify the name of the offline executable in the uenv. Default is ``master_offline_mpi``
      if the mpi parameter is True and ``master_offline_nompi`` otherwise.
    * ``member`` Simulation member.
      NB : This is a deterministic task, only one single member value can be provided
      type: int
    * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID
      type: str
    * ``pgd_user`` User who produced the target PGD file.
      type: str
    * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*
      type: str
    * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*
      type: str
    * ``prep_xpid`` Experiment Identifier of the PREP file, if different from the task's XPID
      type: str
    * ``prep_user`` User who produced the target PREP file.
      type: str
    * ``prep_member`` Member associated to the PREP file if it comes from an ensemble (after a SODA run)
      NB : This is a deterministic task, only one single member value can be provided
      type: int
    * ``prep_vapp`` *vapp* of the PREP file, if different from the task's *vapp*
      type: str
    * ``prep_vconf`` *vconf* of the PREP file, if different from the task's *vconf*
      type: str
    * ``prep_date`` Validity date of the PREP file (if different from *datebegin*)
      type: str
    * ``prep_block`` *block* of the PREP file (default 'prep', but can be different after an assimilation step)
      type: str
    * ``prep_vortex1`` Boolean to identify resources produced with vortex1 (filename without geometry)
      type: bool
    * ``august_threshold`` Threshold to apply to the snow water equivalent (in kg/m2) each 1st August (default: -999)
      type: int
    * ``dailyprep`` TODO :comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
      type: bool
    * ``drhook`` Activate / deactivate the profiling with DRHOOK (default: False)
      type: bool
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str
    * ``nnodes`` Number of available nodes for MPI parallelisation
      type: int
    * ``nprocs`` Number of available processors for MPI parallelisation
      type: int
    * ``ntasks`` Number of MPI tasks
      type: int
    * ``io_duration`` Argument similar to the one of the `get_list_dates_files` method in
      snowtools/utils/dates.py.
      Used to retrieve the list of *datebegin* and *dateend* for IO covering sub-periods.
      Possible values : "yearly", "monthly" or "full"
      type: str
    * ``diff_xpid`` Experiment id of the reference file used for reproducibility test.
      type diff_xpid: str
    * ``diff_user`` *user name* associated with the reference file used for reproducibility test
      (only if different from current user). Default: *None*
      type diff_user: str
    * ``diff_block`` *block* of the reference file used for reproducibility test
    * ``subensemble`` Name of the predefined escroc sub-ensemble to use. typ: str, default: "E2"
    * ``output_storage`` Name of the archive/server where the output files will be stored. type: str

    **Forcing related configuration variables:**

    **Mandatory**

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

    **Optional**

    * ``forcing_member`` *member* footprint, default None (or *member* if provided)
      type forcing_member: int, footprints.stdtypes.FPList
    * ``forcing_namebuild`` *namebuild* footprint, default "flat@cen" (will change soon)
      type forcing_namebuild: str
    * ``forcing_intent`` *intent* footprint (local file permissions), default "in"
      Possible values: "in" (read-only), "inout" (read-write)
      type forcing_intent: str
    * ``forcing_source_app`` *source_app* footprint, default None
      type forcing_source_app: str, footprints.stdtypes.FPList
    * ``forcing_source_conf`` *source_conf* footprint, default None
      type forcing_source_conf: str, footprints.stdtypes.FPList
    * ``forcing_source`` Retrieve *source_app* and *source_conf* footrprints dictionnaries for S2M reanalysis
      Possible values: 'era5', 'era40'
      type forcing_source: str
    * ``forcing_cutoff`` *cutoff* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_cutoff: str
    * ``io_duration`` Argument similar to the one of the `get_list_dates_files` method in
      snowtools/utils/dates.py.
      Used to retrieve the list of *datebegin* and *dateend* for inputs covering sub-periods.
      Possible values: "yearly", "monthly" or "full"
      type io_duration: str
    * ``forcing_vortex1`` Boolean to identify resources produced with vortex1 (filename without geometry)
      type forcing_vortex1: bool

    """

    def get_prep_file_from_cache_or_archive(self, fatal=True, cache_only=False, local="PREP.nc"):
        """
        The input PREP depends on the iteration in the crocO task:
        * the first iteration fethes the single PREP file as defined by the user in the configuration file
        * the next iteration are the output of a SODA execution
        """
        if self.conf.assimdate_prev is None:
            # First iteration
            super().get_prep_file_from_cache_or_archive()
        else:
            # Get a SODA analysis
            prep = vortex.input(
                role           = 'Analysis',
                local          = local,
                experiment     = self.conf.xpid,
                datevalidity   = self.conf.assimdate,
                vapp           = self.conf.vapp,
                vconf          = self.conf.vconf,
                geometry       = self.conf.geometry,
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.cache.fr',
                namebuild      = 'flat@cen',
                block          = 'analysis',
                member         = self.get_list_members(),
                intent         = 'inout',
                fatal          = True,
            ),
            print(self.ticket.prompt, 'Analysis =', prep)
            print()

    def get_executable(self):
        self.get_executable_from_uenv(mpi=True)

    def algo(self):
        """
        Algo component to execute OFFLINE several times in parallel
        """

        ctx = self.ticket.context

        # TODO (MV) : Clarifier la distinction entre les algos "escroc" (multi-physiue uniquement) et "croco"
        # (ensemble météo + multiphysique optionelle).
        # --> Faire des algos distincts
        self.sh.title('Algo Offline-CrocO')
        croco_tba = vortex.task(
            engine         = 'blind',
            kind           = "croco",
            # binary         = 'OFFLINE',  # unused
            verbose        = True,
            # MV TODO : gérer la conversion en Date dans l'algo
            datebegin      = Date(self.conf.assimdate_prev or self.conf.datebegin),
            dateend        = Date(self.conf.assimdate or self.conf.dateend),
            dateinit       = ctx.sequence.effective_inputs(role="SnowpackInit")[0].rh.resource.datevalidity,
            # MV TODO :  La valeur par défaut de "threshold" est à sortir de la tâche
            threshold      = self.conf.get('august_threshold', -999),
            members        = self.get_list_members(),
            geometry_in    = [self.conf.geometry.tag],
            geometry_out   = self.conf.geometry.tag,
            # MV TODO : La valeur par défaut de "subensemble" est à sortir de la tâche
            subensemble    = self.conf.get('subensemble', 'E2'),
            ntasks         = self.conf.get('ntasks', self.conf.nmembers),
            # MV : "nforcing" n'est pas un footprint de l'algo !
            # nforcing       = self.conf.nforcing,
            reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, 'Algo =', croco_tba)
        print()
        return croco_tba

    def put_pro(self):

        # TODO : Gérer les différentes sous-période avec la loopvariable "dateassim"

        self.sh.title('Output PRO')
        pro = vortex.output(
            local          = 'mb[member%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datebegin      = self.conf.assimdate_prev or self.conf.datebegin,
            dateend        = self.conf.assimdate or self.conf.dateend,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            # TODO : le storage de sortie devrait être traité à plus haut niveau, en créant une variable de conf
            # dans la methode "defaults" 'research_task_base'
            storage        = self.conf.get('output_storage', None),
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'pro',
            model          = 'surfex',
            member         = self.get_list_members(),
        ),
        print(self.ticket.prompt, 'pro =', pro)
        print()

    def put_prep(self):

        self.sh.title('Output PREP')
        prep_tbo = vortex.output(
            local          = 'mb[member%04d]/PREP_[datevalidity:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datevalidity   = self.conf.assimdate or self.conf.dateend,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'background',
            model          = 'surfex',
            member         = self.get_list_members(),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()


class EscrocResearch(Escroc):
    """
    **Task: EscrocResearch**

    SURFEX/OFFLINE documentation : https://umr-cnrm.github.io/snowtools-doc/misc/surfex.html

    **Inputs:**

    - FORCING.nc files(s) (near-surface meteorological conditions during the simulation period)
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from the execution of the "PreProcess")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD.nc (Ground physiography) retrieved or produced by the GetPgd1D task
    - PREP.nc (initial conditions) retrieved or produced by the GetPrep task

    **Outputs:**

    - PRO.nc Snowpack simulations covering the entire simulation period
    - PREP.nc SURFEX/Crocus model state variables at the end of the simulation

    **Mandatory configuration variables:**

    * ``datebegin`` *datebegin* of the forcing file(s). type: str, footprints.stdtypes.FPList
    * ``dateend`` *dateend* of the forcing files(s). type: str, footprints.stdtypes.FPList
    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` User-defined Experiment identifier (WARNING : 4-digit strings prohibited)
      type: str
    * ``surfex_uenv`` or ``uenv`` User Environment in which the following resources are to be retrieved:
        - ecoclimapI_covers_param.bin
        - ecoclimapII_eu_covers_param.bin
        - drdt_bst_fit_60.nc
        - OFFLINE executable

      Format : uenv:{uenv_name}@{user}
      type: str
    * ``nmembers`` number of ensemble members.

    **Optional configuration variables (other than forcing-specific ones):**

    * ``exesurfex`` Path to the executable if it should come from a local path.
    * ``offline_gvar`` specify the name of the offline executable in the uenv. Default is ``master_offline_mpi``
      if the mpi parameter is True and ``master_offline_nompi`` otherwise.
    * ``member`` Simulation member.
      NB : This is a deterministic task, only one single member value can be provided
      type: int
    * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID
      type: str
    * ``pgd_user`` User who produced the target PGD file.
      type: str
    * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*
      type: str
    * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*
      type: str
    * ``prep_xpid`` Experiment Identifier of the PREP file, if different from the task's XPID
      type: str
    * ``prep_user`` User who produced the target PREP file.
      type: str
    * ``prep_member`` Member associated to the PREP file if it comes from an ensemble (after a SODA run)
      NB : This is a deterministic task, only one single member value can be provided
      type: int
    * ``prep_vapp`` *vapp* of the PREP file, if different from the task's *vapp*
      type: str
    * ``prep_vconf`` *vconf* of the PREP file, if different from the task's *vconf*
      type: str
    * ``prep_date`` Validity date of the PREP file (if different from *datebegin*)
      type: str
    * ``prep_block`` *block* of the PREP file (default 'prep', but can be different after an assimilation step)
      type: str
    * ``prep_vortex1`` Boolean to identify resources produced with vortex1 (filename without geometry)
      type: bool
    * ``august_threshold`` Threshold to apply to the snow water equivalent (in kg/m2) each 1st August (default: -999)
      type: int
    * ``dailyprep`` TODO :comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
      type: bool
    * ``drhook`` Activate / deactivate the profiling with DRHOOK (default: False)
      type: bool
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str
    * ``nnodes`` Number of available nodes for MPI parallelisation
      type: int
    * ``nprocs`` Number of available processors for MPI parallelisation
      type: int
    * ``ntasks`` Number of MPI tasks
      type: int
    * ``io_duration`` Argument similar to the one of the `get_list_dates_files` method in
      snowtools/utils/dates.py.
      Used to retrieve the list of *datebegin* and *dateend* for IO covering sub-periods.
      Possible values : "yearly", "monthly" or "full"
      type: str
    * ``diff_xpid`` Experiment id of the reference file used for reproducibility test.
      type diff_xpid: str
    * ``diff_user`` *user name* associated with the reference file used for reproducibility test
      (only if different from current user). Default: *None*
      type diff_user: str
    * ``diff_block`` *block* of the reference file used for reproducibility test
    * ``subensemble`` Name of the predefined escroc sub-ensemble to use. typ: str, default: "E2"
    * ``output_storage`` Name of the archive/server where the output files will be stored. type: str

    **Forcing related configuration variables:**

    **Mandatory**

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

    **Optional**

    * ``forcing_member`` *member* footprint, default None (or *member* if provided)
      type forcing_member: int, footprints.stdtypes.FPList
    * ``forcing_namebuild`` *namebuild* footprint, default "flat@cen" (will change soon)
      type forcing_namebuild: str
    * ``forcing_intent`` *intent* footprint (local file permissions), default "in"
      Possible values: "in" (read-only), "inout" (read-write)
      type forcing_intent: str
    * ``forcing_source_app`` *source_app* footprint, default None
      type forcing_source_app: str, footprints.stdtypes.FPList
    * ``forcing_source_conf`` *source_conf* footprint, default None
      type forcing_source_conf: str, footprints.stdtypes.FPList
    * ``forcing_source`` Retrieve *source_app* and *source_conf* footrprints dictionnaries for S2M reanalysis
      Possible values: 'era5', 'era40'
      type forcing_source: str
    * ``forcing_cutoff`` *cutoff* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_cutoff: str
    * ``io_duration`` Argument similar to the one of the `get_list_dates_files` method in
      snowtools/utils/dates.py.
      Used to retrieve the list of *datebegin* and *dateend* for inputs covering sub-periods.
      Possible values: "yearly", "monthly" or "full"
      type io_duration: str
    * ``forcing_vortex1`` Boolean to identify resources produced with vortex1 (filename without geometry)
      type forcing_vortex1: bool

    """

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = [
            "surfex_uenv|uenv",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "exesurfex",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_executable()

    def get_local_inputs(self):
        # Get PGD and PREP locally because they have been retrieved or produced by a previous task
        self.get_pgd_from_cache()
        self.get_prep_file_from_cache_or_archive(fatal=True, cache_only=True)
        # Get namelist from the preprocess task output
        self.get_namelist_from_cache()
        # Get FORCING locally because they have already been retrieved by the preprocess task
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def get_executable(self):

        if "exesurfex" in self.conf:
            self.get_executable_from_path()
        else:
            self.get_executable_from_uenv()
