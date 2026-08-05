# -*- coding: utf-8 -*-
"""
offline.py
----------

Tasks designed to launch the OFFLINE executable with MPI parallelisation.

.. inheritance-diagram:: vortex_cen.tasks.surfex.offline
   :top-classes: vortex_cen.tasks.research_task_base._CenResearchTask
   :private-bases:
   :parts: 2

.. autoclass:: OfflineCommonsMixin
   :members:
   :show-inheritance:

.. autoclass:: _Offline
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: OfflineMpi
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: _Offline_NOMPI
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: Offline_Mpi_Uenv
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: OfflineMpiDailyPrep
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: OfflineAssim
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: OfflineOpenloop
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: OfflineLocalForcing
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""

import vortex
from bronx.stdtypes.date import daterange, tomorrow
from vortex.layout.dataflow import SectionFatalError

from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin


class OfflineCommonsMixin(SurfexCommonsMixin):
    """

    Common OFFLINE-specific IO resources.
    """

    def get_executable_from_uenv(self, mpi=True, fatal=True):
        """
        Get OFFLINE executable from Uenv

        :param mpi: If True, fetch offline executable with MPI support named *master_offline_mpi*,
          if False, fetch offline executable without MPI support named *master_offline_nompi*.
          The names can be overwritten using the ``offline_gvar`` configuration variable. Default: True
        :type mpi: bool
        :param fatal: If True, fails if the executable was not found. Default: True
        :type fatal: bool

        **Configuration variables used:**

        * ``surfex_uenv`` or ``uenv``
        * ``offline_gvar`` or one of *master_offline_mpi* or *master_offline_nompi* depending on the value of the
          mpi parameter.
        """
        if mpi:
            default_gvar = "master_offline_mpi"
        else:
            default_gvar = "master_offline_nompi"

        self.sh.title("Input OFFLINE executable from uenv")
        self.offline_exe = vortex.executable(
            role="offline",
            kind="offline",
            local="OFFLINE",
            model="surfex",
            genv=self.conf.get("surfex_uenv", self.conf.uenv),
            gvar=self.conf.get("offline_gvar", default_gvar),
            fatal=fatal,
        )
        print(self.ticket.prompt, "OFFLINE_tbx =", self.offline_exe)
        print()

    def get_executable_from_path(self, fatal=True):
        """
        Get OFFLINE executable locally

        :param fatal: If True, fails if the executable was not found. Default: True
        :type fatal: bool

        **Configuration variables used:**

        * ``exesurfex`` absolute path to the OFFLINE executable
          type: str

        """
        self.sh.title("Input OFFLINE executable from local")
        self.offline_exe = vortex.executable(
            role="offline",
            kind="offline",
            local="OFFLINE",
            model="surfex",
            remote=self.conf.exesurfex + "/OFFLINE",
            fatal=fatal,
        )
        print(self.ticket.prompt, "OFFLINE_tbx =", self.offline_exe)
        print()


class _Offline(OfflineCommonsMixin, _CenResearchTask):
    """
    **Task: _Offline**

    Abstract task for OFFLINE binary execution.

    SURFEX/OFFLINE documentation : https://umr-cnrm.github.io/snowtools-doc/misc/surfex.html

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
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int

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
    * ``forcing_model`` *model* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_model: str

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
            "datebegin",
            "dateend",
            "xpid",
            "geometry",
            "consts_surfex_uenv|uenv",
            "surfex_uenv|uenv",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "prep",
            "pgd",
            "member",
            "io_duration",
            "namespace_out",
            "august_threshold",
            "offline_gvar",
            "exesurfex",
            "out_block+default=[pro,prep] ",
            "drhook",
            "august_threshold",
            "diff_xpid",
            "diff_user",
            "diff_block+default=offline/prep",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        self.get_forcing(localname="FORCING_[datebegin:ymdh]_[dateend:ymdh].nc")
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_executable()

    def get_local_inputs(self):
        self.get_namelist_from_cache()
        try:
            _ = self.get_prep_file_from_cache_or_archive(fatal=True, cache_only=True)
        except SectionFatalError as e:
            print(
                "Unable to get PREP.nc."
                "Check your configuration (prep_xpid for example)."
                "Check your driver: does it have a node that fetches or computes a prep file? "
                "(e.g.: MakePrepFile, FetchPrepFileOrMake, FetchPrepFileOrCrash)"
            )
            raise e
        self.get_pgd_file_from_cache()

    def get_executable(self):
        """
        get offline executable either from local path or from a UEnv

        **Configuration variables used:**

        * ``exesurfex`` Path to the executable if it should come from a local path. Otherwise,
        * ``surfex_uenv`` If the executable should come from an uenv. Default is ``uenv``
        * ``offline_gvar`` specify the name of the offline executable in the uenv. Default is ``master_offline_mpi``
          if the mpi parameter is True and ``master_offline_nompi`` otherwise.
        * ``mpi`` If True, *mpi* executable is fetched, if False *nompi* executable. Default: True
        """
        if hasattr(self.conf, "exesurfex"):
            self.get_executable_from_path()
        else:
            mpi = self.conf.get("mpi", True)
            self.get_executable_from_uenv(mpi=mpi)

    def algo(self):
        """
        Algo component to execute OFFLINE
        """

        self.sh.title("Algo OFFLINE-MPI")
        offline_tba = vortex.task(
            engine="parallel",
            # binary         = 'OFFLINE',  # unused
            kind="deterministic",
            datebegin=self.conf.datebegin,
            dateend=self.conf.dateend,
            # MV : *dateinit* correspond à la date de validité du fichier PREP
            dateinit=self.ticket.context.sequence.effective_inputs(role="SnowpackInit")[0].rh.resource.datevalidity,
            # MV : la valeur par défaut de "threshold" dans la commande s2m est -999
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            threshold=self.conf.get("august_threshold", -999),
            # daily          = self.conf.dailyprep,
            # MV la valeur par défaut de 'drhook' dans la commande s2m est False
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            drhookprof=self.conf.get("drhook", False),
            # MV : on traitera les question de reproductibilité dans un 2nd temps.
            # reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, "Algo =", offline_tba)
        print()
        return offline_tba

    def put_outputs(self):
        """
        Save SURFEX/OFFLINE relevant output files
        """
        self.put_pro()
        self.put_prep()
        self.put_cumul()
        self.put_diag()

    def put_prep(self):

        self.sh.title("Output PREP")
        prep_tbo = (
            vortex.output(
                local="PREP_[datevalidity:ymdh].nc",
                role="SnowpackInit",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                # TODO : faire une tâche spécifique "reforecast" pour la production de PREP quotidiens
                # date           = list_dates_end_pro if not self.conf.dailyprep else
                #                       list(daterange(tomorrow(base=datebegin), dateend)),
                datevalidity=self.list_dates_end_pro,
                nativefmt="netcdf",
                kind="PREP",
                model="surfex",
                namespace=self.namespace_out,
                namebuild="flat@cen",  # TODO : passer en variable de configuration
                block=self.conf.get("out_block", "prep"),
                member=self.conf.get("member", None),
            ),
        )
        print(self.ticket.prompt, "prep_tbo =", prep_tbo)
        print()

    def put_pro(self):

        self.sh.title("Output PRO")
        pro_tbo = (
            vortex.output(
                local="PRO_[datebegin:ymdh]_[dateend:ymdh].nc",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                datebegin=self.list_dates_begin_pro,
                dateend=self.dict_dates_end_pro,
                nativefmt="netcdf",
                kind="SnowpackSimulation",
                model="surfex",
                namespace=self.namespace_out,
                namebuild="flat@cen",  # TODO : passer en variable de configuration
                block=self.conf.get("out_block", "pro"),
                member=self.conf.get("member", None),
            ),
        )
        print(self.ticket.prompt, "pro_tbo =", pro_tbo)
        print()

    def put_cumul(self):

        self.sh.title("Output CUMUL")
        cumul_tbo = (
            vortex.output(
                local="CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                datebegin=self.list_dates_begin_pro,
                dateend=self.dict_dates_end_pro,
                nativefmt="netcdf",
                kind="SnowpackSimulation",
                model="surfex",
                namespace=self.namespace_out,
                namebuild="flat@cen",  # TODO : passer en variable de configuration
                block=self.conf.get("out_block", "cumul"),
                member=self.conf.get("member", None),
                fatal=False,
            ),
        )
        print(self.ticket.prompt, "cumul_tbo =", cumul_tbo)
        print()

    def put_diag(self):

        self.sh.title("Output DIAG")
        diag_tbo = (
            vortex.output(
                local="DIAG_[datebegin:ymdh]_[dateend:ymdh].nc",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                datebegin=self.list_dates_begin_pro,
                dateend=self.dict_dates_end_pro,
                nativefmt="netcdf",
                kind="SnowpackSimulation",
                model="surfex",
                namespace=self.namespace_out,
                namebuild="flat@cen",  # TODO : passer en variable de configuration
                block=self.conf.get("out_block", "diag"),
                member=self.conf.get("member", None),
                fatal=False,
            ),
        )
        print(self.ticket.prompt, "diag_tbo =", diag_tbo)
        print()

    def diff(self):
        """
        Test output reproducibility [OPTIONAL]
        """
        # Diff of PRO files always fails because netcdf can not properly read them.
        # --> check reproductibility on PREP file
        self.sh.title("Reproducibility check: PREP")
        diff = (
            vortex.diff(
                local="PREP_[datevalidity:ymdh].nc",
                role="SnowpackInit",
                experiment=self.conf.diff_xpid,
                username=self.conf.diff_user,
                geometry=self.conf.geometry,
                datevalidity=self.list_dates_end_pro,
                nativefmt="netcdf",
                kind="PREP",
                model="surfex",
                namespace=self.conf.get("namespace_out", "vortex.multi.fr"),
                namebuild="flat@cen",  # TODO : passer en variable de configuration
                block=self.conf.get("diff_block", "offline/prep"),
                member=self.conf.get("member", None),
            ),
        )
        print(self.ticket.prompt, "diff =", diff)
        print()


class OfflineMpi(_Offline):
    """
    **Task: OfflineMpi**

    Task for the execution of OFFLINE binary with MPI parallelisation.

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
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int

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
    * ``forcing_model`` *model* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_model: str

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

        MANDATORY_CONFIGURATION_VARIABLES = []

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "ntasks",
            "nnodes",
            "nprocs",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def launch_algo(self, algo, **kw):
        """
        Run OFFLINE MPI algo component.
        """
        # Pour un exécution de binaire, il faut donner l'objet "exécutable" associé (récupéré par la commande
        # vortex.executable(...))
        # Il est possible de récupérer cet objet avec la ligne suivante :
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]

        self.component_runner(
            algo,
            executable,
            mpiopts={
                "nnodes": self.conf.nnodes,  # Redondant avec la valeur par défaut dans mkjob
                "nprocs": self.conf.nprocs,  # Redondant avec la valeur par défaut dans mkjob
                "ntasks": self.conf.ntasks,  # Redondant avec la valeur par défaut dans mkjob
            },
        )


class OfflineXiosMpi(_Offline):
    """
    **Task: OfflineXiosMpi**

    Task for the execution of OFFLINE binary with MPI parallelisation and IO server

    **Inputs:**

    - FORCING.nc files(s) (near-surface meteorological conditions during the simulation period)
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD.nc (Ground physiography)
    - PREP.nc (initial conditions)
    - surfex.xml for configuration of output simulation variables
    - iodef.xml for IO server configuration
    - xios_server.exe which is IO server executable

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
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int

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
    * ``forcing_model`` *model* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_model: str

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

        MANDATORY_CONFIGURATION_VARIABLES = []

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "ntasks",
            "nnodes",
            "nprocs",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        super().get_remote_inputs()

        # iodef.xml
        self.sh.title("Toolbox input iodef.xml")
        iodef_tbi = vortex.input(
            format="ascii",
            kind="coverparams",
            genv=self.conf.get("consts_surfex_uenv", self.conf.uenv),
            model="surfex",
            local="iodef.xml",
            gvar="IODEF",
        )
        print(self.ticket.prompt, "iodef_tbi =", iodef_tbi)
        print()

        # surfex.xml
        self.sh.title("Toolbox input surfex.xml")
        conf_xml_tbi = vortex.input(
            format="ascii",
            kind="coverparams",
            genv=self.conf.get("consts_surfex_uenv", self.conf.uenv),
            model="surfex",
            local="surfex.xml",
            gvar="CONF_XML",
        )
        print(self.ticket.prompt, "conf_xml_tbi =", conf_xml_tbi)
        print()

    def get_executable(self):
        self.sh.title("Input XIOS executable from uenv")
        self.xios_exe = vortex.executable(
            role="Binary",
            kind="xios",
            local="XIOS",
            model="surfex",
            binopts="--np 6",
            genv=self.conf.get("surfex_uenv", self.conf.uenv),
            gvar=self.conf.get("xios_gvar", "MASTER_XIOS"),
        )
        print(self.ticket.prompt, "XIOS executable =", self.xios_exe)
        print()
        super().get_executable()

    def algo(self):
        """
        Algo component to execute OFFLINE with XIOS
        """
        import footprints
        self.sh.title("Algo XIOS")
        xios = footprints.proxy.mpibinary(
            kind="ioserv",
            nodes=self.conf.io_nodes,
            tasks=self.conf.io_tasks,
        )
        print(self.ticket.prompt, "Xios =", xios)
        print()
        #xios.master = self.xios_exe[0].container.localpath()
        xios.master = self.xios_exe[0].container.abspath

        self.sh.title("Algo OFFLINE-XIOS-MPI")
        offline_tba = vortex.task(
            engine="parallel",
            # binary         = 'OFFLINE',  # unused
            kind="xios",
            datebegin=self.conf.datebegin,
            dateend=self.conf.dateend,
            # MV : *dateinit* correspond à la date de validité du fichier PREP
            dateinit=self.ticket.context.sequence.effective_inputs(role="SnowpackInit")[0].rh.resource.datevalidity,
            # MV : la valeur par défaut de "threshold" dans la commande s2m est -999
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            threshold=self.conf.get("august_threshold", -999),
            # daily          = self.conf.dailyprep,
            # MV la valeur par défaut de 'drhook' dans la commande s2m est False
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            drhookprof=self.conf.get("drhook", False),
            # MV : on traitera les question de reproductibilité dans un 2nd temps.
            # reprod_info    = self.get_reprod_info,
            ioserver = xios,
            iolocation = 0,
        )
        print(self.ticket.prompt, "Algo =", offline_tba)
        print()
        return offline_tba

    def launch_algo(self, algo, **kw):
        """
        Run OFFLINE MPI algo component.
        """
        # Pour un exécution de binaire, il faut donner l'objet "exécutable" associé (récupéré par la commande
        # vortex.executable(...))
        # Il est possible de récupérer cet objet avec la ligne suivante :
        #executables = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        executables = self.offline_exe

        self.component_runner(algo, executables,
#            mpiopts={
#                "nnodes": self.conf.nnodes,
##                "nprocs": self.conf.nprocs,  # Redondant avec la valeur par défaut dans mkjob
#                "ntasks": self.conf.ntasks - self.conf.io_tasks,
#                "envelope": True,
#            },
        )


class _Offline_NOMPI(_Offline):
    """
    **Task : _Offline_NOMPI**

    Abstract task for the execution of OFFLINE binary without MPI parallelisation.

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
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int

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
    * ``forcing_model`` *model* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_model: str

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

        MANDATORY_CONFIGURATION_VARIABLES = []

        OPTIONAL_CONFIGURATION_VARIABLES = []

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def launch_algo(self, algo, **kw):
        """
        Run OFFLINE algo component without MPI parallelisation.
        """
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        self.component_runner(algo, executable)


class Offline_Mpi_Uenv(OfflineMpi):
    """
    **Task: Offline_MPI_Uenv**

    Get OFFLINE executable from a User Environment.

    **NB:** This is the task to use to guarantee the simulation's reproductibility
    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = []
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "prep",
            "pgd",
            "member",
            "io_duration",
            "namespace_out",
            "august_threshold",
            "offline_gvar",
        ]
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_executable(self):
        self.get_executable_from_uenv(mpi=True)


class OfflineMpiDailyPrep(OfflineMpi):
    """
    Do a surfex simulation with daily prep file output

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
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int

    **Optional configuration variables (other than forcing-specific ones):**

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
    * ``forcing_model`` *model* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_model: str

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

    def get_executable(self):
        self.get_executable_from_uenv()

    def algo(self):
        """
        Algo component to execute OFFLINE with daily prep output
        """
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        self.sh.title("Algo OFFLINE-MPI")
        offline_tba = vortex.task(
            engine="parallel",
            binary="OFFLINE",
            kind="deterministic",
            datebegin=self.conf.get("forcing_datebegin", self.conf.datebegin),
            dateend=self.conf.get("forcing_dateend", self.conf.dateend),
            # MV : *dateinit* correspond à la date de validité du fichier PREP
            dateinit=self.ticket.context.sequence.effective_inputs(role="SnowpackInit")[0].rh.resource.datevalidity,
            # MV : la valeur par défaut de "threshold" dans la commande s2m est -999
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            threshold=self.conf.get("threshold", -999),
            daily=True,
            # MV la valeur par défaut de 'drhook' dans la commande s2m est False
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            drhookprof=self.conf.get("drhook", False),
            # MV : on traitera les question de reproductibilité dans un 2nd temps.
            # reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, "offline_tba =", offline_tba)
        print()
        return offline_tba

    def put_cumul(self):
        self.sh.title("Output CUMUL")
        cumul_tbo = (
            vortex.output(
                local="CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
                # et faire une tâche spécifique à ces cas là.
                datebegin="[dateend]/-PT24H",
                dateend=list(daterange(tomorrow(base=self.conf.datebegin), self.conf.dateend)),
                nativefmt="netcdf",
                kind="SnowpackSimulation",
                model="surfex",
                namespace=self.namespace_out,
                namebuild="flat@cen",  # TODO : passer en variable de configuration
                block="cumul",
                member=self.conf.get("member", None),
                fatal=False,
            ),
        )
        print(self.ticket.prompt, "cumul_tbo =", cumul_tbo)
        print()

    def put_diag(self):
        self.sh.title("Output DIAG")
        diag_tbo = (
            vortex.output(
                local="DIAG_[datebegin:ymdh]_[dateend:ymdh].nc",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                datebegin="[dateend]/-PT24H",
                dateend=list(daterange(tomorrow(base=self.conf.datebegin), self.conf.dateend)),
                nativefmt="netcdf",
                kind="SnowpackSimulation",
                model="surfex",
                namespace=self.namespace_out,
                namebuild="flat@cen",  # TODO : passer en variable de configuration
                block="diag",
                member=self.conf.get("member", None),
                fatal=False,
            ),
        )
        print(self.ticket.prompt, "diag_tbo =", diag_tbo)
        print()

    def put_prep(self):
        self.sh.title("Output PREP")
        prep_tbo = (
            vortex.output(
                local="PREP_[datevalidity:ymdh].nc",
                role="SnowpackInit",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                datevalidity=list(daterange(tomorrow(base=self.conf.datebegin), self.conf.dateend)),
                nativefmt="netcdf",
                kind="PREP",
                model="surfex",
                namespace=self.namespace_out,
                namebuild="flat@cen",  # TODO : passer en variable de configuration
                block="prep",
                member=self.conf.get("member", None),
            ),
        )
        print(self.ticket.prompt, "prep_tbo =", prep_tbo)
        print()

    def put_pro(self):
        self.sh.title("Output PRO")
        pro_tbo = (
            vortex.output(
                local="PRO_[datebegin:ymdh]_[dateend:ymdh].nc",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                datebegin="[dateend]/-PT24H",
                dateend=list(daterange(tomorrow(base=self.conf.datebegin), self.conf.dateend)),
                nativefmt="netcdf",
                kind="SnowpackSimulation",
                model="surfex",
                namespace=self.namespace_out,
                namebuild="flat@cen",  # TODO : passer en variable de configuration
                block="pro",
                member=self.conf.get("member", None),
            ),
        )
        print(self.ticket.prompt, "pro_tbo =", pro_tbo)
        print()

    def diff(self):
        """
        TODO: can be removed as soon as reference data for dailyprep test were created.
        """
        pass


class OfflineAssim(OfflineMpi):
    """
    This is the task for an OFFLINE-MPI execution after a snow data assimilation step.
    Each simulaiton member is initialised by a different PREP file identified by its *member* value.
    """

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = []

        OPTIONAL_CONFIGURATION_VARIABLES = []

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        self.get_forcing(localname="FORCING_[datebegin:ymdh]_[dateend:ymdh].nc")
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_prep_file()  # TODO: check if this could be replaced by a suitable configuration
        #  of the FetchPrepFileOrCrash and OfflineMpi classes? In this case the
        #  corresponding drivers could simply use the OfflineMpi class.
        self.get_executable_from_uenv()

    def get_local_inputs(self):
        self.get_namelist_from_cache()
        self.get_pgd_file_from_cache()

    def get_prep_file(self):
        """
        All members are initialised by a different PREP file coming from a SODA analysis
        --> Force *block* value to "prep/an" and *member" to the associated member value.
        SR: Must *block* really be hard coded here? Can't this be configured?

        """

        self.sh.title("Input PREP")
        prep_tbi = (
            vortex.input(
                local="PREP.nc",
                role="SnowpackInit",
                experiment=self.conf.get("prep_xpid", self.conf.xpid),
                username=self.conf.get("prep_user", None),
                date=self.conf.get("prep_date", self.conf.datebegin),
                vapp=self.conf.get("prep_vapp", self.conf.vapp),
                vconf=self.conf.get("prep_vconf", self.conf.vconf),
                geometry=self.conf.geometry,
                nativefmt="netcdf",
                kind="PREP",
                model="surfex",
                namespace="vortex.multi.fr",
                vortex1=self.conf.get("prep_vortex1", False),
                namebuild="flat@cen",  # TODO : passer en variable de configuration ?
                block=self.conf.get("prep_block", "soda/analysis"),
                member=self.conf.member,  # TODO: where does the "member" configuration come from?
                intent="inout",
            ),
        )
        print(self.ticket.prompt, "prep_tbi =", prep_tbi)
        print()


class OfflineOpenloop(OfflineMpi):
    """
    This is the task for an OFFLINE-MPI execution before any data assimilation.
    All members are initialised by the same PREP file, not associated to any member.
    """

    def get_remote_inputs(self):

        self.get_forcing(localname="FORCING_[datebegin:ymdh]_[dateend:ymdh].nc")
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_prep_file()  # TODO: check if this could be replaced by a suitable configuration
        #  of the FetchPrepFileOrCrash and OfflineMpi classes? In this case the
        #  corresponding drivers could simply use the OfflineMpi class.
        self.get_executable_from_uenv()

    def get_local_inputs(self):

        self.get_namelist_from_cache()
        self.get_pgd_file_from_cache()

    def get_prep_file(self):
        """
        All members are initialised by the same PREP file, not associated to any member.
        --> This method differs from the one in the main _Offline_MPI class because it is
        explicitly NOT associated to any member.

        """

        self.sh.title("Input PREP")
        prep_tbi = (
            vortex.input(
                local="PREP.nc",
                role="SnowpackInit",
                experiment=self.conf.get("prep_xpid", self.conf.xpid),
                username=self.conf.get("prep_user", None),
                date=self.conf.get("prep_date", self.conf.datebegin),
                vapp=self.conf.get("prep_vapp", self.conf.vapp),
                vconf=self.conf.get("prep_vconf", self.conf.vconf),
                geometry=self.conf.geometry,
                nativefmt="netcdf",
                kind="PREP",
                model="surfex",
                namespace="vortex.multi.fr",
                vortex1=self.conf.get("prep_vortex1", False),
                namebuild="flat@cen",  # TODO : passer en variable de configuration ?
                block=self.conf.get("prep_block", "prep"),
                intent="inout",
            ),
        )
        print(self.ticket.prompt, "prep_tbi =", prep_tbi)
        print()

    def put_prep(self):
        """
        Archive PREP files as "background" state --> force block value to "prep/bg"
        """

        self.sh.title("Output PREP")
        prep_tbo = (
            vortex.output(
                local="PREP_[date:ymdh].nc",
                role="SnowpackInit",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
                # et faire une tâche spécifique à ces cas là.
                #            date           = list_dates_end_pro if not self.conf.dailyprep else
                #                                list(daterange(tomorrow(base=datebegin), dateend)),
                date=self.list_dates_end_pro,
                nativefmt="netcdf",
                kind="PREP",
                model="surfex",
                namespace=self.namespace_out,
                namebuild="flat@cen",  # TODO : passer en variable de configuration
                block="prep/background",
                member=self.conf.get("member", None),
            ),
        )
        print(self.ticket.prompt, "prep_tbo =", prep_tbo)
        print()


class OfflineLocalForcing(OfflineMpi):
    """
    **Task : OfflineLocalForcing**

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
        self.get_pgd_file_from_cache()
        _ = self.get_prep_file_from_cache_or_archive(fatal=True, cache_only=True)
        # Get namelist from the preprocess task output
        self.get_namelist_from_cache()
        # Get FORCING locally because they have already been retrieved by the preprocess task
        self.get_forcing(localname="FORCING_[datebegin:ymdh]_[dateend:ymdh].nc")
