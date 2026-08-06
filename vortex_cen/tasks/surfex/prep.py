# -*- coding: utf-8 -*-
"""
prep.py
-------

Tasks designed to launch the PREP executable.

.. inheritance-diagram:: vortex_cen.tasks.surfex.prep
   :top-classes: vortex_cen.tasks.research_task_base._CenResearchTask
   :private-bases:
   :parts: 2

.. autoclass:: PrepCommonsMixin
   :members:
   :show-inheritance:

.. autoclass:: _PrepConstruct
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: FetchPrepFileOrMake
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: FetchPrepFileOrCrash
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: PrepRefill
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: MakePrepFile
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""

import vortex
import footprints
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin


class PrepCommonsMixin(SurfexCommonsMixin):
    """
    Mixin class prviding methods for fetching a PREP executable.

    **Optional configuration variables:**

    * ``surfex_uenv`` If the executable should come from an uenv. Default is ``uenv``
    * ``prep_gvar`` specify the name of the PREP executable in the uenv. Default is ``master_prep_mpi``
      if the mpi parameter is True and ``master_prep_nompi`` otherwise.
    * ``exesurfex`` Path to the executable if it should come from a local path.

    """

    def get_prep_exe_from_uenv(self, mpi=True, fatal=True):
        """
        method to get a PREP executable from uenv.

        :param mpi: True if an executable with MPI support should be fetched, False otherwise. Default is True.
        :param fatal: True if failing to fetch the executable should cause a fatal error, False otherwise.
         Default is True.

        **Configuration variables used:**

        * ``surfex_uenv`` If the executable should come from an uenv. Default is ``uenv``
        * ``prep_gvar`` specify the name of the PREP executable in the uenv. Default is ``master_prep_mpi``
          if the mpi parameter is True and ``master_prep_nompi`` otherwise.

        """

        if mpi:
            default_gvar = 'master_prep_mpi'
        else:
            default_gvar = 'master_prep_nompi'

        self.sh.title('Toolbox input PREP executable from uenv')
        PREP_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
            gvar           = self.conf.get('prep_gvar', default_gvar),
            fatal          = fatal,
        )
        print(self.ticket.prompt, 'PREP_tbx =', PREP_tbx)
        print()

    def get_prep_exe_from_path(self):
        """
        Fetch a PREP executable from a local path.

        **Configuration variables used:**

        * ``exesurfex`` Path to the executable if it should come from a local path.
        """

        self.sh.title('Toolbox input PREP executable from local path')
        prep_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PREP"
        )
        print(self.ticket.prompt, 'PREP_tbx =', prep_tbx)
        print()


class _PrepConstruct(PrepCommonsMixin, _CenResearchTask):
    """
    **Task: _PrepConstruct**

    Abstract task for the generation of initial conditions (PREP.nc file)

    **Inputs:**

    * ``OPTIONS.nam`` ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    * ``ecoclimapI_covers_param.bin`` and ``ecoclimapII_eu_covers_param.bin`` (binaries for vegetation generation)
    * ``drdt_bst_fit_60.nc`` (Crocus metamorphism parameters)
    * ``Init_TG.nc`` Initial values of ground temperature coming from the cache
      (put there by an execution of an InitClimGroundTemperature or GetClimGroundTemperature task)
    * ``PGD.nc`` Ground physiography coming from the cache (put there by an execution of a Pgd* task or GetPgd1D task

    **Outputs:**

    - PREP.nc (initial conditions)

    **Mandatory configuration variables:**

    * ``date`` Date of validity of the PREP.nc file to generate. Default is ``datebegin``
      type: str, Date
    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
      type: str
    * ``surfex_uenv`` or if not present ``uenv`` User Environment in which the following resources are to be retrieved :
      - ecoclimapI_covers_param.bin
      - ecoclimapII_eu_covers_param.bin
      - drdt_bst_fit_60.nc
      - PREP executable

      Format : uenv:{uenv_name}@{user}
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int

    **Optional configuration variables:**

    * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID
      type: str
    * ``pgd_user`` Name of the user who produced the PGD file
      type: str
    * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*
      type: str
    * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*
      type: str
    * ``dailyprep`` TODO :comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
      type: bool
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str
    * ``diff_xpid`` Experiment id of the reference file used for reproducibility test.
      type diff_xpid: str
    * ``diff_user`` *user name* associated with the reference file used for reproducibility test
      (only if different from current user). Default: *None*
      type diff_user: str

    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "geometry",
            "xpid",
            "consts_surfex_uenv|uenv",
            "surfex_uenv|uenv",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "pgd",
            "ntasks",
            "nnodes",
            "nprocs",
            "date",
            "tg_gvar",
            "diff_xpid",
            "diff_user",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        Get drdt_bst_fit_60.nc, PGD.nc
        """
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_prep_executable()

    def get_local_inputs(self):
        """
        Get OPTIONS.nam which is always in cache and
        init_TG.nc and PGD that should be in cache as well at this point.
        """
        self.get_namelist_from_cache()
        self.get_pgd_file_from_cache()
        self.get_init_TG_from_cache_or_archive(fatal=True, cache_only=True)

    def get_prep_executable(self):
        """
        Call either "get_prep_exe_from_path" or "get_prep_exe_from_uenv" method.
        """
        raise NotImplementedError("A get_prep_executable method should be implemented, it should call either the "
                                  "get_prep_exe_from_path or the get_prep_exe_from_uenv method.")

    def algo(self):
        """
        Algo component to produce the PREP file if not found in the inputs
        """
        self.sh.title('Toolbox algo PREP')
        PREP_tba = vortex.task(
            kind       = 'make_prep',
            engine     = 'parallel',
            date       = self.conf.get('date', self.conf.get('datebegin', None)),
        )
        print(self.ticket.prompt, 'Toolbox algo prep=', PREP_tba)
        print()
        return PREP_tba

    def launch_algo(self, algo, **kw):
        """
        Run PREP algo component.
        """
        self.launch_executable(algo)

    def put_outputs(self):
        """
        Save the PREP file
        """
        self.sh.title('Output PREP')
        prep_tbo = vortex.output(
            local        = 'PREP.nc',
            role         = 'SnowpackInit',
            experiment   = self.conf.xpid,
            datevalidity = self.conf.get('date', self.conf.get('datebegin', None)),
            vapp         = self.conf.vapp,
            vconf        = self.conf.vconf,
            geometry     = self.conf.geometry,
            nativefmt    = 'netcdf',
            kind         = 'PREP',
            model        = 'surfex',
            namespace    = self.conf.get('namespace_out', 'vortex.multi.fr'),
            namebuild    = 'flat@cen',  # TODO : passer en variable de configuration
            block        = "prep", # finally output blocks should be fixed and input blocks configurable. self.conf.get('prep_block', 'prep'),
            member       = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()

    def diff(self):
        """
        Test output reproductibility [OPTIONAL]
        """
        self.sh.title("Reproductibility check : PREP")
        diff = vortex.diff(
            local        = 'PREP.nc',
            role         = 'SnowpackInit',
            experiment   = self.conf.diff_xpid,
            username     = self.conf.get('diff_user', None),
            datevalidity = self.conf.get('date', self.conf.get('datebegin', None)),
            vapp         = self.conf.vapp,
            vconf        = self.conf.vconf,
            geometry     = self.conf.geometry,
            nativefmt    = 'netcdf',
            kind         = 'PREP',
            model        = 'surfex',
            namespace    = 'vortex.multi.fr',
            namebuild    = 'flat@cen',  # TODO : passer en variable de configuration
            block        = 'prep',
            member       = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'diff =', diff)
        print()


class FetchPrepFileOrMake(_PrepConstruct):
    """
    **Task: FetchPrepFileOrMake**

    Generation of initial conditions (PREP.nc file)
    Look if the requested PREP.nc file is available in the cache or archive. If not,
    calculate it. The output is put to the cache but not archived!
    Use the *MakePrepFile* class to generate and archive a PREP.nc file

    WARNING : The simulation's reproducibility can not be guaranteed with this task!

    **Inputs:**

    * ``OPTIONS.nam`` ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    * ``ecoclimapI_covers_param.bin`` and ``ecoclimapII_eu_covers_param.bin`` (binaries for vegetation generation)
    * ``drdt_bst_fit_60.nc`` (Crocus metamorphism parameters)
    * ``Init_TG.nc`` Initial values of ground temperature coming from the cache
      (put there by an execution of an InitClimGroundTemperature or GetClimGroundTemperature task)
    * ``PGD.nc`` Ground physiography

    **Outputs:**

    - PREP.nc (initial conditions)

    **Mandatory configuration variables:**

    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
      type: str
    * ``surfex_uenv`` or if not present ``uenv`` User Environment in which the following resources are to be retrieved :
      - ecoclimapI_covers_param.bin
      - ecoclimapII_eu_covers_param.bin
      - drdt_bst_fit_60.nc
      - PREP executable

      Format : uenv:{uenv_name}@{user}


    **Optional configuration variables:**

    * ``exesurfex`` Path to the executable if it should come from a local path.
    * ``prep_gvar`` specify the name of the PREP executable in the uenv.
    * ``prep_xpid`` or ``xpid`` Experiment id the prep file should be searched for or put in cache.
    * ``prep_user`` name of the user who produced the PREP file. Default: None.
    * ``prep_date`` or ``datebegin`` Validity date of the prep file. Default is ``datebegin`` but can be any date.
    * ``prep_vapp`` or ``vapp`` Application name to search the PREP.nc file.
    * ``prep_vconf`` or ``vconf`` Configuration name to search the PREP.nc file.
    * ``prep_vortex1`` type: bool. *True* if the requested PREP.nc file was produced with vortex 1 and thus uses
      vortex 1 naming conventions. Default is *False*.
    * ``prep_geometry`` or ``geometry`` *geometry* of the PREP file.
    * ``prep_namebuild`` Default: *flat@cen*
    * ``prep_block`` block part of the data tree to search for the PREP.nc file. Default is ``prep``.
    * ``prep_member`` or ``member`` If the PREP.nc file comes from an ensemble, a member can be chosen.
      Default is ``None``.
    * ``prep_cutoff`` Can be used to select a PREP file coming from an operational forecast (*forecast*) or
      analysis (*assimilation*). Default is *None*. Might be useful for reforecasts.
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "xpid",
            "geometry",
            "uenv",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "date",
            "member",
            "exesurfex",
            "tg_cache",
            "prep",
            "pgd",
            "prep_block"
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_prep_executable(self):
        """
        get PREP executable either from local path or from a UEnv

        **Configuration variables used:**

        * ``exesurfex`` Path to the executable if it should come from a local path. Otherwise,
        * ``surfex_uenv`` If the executable should come from an uenv. Default is ``uenv``
        * ``prep_gvar`` specify the name of the PREP executable in the uenv. Default is ``master_prep_mpi``
          if the mpi parameter is True and ``master_prep_nompi`` otherwise.
        """
        if hasattr(self.conf, 'exesurfex'):
            self.get_prep_exe_from_path()
        else:
            self.get_prep_exe_from_uenv()

    def get_remote_inputs(self):

        prep_tbi = self.get_prep_file_from_cache_or_archive(fatal=False)
        if len(self.ctx.sequence.effective_inputs(role="SnowpackInit")) == 0:
            super().get_remote_inputs()

    def get_local_inputs(self):
        if len(self.ctx.sequence.effective_inputs(role="SnowpackInit")) > 0:
            pass
        else:
            super().get_local_inputs()

    def algo(self):
        if len(self.ctx.sequence.effective_inputs(role="SnowpackInit")) > 0:
            pass
        else:
            myalgo = super().algo()
            return myalgo

    def launch_algo(self, algo, **kwargs):
        if len(self.ctx.sequence.effective_inputs(role="SnowpackInit")) > 0:
            pass
        else:
            super().launch_algo(algo, **kwargs)

    def put_outputs(self):

        self.sh.title('Put PREP to cache')
        prep_tbo = vortex.output(
            local       = 'PREP.nc',
            role        = 'SnowpackInit',
            experiment  = self.conf.xpid,
            datevalidity = self.conf.get('prep_date', self.conf.datebegin),
            vapp        = self.conf.get('prep_vapp', self.conf.vapp),
            vconf       = self.conf.get('prep_vconf', self.conf.vconf),
            geometry    = self.conf.geometry,
            nativefmt   = 'netcdf',
            kind        = 'PREP',
            model       = 'surfex',
            namespace   = 'vortex.cache.fr',
            namebuild   = 'flat@cen',  # TODO : passer en variable de configuration
            block       = "prep", # after discussion, output blocks should be fixed and input blocks configurable. self.conf.get('prep_block', 'prep'),
            member      = self.conf.get('prep_member', self.conf.get('member', None)),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()


class FetchPrepFileOrCrash(FetchPrepFileOrMake):
    """
    Fetch a prep file from the archive (or vortex cache) and put it in
    the cache of the current experiment.

    **Mandatory configuration variables:**

    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
      type: str
    * ``prep_xpid`` or ``xpid`` Experiment id the prep file should be searched for or put in cache.
    * ``prep_user`` name of the user who produced the PREP file. Default: None.
    * ``prep_date`` or ``datebegin`` Validity date of the prep file. Default is ``datebegin`` but can be any date.
    * ``prep_vapp`` or ``vapp`` Application name to search the PREP.nc file.
    * ``prep_vconf`` or ``vconf`` Configuration name to search the PREP.nc file.
    * ``prep_vortex1`` type: bool. *True* if the requested PREP.nc file was produced with vortex 1 and thus uses
      vortex 1 naming conventions. Default is *False*.
    * ``prep_geometry`` or ``geometry`` *geometry* of the PREP file.
    * ``prep_namebuild`` Default: *flat@cen*
    * ``prep_block`` block part of the data tree to search for the PREP.nc file. Default is ``prep``.
    * ``prep_member`` or ``member`` If the PREP.nc file comes from an ensemble, a member can be chosen.
      Default is ``None``.
    * ``prep_cutoff`` Can be used to select a PREP file coming from an operational forecast (*forecast*) or
      analysis (*assimilation*). Default is *None*. Might be useful for reforecasts.

    """
    def __init__(self, **kw):
        MANDATORY_CONFIGURATION_VARIABLES = [
            "xpid",
            "geometry",
            "date",
            "member",
            "prep",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            'prep_xpid',
            'prep_user',
        ]
        overwrite = [
            "prep",
            "exesurfex",
            "tg_cache",
            "tg_gvar",
            "uenv",
            "date",
            "pgd_cache",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES, overwrite=overwrite)

    def get_remote_inputs(self):

        prep_tbi = self.get_prep_file_from_cache_or_archive(fatal=True)

    def get_local_inputs(self):
        pass

    def algo(self):
        pass

    def launch_algo(self, algo, **kwargs):
        pass

class PrepRefill(FetchPrepFileOrCrash):
    """
    **Task : PrepRefill**


    **Input:**

    - A single PREP.nc

    **Output:**

    - A copy of the input PREP.nc file into the cache of all members of the real-time chain

    **Mandatory configuration variables:**

    * ``geometry`` *geometry* of the forcing file(s)
    * ``xpid`` Experiment identifier. type: str
    * ``prep_date`` or ``datebegin`` Validity date of the prep file. Default is ``datebegin`` but can be any date.
    * ``prep_xpid`` Experiment id the prep file should be searched for.
    * ``rundate`` Date of run. choices: YYYYMMDD[03 06 09 12], type: str or Date
    * ``datevalidity`` Date of validity of the PREP.nc file to generate. Default is ``datebegin``
      type: str, Date
    * ``members``

    **Optional configuration variables:**

    * ``prep_user`` name of the user who produced the PREP file. Default: None.
    * ``prep_vapp`` or ``vapp`` Application name to search the PREP.nc file.
    * ``prep_vconf`` or ``vconf`` Configuration name to search the PREP.nc file.
    * ``prep_vortex1`` type: bool. *True* if the requested PREP.nc file was produced with vortex 1 and thus uses
      vortex 1 naming conventions. Default is *False*.
    * ``prep_block`` block part of the data tree to search for the PREP.nc file. Default is ``prep``.
    * ``cutoff`` Target *cutoff* (refill an analysis or a forecast output). type: str, choices:
      'assimilation', 'production', default: 'assimilation'
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "prep_date",
            "prep_xpid",
            "geometry",
            "xpid",
            "rundate+help=Date of run;choices=YYYYMMDD[03 06 09 12];type=str or Date",
            "members",
            "datevalidity",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "prep_user",
            "prep_vapp",
            "prep_vconf",
            "prep_vortex1",
            "prep_block",
            "cutoff+help=Target *cutoff* (refill an analysis or a forecast output);type=str;"
            "choices='assimilation', 'production';default='assimilation'",
        ]
        overwrite = [
            "datebegin",
            "dateend",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES, overwrite=overwrite)

    def put_outputs(self):

        self.sh.title('Output PREP(s)')
        prep = vortex.output(
            role           = 'SnowpackInit',
            local          = 'PREP.nc',
            block          = 'prep',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datevalidity   = self.conf.datevalidity,
            date           = self.conf.rundate,
            member         = footprints.util.rangex(self.conf.members),
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            vortex1        = self.conf.get('prep_vortex1', False),
            cutoff         = self.conf.get('cutoff', 'assimilation'),
        ),
        print(self.ticket.prompt, 'Prep =', prep)
        print()



class MakePrepFile(_PrepConstruct):
    """
    **Task : MakePrepFile**

    Task for the generation of initial conditions (PREP.nc file).
    By default the resulting file is archived. Use the ``namespace_out`` configuration variable to change
    this behavior.

    **Inputs:**

    * ``OPTIONS.nam`` ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    * ``ecoclimapI_covers_param.bin`` and ``ecoclimapII_eu_covers_param.bin`` (binaries for vegetation generation)
    * ``drdt_bst_fit_60.nc`` (Crocus metamorphism parameters)
    * ``Init_TG.nc`` Initial values of ground temperature coming from the cache
      (put there by an execution of an InitClimGroundTemperature or GetClimGroundTemperature task)
    * ``PGD.nc`` Ground physiography coming from the cache (put there by an execution of a Pgd* task or GetPgd1D task

    **Outputs:**

    - PREP.nc (initial conditions)

    **Mandatory configuration variables:**

    * ``datevalidity`` Date of validity of the PREP.nc file to generate. Default is ``datebegin``
      type: str, Date
    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
      type: str
    * ``surfex_uenv`` or if not present ``uenv`` User Environment in which the following resources are to be retrieved :
      - ecoclimapI_covers_param.bin
      - ecoclimapII_eu_covers_param.bin
      - drdt_bst_fit_60.nc
      - PREP executable

      Format : uenv:{uenv_name}@{user}
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int

    **Optionnal configuration variables:**

    * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID
      type: str
    * ``pgd_user`` Name of the user who produced the PGD file
      type: str
    * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*
      type: str
    * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*
      type: str
    * ``dailyprep`` TODO :comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
      type: bool
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str
    * ``diff_xpid`` Experiment id of the reference file used for reproducibility test.
      type diff_xpid: str
    * ``diff_user`` *user name* associated with the reference file used for reproducibility test
      (only if different from current user). Default: *None*
      type diff_user: str
    * ``exesurfex`` Path to the executable if it should come from a local path. Otherwise,
    * ``surfex_uenv`` If the executable should come from an uenv. Default is ``uenv``
    * ``prep_gvar`` specify the name of the PREP executable in the uenv. Default is ``master_prep_mpi``
      if the mpi parameter is True and ``master_prep_nompi`` otherwise.

    """
    def __init__(self, **kw):
        MANDATORY_CONFIGURATION_VARIABLES = []
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "exesurfex",
            "prep_gvar",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_prep_executable(self):
        """
        get PREP executable either from local path or from a UEnv

        **Configuration variables used:**

        * ``exesurfex`` Path to the executable if it should come from a local path. Otherwise,
        * ``surfex_uenv`` If the executable should come from an uenv. Default is ``uenv``
        * ``prep_gvar`` specify the name of the PREP executable in the uenv. Default is ``master_prep_mpi``
          if the mpi parameter is True and ``master_prep_nompi`` otherwise.
        * ``mpi`` If True, *mpi* executable is fetched, if False *nompi* executable. Default: True
        """
        if hasattr(self.conf, 'exesurfex'):
            self.get_prep_exe_from_path()
        else:
            mpi = self.conf.get("mpi", True)
            self.get_prep_exe_from_uenv(mpi=mpi)
