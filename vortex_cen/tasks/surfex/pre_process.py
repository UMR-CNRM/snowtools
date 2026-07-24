# -*- coding: utf-8 -*-
"""
pre_process.py
--------------

Tasks designed to launch the pre-process the OPTIONS.nam namelist before any SURFEX binary execution.

.. inheritance-diagram:: vortex_cen.tasks.surfex.pre_process
   :top-classes: vortex_cen.tasks.research_task_base._CenResearchTask
   :private-bases:
   :parts: 2

.. autoclass:: _Preprocess
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: PreprocessNamelist
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: SodaNamelistPreprocess
   :no-members:
   :class-doc-from: class
   :show-inheritance:

"""

import vortex
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex.util.helpers import InputCheckerError
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin


class _Preprocess(SurfexCommonsMixin, _CenResearchTask):
    """
    **Task: _Preprocess**

    Abstract task for pre-processing namelist:
    add infos like points and dates from forcing to namelist.

    **Inputs:**

    - SURFEX namelist (OPTIONS.nam)
    - FORCING file

    **Outputs:**

    - Modified and ready-to-use SURFEX namelist

    **Mandatory configuration variables:**

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

    **Optional configuration variables:**

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
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "diff_xpid",
            "diff_user",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get forcing file(s) and namelist in order to transform the namelist
        """

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Change the namelist when forcings and namelist are here
        """
        avail_forcings = self.ticket.context.sequence.effective_inputs(role='Forcing')
        if len(avail_forcings) > 0:
            firstforcing = avail_forcings[0]
        else:
            raise InputCheckerError('No FORCING file present, the task can not run properly')

        # Algo component to preprocess the namelist (adjust dates, etc.)
        self.sh.title('Toolbox algo preprocess')
        preprocess_tba = vortex.task(
            kind         = 'surfex_preprocess',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            # Le nom local de la ressource est fourni par le "container"
            forcingname  = firstforcing.rh.container.basename,
        )
        print(self.ticket.prompt, 'Toolbox algo preprocess =', preprocess_tba)
        print()
        return preprocess_tba

    def launch_algo(self, algo, **kwargs):
        """
        Launch an algo component.
        :param algo: algo component
        :param kwargs: additional arguments not used
        """
        self.launch_python_algo(algo=algo)

    def put_outputs(self):
        """
        Save the changed namelist in cache -> namespace = 'vortex.CACHE.fr'
        """
        self.sh.title('Toolbox output Namelist after modification (local cache only)')
        namelist_tbo = vortex.output(
            role         = 'Nam_surfex',
            kind         = 'namelist',
            model        = 'surfex',
            local        = 'OPTIONS.nam',
            experiment   = self.conf.xpid,
            namespace    = 'vortex.cache.fr',  # Never put a namelist on Hendrix
            block        = 'namelist',
            nativefmt    = 'nam',
        ),
        print(self.ticket.prompt, 'namelist_tbo =', namelist_tbo)
        print()

    def diff(self):
        """
        Test output reproductibility [OPTIONAL]
        """
        pass
#        self.sh.title("Reproductibility check : OPTIONS.nam")
#        diff = vortex.diff(
#            role         = 'Nam_surfex',
#            kind         = 'namelist',
#            model        = 'surfex',
#            local        = 'OPTIONS.nam',
#            experiment   = self.conf.diff_xpid,
#            username     = self.conf.get('diff_user', None),
#            namespace    = 'vortex.multi.fr',  # A single reference namelist should be on Hendrix
#            block        = 'namelist',
#            nativefmt    = 'nam',
#        ),
#        print(self.ticket.prompt, 'diff =', diff)
#        print()


class PreprocessNamelist(_Preprocess):
    """
    **Task: PreprocessNamelist**

    Task for pre-processing a namelist coming from a User Environment.
    NB: This is the task to use to guarantee the simulation's reproductibility

    **Mandatory configuration variables:**

    * ``surfex_uenv`` or if not present ``uenv`` User Environment from which the namelist file should be fetched.
      Format : uenv:{uenv_name}@{user}
    * ``namelist_source`` In an UEnv, several namelistes can be present in an *.tar* archive,
      the *source*  footprint allows to define the exact name of the nameliste to fetch.
      For example, *OPTIONS_default.nam*.
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

    **Optional configuration variables:**

    * ``namelist_path`` absolute path to the namelist file if the namelist is not in the uenv.
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
            "namelist_path",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        if 'namelist_path' in self.conf:
            self.get_namelist_from_path()
        else:
            self.get_namelist_from_uenv()
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')


class SodaNamelistPreprocess(SurfexCommonsMixin, _CenResearchTask):
    """
    **Task: SodaNamelistPreprocess**

    Pre-process SURFEX namelist for SODA executable

    **Inputs:**

    - SODA namelist (OPTIONS.nam)

    **Outputs:**

    - SODA namelist (OPTIONS.nam)

    **Mandatory configuration variables**

    * ``uenv`` User Environment in which the namelist is to be retrieved
      type: str
    * ``namelist_source`` Name of the namelist in the user environment
      type: str
    * ``nmembers`` Number of ensemble members in the background state
      type: int
    * ``xpid`` Experiment Identifier
      type: str

    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
            "surfex_uenv|uenv",
            "namelist_source",
            "nmembers",

        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        self.get_namelist_from_uenv()

    def get_local_inputs(self):
        pass

    def algo(self):

        self.sh.title('Algo soda preprocess')
        algo = vortex.task(
            kind         = 'soda_preprocess',
            nmembers      = self.conf.nmembers,
        )
        print(self.ticket.prompt, 'Algo soda preprocess =', algo)
        print()

        return algo

    def launch_algo(self, algo, **kwargs):
        """
        Launch a soda namelist preprocess algorithm.
        :param algo: algorithm to launch
        :param kwargs: keyword arguments not used
        """
        self.launch_python_algo(algo=algo)

    def put_outputs(self):

        self.sh.title('Output namelist')
        namelist = vortex.output(
            role            = 'Nam_surfex',
            kind            = 'namelist',
            model           = 'surfex',
            local           = 'OPTIONS_OUT.nam',
            experiment      = self.conf.xpid,
            namespace       = 'vortex.cache.fr',
            block           = 'namelist',
            nativefmt       = 'nam',
        )
        print(self.ticket.prompt, 'Namelist = ', namelist)
        print()
