# -*- coding: utf-8 -*-
"""
init_clim_ground_temperature.py
-------------------------------

Tasks designed to generate an init_TG.nc file.

.. inheritance-diagram:: vortex_cen.tasks.surfex.init_clim_ground_temperature
   :top-classes: vortex_cen.tasks.research_task_base._CenResearchTask
   :private-bases:
   :parts: 2

.. autoclass:: InitClimGroundTemperature
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: FetchClimGroundTemperatureOrMake
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: MakeClimGroundTemperatureIfNoPrep
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: FetchClimGroundTemperatureOrCrash
   :no-members:
   :class-doc-from: class
   :show-inheritance:

"""

import vortex
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin


class InitClimGroundTemperature(SurfexCommonsMixin, _CenResearchTask):
    """
    **Task: InitClimGroundTemperature**

    Initialize Surfex ground temperature (GT) by taking the climatological mean of the input forcing air temperature.

    **Inputs:**

    - FORCING file(s) on simulation geometry

    **Outputs:**

    - Init_TG file (initial values of ground temperature)

    **Mandatory configuration variables:**

    * ``forcing_datebegin`` *datebegin* footprint, default self.conf.datebegin
      type forcing_datebegin: str, footprints.stdtypes.FPList
    * ``forcing_dateend`` *dateend* footprint, default self.conf.dateend
      type forcing_dateend: str, footprints.stdtypes.FPList
    * ``forcing_xpid`` Experiment identifier, default self.conf.xpid
      type forcing_xpid: str
    * ``geometry`` *geometry* of the current experiment.
      type geometry: str, footprints.stdtypes.FPList
    * ``forcing_geometry`` *geometry* footprint, default self.conf.geometry
      type forcing_geometry: str, footprints.stdtypes.FPList (should be the same as *geometry* for this task, except
      if you have a very good reason why not.)
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
    * ``xpid`` experiment id of the current experiment. Used to store the output.
    * ``geometry`` *geometry* of the current experiment.

    **Optionnal configuration variables:**

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
    * ``out_block``: *block* part of the vortex output path. Default: "prep"
      type out_block: str
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str

    **Configuration variables for reproducibility test**

    In general, the following variables will be used only for test cases.

    * ``diff_xpid`` Experiment id of the reference file.
      type diff_xpid: str
    * ``diff_user`` *user name* associated with the reference file (only if different from current user). Default: None
      type diff_user: str
    * ``diff_block`` *block* part of the vortex path of the reference file. Default: "init_tg"
      type diff_block: str
    """

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = [
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "out_block+default=prep",
            "namespace_out+default=vortex.multi.fr",
            "diff_xpid",
            "diff_user",
            "diff_block+default=init_tg",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get FORCING file as "FORCING_[datebegin:ymdh]_[dateend:ymdh].nc" in the different working sub-directories.
        """

        self.get_forcing(localname="FORCING_[datebegin:ymdh]_[dateend:ymdh].nc")

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Return an InitClimGroundTemperatureAlgo with the appropriate arguments.


        Working tree :
        rootdir
        |-- FORCING_datebegin1_dateend1.nc
        |-- FORCING_datebegin2_dateend2.nc

        """

        self.sh.title("Toolbox algo calculate ground temperature climatology")
        algo = vortex.task(
            engine="algo",
            kind="clim",
        )
        print(self.ticket.prompt, "algo =", algo)
        print()
        return algo

    def launch_algo(self, algo, **kw):
        """
        Launch an algo component.

        :param algo: algo component
        :param kw: optional keyword arguments (not used)
        """
        self.launch_python_algo(algo=algo)

    def put_outputs(self):
        """
        Save the output Ground temperature (GT) initialization based on the climatological mean file in the simulation
        geometry.
        """

        self.sh.title("Toolbox output for initial values of ground temperature")
        init_ground_temperature_out = vortex.output(
            role       = "InitialValuesOfGroundTemperature",
            kind       = "climTG",
            nativefmt  = "netcdf",
            local      = "init_TG.nc",
            experiment = self.conf.xpid,
            geometry   = self.conf.geometry,
            model      = "surfex",
            namespace  = self.namespace_out,
            namebuild  = "flat@cen",
            block      = self.conf.get("out_block", "init_tg"),
        )
        print(self.ticket.prompt, "Output init ground temperature =", init_ground_temperature_out)
        print()

    def diff(self):
        """
        Test output reproductibility [OPTIONAL]
        """

        self.sh.title("Reproductibility check : init_TG")
        init_tg_diff = vortex.diff(
            role       = "InitialValuesOfGroundTemperature",
            kind       = "climTG",
            nativefmt  = "netcdf",
            local      = "init_TG.nc",
            experiment = self.conf.diff_xpid,
            username   = self.conf.get("diff_user", None),
            geometry   = self.conf.geometry,
            model      = "surfex",
            namespace  = "vortex.multi.fr",
            namebuild  = "flat@cen",
            block      = self.conf.get("diff_block", "init_tg"),
        )
        print(self.ticket.prompt, "diff init_tg =", init_tg_diff)
        print()


class FetchClimGroundTemperatureOrMake(InitClimGroundTemperature):
    """
    **Task: FetchClimGroundTemperatureOrMake**

    If InitTG is available in cache or archive for the current experiment fetch it.
    If not, try to get it from an uenv.
    If not either, generate it by calling the methods from the mother class.

    **Configuration variables used for fetching from cache or archive**

    * ``tg_xpid`` or ``xpid`` experiment id the init_TG.nc file should be fetched from.
    * ``tg_user`` name of the user that produced the target the init_TG.nc file. Default: *None*
    * ``tg_geometry`` or ``geometry`` geometry of the init_TG. Logically the same as for the rest of the simulation
    * ``tg_vapp`` or ``vapp`` Application name to search the init_TG.nc file.
    * ``tg_vconf`` or ``vconf`` Configuration name to search the init_TG.nc file.
    * ``tg_block`` Block name to search the init_TG.nc file. Default: *prep*

    **Configuration variables used for fetching from uenv:**

    * ``tg_geometry`` or ``geometry`` geometry of the init_TG. Logically the same as for the rest of the simulation
    * ``surfex_uenv`` or if not present ``uenv`` User Environment from which the init_TG.nc file should be fetched.
      Format : uenv:{uenv_name}@{user}
    * ``tg_gvar`` key to look up the init_TG.nc file in the uenv the file should come from.

    **Configuration variables used for calculating initial ground temperature:**

    **Mandatory**

    * ``forcing_datebegin`` *datebegin* footprint, default self.conf.datebegin
      type forcing_datebegin: str, footprints.stdtypes.FPList
    * ``forcing_dateend`` *dateend* footprint, default self.conf.dateend
      type forcing_dateend: str, footprints.stdtypes.FPList
    * ``forcing_xpid`` Experiment identifier, default self.conf.xpid
      type forcing_xpid: str
    * ``geometry`` *geometry* of the current experiment.
      type geometry: str, footprints.stdtypes.FPList
    * ``forcing_geometry`` *geometry* footprint, default self.conf.geometry
      type forcing_geometry: str, footprints.stdtypes.FPList (should be the same as *geometry* for this task, except
      if you have a very good reason why not.)
    * ``forcing_vapp`` *vapp* footprint, default self.conf.vapp
      type forcing_vapp: str
    * ``forcing_vconf`` *vconf* footprint, default self.conf.vconf
      type forcing_vconf: str
    * ``forcing_block`` *block* footprint, default "meteo"
      type forcing_vconf: str
    * ``forcing_namespace`` *namespace* footprint, default "vortex.multi.fr" (hendrix + local cache)
      type forcing_namespace: str
    * ``forcing_model`` *model* footprint (to be made optional for SurfaceIO objects), default None
      type forcing_model: str
    * ``xpid`` experiment id of the current experiment. Used to store the output.
    * ``geometry`` *geometry* of the current experiment.

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
    * ``out_block``: *block* part of the vortex output path. Default: "prep"
      type out_block: str

    **Configuration variables for reproducibility test**

    In general, the following variables will be used only for test cases.

    * ``diff_xpid`` Experiment id of the reference file.
      type diff_xpid: str
    * ``diff_user`` *user name* associated with the reference file (only if different from current user). Default: None
      type diff_user: str
    * ``diff_block`` *block* part of the vortex path of the reference file. Default: "init_tg"
      type diff_block: str

    """

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = [
            "geometry",
            "uenv|surfex_uenv",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "tg_cache",
            "tg_gvar",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    @property
    def namespace_out(self):
        """Namespace for output files."""
        return "vortex.cache.fr"

    def get_remote_inputs(self):
        # First try to get an init_TG file from the local cache or the archive
        self.init_tg = self.get_init_TG_from_cache_or_archive(fatal=False, cache_only=False)
        # then try to get init_TG from uenv
        if len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")) == 0:
            self.get_init_TG_from_uenv(fatal=False)

        # If no init_TG file was found, launch the actual init_TG task
        if len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")) == 0:
            super().get_remote_inputs()

    def algo(self):
        # If no init_TG file was found, launch the actual init_TG task
        if len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")) == 0:
            myalgo = super().algo()
            return myalgo
        else:
            pass

    def launch_algo(self, algo):
        # If no init_TG file was found, launch the actual init_TG task
        if len(self.ctx.sequence.effective_inputs(role="InitialValuesOfGroundTemperature")) == 0:
            super().launch_algo(algo)
        else:
            pass


class MakeClimGroundTemperatureIfNoPrep(FetchClimGroundTemperatureOrMake):
    """
    **Task: MakeClimGroundTemperatureIfNoPrep**

    If the "climground" is provided and set to "True", this task will look for a PREP.nc file and if none is found,
    it will initialize Surfex ground temperature (GT) by taking the climatological mean of the input forcing air
    temperature.

    **Inputs:**

    - FORCING file(s) on simulation geometry

    **Outputs:**

    - Init_TG file (initial values of ground temperature)

    **Optional configuration variables**

    * ``climground`` Allow the generation of a ground initialization file by computing a climatological
      average of air temperature on the provided period. Default: False
      type climground: bool

    **Configuration variables if ``climground`` is ``True``:**

    **Search for PREP file**

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

    **Search for initTG file in archive:**

    * ``tg_xpid`` or ``xpid`` experiment id the init_TG.nc file should be fetched from.
    * ``tg_user`` name of the user that produced the target the init_TG.nc file. Default: *None*
    * ``tg_geometry`` or ``geometry`` geometry of the init_TG. Logically the same as for the rest of the simulation
    * ``tg_vapp`` or ``vapp`` Application name to search the init_TG.nc file.
    * ``tg_vconf`` or ``vconf`` Configuration name to search the init_TG.nc file.
    * ``tg_block`` Block name to search the init_TG.nc file. Default: *prep*

    **Search for initTG file in unev:**

    * ``tg_geometry`` or ``geometry`` geometry of the init_TG. Logically the same as for the rest of the simulation
    * ``surfex_uenv`` or if not present ``uenv`` User Environment from which the init_TG.nc file should be fetched.
      Format : uenv:{uenv_name}@{user}
    * ``tg_gvar`` key to look up the init_TG.nc file in the uenv the file should come from.

    **Calculate initial ground temperature:**

    * ``forcing_datebegin`` *datebegin* footprint, default self.conf.datebegin
      type forcing_datebegin: str, footprints.stdtypes.FPList
    * ``forcing_dateend`` *dateend* footprint, default self.conf.dateend
      type forcing_dateend: str, footprints.stdtypes.FPList
    * ``forcing_xpid`` Experiment identifier, default self.conf.xpid
      type forcing_xpid: str
    * ``geometry`` *geometry* of the current experiment.
      type geometry: str, footprints.stdtypes.FPList
    * ``forcing_geometry`` *geometry* footprint, default self.conf.geometry
      type forcing_geometry: str, footprints.stdtypes.FPList (should be the same as *geometry* for this task, except
      if you have a very good reason why not.)
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
    * ``xpid`` experiment id of the current experiment. Used to store the output.
    * ``geometry`` *geometry* of the current experiment.
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
    * ``out_block``: *block* part of the vortex output path. Default: "prep"
      type out_block: str

    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "climground:prep",
        ]
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def process(self):
        if self.conf.get('climground', False):
            # Check if a PREP file already exists
            prep = self.get_prep_file_from_cache_or_archive(fatal=False, cache_only=False)
            # If no PREP file found, launch the generation of init_TG file
            if not prep[0]:
                super().process()
        else:
            pass


class FetchClimGroundTemperatureOrCrash(InitClimGroundTemperature, _CenResearchTask):
    """
    Try to get the ground temperature climatology file from an uenv or potentially from the archive and put it to the
    cache.
    Crash if the file is not available.

    **Mandatory configuration variables:**

    * ``surfex_uenv`` or if not present ``uenv`` User Environment from which the PGD.nc file should be fetched.
      Format : uenv:{uenv_name}@{user}
    * ``tg_gvar`` key to look up the init_TG.nc file in the uenv the file should come from.
    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier

    **Mandatory configuration variables unless ``force_uenv`` is *True*:**

    * ``tg_xpid`` or ``xpid`` experiment id the init_TG.nc file should be fetched from.
    * ``tg_user`` name of the user that produced the target the init_TG.nc file. Default: *None*
    * ``tg_geometry`` or ``geometry`` geometry of the init_TG. Logically the same as for the rest of the simulation
    * ``tg_vapp`` or ``vapp`` Application name to search the init_TG.nc file.
    * ``tg_vconf`` or ``vconf`` Configuration name to search the init_TG.nc file.
    * ``tg_block`` Block name to search the init_TG.nc file. Default: *prep*

    **Optional configuration variables:**

    * ``force_uenv`` If *True* the Init_TG.nc file must come from an uenv. Default: *False*

    """
    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "surfex_uenv|uenv",
            "geometry",
            "xpid",
            "tg_gvar",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "force_uenv",
            "tg_cache",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    @property
    def namespace_out(self):
        """Namespace for output files."""
        return "vortex.cache.fr"

    def get_remote_inputs(self):
        force_uenv = self.conf.get("force_uenv", True)
        initg = self.get_init_TG_from_uenv(fatal=force_uenv)
        if not initg[0] and not force_uenv:
            _ = self.get_init_TG_from_cache_or_archive(fatal=True, cache_only=False)

        # Place the retrieved file in th cache where the next task will look for it
        self.sh.title("Refill local cache with retrieved initial values of ground temperature")
        init_ground_temperature_out = vortex.output(
            role       = "InitialValuesOfGroundTemperature",
            kind       = "climTG",
            nativefmt  = "netcdf",
            local      = "init_TG.nc",
            experiment = self.conf.xpid,
            geometry   = self.conf.geometry,
            model      = "surfex",
            namespace  = "vortex.cache.fr",
            namebuild  = "flat@cen",
            block      = self.conf.get("out_block", "prep"),
        )
        print(self.ticket.prompt, "Output init ground temperature =", init_ground_temperature_out)
        print()

    def get_local_inputs(self):
        pass

    def algo(self):
        pass

    def launch_algo(self, algo, **kwargs):
        pass

    def put_outputs(self):
        pass
