# -*- coding: utf-8 -*-
"""
interpol.py
-----------

.. autoclass:: InterpolateS2MForcing
   :no-members:
   :class-doc-from: class
   :show-inheritance:

.. autoclass:: InterpolMixIn
   :members:
   :show-inheritance:

.. autoclass:: InterpolateS2MLocalForcing
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""

import vortex

from vortex_cen.tasks.research_task_base import _CenResearchTask


class InterpolMixIn:
    """
    Methods for interpolation task
    """

    def get_output_grid_definition(self):
        """
        get output grid file.

        **Configuration variables used:**

        * ``uenv``
        * ``gridout`` The variable name of the output grid file in the uenv (gvar). Default is *DEM*.
        """
        # Target grid file for interpolation
        # the path must be provided in the configuration file
        self.sh.title("Input definition of the output grid")
        grid_tbi = vortex.input(
            role="gridout",
            kind="interpolgrid",
            model="surfex",
            genv=self.conf.uenv,
            gvar=self.conf.get("gridout", "DEM"),
            local="GRID.nc",
        )
        print(self.ticket.prompt, "toolbox input grid definition file =", grid_tbi)
        print()

    def get_interpolation_binary(self):
        """
        Get the interpolation binary from the uenv

        **Configuration variables used:**

        * ``uenv``
        """
        #
        bin_interpol_tbx = vortex.executable(
            role="Binary",
            kind="offline",
            local="INTERPOL",
            model="surfex",
            genv=self.conf.uenv,
            gvar="master_interpol_mpi",
        )

        print(self.ticket.prompt, "interpolation binary =", bin_interpol_tbx)
        print()


class InterpolateS2MForcing(_CenResearchTask, InterpolMixIn):
    """
    Interpolate a forcing file in "massif" geometry onto a 2D grid, or 1D grid, that is a list of points.

    **Inputs:**

    - FORCING file in the "massif" geometry.
    - GRID file containing the desired output grid.
    - interpolation binary

    **Outputs:**

    - FORCING file on the new grid.

    **Configuration variables:**

    **Mandatory**

    * ``forcing_datebegin`` *datebegin* footprint, default self.conf.datebegin
      type forcing_datebegin: str, footprints.stdtypes.FPList
    * ``forcing_dateend`` *dateend* footprint, default self.conf.dateend
      type forcing_dateend: str, footprints.stdtypes.FPList
    * ``forcing_xpid`` Experiment identifier, default self.conf.xpid
      type forcing_xpid: str
    * ``forcing_geometry`` geometry of input file
      type: str, footprints.stdtypes.FPList
    * ``forcing_block`` *block* footprint, default "meteo"
      type forcing_vconf: str
    * ``gridout`` path to output grid file
      type: str, pathlike
    * ``uenv`` environment containing the interpolation executable
    * ``xpid`` Experiment identifier
      type: str
    * ``geometry`` Geometry of the output file(s)
      type: str
    * ``datebegin`` begin date(s) of files
    * ``dateend`` end date(s) of files
    * ``namespace_out`` namespace of output files

    **Optional**

    * ``forcing_vapp`` *vapp* footprint, default self.conf.vapp
      type forcing_vapp: str
    * ``forcing_vconf`` *vconf* footprint, default self.conf.vconf
      type forcing_vconf: str
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
    * ``out_block`` block part of the output directory. Default: *interpol*
    * ``diff_xpid`` Experiment identifier of the reference experiment in case of reproducibility check
    * ``diff_user`` vortex user of the reference experiment
    * ``diff_block`` block part of the reference file directory. Default: *interpol*

    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "forcing_datebegin|datebegin",
            "forcing_dateend|dateend",
            "forcing_xpid",
            "xpid",
            "forcing_geometry+help=A SAFRAN massif geometry",
            "forcing_block",
            "geometry",
            "uenv+help=Name of the UEnv containing the DEM file and interpolator executable",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "member",
            "out_block+default=interpol",
            "diff_xpid",
            "diff_user",
            "diff_block+default=interpol",
        ]
        overwrite = [
            "datebegin",
            "dateend",
        ]
        super().__init__(**kw)

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES,
                overwrite=overwrite)

        # MF: during initialisation, self.conf is None
        # -> there is no attribute 'forcing_geometry', the check under should be done in another way
        #
        # if self.conf.forcing_geometry == self.conf.geometry:
        #    print(self.conf.forcing_geometry, self.conf.geometry)
        #    raise ValueError("The output 'geometry' can not be the same as the input one.\n"
        #                     "Please provide two different 'geometry' and 'forcing_geometry' configuration variables")

    def get_remote_inputs(self):
        """
        get forcing files in the "massif" geometry, output grid file and interpolation binary.

        """
        if self.conf.forcing_geometry == self.conf.geometry:
            print(self.conf.forcing_geometry, self.conf.geometry)
            raise ValueError("The output 'geometry' can not be the same as the input one.\n"
                             "Please provide two different 'geometry' and 'forcing_geometry' configuration variables")

        self.get_output_grid_definition()
        self.get_interpolation_binary()
        self.get_forcing(localname="FORCING_[datebegin:ymdh]_[dateend:ymdh].nc")

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Algo component for interpolation of the forcing on a regular grid
        """
        self.sh.title("Toolbox algo interpolation")
        interpolation_tba = vortex.task(
            engine="parallel",
            binary="INTERPOL",
            kind="deterministic",
            reprod_info=dict(genv=self.conf.uenv),
        )
        print(self.ticket.prompt, "interpolation algo component =", interpolation_tba)
        print()

        return interpolation_tba

    def launch_algo(self, algo, **kwargs):
        """
        launch the algo component.

        :param algo: Algorithm to be launched.
        :type algo: AlgoComponent
        :param kwargs: Keyword arguments to be passed to the algo component. Not used.
        """
        # mpiopts = dict(nnodes=self.conf.nnodes, nprocs=self.conf.nprocs, ntasks=self.conf.ntasks)
        # self.launch_MPI_executable(algo, mpiopts=mpiopts)
        self.launch_executable(algo=algo)

    def put_outputs(self):

        self.sh.title("Toolbox output interpolated forcing file")
        forcing_tbo = (
            vortex.output(
                local="FORCING_[datebegin:ymdh]_[dateend:ymdh].nc",
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                datebegin=self.conf.datebegin,
                dateend=self.conf.dateend,
                nativefmt="netcdf",
                kind="MeteorologicalForcing",
                model="s2m",
                namespace=self.namespace_out,
                namebuild="flat@cen",
                block=self.conf.get("out_block", "interpol"),
                member=self.conf.get("member", None),
            ),
        )
        print(self.ticket.prompt, "interpolated forcing file toolbox =", forcing_tbo)
        print()

    def diff(self):
        """
        Test output reproductibility [OPTIONAL]
        """
        self.sh.title("Reproductibility check : FORCING")
        diff = vortex.diff(
                local="FORCING_[datebegin:ymdh]_[dateend:ymdh].nc",
                experiment=self.conf.diff_xpid,
                username=self.conf.get("diff_user", None),
                geometry=self.conf.geometry,
                datebegin=self.conf.datebegin,
                dateend=self.conf.dateend,
                nativefmt="netcdf",
                kind="MeteorologicalForcing",
                model="s2m",
                namespace=self.namespace_out,
                namebuild="flat@cen",
                block=self.conf.get("diff_block", "interpol"),
                member=self.conf.get("member", None),
            ),
        print(self.ticket.prompt, "diff =", diff)
        print()


class InterpolateS2MLocalForcing(InterpolateS2MForcing):
    """
    Interpolate a forcing file in "massif" geometry onto a 2D grid, or 1D grid, that is a list of points.

    **Inputs:**

    - Local FORCING file in the "massif" geometry.
    - GRID file containing the desired output grid.
    - interpolation binary

    **Outputs:**

    - FORCING file on the new grid.

    **Configuration variables:**

    **Mandatory**

    * ``forcing_datebegin`` *datebegin* footprint, default self.conf.datebegin
      type forcing_datebegin: str, footprints.stdtypes.FPList
    * ``forcing_dateend`` *dateend* footprint, default self.conf.dateend
      type forcing_dateend: str, footprints.stdtypes.FPList
    * ``forcing_xpid`` Experiment identifier, default self.conf.xpid
      type forcing_xpid: str
    * ``forcing_geometry`` geometry of input file
      type: str, footprints.stdtypes.FPList
    * ``forcing_block`` *block* footprint, default "meteo"
      type forcing_vconf: str
    * ``gridout`` path to output grid file
      type: str, pathlike
    * ``uenv`` environment containing the interpolation executable
    * ``xpid`` Experiment identifier
      type: str
    * ``geometry`` Geometry of the output file(s)
      type: str
    * ``datebegin`` begin date(s) of files
    * ``dateend`` end date(s) of files
    * ``namespace_out`` namespace of output files

    **Optional**

    * ``forcing_vapp`` *vapp* footprint, default self.conf.vapp
      type forcing_vapp: str
    * ``forcing_vconf`` *vconf* footprint, default self.conf.vconf
      type forcing_vconf: str
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
    * ``out_block`` block part of the output directory. Default: *interpol*
    * ``diff_xpid`` Experiment identifier of the reference experiment in case of reproducibility check
    * ``diff_user`` vortex user of the reference experiment
    * ``diff_block`` block part of the reference file directory. Default: *interpol*

    """
    def get_remote_inputs(self):
        """
        get output grid file and interpolation binary.

        """
        self.get_output_grid_definition()
        self.get_interpolation_binary()

    def get_local_inputs(self):
        """
        FORCING can come from local cache because just a subpart of yearly forcing is used.
        """
        self.get_forcing(localname="FORCING_[datebegin:ymdh]_[dateend:ymdh].nc")
