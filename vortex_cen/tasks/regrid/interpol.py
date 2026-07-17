# -*- coding: utf-8 -*-
"""
Created the 14 January 2026
@author: Radanovics S.
"""


from vortex_cen.tasks.research_task_base import _CenResearchTask
import vortex


class InterpolateS2MForcing(_CenResearchTask):
    """
    Interpolate a forcing file in "massif" geometry onto a 2D grid, or 1D grid, that is a list of points.

    Inputs:
    --------
    - FORCING file in the "massif" geometry.
    - GRID file containing the desired output grid.
    - interpolation binary

    Outputs:
    ---------
    - FORCING file on the new grid.

    Configuration variables:
    ------------------------

    * ``forcing_geometry`` geometry of input file
      type: str, footprints.stdtypes.FPList
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

        if self.conf.forcing_geometry == self.conf.geometry:
            print(self.conf.forcing_geometry, self.conf.geometry)
            raise ValueError("The output 'geometry' can not be the same as the input one.\n"
                             "Please provide two different 'geometry' and 'forcing_geometry' configuration variables")

    def get_remote_inputs(self):
        """
        get forcing files in the "massif" geometry, output grid file and interpolation binary.

        """
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

        # Target grid file for interpolation
        # the path must be provided in the configuration file
        self.sh.title('Input definition of the output grid')
        grid_tbi = vortex.input(
            role='gridout',
            kind='interpolgrid',
            model='surfex',
            genv=self.conf.uenv,
            gvar='DEM',
            local='GRID.nc',
        )
        print(self.ticket.prompt, 'toolbox input grid definition file =', grid_tbi)
        print()

        # take the interpolation binary from the uenv
        bin_interpol_tbx = vortex.executable(
            role='Binary',
            kind='offline',
            local='INTERPOL',
            model='surfex',
            genv=self.conf.uenv,
            gvar='master_interpol_mpi',
        )

        print(self.ticket.prompt, 'interpolation binary =', bin_interpol_tbx)
        print()

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Algo component for interpolation of the forcing on a regular grid
        """
        self.sh.title('Toolbox algo interpolation')
        interpolation_tba = vortex.task(
            engine='parallel',
            binary='INTERPOL',
            kind='deterministic',
            reprod_info=dict(genv=self.conf.uenv),
        )
        print(self.ticket.prompt, 'interpolation algo component =', interpolation_tba)
        print()

        return interpolation_tba

    def launch_algo(self, algo):
        """
        launch the algo component.

        :param algo: Algorithm to be launched.
        :type algo: AlgoComponent
        """
        # mpiopts = dict(nnodes=self.conf.nnodes, nprocs=self.conf.nprocs, ntasks=self.conf.ntasks)
        # self.launch_MPI_executable(algo, mpiopts=mpiopts)
        self.launch_executable(algo=algo)

    def put_outputs(self):

        self.sh.title('Toolbox output interpolated forcing file')
        forcing_tbo = vortex.output(
            local       = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment  = self.conf.xpid,
            geometry    = self.conf.geometry,
            datebegin   = self.list_dates_begin,
            dateend     = self.dict_dates_end,
            nativefmt   = 'netcdf',
            kind        = 'MeteorologicalForcing',
            namespace   = self.namespace_out,
            namebuild   = 'flat@cen',
            block       = self.conf.get('out_block', 'interpol'),
            member      = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'interpolated forcing file toolbox =', forcing_tbo)
        print()

    def diff(self):
        """
        Test output reproductibility [OPTIONAL]
        """
        self.sh.title("Reproductibility check : FORCING")
        diff = vortex.diff(
            local       = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment  = self.conf.diff_xpid,
            username    = self.conf.get('diff_user', None),
            geometry    = self.conf.geometry,
            datebegin   = self.list_dates_begin,
            dateend     = self.dict_dates_end,
            nativefmt   = 'netcdf',
            kind        = 'MeteorologicalForcing',
            namespace   = self.namespace_out,
            namebuild   = 'flat@cen',
            block       = self.conf.get('diff_block', 'interpol'),
            member      = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'diff =', diff)
        print()
