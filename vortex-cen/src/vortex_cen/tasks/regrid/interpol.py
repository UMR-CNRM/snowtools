# -*- coding: utf-8 -*-
"""
Created the 14 January 2026
@author: Radanovics S.
"""


from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex import toolbox


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

    :param geoin: geometry of input file
    :type geoin: str, footprints.stdtypes.FPList
    :param gridout: path to output grid file
    :type gridout: str, pathlike
    :param genv: environment containing the interpolation executable
    :param xpid: Experiment identifier (format "experiment_name@user")
    :type xpid: str
    :param geometry: Geometry of the output file(s)
    :type geometry: str
    :param datebegin: begin date(s) of files
    :param dateend: end date(s) of files
    :param namespace_out: namespace of output files
    """


    def get_remote_inputs(self):
        """
        get forcing files in the "massif" geometry, output grid file and interpolation binary.

        """
        self.get_forcing(forcing_geometry=self.conf.geometry_in, localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

        # Target grid file for interpolation
        # the path must be provided in the configuration file
        self.sh.title('Toolbox input output grid definition')
        grid_tbi = toolbox.input(
            role='gridout',
            kind='interpolgrid',
            model='surfex',
            genv=self.conf.genv,
            gvar='DEM',
            local='GRID.nc',
        )
        print(self.ticket.prompt, 'toolbox input grid definition file =', grid_tbi)
        print()

        # take the interpolation binary from the uenv
        bin_interpol_tbx = toolbox.executable(
            role='Binary',
            kind='offline',
            local='INTERPOL',
            model='surfex',
            genv=self.conf.genv,
            gvar='master_interpol_mpi',
        )

        print(self.ticket.prompt, 'interpolation binary =', bin_interpol_tbx)
        print()

    def algo(self):
        """

        """
        # Algo component for interpolation of the forcing on a regular grid
        self.sh.title('Toolbox algo interpolation')
        interpolation_tba = toolbox.algo(
            engine='parallel',
            binary='INTERPOL',
            kind='deterministic'
        )
        print(self.ticket.prompt, 'interpolation algo component =', interpolation_tba)
        print()

        return interpolation_tba

    def put_remote_outputs(self):
        """

        """
        if self.conf.geometry_in == self.conf.geometry:
            raise ValueError("The 'out_geometry' can not be the same as the input one.\n"
                             "Please provide a different 'out_geometry' configuration variable")
        else:
            self.sh.title('Toolbox output interpolated forcing file')
            forcing_tbo = toolbox.output(
                local='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment=self.conf.xpid,
                geometry=self.conf.geometry,
                datebegin=self.list_dates_begin,
                dateend=self.dict_dates_end,
                nativefmt='netcdf',
                kind='MeteorologicalForcing',
                model='s2m',
                namespace=self.conf.namespace_out,
                namebuild='flat@cen',
                block='meteo',
                member=self.conf.member if hasattr(self.conf, 'member') else None,
            ),
            print(self.ticket.prompt, 'interpolated forcing file toolbox =', forcing_tbo)
            print()