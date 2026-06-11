# -*- coding: utf-8 -*-
"""
Created the 14 January 2026
@author: Radanovics S.
"""


from vortex_cen.tasks.research_task_base import _CenResearchTask
import vortex


class ExtractSubPeriod(_CenResearchTask):
    """
    Extract a sub period in a Forcing file

    Inputs:
    --------
    - FORCING file 

    Outputs:
    ---------
    - FORCING file on a shorter period

    Configuration variables:

    :param datebegin: begin date(s) of files
    :param dateend: end date(s) of files

    """

    def get_remote_inputs(self):
        """
        get forcing files in the "massif" geometry, output grid file and interpolation binary.

        """
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def get_local_inputs(self):
        pass

    def algo(self):
        """
        Direct algo for extraction of period
        """
        import xarray as xr

        ds = xr.open_dataset('FORCING_{:s}_{:s}.nc'.format(self.conf.forcing_datebegin, self.conf.forcing_dateend))
        shorter_forcing = ds.sel(time=slice(self.conf.datebegin,self.conf.dateend))
        shorter_forcing.to_netcdf('FORCING_{:s}_{:s}.nc'.format(self.conf.datebegin, self.conf.dateend), format='NETCDF4_CLASSIC')

        return None

    def launch_algo(self, algo):
        """
        launch the algo component.

        :param algo: Algorithm to be launched.
        :type algo: AlgoComponent
        """
        self.launch_python_algo(algo=algo)

    def put_outputs(self):
        """

        """
        if self.conf.forcing_geometry == self.conf.geometry:
            print(self.conf.forcing_geometry, self.conf.geometry)
            raise ValueError("The output 'geometry' can not be the same as the input one.\n"
                             "Please provide two different 'geometry' and 'forcing_geometry' configuration variables")
        else:
            self.sh.title('Toolbox output interpolated forcing file')
            forcing_tbo = vortex.output(
                local       = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment  = self.conf.xpid,
                geometry    = self.conf.geometry,
                datebegin   = self.list_dates_begin,
                dateend     = self.dict_dates_end,
                nativefmt   = 'netcdf',
                kind        = 'MeteorologicalForcing',
                model       = 's2m',
                namespace   = 'vortex.multi.fr',
                namebuild   = 'flat@cen',
                block       = 'meteo',
                member      = self.conf.get('member', None),
            ),
            print(self.ticket.prompt, 'interpolated forcing file toolbox =', forcing_tbo)
            print()
