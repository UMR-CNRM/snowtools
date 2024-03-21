# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from cen.layout.nodes import S2MTaskMixIn


def setup(t, **kw):
    return Driver(
        tag='Diag',
        ticket=t,
        nodes=[
            Diag_sentinel2(tag='diag', ticket=t, **kw),
        ],
        options=kw
    )


class Diag_sentinel2(Task, S2MTaskMixIn):
    '''
    Generic task for the generation of 'hybrid' FORCING files (variables coming from different sources)
    WARNING : TASK IN DEVELOPMENT
    This task will evolve in line with ongoing EDELWEISS developments.
    '''

    def process(self):

        t = self.ticket

        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            # TODO : Meteorological variables that are not yet processed in EDELWEISS system
            # come from SAFRAN reanalysis
            self.sh.title('Toolbox input safran')
            safran = toolbox.input(
                    role        = 'Forcing file',
                    kind        = 'MeteorologicalForcing',
                    vapp        = self.conf.vapp,
                    vconf       = '[geometry:tag]',
                    cutoff      = 'assimilation',
                    filename    = f'mb[member]/FORCING_IN.nc',
                    experiment  = self.conf.safran_xpid,  # TODO : à définir
                    geometry    = self.conf.geometry,
                    nativefmt   = 'netcdf',
                    namebuild   = 'flat@cen',
                    model       = 'edelweiss',
                    date        = self.conf.dateend,
                    datebegin   = self.conf.datebegin,
                    dateend     = self.conf.dateend,
                    namespace   = self.conf.namespace_in,  # TODO : à définir
                    member      = self.conf.members,
                    block       = 'meteo',
                )
            print(t.prompt, 'SAFRAN =', safran)
            print()

            self.sh.title('Toolbox input precipitation')
            precipitation = toolbox.input(
                    role        = 'Precipitation analysis',
                    kind        = 'Precipitation',
                    vapp        = 'edelweiss',
                    vconf       = '[geometry:tag]',
                    source_app  = 'antilope',
                    source_conf = self.conf.source_conf,  # TODO : à définir
                    cutoff      = 'assimilation',
                    filename    = f'mb[member]/PRECIPITATION.nc',
                    experiment  = self.conf.precipitation_xpid,  # TODO : à définir
                    geometry    = self.conf.geometry,
                    nativefmt   = 'netcdf',
                    namebuild   = 'flat@cen',
                    model       = 'edelweiss',
                    date        = self.conf.dateend,
                    datebegin   = self.conf.datebegin,
                    dateend     = self.conf.dateend,
                    namespace   = self.conf.namespace_in,  # TODO : à définir
                    member      = self.conf.members,
                    block       = 'meteo',
                )
            print(t.prompt, 'Precipitation =', precipitation)
            print()

            self.sh.title('Toolbox input wind')
            wind = toolbox.input(
                role        = 'Wind',
                kind        = 'Wind',
                vapp        = self.conf.vapp,
                vconf       = '[geometry:tag]',
                source_app  = 'arome',
                source_conf = 'devine',
                cutoff      = 'assimilation',
                filename    = f'wind/WIND.nc',
                experiment  = self.conf.wind_xpid,  # TODO : à définir
                geometry    = self.conf.geometry,
                nativefmt   = 'netcdf',
                namebuild   = 'flat@cen',
                model       = 'devine',
                date        = self.conf.dateend,
                datebegin   = self.conf.datebegin,
                dateend     = self.conf.dateend,
                namespace   = self.conf.namespace_in,  # TODO : à définir
                block       = 'meteo',
            )
            print(t.prompt, 'Wind =', wind)
            print()

        #######################################################################
        #                            Compute step                             #
        #######################################################################

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo FORCING generator')
            tbalgo = toolbox.algo(
                kind         = 'ForcingGenerator',
                datebegin    = self.conf.datebegin,
                dateend      = self.conf.dateend,
                members      = self.conf.members,
            )
            print(t.prompt, 'tbalgo =', tbalgo)
            print()
            tbalgo.run()

        #######################################################################
        #                               Backup                                #
        #######################################################################

        if 'backup' in self.steps or 'late-backup' in self.steps:

            forcing = toolbox.output(
                    role        = 'Forcing file',
                    kind        = 'MeteorologicalForcing',
                    vapp        = self.conf.vapp,
                    vconf       = '[geometry:tag]',
                    cutoff      = 'assimilation',
                    filename    = f'mb[member]/FORCING_OUT.nc',
                    experiment  = self.conf.forcing_xpid,  # TODO : à définir
                    geometry    = self.conf.geometry,
                    nativefmt   = 'netcdf',
                    namebuild   = 'flat@cen',
                    model       = 'edelweiss',
                    date        = self.conf.dateend,
                    datebegin   = self.conf.datebegin,
                    dateend     = self.conf.dateend,
                    namespace   = self.conf.namespace_out,  # TODO : à définir
                    member      = self.conf.members,
                    block       = 'meteo,
                )
