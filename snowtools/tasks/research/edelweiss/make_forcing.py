# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from cen.layout.nodes import S2MTaskMixIn

import footprints


def setup(t, **kw):
    return Driver(
        tag='forcing',
        ticket=t,
        nodes=[
            Forcing(tag='forcing', ticket=t, **kw),
        ],
        options=kw
    )


class Forcing(Task, S2MTaskMixIn):
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
                filename    = 'FORCING_IN.nc',
                experiment  = self.conf.safran_xpid,
                geometry    = self.conf.geometry,
                nativefmt   = 'netcdf',
                namebuild   = 'flat@cen',
                model       = 'edelweiss',
                date        = self.conf.dateend,
                datebegin   = self.conf.datebegin,
                dateend     = self.conf.dateend,
                namespace   = self.conf.namespace_in,
                storage     = self.conf.storage,
                block       = 'meteo',
            )
            print(t.prompt, 'SAFRAN =', safran)
            print()

            # TODO : Cette verrue montre que le source_conf est inutile
            # A retirer dans les toolbox de Sabine
            conf_map    = dict(
                RS   = 'RandomSampling',
                EnKF = 'EnsembleKalmanFilter',
                PF   = 'APrticleFilter',

            )
            if self.conf.precipitation_xpid.split('@')[0][:-2] in conf_map.keys():
                source_conf = conf_map[self.conf.precipitation_xpid.split('@')[0][:-2]]
            else:
                source_conf = None

            self.sh.title('Toolbox input precipitation')
            precipitation = toolbox.input(
                role        = 'Precipitation',
                kind        = 'Precipitation',
                vapp        = 'edelweiss',
                vconf       = '[geometry:tag]',
                source_app  = 'antilope',
                source_conf = source_conf,
                cutoff      = 'assimilation',
                filename    = 'mb[member]/PRECIPITATION.nc',
                experiment  = self.conf.precipitation_xpid,
                geometry    = self.conf.geometry,
                nativefmt   = 'netcdf',
                namebuild   = 'flat@cen',
                model       = 'edelweiss',
                date        = self.conf.dateend,
                datebegin   = self.conf.datebegin.replace(day=2),  # TODO : virer cette verrue
                dateend     = self.conf.dateend,
                namespace   = self.conf.namespace_in,
                storage     = self.conf.storage,
                member      = footprints.util.rangex(self.conf.members),
                # TODO : change block to 'meteo' in Sabine's task
                # block       = 'meteo',
                block       = 'analysis',
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
                filename    = 'WIND.nc',
                experiment  = self.conf.wind_xpid,
                geometry    = self.conf.geometry,
                nativefmt   = 'netcdf',
                namebuild   = 'flat@cen',
                model       = 'devine',
                date        = self.conf.dateend,
                datebegin   = self.conf.datebegin.replace(month=7, day=31),  # TODO : virer cette verrue
                dateend     = self.conf.dateend,
                namespace   = self.conf.namespace_in,
                storage     = self.conf.storage,
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
                kind         = 'ForcingConstructor',
                datebegin    = self.conf.datebegin,
                dateend      = self.conf.dateend,
                engine       = 'algo',  #  `_CENTaylorRun` algo components familly
                members      = self.conf.members,
                ntasks       = self.conf.ntasks,
                role_members = 'Precipitation',
            )
            print(t.prompt, 'tbalgo =', tbalgo)
            print()
            tbalgo.run()

        #######################################################################
        #                               Backup                                #
        #######################################################################

        if 'backup' in self.steps or 'late-backup' in self.steps:

            # TODO : différencier le membre 0 (ANTILOPE Post-traité)
            # des autres membres (avec perturbation)
            # Ou bien conserver ANTILOPE PT comme le membre 0 et adapter plutot les tâches aval ?
            self.sh.title('Toolbox ouput FORCING')
            forcing = toolbox.output(
                role        = 'Forcing file',
                kind        = 'MeteorologicalForcing',
                vapp        = self.conf.vapp,
                vconf       = '[geometry:tag]',
                cutoff      = 'assimilation',
                filename    = 'mb[member]/FORCING_OUT.nc',
                experiment  = self.conf.xpid,
                geometry    = self.conf.geometry,
                nativefmt   = 'netcdf',
                namebuild   = 'flat@cen',
                model       = 'edelweiss',
                date        = self.conf.dateend,
                datebegin   = self.conf.datebegin,
                dateend     = self.conf.dateend,
                namespace   = self.conf.namespace_out,
                storage     = self.conf.storage,
                member      = footprints.util.rangex(self.conf.members),
                block       = 'meteo',
            )
            print(t.prompt, 'FORCING =', forcing)
            print()
