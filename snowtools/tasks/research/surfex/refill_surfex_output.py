# -*- coding: utf-8 -*-
'''
Created on 15 January 2024

@author: Vernay

Refill task to use in the case when a simulation went well and the output have been stored in the local
cache but not on Hendrix (for example after un password update).
This task can be called with the exact same s2m command by adding a --refill option.
It retrieves the ressources produced by the previous simulation on the local cache and put them on Hendrix
as intended initially.
'''

import shlex

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import Date, daterange, tomorrow
from cen.layout.nodes import S2MTaskMixIn


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            Surfex_Vortex_Task(tag='Surfex_Vortex_Task', ticket=t, **kw),
        ],
        options=kw
    )


class Surfex_Vortex_Task(Task, S2MTaskMixIn):
    '''
    Generic task for a MPI Surfex/Crocus reanalysis
    '''

    def process(self):

        t = self.ticket

        if not hasattr(self.conf, "genv"):
            self.conf.genv = 'uenv:cen.10@CONST_CEN'

        # Definition of geometries, safran xpid/block and list of dates from S2MTaskMixIn methods
        list_geometry = self.get_list_geometry(meteo=self.conf.meteo)
        source_safran, block_safran = self.get_source_safran(meteo=self.conf.meteo)
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_forc = get_dic_dateend(list_dates_begin_forc, list_dates_end_forc)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)

        if not hasattr(self.conf, "era5"):
            self.era5 = 'era5' in self.conf.forcingid
        dict_source_app_safran, dict_source_conf_safran = self.get_safran_sources(list_dates_begin_forc, era5=self.era5)

        # Logicals to activate optional parts of the task
        if not hasattr(self.conf, "interpol"):
            self.conf.interpol = False
        if not hasattr(self.conf, "climground"):
            self.conf.climground = False
        if not hasattr(self.conf, "dailyprep"):
            self.conf.dailyprep = False
        if not hasattr(self.conf, "simu2D"):
            self.conf.simu2D = False
        if hasattr(self.conf, "simu2D"):
            self.conf.genv2D = 'uenv:pgd.002@SURFEX_CEN'

        if 'early-fetch' in self.steps:

            namespace = 'vortex.cache.fr'
            if hasattr(self.conf, 'save_pro') and self.conf.save_pro in ['cache', 'archive', 'multi']:
                namespace = 'vortex.' + self.conf.save_pro + '.fr'

            # First we try to save a PRO file covering the whole simulation period if present
            datebegin = self.conf.datebegin
            dateend = self.conf.dateend
            self.sh.title('Toolbox output tb19')
            tb19 = toolbox.input(
                local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                dateend        = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                        dateend)),
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = namespace,
                namebuild      = 'flat@cen',
                block          = 'pro',
                fatal          = False
            ),
            print(t.prompt, 'tb19 =', tb19)
            print()

            if hasattr(self.conf, "writesx"):
                if self.conf.writesx:
                    self.sh.title('Toolbox output tb19bis')
                    tb19bis = toolbox.input(
                        local       = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment  = self.conf.xpid,
                        geometry    = self.conf.geometry,
                        datebegin   = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                        dateend     = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                             dateend)),
                        nativefmt   ='netcdf',
                        kind        = 'SnowpackSimulation',
                        model       = 'surfex',
                        namespace   = 'vortex.archive.fr',
                        storage     = 'sxcen.cnrm.meteo.fr',
                        enforcesync = True, # to forbid asynchronous transfers and not saturate sxcen
                        namebuild   = 'flat@cen',
                        block       = 'pro',
                        fatal       = False
                    ),
                    print(t.prompt, 'tb19bis =', tb19bis)
                    print()


            if tb19[0]:
                # Only one pro file for the whole simulation period
                # Save only one cumul and diag file covering the whole simulation
                # Save only one prep at the end of the simulation

                self.sh.title('Toolbox output tb19')
                tb19bis = toolbox.input(
                    local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                            dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'cumul',
                    fatal          = False
                ),
                print(t.prompt, 'tb19bis =', tb19bis)
                print()

                tb19ter = toolbox.input(
                    local          = 'DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                            dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'diag',
                    fatal          = False
                ),
                print(t.prompt, 'tb19 =', tb19ter)
                print()

                self.sh.title('Toolbox output tb20')
                tb20 = toolbox.input(
                    local          = 'PREP_[date:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    date           = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                            dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep',
                ),
                print(t.prompt, 'tb20 =', tb20)
                print()

            else:
                # PRO not available for the whole simulation period: try to save yearly PRO, DIAG, CUMUL 
                # files + 1 PREP file per year
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.input(
                    local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dict_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = namespace,
                    namebuild      = 'flat@cen',
                    block          = 'pro',
                ),
                print(t.prompt, 'tb19 =', tb19)
                print()

                if hasattr(self.conf, "writesx"):
                    if self.conf.writesx:
                        self.sh.title('Toolbox output tb19bis')
                        tb19bis = toolbox.input(
                            local       = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                            experiment  = self.conf.xpid,
                            geometry    = self.conf.geometry,
                            datebegin   = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                            dateend     = dict_dates_end_pro if not self.conf.dailyprep else \
                                          list(daterange(tomorrow(base=datebegin), dateend)),
                            nativefmt   = 'netcdf',
                            kind        = 'SnowpackSimulation',
                            model       = 'surfex',
                            namespace   ='vortex.archive.fr',
                            storage     = 'sxcen.cnrm.meteo.fr',
                            enforcesync = True,  # to forbid asynchronous transfers and not saturate sxcen
                            namebuild   = 'flat@cen',
                            block       = 'pro',
                        ),
                        print(t.prompt, 'tb19bis =', tb19bis)
                        print()

                tb19b = toolbox.input(
                    local          = 'DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dict_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'diag',
                    fatal          = False,
                ),
                print(t.prompt, 'tb19b =', tb19b)
                print()

                tb19c = toolbox.input(
                    local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dict_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'cumul',
                    fatal          = False,
                ),
                print(t.prompt, 'tb19c =', tb19c)
                print()

                self.sh.title('Toolbox output tb20')
                tb20 = toolbox.input(
                    local          = 'PREP_[date:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    date           = list_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep',
                ),
                print(t.prompt, 'tb20 =', tb20)
                print()

# The following condition does not work. --> Ask leffe how to do
#                 if not (tb02[0] or tb02_a[0]):
            # Save the PGD file for this xpid and geometry
            tb21 = toolbox.input(
                role           = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'pgd'),
            print(t.prompt, 'tb21 =', tb21)
            print()

            # Save the climatological file for this xpid and geometry if built during this task
            if self.conf.climground:
                tb22 = toolbox.input(
                    role           = 'initial values of ground temperature',
                    kind           = 'climTG',
                    nativefmt      = 'netcdf',
                    local          = 'init_TG.nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep'),
                print(t.prompt, 'tb22 =', tb22)
                print()

            # Save the forcing files only if they have been converted towards a new geometry.
            if self.conf.meteo == 'safran' or self.conf.interpol:
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.input(
                    local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_forc,
                    dateend        = dict_dates_end_forc,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'meteo',
                ),
                print(t.prompt, 'tb19 =', tb19)
                print()

            self.sh.title('Toolbox output tb20 (profiling)')
            tb20 = toolbox.input(
                kind       = "drhook",
                mpi        = "[glob:n]",
                task       = self.tag,
                block      = "profiling",
                experiment = self.conf.xpid,
                model      = 'surfex',
                local      = "drhook.prof.{glob:n:\d+}",
                format     = "ascii",
            ),
            print(t.prompt, 'tb20 =', tb20)
            print()


        if 'late-backup' in self.steps:

            namespace = 'vortex.multi.fr'
            if hasattr(self.conf, 'save_pro') and self.conf.save_pro in ['cache', 'archive', 'multi']:
                namespace = 'vortex.' + self.conf.save_pro + '.fr'

            # First we try to save a PRO file covering the whole simulation period if present
            datebegin = self.conf.datebegin
            dateend = self.conf.dateend
            self.sh.title('Toolbox output tb19')
            tb19 = toolbox.output(
                local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                dateend        = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                        dateend)),
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = namespace,
                namebuild      = 'flat@cen',
                block          = 'pro',
                fatal          = False
            ),
            print(t.prompt, 'tb19 =', tb19)
            print()

            if hasattr(self.conf, "writesx"):
                if self.conf.writesx:
                    self.sh.title('Toolbox output tb19bis')
                    tb19bis = toolbox.output(
                        local       = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment  = self.conf.xpid,
                        geometry    = self.conf.geometry,
                        datebegin   = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                        dateend     = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                             dateend)),
                        nativefmt   ='netcdf',
                        kind        = 'SnowpackSimulation',
                        model       = 'surfex',
                        namespace   = 'vortex.archive.fr',
                        storage     = 'sxcen.cnrm.meteo.fr',
                        enforcesync = True, # to forbid asynchronous transfers and not saturate sxcen
                        namebuild   = 'flat@cen',
                        block       = 'pro',
                        fatal       = False
                    ),
                    print(t.prompt, 'tb19bis =', tb19bis)
                    print()


            if tb19[0]:
                # Only one pro file for the whole simulation period
                # Save only one cumul and diag file covering the whole simulation
                # Save only one prep at the end of the simulation

                self.sh.title('Toolbox output tb19')
                tb19bis = toolbox.output(
                    local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                            dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'cumul',
                    fatal          = False
                ),
                print(t.prompt, 'tb19bis =', tb19bis)
                print()

                tb19ter = toolbox.output(
                    local          = 'DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                            dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'diag',
                    fatal          = False
                ),
                print(t.prompt, 'tb19 =', tb19ter)
                print()

                self.sh.title('Toolbox output tb20')
                tb20 = toolbox.output(
                    local          = 'PREP_[date:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    date           = dateend if not self.conf.dailyprep else list(daterange(tomorrow(base=datebegin),
                                                                                            dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep',
                ),
                print(t.prompt, 'tb20 =', tb20)
                print()

            else:
                # PRO not available for the whole simulation period: try to save yearly PRO, DIAG, CUMUL 
                # files + 1 PREP file per year
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.output(
                    local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dict_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = namespace,
                    namebuild      = 'flat@cen',
                    block          = 'pro',
                ),
                print(t.prompt, 'tb19 =', tb19)
                print()

                if hasattr(self.conf, "writesx"):
                    if self.conf.writesx:
                        self.sh.title('Toolbox output tb19bis')
                        tb19bis = toolbox.output(
                            local       = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                            experiment  = self.conf.xpid,
                            geometry    = self.conf.geometry,
                            datebegin   = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                            dateend     = dict_dates_end_pro if not self.conf.dailyprep else \
                                          list(daterange(tomorrow(base=datebegin), dateend)),
                            nativefmt   = 'netcdf',
                            kind        = 'SnowpackSimulation',
                            model       = 'surfex',
                            namespace   ='vortex.archive.fr',
                            storage     = 'sxcen.cnrm.meteo.fr',
                            enforcesync = True,  # to forbid asynchronous transfers and not saturate sxcen
                            namebuild   = 'flat@cen',
                            block       = 'pro',
                        ),
                        print(t.prompt, 'tb19bis =', tb19bis)
                        print()

                tb19b = toolbox.output(
                    local          = 'DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dict_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'diag',
                    fatal          = False,
                ),
                print(t.prompt, 'tb19b =', tb19b)
                print()

                tb19c = toolbox.output(
                    local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
                    dateend        = dict_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'cumul',
                    fatal          = False,
                ),
                print(t.prompt, 'tb19c =', tb19c)
                print()

                self.sh.title('Toolbox output tb20')
                tb20 = toolbox.output(
                    local          = 'PREP_[date:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    date           = list_dates_end_pro if not self.conf.dailyprep else \
                                     list(daterange(tomorrow(base=datebegin), dateend)),
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep',
                ),
                print(t.prompt, 'tb20 =', tb20)
                print()

# The following condition does not work. --> Ask leffe how to do
#                 if not (tb02[0] or tb02_a[0]):
            # Save the PGD file for this xpid and geometry
            tb21 = toolbox.output(
                role           = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'pgd'),
            print(t.prompt, 'tb21 =', tb21)
            print()

            # Save the climatological file for this xpid and geometry if built during this task
            if self.conf.climground:
                tb22 = toolbox.output(
                    role           = 'initial values of ground temperature',
                    kind           = 'climTG',
                    nativefmt      = 'netcdf',
                    local          = 'init_TG.nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep'),
                print(t.prompt, 'tb22 =', tb22)
                print()

            # Save the forcing files only if they have been converted towards a new geometry.
            if self.conf.meteo == 'safran' or self.conf.interpol:
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.output(
                    local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_forc,
                    dateend        = dict_dates_end_forc,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'meteo',
                ),
                print(t.prompt, 'tb19 =', tb19)
                print()

            self.sh.title('Toolbox output tb20 (profiling)')
            tb20 = toolbox.output(
                kind       = "drhook",
                mpi        = "[glob:n]",
                task       = self.tag,
                block      = "profiling",
                experiment = self.conf.xpid,
                model      = 'surfex',
                local      = "drhook.prof.{glob:n:\d+}",
                format     = "ascii",
            ),
            print(t.prompt, 'tb20 =', tb20)
            print()
