# -*- coding: utf-8 -*-
'''
Created on 7 nov. 2017

@author: lafaysse
'''

import shlex

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import Date, daterange, tomorrow
from cen.layout.nodes import S2MTaskMixIn


def setup(t, **kw):
    return Driver(
        tag='Interpol_Driver',
        ticket=t,
        nodes=[
            Interpol_Task(tag='Interpol_Task', ticket=t, **kw),
        ],
        options=kw
    )


class Interpol_Task(Task, S2MTaskMixIn):
    '''
    Generic task for a MPI Surfex/Crocus reanalysis
    '''

    def process(self):

        t = self.ticket

        if not hasattr(self.conf, "genv"):
            self.conf.genv = 'uenv:cen.11@CONST_CEN'

        # Definition of geometries, safran xpid/block and list of dates from S2MTaskMixIn methods
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_forc = get_dic_dateend(list_dates_begin_forc, list_dates_end_forc)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)

        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            # Try to find a forcing covering the full simulation period
            # Role = Forcing because expected this way in the algo
            tb01 = toolbox.input(
                role           = 'Forcing',
                kind           = 'SnowpackSimulation',
                vapp           = self.conf.meteo,
                vconf          = '[geometry:tag]',
                local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.forcingid,
                block          = 'pro',
                geometry       = self.conf.geoin,
                nativefmt      = 'netcdf',
                model          = 'surfex',
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                intent         = 'in',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                fatal          = False,
            ),

            if tb01[0]:
                onepro = True
            else:
                onepro = False
                # Look for yearly forcing files
                # Role = Forcing because expected this way in the algo
                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
                    role           = 'Forcing',
                    kind           = 'SnowpackSimulation',
                    vapp           = self.conf.meteo,
                    vconf          = '[geometry:tag]',
                    local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.forcingid,
                    block          = 'pro',
                    geometry       = self.conf.geoin,
                    nativefmt      = 'netcdf',
                    model          = 'surfex',
                    datebegin      = list_dates_begin_forc,
                    dateend        = dict_dates_end_forc,
                    intent         = 'in',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    fatal          = True,
                ),

                print(t.prompt, 'tb01 =', tb01)
                print()

            # Target grid file
            # the path must be provided in the configuration file
            tbgrid = toolbox.input(
                role   = 'gridout',
                remote = self.conf.gridout,
                kind   = 'interpolgrid',
                model  = 'surfex',
                local  = 'GRID.nc',
            )
            print(t.prompt, 'tbgrid =', tbgrid)
            print()

            # take the interpolation binary from the uenv
            tbI = tbxi = toolbox.executable(
                role           = 'Binary',
                kind           = 'offline',
                local          = 'INTERPOL',
                model          = 'surfex',
                genv           = self.conf.genv,
                gvar           = 'master_interpol_mpi',
            )

            print(t.prompt, 'tbI =', tbI)
            print()


        #######################################################################
        #                            Compute step                             #
        #######################################################################
        if 'compute' in self.steps:
            # Algo component for interpolation of the forcing on a regular grid
            self.sh.title('Toolbox algo tb09a')
            tb09a = tbalgo1 = toolbox.algo(
                engine         = 'parallel',
                binary         = 'INTERPOL',
                kind           = 'deterministic'
            )
            print(t.prompt, 'tb09a =', tb09a)
            print()

            self.component_runner(tbalgo1, tbxi)


        #######################################################################
        #                               Backup                                #
        #######################################################################
        if 'backup' in self.steps:
            pass

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
                datebegin      = datebegin,
                dateend        = dateend,
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
                        datebegin   = datebegin,
                        dateend     = dateend,
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

            if not tb19[0]:
                # PRO not available for the whole simulation period: try to save yearly PRO, DIAG, CUMUL 
                # files + 1 PREP file per year
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.output(
                    local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = list_dates_begin_pro,
                    dateend        = dict_dates_end_pro,
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
                            datebegin   = list_dates_begin_pro,
                            dateend     = dict_dates_end_pro,
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
