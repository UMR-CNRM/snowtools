# -*- coding: utf-8 -*-
'''
Created on 7 nov. 2017

@author: lafaysse
'''

import shlex

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import Date, daterange, tomorrow, Period
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
            self.conf.genv = 'uenv:cen.12@CONST_CEN'

        # Definition of geometries, safran xpid/block and list of dates from S2MTaskMixIn methods
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_forc = get_dic_dateend(list_dates_begin_forc, list_dates_end_forc)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)

        dickind = dict(meteo='MeteorologicalForcing', pro='SnowpackSimulation')
        diclocal = dict(meteo='FORCING', pro='PRO')
        dicmodel = dict(meteo=self.conf.meteo, pro='surfex')

        if type(self.conf.interpol_blocks) is not list:
            self.conf.interpol_blocks = [self.conf.interpol_blocks]
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            for block in self.conf.interpol_blocks:

                if 'oper' in self.conf.forcingid or 'OPER' in self.conf.forcingid:
                    tboper = toolbox.input(
                        role='Forcing',
                        kind=dickind[block],
                        vapp=self.conf.meteo,
                        vconf='[geometry:tag]',
                        local=diclocal[block] + '_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment=self.conf.forcingid,
                        block=block,
                        geometry=self.conf.geoin,
                        nativefmt='netcdf',
                        model=dicmodel[block],
                        date=self.conf.dateend.replace(hour=12) + Period(days=4),
                        datebegin=self.conf.datebegin,
                        dateend=self.conf.dateend,
                        cutoff='assimilation',
                        intent='in',
                        namespace='vortex.multi.fr',
                        fatal=True,
                    )

                    print(t.prompt, 'tboper =', tboper)
                    print()

                else:

                    # Try to find a forcing covering the full simulation period
                    # Role = Forcing because expected this way in the algo
                    tb01 = toolbox.input(
                        role           = 'Forcing',
                        kind           = dickind[block],
                        vapp           = self.conf.meteo,
                        vconf          = '[geometry:tag]',
                        local          = diclocal[block] + '_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment     = self.conf.forcingid,
                        block          = block,
                        geometry       = self.conf.geoin,
                        nativefmt      = 'netcdf',
                        model          = dicmodel[block],
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
                            kind           = dickind[block],
                            vapp           = self.conf.meteo,
                            vconf          = '[geometry:tag]',
                            local          = diclocal[block] + '_[datebegin:ymdh]_[dateend:ymdh].nc',
                            experiment     = self.conf.forcingid,
                            block          = block,
                            geometry       = self.conf.geoin,
                            nativefmt      = 'netcdf',
                            model          = dicmodel[block],
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

            if self.conf.namelist:
                tbnamelist = toolbox.input(
                    role            = 'Nam_surfex',
                    remote          = self.conf.namelist,
                    kind            = 'namelist',
                    model           = 'surfex',
                    local           = 'interpolate_safran.nam',
                )

                print(t.prompt, 'tbnamelist =', tbnamelist)
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

            for block in self.conf.interpol_blocks:
                # First we try to save a PRO file covering the whole simulation period if present
                datebegin = self.conf.datebegin
                dateend = self.conf.dateend
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.output(
                    local          = diclocal[block] + '_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin,
                    dateend        = dateend,
                    nativefmt      = 'netcdf',
                    kind           = dickind[block],
                    model          = dicmodel[block],
                    namespace      = namespace,
                    namebuild      = 'flat@cen',
                    block          = block,
                    fatal          = False
                ),
                print(t.prompt, 'tb19 =', tb19)
                print()

                if hasattr(self.conf, "writesx"):
                    if self.conf.writesx:
                        self.sh.title('Toolbox output tb19bis')
                        tb19bis = toolbox.output(
                            local       = diclocal[block] + '_[datebegin:ymdh]_[dateend:ymdh].nc',
                            experiment  = self.conf.xpid,
                            geometry    = self.conf.geometry,
                            datebegin   = datebegin,
                            dateend     = dateend,
                            nativefmt   ='netcdf',
                            kind        = dickind[block],
                            model       = dicmodel[block],
                            namespace   = 'vortex.archive.fr',
                            storage     = 'sxcen.cnrm.meteo.fr',
                            enforcesync = True, # to forbid asynchronous transfers and not saturate sxcen
                            namebuild   = 'flat@cen',
                            block       = block,
                            fatal       = False
                        ),
                        print(t.prompt, 'tb19bis =', tb19bis)
                        print()

                if not tb19[0]:
                    # PRO not available for the whole simulation period: try to save yearly PRO, DIAG, CUMUL
                    # files + 1 PREP file per year
                    self.sh.title('Toolbox output tb19')
                    tb19 = toolbox.output(
                        local          = diclocal[block] + '_[datebegin:ymdh]_[dateend:ymdh].nc',
                        experiment     = self.conf.xpid,
                        geometry       = self.conf.geometry,
                        datebegin      = list_dates_begin_pro,
                        dateend        = dict_dates_end_pro,
                        nativefmt      = 'netcdf',
                        kind           = dickind[block],
                        model          = dicmodel[block],
                        namespace      = namespace,
                        namebuild      = 'flat@cen',
                        block          = block,
                    ),
                    print(t.prompt, 'tb19 =', tb19)
                    print()

                    if hasattr(self.conf, "writesx"):
                        if self.conf.writesx:
                            self.sh.title('Toolbox output tb19bis')
                            tb19bis = toolbox.output(
                                local       = diclocal[block] + '_[datebegin:ymdh]_[dateend:ymdh].nc',
                                experiment  = self.conf.xpid,
                                geometry    = self.conf.geometry,
                                datebegin   = list_dates_begin_pro,
                                dateend     = dict_dates_end_pro,
                                nativefmt   = 'netcdf',
                                kind        = dickind[block],
                                model       = dicmodel[block],
                                namespace   ='vortex.archive.fr',
                                storage     = 'sxcen.cnrm.meteo.fr',
                                enforcesync = True,  # to forbid asynchronous transfers and not saturate sxcen
                                namebuild   = 'flat@cen',
                                block       = block,
                            ),
                            print(t.prompt, 'tb19bis =', tb19bis)
                            print()
