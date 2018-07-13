#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 22 mai 2018

@author: cluzetb
task for running SODA-SNOW sequence on HPC
'''

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from bronx.stdtypes.date import Date
from utils.dates import get_list_dates_files, check_and_convert_date
import footprints
import os


def setup(t, **kw):
    return Driver(
        tag = 'Surfex_Parallel',
        ticket = t,
        nodes = [
            SodaSnow_Vortex_Task(tag='SodaSnow_Vortex_Task', ticket=t, **kw),
        ],
        options=kw
    )


class SodaSnow_Vortex_Task(Task):
    '''

    '''

    def process(self):

        t = self.ticket
        assDates = []

        try:
            for dt in self.conf.assimdates:
                dt = Date(check_and_convert_date(str(dt)))
                if dt < self.conf.dateend and dt > self.conf.datebegin:
                    assDates.append(dt)
            print("assimilation sequence, dates")
            print(assDates)
        except AttributeError:
            print('openloop simulation whitout pauses')

        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration, assDates)
        startmember = int(self.conf.startmember) if hasattr(self.conf, "startmember") else 1
        members = list(range(startmember, int(self.conf.nmembers) + startmember)) if hasattr( self.conf, "nmembers") else list(range(1, 36))

        if 'early-fetch' in self.steps or 'fetch' in self.steps:
            # we fetch forcing files into the members directories using the remainder function %
            # since we usually have more members than forcing files, we loop over forcing files
            # -> some will be used more than others
            print(self.conf.vapp)
            print(self.conf.vconf)

            forcExp = self.conf.forcing + '@' + os.environ['USER']
            for mb in members:
                # we fetch forcing files into the members directories using the remainder function %
                gg = mb % int(self.conf.nforcing)
                if gg != 0:
                    forcingdir = gg
                else:
                    forcingdir = int(self.conf.nforcing)
                print(forcingdir)
                for p, datebegin in enumerate(list_dates_begin_forc):
                    dateend = list_dates_end_forc[p]

                    self.sh.title('Toolbox input tb01')

                    # this has to be changed to include an ensemble of forcing files
                    tb01 = toolbox.input(
                        role           = 'Forcing',
                        local          = 'mb{0:04d}'.format(mb) + '/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                        vapp           = self.conf.meteo,
                        experiment     = forcExp,
                        member         = '{0:04d}'.format(forcingdir),
                        geometry       = self.conf.geometry,
                        datebegin      = datebegin,
                        dateend        = dateend,
                        nativefmt      = 'netcdf',
                        kind           = 'MeteorologicalForcing',
                        model          = 'safran',
                        namespace      = 'cenvortex.multi.fr',
                    ),
                    print(t.prompt, 'tb01 =', tb01)
                    print()

            self.sh.title('Toolbox input tb02')  # normal to fail if pgd in diroutput
            tb02 = toolbox.input(
                role           = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                geometry       = self.conf.geometry,
                genv            = 'uenv:cen.01@CONST_CEN',
                gvar           = 'pgd_[geometry::area]',
                model          = 'surfex',
                fatal          = False,
            ),
            print(t.prompt, 'tb02 =', tb02)
            print()

            self.sh.title('Toolbox input tb02_a')  # normal to fail if pgd in diroutput
            tb02_a = toolbox.input(
                alternate      = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                model          = 'surfex',
                namespace      = 'cenvortex.multi.fr',
                fatal          = False,
            ),
            print(t.prompt, 'tb02_a =', tb02_a)
            print()

            self.sh.title('Toolbox input tb02_s')  # this step should work if PGD properly in spinup on hendrix
            tb02_s = toolbox.input(
                alternate      = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                experiment     = 'spinup@' + os.environ['USER'],
                geometry       = self.conf.geometry,
                model          = 'surfex',
                namespace      = 'cenvortex.multi.fr',
                fatal          = False,
            ),
            print(t.prompt, 'tb02_s =', tb02_s)
            print()

            self.sh.title('Toolbox input tb03')
            tb03 = toolbox.input(
                role           = 'SnowpackInit',
                local          = 'PREP.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                date           = self.conf.datespinup,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'SnowpackState',
                model          = 'surfex',
                namespace      = 'cenvortex.multi.fr',
                fatal          = False,
            ),
            print(t.prompt, 'tb03 =', tb03)
            print()

            self.sh.title('Toolbox input tb03_s')
            tb03_s = toolbox.input(
                alternate      = 'SnowpackInit',
                local          = 'PREP.nc',
                experiment     = 'spinup@' + os.environ['USER'],
                geometry       = self.conf.geometry,
                date           = self.conf.datespinup,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'SnowpackState',
                model          = 'surfex',
                namespace      = 'cenvortex.multi.fr',
                fatal          = False,
            ),
            print(t.prompt, 'tb03_s =', tb03_s)
            print()

            self.sh.title('Toolbox input tb03b')
            tb03b = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapI_covers_param.bin',
                geometry       = 'alp_allslopes',
                genv           = 'uenv:cen.01@CONST_CEN',
                source         = 'ecoclimap1',
                model          = 'surfex',
            ),
            print(t.prompt, 'tb03b =', tb03b)
            print()

            self.sh.title('Toolbox input tb03c')
            tb03c = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapII_eu_covers_param.bin',
                geometry       = 'alp_allslopes',
                genv           = 'uenv:cen.01@CONST_CEN',
                source         = 'ecoclimap2',
                model          = 'surfex',
            ),
            print(t.prompt, 'tb03c =', tb03c)
            print()

            self.sh.title('Toolbox input tb04')
            tb04 = toolbox.input(
                role            = 'Parameters for F06 metamorphism',
                kind            = 'ssa_params',
                genv            = 'uenv:cen.01@CONST_CEN',
                nativefmt       = 'netcdf',
                local           = 'drdt_bst_fit_60.nc',
                model          = 'surfex',
            )
            print(t.prompt, 'tb04 =', tb04)
            print()

            self.sh.title('Toolbox input tb05')
            tb05 = toolbox.input(
                role            = 'Nam_surfex',
                remote          = self.conf.namelist,
                kind            = 'namelist',
                model           = 'surfex',
                local           = 'OPTIONS.nam',
            )
            print(t.prompt, 'tb05 =', tb05)
            print()

            if self.conf.openloop == 'off':
                self.sh.title('Toolbox input tobs')
                tobs = toolbox.input(
                    geometry        = self.conf.geometry,
                    nativefmt       = 'netcdf',
                    dateobs         = assDates,
                    model           = 'obs',
                    part            = 'MODIS',
                    kind            = 'SnowObservations',
                    namespace       = 'cenvortex.multi.fr',
                    experiment      = 'obs@' + os.environ['USER'],
                    local           = 'workSODA/OBSERVATIONS_[dateobs:ymdHh].nc',
                    stage           = '1date',
                    fatal           = False
                )
                print(t.prompt, 'tobs =', tobs)
                print()

            if hasattr(self.conf, "exesurfex"):
                self.sh.title('Toolbox executable tb06= tbx1')
                tb06 = tbx3 = toolbox.executable(
                    role           = 'Binary',
                    kind           = 'offline',
                    local          = 'OFFLINE',
                    model          = 'surfex',
                    remote          = self.conf.exesurfex + "/OFFLINE"
                )

                print(t.prompt, 'tb06 =', tb06)
                print()
#                 if not (tb02[0] or tb02_a[0]):
                if not (tb02_a[0]):

                    self.sh.title('Toolbox executable tb07= tbx2')
                    tb07 = tbx1 = toolbox.executable(
                        role           = 'Binary',
                        kind           = 'buildpgd',
                        local          = 'PGD',
                        model          = 'surfex',
                        remote          = self.conf.exesurfex + "/PGD"
                    )

                    print(t.prompt, 'tb07 =', tb07)
                    print()

                if not (tb03[0] or tb03_s[0]):

                    self.sh.title('Toolbox executable tb08= tbx3')
                    tb08 = tbx2 = toolbox.executable(
                        role           = 'Binary',
                        kind           = 'prep',
                        local          = 'PREP',
                        model          = 'surfex',
                        remote          = self.conf.exesurfex + "/PREP"
                    )

                    print(t.prompt, 'tb08 =', tb08)
                    print()

                self.sh.title('Toolbox executable tb08_s= tbx4')
                tb08_s = tbx4 = toolbox.executable(
                    role           = 'Binary',
                    kind           = 'soda',
                    local          = 'SODA',
                    model          = 'surfex',
                    remote          = self.conf.exesurfex + "/SODA"
                )

                print(t.prompt, 'tb08_s =', tb08_s)
                print()
            else:
                print('you fool ! you should prescribe a --exesurfex path to your s2m command !')

        if 'compute' in self.steps:
            # force first forcing to the first forcing of first member 0001 doesn't work on several nodes...
            firstforcing = 'mb0001/FORCING_' + list_dates_begin_forc[0].strftime("%Y%m%d%H") + "_" + list_dates_end_forc[0].strftime("%Y%m%d%H") + ".nc"
            self.sh.title('Toolbox algo tb09a')
            tb09a = tbalgo1 = toolbox.algo(
                kind         = 'surfex_preprocess',
                datebegin    = self.conf.datebegin,
                dateend      = self.conf.dateend,
                forcingname  = firstforcing,
                nmembers = self.conf.nmembers,
            )
            print(t.prompt, 'tb09a =', tb09a)
            print()
            tb09a.run()

            # Take care : PGD parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
#             if not (tb02[0] or tb02_a[0]):
            if not tb02_a[0]:
                self.sh.title('Toolbox algo tb09 = PGD')
                tb09 = tbalgo2 = toolbox.algo(
                    kind         = 'pgd_from_forcing',
                    forcingname  = firstforcing,
                )
                print(t.prompt, 'tb09 =', tb09)
                print()
                self.component_runner(tbalgo2, tbx1, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))

            # Take care : PREP parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
            if not (tb03[0] or tb03_s[0]):
                self.sh.title('Toolbox algo tb09 = PREP')
                tb10 = tbalgo3 = toolbox.algo(
                    engine         = 'parallel',
                )
                print(t.prompt, 'tb10 =', tb10)
                print()
                self.component_runner(tbalgo3, tbx2, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))

            if self.conf.subensemble == 'Crocus':
                ntasksEsc = 1
            else:  # to check, it's not clear obvious to me yet -> mem. saturation if 35 members on a node. need to reduce consumption and members/node
                # ntasksEsc = 40 * int(self.conf.nnodes)
                ntasksEsc = min(self.conf.nmembers, 40)
            # assimilation loop
            datestart = self.conf.datebegin
            print(assDates)
            stopstep = 1
            for dateassim in assDates:
                self.sh.title('Toolbox algo tb11 = OFFLINE')

                # forcing files attribution (mixing ) in offline algocomp
                datestop = dateassim
                tb11 = tbalgo4 = toolbox.algo(
                    engine         = 'blind',
                    binary         = 'OFFLINE',
                    kind           = "escroc",
                    datebegin      = datestart,
                    dateend        = datestop,
                    dateinit       = self.conf.datespinup,
                    threshold      = self.conf.threshold,
                    members        = footprints.util.rangex(members),
                    subensemble    = self.conf.subensemble if hasattr(self.conf, "subensemble")  else "EZob",
                    ntasks         = ntasksEsc,
                    nforcing       = self.conf.nforcing,
                    stopcount      = stopstep,
                    confvapp       = self.conf.vapp,
                    confvconf      = self.conf.vconf,
                )
                print(t.prompt, 'tb11 =', tb11)
                print()
                self.component_runner(tbalgo4, tbx3)
                stopstep += 1

                if self.conf.openloop == 'off' and os.path.exists('workSODA/OBSERVATIONS_' + dateassim.ymdHh + '.nc'):  # test of obs exists/successfully downloaded

                    # soda
                    self.sh.title('Toolbox algo tb11_s = SODA')

                    tb11_s = tbalgo4s = toolbox.algo(
                        engine         = 'parallel',
                        binary         = 'SODA',
                        kind           = "s2m_soda",
                        dateassim      = dateassim,
                        members        = footprints.util.rangex(members),
                    )
                    print(t.prompt, 'tb11_s =', tb11_s)
                    print()
                    self.component_runner(tbalgo4s, tbx4, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))

                # increment the date
                datestart = datestop

            # last propagation -> better include in the above loop
            self.sh.title('Toolbox algo tb11_f = OFFLINE')
            tb11_f = tbalgo4f = toolbox.algo(
                engine         = 'blind',
                binary         = 'OFFLINE',
                kind           = "escroc",
                datebegin      = datestart,
                dateend        = self.conf.dateend,
                dateinit       = self.conf.datespinup,
                threshold      = self.conf.threshold,
                members        = footprints.util.rangex(members),
                subensemble    = self.conf.subensemble if hasattr(self.conf, "subensemble")  else "EZob",
                ntasks         = ntasksEsc,
                nforcing       = self.conf.nforcing,
                stopcount      = stopstep,
                confvapp       = self.conf.vapp,
                confvconf      = self.conf.vconf,
            )
            print(t.prompt, 'tb11_f =', tb11_f)
            print()
            self.component_runner(tbalgo4f, tbx3)

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:

            #                                 local              -->     remote
            # SODA        obs | analysis    PREP_YYYYMMDDHH.nc        PREP_YYYYMMDDHH_an.nc
            # ----            | background  PREP_YYYYMMDDYHH_bg.nc    PREP_YYYYMMDDHH_bg.nc
            # ----      noobs | analysis    NONE                      NONE
            # ----            | background  PREP_YYYYMMDDHH.nc        PREP_YYYYMMDDHH_bg.nc
            # OPENLOOP          analysis    NONE                      NONE
            # -------           background  PREP_YYYYMMDDHH.nc        PREP_YYYYMMDDHH_bg.nc

            for p, datebegin in enumerate(list_dates_begin_pro):
                dateend = list_dates_end_pro[p]

                print()
                print('test')
                print('workSODA/OBSERVATIONS_' + Date(dateend).ymdHh + '.nc')
                print()
                print()
                if self.conf.openloop == 'on' or not os.path.exists('workSODA/OBSERVATIONS_' + Date(dateend).ymdHh + '.nc'):  # openloop or noobs/final step
                    localan = 'NONE'
                    localbg = 'mb[member]/PREP_[date:ymdh].nc'
                else:  # usual soda step
                    localan = 'mb[member]/PREP_[date:ymdh].nc'
                    localbg = 'mb[member]/PREP_[date:ymdh]_bg.nc'

                    if os.path.exists('workSODA/OBSERVATIONS_' + Date(dateend).ymdHh + '.nc'):
                        locObs = 'workSODA/obs_MODIS_' + self.conf.geometry.area + '_' + Date(dateend).ymdHh + '.nc'
                        os.rename('workSODA/OBSERVATIONS_' + Date(dateend).ymdHh + '.nc', locObs )
                        if hasattr(self.conf, 'writesx'):
                            self.sh.title('Toolbox output tobsOUT_sx')
                            tobsout = toolbox.output(
                                local = locObs,
                                geometry        = self.conf.geometry,
                                nativefmt       = 'netcdf',
                                dateobs         = dateend,
                                model           = 'obs',
                                part            = 'MODIS',
                                kind            = 'SnowObservations',
                                namespace       = 'cenvortex.sxcen.fr',
                                storage         = 'sxcen.cnrm.meteo.fr',
                                rootpath = self.conf.writesx,
                                experiment      = self.conf.xpid,
                                stage           = '1date',
                                block           = 'obs',
                                fatal           = False
                            )
                            print(t.prompt, 'tobsout =', tobsout)
                            print()

                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.output(
                    local          = 'mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin,
                    dateend        = dateend,
                    member         = members,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'cenvortex.multi.fr',
                ),
                print(t.prompt, 'tb19 =', tb19)
                print()

                self.sh.title('Toolbox output tb20an')
                tb20 = toolbox.output(
                    local          = localan,
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    date           = dateend,
                    period         = dateend,
                    member         = members,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackState',
                    model          = 'surfex',
                    namespace      = 'cenvortex.multi.fr',
                    stage          = '_an',
                    fatal          = False  # doesn't exist if openloop
                ),
                print(t.prompt, 'tb20 =', tb20)
                print()

                self.sh.title('Toolbox output tb21bg')
                tb21 = toolbox.output(
                    local          = localbg,
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    date           = dateend,
                    period         = dateend,
                    member         = members,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackState',
                    model          = 'surfex',
                    namespace      = 'cenvortex.multi.fr',
                    stage          = '_bg',
                    fatal          = False
                ),
                print(t.prompt, 'tb21 =', tb21)
                print()

                if hasattr(self.conf, 'writesx'):
                    self.sh.title('Toolbox output tb20an_sx')
                    tb20_b = toolbox.output(
                        local          = localan,
                        role           = 'SnowpackInit',
                        experiment     = self.conf.xpid,
                        geometry       = self.conf.geometry,
                        date           = dateend,
                        period         = dateend,
                        member         = members,
                        nativefmt      = 'netcdf',
                        kind           = 'SnowpackState',
                        model          = 'surfex',
                        namespace      = 'cenvortex.sxcen.fr',
                        storage        = 'sxcen.cnrm.meteo.fr',
                        rootpath       = self.conf.writesx,
                        stage          = '_an',
                        fatal          = False  # doesn't exist if openloop
                    ),
                    print(t.prompt, 'tb20an_sx =', tb20_b)
                    print()

                    self.sh.title('Toolbox output tb21bg_sx')
                    tb21_b = toolbox.output(
                        local          = localbg,
                        role           = 'SnowpackInit',
                        experiment     = self.conf.xpid,
                        geometry       = self.conf.geometry,
                        date           = dateend,
                        period         = dateend,
                        member         = members,
                        nativefmt      = 'netcdf',
                        kind           = 'SnowpackState',
                        model          = 'surfex',
                        namespace      = 'cenvortex.sxcen.fr',
                        storage        = 'sxcen.cnrm.meteo.fr',
                        rootpath       = self.conf.writesx,
                        stage          = '_bg',
                        fatal          = False,
                    ),
                    print(t.prompt, 'tb21 =', tb21_b)
                    print()
                    self.sh.title('Toolbox output tbconf')
                    tbconf = toolbox.output(
                        kind           = 'ini_file',
                        namespace      = 'cenvortex.sxcen.fr',
                        storage        = 'sxcen.cnrm.meteo.fr',
                        rootpath       = self.conf.writesx,
                        experiment     = self.conf.xpid,
                        local          = self.conf.vapp + '_' + self.conf.vconf + '.ini',
                        vapp           = self.conf.vapp,
                        vconf          = self.conf.vconf,
                        fatal          = False,
                    ),
                    print(t.prompt, 'tbconf =', tbconf)
                    print()

            if hasattr(self.conf, 'writesx'):
                self.sh.title('Toolbox output tb23')
                tb23 = toolbox.output(
                    role            = 'Nam_surfex',
                    namespace       = 'cenvortex.sxcen.fr',
                    storage         = 'sxcen.cnrm.meteo.fr',
                    experiment      = self.conf.xpid,
                    rootpath       = self.conf.writesx,
                    kind            = 'cen_namelist',
                    model           = 'surfex',
                    local           = 'OPTIONS.nam',
                    fatal           = False,
                )
                print(t.prompt, 'tb23 =', tb23)
                print()
