# -*- coding: utf-8 -*-
'''
Created in April 2019

@author: carmagnola (PROSNOW)
'''


from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from snowtools.utils.dates import get_list_dates_files
from bronx.stdtypes.date import Date, daterange, tomorrow
from cen.layout.nodes import S2MTaskMixIn


def setup(t, **kw):
    return Driver(
        tag='Prosnow_Analysis',
        ticket=t,
        nodes=[
            Prosnow_Analysis(tag='prosnow_analysis', ticket=t, **kw),
        ],
        options=kw
    )


class Prosnow_Analysis(Task, S2MTaskMixIn):

    def process(self):

        t = self.ticket

        list_geometry = self.get_list_geometry(meteo=self.conf.meteo)
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, "yearly")

        '''-----------------------------------'''
        '''              step.01              '''
        '''-----------------------------------'''

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            '''------------ global----------------'''

            # '''1) Past years'''
#             xpid = "reanalysis@lafaysse"
            # '''2) Real-time'''
#             xpid = "OPER@vernaym"
            xpid = "oper"

            for p, datebegin in enumerate(list_dates_begin_forc):
                dateend = list_dates_end_forc[p]

                self.sh.title('Toolbox input in_tb01')
                '''1) INPUT -> search for forcing'''
                in_tb01 = toolbox.input(
                # '''1) Past years'''
#                     vapp           = 's2m',
#                     vconf          = '[geometry:area]',
#                     local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
#                     experiment     = xpid,
#                     block          = 'meteo',
#                     geometry       = self.conf.geometry,
#                     date           = '[datebegin]',
#                     datebegin      = [datebegin],
#                     dateend        = [dateend],
#                     nativefmt      = 'netcdf',
#                     kind           = 'MeteorologicalForcing',
#                     model          = 's2m',
#                     namespace      = 'vortex.multi.fr',
#                     namebuild      = 'flat@cen',
                # '''2) Real-time'''
                    role           = 'Ana_massifs',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = '4dvarfr',
                    cutoff         = 'assimilation',
                    local          = 'FORCING_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = xpid,
                    block          = 'massifs',
                    geometry       = self.conf.geom_safran,
                    nativefmt      = 'netcdf',
                    model          = 'safran',
                    datebegin      = self.conf.datebegin,
                    dateend        = self.conf.dateend,
                    namespace      = 'vortex.multi.fr',
                    date           = '[dateend::ymd]09',
                    vapp           = 's2m',
                    vconf          = self.conf.geom_safran,
                    fatal          = False,
                )
                print(t.prompt, 'in_tb01 =', in_tb01)
                print()

                '''2) Real-time'''
                self.sh.title('Toolbox input in_tb01')
                '''1) INPUT -> search for forcing'''
                in_tb01 = toolbox.input(
                    role           = 'alternate',
                    kind           = 'MeteorologicalForcing',
                    source_app     = 'arpege',
                    source_conf    = '4dvarfr',
                    cutoff         = 'assimilation',
                    local          = 'FORCING_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                    experiment     = xpid,
                    block          = 'massifs',
                    geometry       = self.conf.geom_safran,
                    nativefmt      = 'netcdf',
                    model          = 'safran',
                    datebegin      = self.conf.datebegin,
                    dateend        = self.conf.dateend,
                    namespace      = 'vortex.multi.fr',
                    date           = '[dateend::ymd]06',
                    vapp           = 's2m',
                    vconf          = self.conf.geom_safran,
                )
                print(t.prompt, 'in_tb01 =', in_tb01)
                print()

            self.sh.title('Toolbox input in_tb02')
            '''2) INPUT -> search for namelist'''
            in_tb02 = toolbox.input(
                role            = 'Namelist for surfex',
                kind            = 'namelist',
                model           = 'surfex',
                source          = 'OPTIONS_V8.1_1h_' + self.conf.nam_nam + '.nam',
                genv            = 'uenv:prosnow.01@CONST_PROSNOW',
                gvar            = 'namelist_surfex',
                local           = 'OPTIONS_unmodified.nam',
                intent          = 'inout'
            )
            print(t.prompt, 'in_tb02 =', in_tb02)
            print()

            self.sh.title('Toolbox input in_tb03')
            '''3) INPUT -> ecoclimap1'''
            in_tb03 = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapI_covers_param.bin',
                geometry       = self.conf.geometry,
                genv           = 'uenv:cen.01@CONST_CEN',
                source         = 'ecoclimap1',
                model          = 'surfex',
            ),
            print(t.prompt, 'in_tb03 =', in_tb03)
            print()

            self.sh.title('Toolbox input in_tb04')
            '''4) INPUT -> ecoclimap2'''
            in_tb04 = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapII_eu_covers_param.bin',
                geometry       = self.conf.geometry,
                genv           = 'uenv:cen.01@CONST_CEN',
                source         = 'ecoclimap2',
                model          = 'surfex',
            ),
            print(t.prompt, 'in_tb04 =', in_tb04)
            print()

            self.sh.title('Toolbox input in_tb05')
            '''5) INPUT -> drdt_bst_fit_60.nc'''
            in_tb05 = toolbox.input(
                role            = 'Parameters for F06 metamorphism',
                kind            = 'ssa_params',
                genv            = 'uenv:cen.01@CONST_CEN',
                nativefmt       = 'netcdf',
                local           = 'drdt_bst_fit_60.nc',
                model           = 'surfex',
            )
            print(t.prompt, 'in_tb05 =', in_tb05)
            print()

            self.sh.title('Toolbox input in_tb06')
            '''6) INPUT -> variables (for insertion of snow height)'''
            in_tb06 = toolbox.input(
                role           = 'list_updated_variables',
                kind           = 'list_updated_variables',
                nativefmt      = 'ascii',
                local          = 'variables',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                model          = 'surfex',
            )
            print(t.prompt, 'in_tb06 =', in_tb06)
            print()

            self.sh.title('Toolbox input in_tb07')
            '''7) INPUT -> prep_fillup_5 (for insertion of snow height)'''
            in_tb07 = toolbox.input(
                role           = 'prep_fillup_5',
                kind           = 'prep_fillup_5',
                nativefmt      = 'netcdf',
                local          = 'prep_fillup_5.nc',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                model          = 'surfex',
            )
            print(t.prompt, 'in_tb07 =', in_tb07)
            print()

            self.sh.title('Toolbox input in_tb08')
            '''8) INPUT -> prep_fillup_50 (for insertion of snow height)'''
            in_tb08 = toolbox.input(
                role           = 'prep_fillup_50',
                kind           = 'prep_fillup_50',
                nativefmt      = 'netcdf',
                local          = 'prep_fillup_50.nc',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                model          = 'surfex',
            )
            print(t.prompt, 'in_tb8 =', in_tb08)
            print()

            '''------------ per resort----------------'''

            self.sh.title('Toolbox input in_tb09')
            '''9) INPUT -> search for SRU geometry'''
            in_tb09 = toolbox.input(
                role           = 'SRU geometry',
                kind           = 'sru',
                nativefmt      = 'ascii',
                local          = 'SRU.txt',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                model          = 'surfex',
                resort         = self.conf.resort,
            )
            print(t.prompt, 'in_tb09 =', in_tb09)
            print()

            xpid = "outputs@carmagnolac"

            self.sh.title('Toolbox input in_tb10')
            '''10) INPUT -> search for PGD of previous simulation'''
            in_tb10 = toolbox.input(
                local          = 'PGD.nc',
                experiment     = xpid,
                geometry       = self.conf.geometry,
                nativefmt      = 'netcdf',
                kind           = 'pgdnc',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'pgd',
            )
            print(t.prompt, 'in_tb10 =', in_tb10)
            print()

            self.sh.title('Toolbox input in_tb11')
            '''11) INPUT -> search for PREP of previous simulation'''
            in_tb11 = toolbox.input(
                local          = 'PREP.nc',
                experiment     = xpid,
                geometry       = self.conf.geometry,
                date           = self.conf.datebegin,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'prep',
            )
            print(t.prompt, 'in_tb11 =', in_tb11)
            print()

            self.sh.title('Toolbox input in_tb12')
            '''12) INPUT -> search for water data'''
            in_tb12 = toolbox.input(
                role           = 'water data',
                kind           = 'water',
                nativefmt      = 'ascii',
                local          = 'water.txt',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                model          = 'surfex',
                resort         = self.conf.resort,
            )
            print(t.prompt, 'in_tb12 =', in_tb12)
            print()

            self.sh.title('Toolbox input in_tb13')
            '''13) INPUT -> search for snow data'''
            in_tb13 = toolbox.input(
                role           = 'snow data',
                kind           = self.conf.snowsat,
                nativefmt      = 'ascii',
                local          = 'snow.txt',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                model          = 'surfex',
                resort         = self.conf.resort,
            )
            print(t.prompt, 'in_tb13 =', in_tb13)
            print()

            date_obs = self.conf.dateend.strftime("%Y%m%d%H")
            self.sh.title('Toolbox input in_tb14')
            '''14) INPUT -> search for obs'''
            in_tb14 = toolbox.input(
                role            = 'empty obs',
                kind            = 'obs_empty',
                nativefmt       = 'netcdf',
                local           = 'OBS_' + date_obs + '.nc',
                genv            = 'uenv:prosnow.01@CONST_PROSNOW',
                model           = 'surfex',
                resort          = self.conf.resort,
                intent          = 'inout',
            )
            print(t.prompt, 'in_tb14 =', in_tb14)
            print()

            '''------------ executables----------------'''

            self.sh.title('Toolbox executable ex_tb01')
            '''1) EXECUTABLE -> PGD'''
            ex_tb01 = toolbox.executable(
                role           = 'Surfex pgd file',
                kind           = 'buildpgd',
                model          = 'surfex',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                gvar           = 'pgd_prosnow_mpi',
                local          = 'PGD',
            )
            print(t.prompt, 'ex_tb01 =', ex_tb01)
            print()

            self.sh.title('Toolbox executable ex_tb02')
            '''2) EXECUTABLE -> PREP'''
            ex_tb02 = toolbox.executable(
                role           = 'Surfex prep file',
                kind           = 'prep',
                model          = 'surfex',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                gvar           = 'prep_prosnow_mpi',
                local          = 'PREP',
            )
            print(t.prompt, 'ex_tb02 =', ex_tb02)
            print()

            self.sh.title('Toolbox executable ex_tb03')
            '''3) EXECUTABLE -> OFFLINE'''
            ex_tb03 = toolbox.executable(
                role           = 'Surfex offline file',
                kind           = 'offline',
                model          = 'surfex',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                gvar           = 'offline_prosnow_mpi',
                local          = 'OFFLINE',
            )
            print(t.prompt, 'ex_tb03 =', ex_tb03)
            print()

        '''-----------------------------------'''
        '''              step.02              '''
        '''-----------------------------------'''

        if 'compute' in self.steps:

            ntasks = min(int(self.conf.nproc), len(list_dates_begin_forc))
            self.sh.title('Toolbox algo alg_tb01')
            '''1) ALGO -> generate new forcing over SRU geometry'''
            alg_tb01 = toolbox.algo(
                kind           = 'extractforcing',
                engine         = 's2m',
                # '''1) Past years'''
#                 datebegin      = [list_dates_begin_forc],
#                 dateend        = [list_dates_end_forc],
                # '''2) Real-time'''
                datebegin      = [self.conf.datebegin],
                dateend        = [self.conf.dateend],
                ntasks         = ntasks,
                geometry_in    = list_geometry,
                geometry_out   = 'allslopes',
            )
            print(t.prompt, 'alg_tb01 =', alg_tb01)
            print()
            alg_tb01.run()

            # '''1) Past years'''
#             firstforcing = 'FORCING_' + list_dates_begin_forc[0].strftime("%Y%m%d%H") + "_" + list_dates_end_forc[0].strftime("%Y%m%d%H") + ".nc"
            # '''2) Real-time'''
            firstforcing = 'FORCING_' + self.conf.datebegin.strftime("%Y%m%d%H") + "_" + \
                self.conf.dateend.strftime("%Y%m%d%H") + ".nc"

            self.sh.title('Toolbox algo tb02')
            '''2) ALGO -> modify namelist to end simulation before the end of forcing file'''
            tb02 = toolbox.algo(
                kind         = 'surfex_preprocess',
                datebegin    = self.conf.datebegin,
                dateend      = self.conf.dateend,
                forcingname  = firstforcing
            )
            print(t.prompt, 'tb02 =', tb02)
            print()
            tb02.run()

            self.sh.title('Toolbox algo alg_tb03')
            '''3) ALGO -> run surfex (Prosnow_Parallel)'''
            alg_tb03 = toolbox.algo(
                insert_data                = 'prosnow_insert_data',
                engine                     = 'parallel',
                binary                     = 'OFFLINE',
                kind                       = 'deterministic',
                datebegin                  = self.conf.datebegin,
                dateend                    = self.conf.dateend,
            )
            print(t.prompt, 'alg_tb03 =', alg_tb03)
            print()
            self.component_runner(alg_tb03, ex_tb03)

        '''-----------------------------------'''
        '''              step.03              '''
        '''-----------------------------------'''

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:

            xpid = "outputs@carmagnolac"

            for p, datebegin in enumerate(list_dates_begin_forc):
                dateend = list_dates_end_forc[p]

                self.sh.title('Toolbox output out_tb01')
                '''1) OUTPUT -> pgd'''
                out_tb01 = toolbox.output(
                    local          = 'PGD.nc',
                    experiment     = xpid,
                    geometry       = self.conf.geometry,
                    nativefmt      = 'netcdf',
                    kind           = 'pgdnc',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'pgd'
                ),
                print(t.prompt, 'out_tb01 =', out_tb01)
                print()

                self.sh.title('Toolbox output out_tb02')
                '''2) OUTPUT -> prep'''
                out_tb02 = toolbox.output(
                    local          = 'PREP_[date:ymdh].nc',
                    date           = [self.conf.dateend],
                    experiment     = xpid,
                    geometry       = self.conf.geometry,
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep',
                ),
                print(t.prompt, 'out_tb02 =', out_tb02)
                print()

                self.sh.title('Toolbox output out_tb03')
                '''3) OUTPUT -> pro'''
                out_tb03 = toolbox.output(
                    local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = [self.conf.datebegin],
                    dateend        = [self.conf.dateend],
                    date           = [self.conf.datebegin],
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'pro',
                ),
                print(t.prompt, 'out_tb03 =', out_tb03)
                print()

                self.sh.title('Toolbox input out_tb04')
                '''4) OUTPUT -> forcing'''
                out_tb04 = toolbox.output(
                    local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = xpid,
                    geometry       = self.conf.geometry,
                    # '''1) Past years'''
#                     datebegin      = [datebegin],
#                     dateend        = [dateend],
#                     date           = [datebegin],
                    # '''2) Real-time'''
                    datebegin      = self.conf.datebegin,
                    dateend        = self.conf.dateend,
                    date           = self.conf.datebegin,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'meteo',
                ),
                print(t.prompt, 'out_tb04 =', out_tb04)
                print()


#             ### Force vortex to fail, in order to save info in the "abort" folder
#             from vortex.tools.systems import ExecutionError
#             raise ExecutionError('')