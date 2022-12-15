# -*- coding: utf-8 -*-
'''
Created in April 2019

@author: carmagnola (PROSNOW)
'''

import tarfile
import glob

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from snowtools.utils.dates import get_list_dates_files
from bronx.stdtypes.date import Date, daterange, tomorrow, Period
from cen.layout.nodes import S2MTaskMixIn


def setup(t, **kw):
    return Driver(
        tag='Prosnow_ST_Forecast',
        ticket=t,
        nodes=[
            Prosnow_ST_Forecast(tag='prosnow_ST_forecast', ticket=t, **kw),
        ],
        options=kw
    )


class Prosnow_ST_Forecast(Task, S2MTaskMixIn):

    def process(self):

        t = self.ticket

        list_geometry = self.get_list_geometry(meteo=self.conf.meteo)
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, "yearly")

        members = ['00', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '13', '14', '15',
                   '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31',
                   '32', '33', '34']

        my_datebegin = self.conf.datebegin
        my_dateend = self.conf.dateend

        '''-----------------------------------'''
        '''              step.01              '''
        '''-----------------------------------'''

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            '''------------ global----------------'''

            # '''1) Past years'''
#             xpid = "reforecast@vernaym"
            # '''2) Real-time'''
#             xpid = "OPER@vernaym"
            xpid = "oper"

            self.sh.title('Toolbox input in_tb01')
            '''1) INPUT -> search for forcing'''
            in_tb01 = toolbox.input(
                # '''1) Past years'''
#                 role           = 'Forcing',
#                 local          = 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
#                 vapp           = 'safran',
#                 vconf          = self.conf.geom_safran,
#                 block          = 'massifs/[datebegin:nivologyseason]',
#                 source_app     = 'arpege',
#                 source_conf    = 'pearp',
#                 experiment     = xpid,
#                 geometry       = list_geometry,
#                 date           = '[datebegin]',
#                 datebegin      = self.conf.datebegin,
#                 dateend        = self.conf.dateend,
#                 member         = members,
#                 nativefmt      = 'netcdf',
#                 kind           = 'MeteorologicalForcing',
#                 namespace      = 'vortex.multi.fr',
#                 model          = 's2m',
#                 namebuild      = 'flat@cen',
                # '''2) Real-time'''
                 role           = 'Forcing',
                 kind           = 'MeteorologicalForcing',
                 source_app     = 'arpege',
                 source_conf    = 'pearp',
                 cutoff         = 'production',
                 local          = 'mb[member]/FORCING_{0:s}_[dateend::ymd6h].nc'.format(self.conf.datebegin.ymdh),
                 experiment     = xpid,
                 block          = 'massifs',
                 geometry       = self.conf.geom_safran,
                 member         = members,
                 nativefmt      = 'netcdf',
                 model          = 'safran',
                 datebegin      = '{0:s}/-PT24H'.format(self.conf.datebegin.ymdh),
                 dateend        = self.conf.dateend,
                 namespace      = 'vortex.multi.fr',
                 date           = '{0:s}03'.format(self.conf.datebegin.ymd),
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
                source          = 'OPTIONS_V8.1_1h_' + self.conf.nam_nam[0] + '.nam',
                genv            = 'uenv:prosnow.01@CONST_PROSNOW',
                gvar            = 'namelist_surfex',
                local           = 'OPTIONS1.nam',
                intent          = 'inout'
            )
            print(t.prompt, 'in_tb02 =', in_tb02)
            print()

            self.sh.title('Toolbox input in_tb02')
            '''2) INPUT -> search for namelist'''
            in_tb02 = toolbox.input(
                role            = 'Namelist for surfex',
                kind            = 'namelist',
                model           = 'surfex',
                source          = 'OPTIONS_V8.1_1h_' + self.conf.nam_nam[1] + '.nam',
                genv            = 'uenv:prosnow.01@CONST_PROSNOW',
                gvar            = 'namelist_surfex',
                local           = 'OPTIONS2.nam',
            )
            print(t.prompt, 'in_tb02 =', in_tb02)
            print()

            self.sh.title('Toolbox input in_tb02')
            '''2) INPUT -> search for namelist'''
            in_tb02 = toolbox.input(
                role            = 'Namelist for surfex',
                kind            = 'namelist',
                model           = 'surfex',
                source          = 'OPTIONS_V8.1_3h_' + self.conf.nam_nam[2] + '.nam',
                genv            = 'uenv:prosnow.01@CONST_PROSNOW',
                gvar            = 'namelist_surfex',
                local           = 'OPTIONS3.nam',
            )
            print(t.prompt, 'in_tb02 =', in_tb02)
            print()

            self.sh.title('Toolbox input in_tb02')
            '''2) INPUT -> search for namelist'''
            in_tb02 = toolbox.input(
                role            = 'Namelist for surfex',
                kind            = 'namelist',
                model           = 'surfex',
                source          = 'OPTIONS_V8.1_3h_' + self.conf.nam_nam[3] + '.nam',
                genv            = 'uenv:prosnow.01@CONST_PROSNOW',
                gvar            = 'namelist_surfex',
                local           = 'OPTIONS4.nam',
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

            '''------------ per resort----------------'''

            self.sh.title('Toolbox input in_tb06')
            '''6) INPUT -> search for SRU geometry'''
            in_tb06 = toolbox.input(
                role           = 'SRU geometry',
                kind           = 'sru',
                nativefmt      = 'ascii',
                local          = 'SRU.txt',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                model          = 'surfex',
                resort         = self.conf.resort,
            )
            print(t.prompt, 'in_tb06 =', in_tb06)
            print()

            xpid = "outputs@carmagnolac"

            self.sh.title('Toolbox input in_tb07')
            '''7) INPUT -> search for PGD of previous simulation'''
            in_tb07 = toolbox.input(
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
            print(t.prompt, 'in_tb07 =', in_tb07)
            print()

            self.sh.title('Toolbox input in_tb08')
            '''8) INPUT -> search for PREP of previous simulation'''
            in_tb08 = toolbox.input(
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
            print(t.prompt, 'in_tb08 =', in_tb08)
            print()

            '''------------ executables----------------'''

            self.sh.title('Toolbox executable ex_tb01')
            '''1) EXECUTABLE -> PGD'''
            ex_tb01 = toolbox.executable(
                role           = 'Surfex pgd file',
                kind           = 'buildpgd',
                model          = 'surfex',
                genv           = 'uenv:prosnow.01@CONST_PROSNOW',
                gvar           = 'pgd_prosnow_nompi',
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
                gvar           = 'prep_prosnow_nompi',
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
                gvar           = 'offline_prosnow_nompi',
                local          = 'OFFLINE',
            )
            print(t.prompt, 'ex_tb03 =', ex_tb03)
            print()

        '''-----------------------------------'''
        '''              step.02              '''
        '''-----------------------------------'''

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo alg_tb01')
            '''1) ALGO -> generate new forcing over SRU geometry'''
            alg_tb01 = toolbox.algo(
                kind           = 'extractforcing_STforecast',
                engine         = 's2m',
                datebegin      = [[self.conf.datebegin]],
                dateend        = [[self.conf.dateend]],
                ntasks         = self.conf.ntasks,
                geometry_in    = list_geometry,
                geometry_out   = 'allslopes',
            )
            print(t.prompt, 'alg_tb01 =', alg_tb01)
            print()
            alg_tb01.run()

            self.sh.title('Toolbox algo alg_tb02')
            '''2) ALGO -> run surfex (Surfex_Component)'''
            alg_tb02 = toolbox.algo(
                kind                       = 'ensmeteonodet',
                engine                     = 's2m',
                datebegin                  = my_datebegin,
                dateend                    = my_dateend,
                ntasks                     = self.conf.ntasks,
                geometry_in                = [u'allslopes'],
                geometry_out               = 'allslopes',
                daily                      = True,
                dailynamelist              = ["OPTIONS1.nam", "OPTIONS2.nam", "OPTIONS3.nam", "OPTIONS4.nam"],
            )
            print(t.prompt, 'alg_tb02 =', alg_tb02)
            print()
            self.component_runner(alg_tb02, ex_tb03)

#             '''Save as .tar'''
            with tarfile.open('FORCING.tar', mode='w') as tarfic:
                for f in glob.glob('*/FORCING_2*'):
                    tarfic.add(f)
            with tarfile.open('PRO.tar', mode='w') as tarfic:
                for f in glob.glob('*/PRO_*'):
                    tarfic.add(f)

        '''-----------------------------------'''
        '''              step.03              '''
        '''-----------------------------------'''

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:

            xpid = "outputs@carmagnolac"

            self.sh.title('Toolbox output out_tb01')
            '''1) OUTPUT -> pgd'''
            out_tb01 = toolbox.output(
                local          = 'mb[member]/PGD.nc',
                experiment     = xpid,
                geometry       = self.conf.geometry,
                nativefmt      = 'netcdf',
                kind           = 'pgdnc',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                member         = members,
                block          = self.conf.snow_conf + '/pgd',
            ),
            print(t.prompt, 'out_tb01 =', out_tb01)
            print()

            self.sh.title('Toolbox output out_tb02')
            '''2) OUTPUT -> prep'''
            out_tb02 = toolbox.output(
                local          = 'mb[member]/PREP_[date:ymdh].nc',
                date           = list(daterange(tomorrow(base=my_datebegin), my_dateend)),
                experiment     = xpid,
                geometry       = self.conf.geometry,
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                member         = members,
                block          = self.conf.snow_conf + '/prep',
            ),
            print(t.prompt, 'out_tb02 =', out_tb02)
            print()

#             self.sh.title('Toolbox output out_tb03')
#             '''3) OUTPUT -> pro'''
#             out_tb03 = toolbox.output(
#                 local          = 'mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
#                 experiment     = xpid,
#                 geometry       = self.conf.geometry,
#                 datebegin      = '[dateend]/-PT24H',
#                 dateend        = list(daterange(tomorrow(base=my_datebegin), my_dateend)),
#                 date           = '[dateend]/-PT24H',
#                 nativefmt      = 'netcdf',
#                 kind           = 'SnowpackSimulation',
#                 model          = 'surfex',
#                 namespace      = 'vortex.multi.fr',
#                 namebuild      = 'flat@cen',
#                 member         = members,
#                 block          = self.conf.snow_conf + '/pro',
#             ),
#             print(t.prompt, 'out_tb03 =', out_tb03)
#             print()

            self.sh.title('Toolbox output out_tb03')
            '''3) OUTPUT -> pro'''
            out_tb03 = toolbox.output(
                role           = 'PRO',
                local          = 'PRO.tar',
                remote         = '/home/carmagnolac/vortex/surfex/{0:s}/outputs/{1:s}/{2:s}'.\
                format(self.conf.resort, self.conf.snow_conf, 'PRO_ST.tar'),
                hostname       = 'hendrix.meteo.fr',
                unknownflow    = True,
                username       = 'carmagnolac',
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                date           = self.conf.datebegin,
                tube           = 'ftp',
                geometry       = self.conf.geometry,
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
            ),
            print(t.prompt, 'out_tb03 =', out_tb03)
            print()

#             self.sh.title('Toolbox input out_tb04')
#             '''4) OUTPUT -> forcing'''
#             out_tb04 = toolbox.output(
#                 local          = 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
#                 experiment     = xpid,
#                 geometry       = self.conf.geometry,
#                 datebegin      = [my_datebegin],
#                 dateend        = [my_dateend],
#                 date           = [my_datebegin],
#                 nativefmt      = 'netcdf',
#                 kind           = 'MeteorologicalForcing',
#                 model          = 's2m',
#                 namespace      = 'vortex.multi.fr',
#                 namebuild      = 'flat@cen',
#                 member         = members,
#                 block          = self.conf.snow_conf + '/meteo',
#             ),
#             print(t.prompt, 'out_tb04 =', out_tb04)
#             print()

            self.sh.title('Toolbox input out_tb04')
            '''4) OUTPUT -> forcing'''
            out_tb04 = toolbox.output(
                role           = 'FORCING',
                local          = 'FORCING.tar',
                remote         = '/home/carmagnolac/vortex/surfex/{0:s}/outputs/{1:s}/{2:s}'.\
                format(self.conf.resort, self.conf.snow_conf, 'FORCING_ST.tar'),
                hostname       = 'hendrix.meteo.fr',
                unknownflow    = True,
                username       = 'carmagnolac',
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                date           = self.conf.datebegin,
                tube           = 'ftp',
                geometry       = self.conf.geometry,
                nativefmt      = 'netcdf',
                kind           = 'MeteorologicalForcing',
                model          = 's2m',
            ),
            print(t.prompt, 'out_tb04 =', out_tb04)
            print()


#             ### Force vortex to fail, in order to save info in the "abort" folder
#             from vortex.tools.systems import ExecutionError
#             raise ExecutionError('')

