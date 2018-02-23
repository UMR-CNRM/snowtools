'''
Created on 7 nov. 2017

@author: lafaysse
'''

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from utils.dates import get_list_dates_files
import footprints

def setup(t, **kw):
    return Driver(
        tag = 'Surfex_Parallel',
        ticket = t,
        nodes = [
            Surfex_Vortex_Task(tag='Surfex_Vortex_Task', ticket=t, **kw),
        ],
        options=kw
    )


class Surfex_Vortex_Task(Task):
    '''

    '''

    def process(self):

        t = self.ticket

        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)

        if 'early-fetch' in self.steps or 'fetch' in self.steps:
            for p, datebegin in enumerate(list_dates_begin_forc):
                dateend = list_dates_end_forc[p]

                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
                    role           = 'Forcing',
                    local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    vapp           = self.conf.meteo,
                    experiment     = self.conf.forcingid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin,
                    dateend        = dateend,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 'safran',
                    namespace      = 'cenvortex.multi.fr',
                ),
                print t.prompt, 'tb01 =', tb01
                print

            self.sh.title('Toolbox input tb02')
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
            print t.prompt, 'tb02 =', tb02
            print

            self.sh.title('Toolbox input tb03')
            tb03 = toolbox.input(
                role           = 'SnowpackInit',
                local          = 'PREP.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                date           = self.conf.datebegin,
                nativefmt      = 'netcdf',
                kind           = 'SnowpackState',
                model          = 'surfex',
                namespace      = 'cenvortex.multi.fr',
                fatal          = False,
            ),
            print t.prompt, 'tb03 =', tb03
            print

            if not tb03[0]:
                tb03a = toolbox.input(
                    role           = 'initial values of ground temperature',
                    kind           = 'climTG',
                    nativefmt      = 'netcdf',
                    local          = 'init_TG.nc',
                    geometry       = self.conf.geometry,
                    genv            = 'uenv:cen.01@CONST_CEN',
                    gvar           = 'climtg_[geometry::area]',
                    model          = 'surfex',
                ),
                print t.prompt, 'tb03a =', tb03a
                print

            self.sh.title('Toolbox input tb03b')
            tb03b = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapI_covers_param.bin',
                geometry       = self.conf.geometry,
                genv           = 'uenv:cen.01@CONST_CEN',
                source         = 'ecoclimap1',
                model          = 'surfex',
            ),
            print t.prompt, 'tb03b =', tb03b
            print

            self.sh.title('Toolbox input tb03c')
            tb03c = toolbox.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapII_eu_covers_param.bin',
                geometry       = self.conf.geometry,
                genv            = 'uenv:cen.01@CONST_CEN',
                source         = 'ecoclimap2',
                model          = 'surfex',
            ),
            print t.prompt, 'tb03c =', tb03c
            print

            self.sh.title('Toolbox input tb04')
            tb04 = toolbox.input(
                role            = 'Parameters for F06 metamorphism',
                kind            = 'ssa_params',
                genv            = 'uenv:cen.01@CONST_CEN',
                nativefmt       = 'netcdf',
                local           = 'drdt_bst_fit_60.nc',
                model          = 'surfex',
            )
            print t.prompt, 'tb04 =', tb04
            print

            self.sh.title('Toolbox input tb05')
            tb05 = toolbox.input(
                role            = 'Nam_surfex',
                source          = 'OPTIONS_default.nam',
                genv            = 'uenv:cen.01@CONST_CEN',
                kind            = 'namelist',
                model           = 'surfex',
                local           = 'OPTIONS.nam',
            )
            print t.prompt, 'tb05 =', tb05
            print

            self.sh.title('Toolbox executable tb06= tbx1')
            tb06 = tbx3 = toolbox.executable(
                role           = 'Binary',
                kind           = 'offline',
                local          = 'OFFLINE',
                model          = 'surfex',
                genv           = 'uenv:cen.01@CONST_CEN',
                gvar           = 'master_offline_mpi',
            )

            print t.prompt, 'tb06 =', tb06
            print

            if not tb02[0]:

                self.sh.title('Toolbox executable tb07= tbx2')
                tb07 = tbx1 = toolbox.executable(
                    role           = 'Binary',
                    kind           = 'buildpgd',
                    local          = 'PGD',
                    model          = 'surfex',
                    genv           = 'uenv:cen.01@CONST_CEN',
                    gvar           = 'master_pgd',
                )

                print t.prompt, 'tb07 =', tb07
                print

            if not tb03[0]:

                self.sh.title('Toolbox executable tb08= tbx3')
                tb08 = tbx2 = toolbox.executable(
                    role           = 'Binary',
                    kind           = 'prep',
                    local          = 'PREP',
                    model          = 'surfex',
                    genv           = 'uenv:cen.01@CONST_CEN',
                )

                print t.prompt, 'tb08 =', tb08
                print

        if 'compute' in self.steps:

            firstforcing = 'FORCING_' + list_dates_begin_forc[0].strftime("%Y%m%d%H") + "_" + list_dates_end_forc[0].strftime("%Y%m%d%H") + ".nc"
            self.sh.title('Toolbox algo tb09a')
            tb09a = tbalgo1 = toolbox.algo(
                kind         = 'surfex_preprocess',
                date           = self.conf.datebegin,
                forcingname  = firstforcing
            )
            tb09a.run()

            # Take care : PGD parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
            if not tb02[0]:
                self.sh.title('Toolbox algo tb09 = PGD')
                tb09 = tbalgo0 = toolbox.algo(
                    kind         = 'pgd_from_forcing',
                    forcingname  = firstforcing,
                )
                self.component_runner(tbalgo0, tbx1, mpiopts = dict(nn=1, nnp=1, np=1))

            # Take care : PREP parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
            if not tb03[0]:
                self.sh.title('Toolbox algo tb09 = PREP')
                tb09 = tbalgo1 = toolbox.algo(
                    engine         = 'parallel',
                )
                self.component_runner(tbalgo1, tbx2, mpiopts = dict(nn=1, nnp=1, np=1))

            self.sh.title('Toolbox algo tb11 = OFFLINE')
            tb11 = tbalgo3 = toolbox.algo(
                engine         = 'parallel',
                binary         = 'OFFLINE',
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend
            )
            self.component_runner(tbalgo3, tbx3)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            for p, datebegin in enumerate(list_dates_begin_pro):
                dateend = list_dates_end_pro[p]
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.output(
                    local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    block          = 'surfex',
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin,
                    dateend        = dateend,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'cenvortex.multi.fr',
                ),
                print t.prompt, 'tb19 =', tb19
                print

                self.sh.title('Toolbox output tb20')
                tb20 = toolbox.output(
                    local          = 'PREP_[date:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    block          = 'surfex',
                    geometry       = self.conf.geometry,
                    date           = dateend,
                    period         = dateend,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackState',
                    model          = 'surfex',
                    namespace      = 'cenvortex.multi.fr',
                ),
                print t.prompt, 'tb20 =', tb20
                print
