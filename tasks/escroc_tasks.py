'''
Created on 7 nov. 2017

@author: lafaysse
'''

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from utils.dates import get_list_dates_files
import footprints
import os

def setup(t, **kw):
    return Driver(
        tag = 'Surfex_Parallel',
        ticket = t,
        nodes = [
            Escroc_Vortex_Task(tag='Escroc_Vortex_Task', ticket=t, **kw),
        ],
        options=kw
    )


class Escroc_Vortex_Task(Task):
    '''

    '''

    def process(self):

        t = self.ticket

        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)

        startmember = int(self.conf.startmember) if hasattr(self.conf, "startmember") else 1
        members = range(startmember, int(self.conf.nmembers) + startmember) if hasattr( self.conf, "nmembers") else range(1, 36)

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

            self.sh.title('Toolbox input tb02')
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
            print t.prompt, 'tb02_a =', tb02_a
            print

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
            print t.prompt, 'tb03 =', tb03
            print

            self.sh.title('Toolbox input tb03_s')
            tb03_s = toolbox.input(
                alternate      = 'SnowpackInit',
                local          = 'PREP.nc',
                experiment     = 'spinup@' + os.getlogin(),
                geometry       = self.conf.geometry,
                date           = self.conf.datespinup,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'SnowpackState',
                model          = 'surfex',
                namespace      = 'cenvortex.multi.fr',
                fatal          = False,
            ),
            print t.prompt, 'tb03_s =', tb03_s
            print


#             if not tb03[0]:
# #                 tbi = toolbox.input(
# #                     role           = 'initial values of ground temperature',
# #                     kind           = 'climTG',
# #                     nativefmt      = 'netcdf',
# #                     local          = 'init_TG.nc',
# #                     geometry       = self.conf.geometry,
# #                     genv            = 'uenv:cen.01@CONST_CEN',
# #                     gvar           = 'climtg_[geometry::area]',
# #                     model          = 'surfex',
# #                     fatal          = False
# #                 ),
# #                 print t.prompt, 'tbi =', tbi
# #                 print
# 
#                 tbi_a = toolbox.input(
#                     alternate      = 'initial values of ground temperature',
#                     kind           = 'climTG',
#                     nativefmt      = 'netcdf',
#                     local          = 'init_TG.nc',
#                     experiment     = self.conf.xpid,
#                     geometry       = self.conf.geometry,
#                     model          = 'surfex',
#                     namespace      = 'cenvortex.multi.fr',
#                 ),
#
#                 print t.prompt, 'tbi_a =', tbi_a
#                 print

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
            if hasattr(self.conf, "namelist"):
                tb05 = toolbox.input(
                    role            = 'Nam_surfex',
                    remote          = self.conf.namelist,
                    kind            = 'namelist',
                    model           = 'surfex',
                    local           = 'OPTIONS.nam',
                )
            else:
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

            if hasattr(self.conf, "exesurfex"):
                self.sh.title('Toolbox executable tb06= tbx1')
                tb06 = tbx3 = toolbox.executable(
                    role           = 'Binary',
                    kind           = 'offline',
                    local          = 'OFFLINE',
                    model          = 'surfex',
                    remote          = self.conf.exesurfex + "/OFFLINE"
                )

                print t.prompt, 'tb06 =', tb06
                print

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

                    print t.prompt, 'tb07 =', tb07
                    print

                if not (tb03[0] or tb03_s[0]):

                    self.sh.title('Toolbox executable tb08= tbx3')
                    tb08 = tbx2 = toolbox.executable(
                        role           = 'Binary',
                        kind           = 'prep',
                        local          = 'PREP',
                        model          = 'surfex',
                        remote          = self.conf.exesurfex + "/PREP"
                    )

                    print t.prompt, 'tb08 =', tb08
                    print

            else:

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

                if not (tb02[0] or tb02_a[0]):

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

                if not (tb03[0] or tb03_s[0]):

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
                datebegin    = self.conf.datebegin,
                dateend      = self.conf.dateend,
                forcingname  = firstforcing
            )
            print t.prompt, 'tb09a =', tb09a
            print
            tb09a.run()

            # Take care : PGD parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
#             if not (tb02[0] or tb02_a[0]):
            if not tb02_a[0]:
                self.sh.title('Toolbox algo tb09 = PGD')
                tb09 = tbalgo2 = toolbox.algo(
                    kind         = 'pgd_from_forcing',
                    forcingname  = firstforcing,
                )
                print t.prompt, 'tb09 =', tb09
                print
                self.component_runner(tbalgo2, tbx1, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))

            # Take care : PREP parallelization will be available in v8.1 --> nproc and ntasks will have to be set to 40
            if not (tb03[0] or tb03_s[0]):
                self.sh.title('Toolbox algo tb09 = PREP')
                tb10 = tbalgo3 = toolbox.algo(
                    engine         = 'parallel',
                )
                print t.prompt, 'tb10 =', tb10
                print
                self.component_runner(tbalgo3, tbx2, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))

            self.sh.title('Toolbox algo tb11 = OFFLINE')
            tb11 = tbalgo4 = toolbox.algo(
                engine         = 'blind',
                binary         = 'OFFLINE',
                kind           = "escroc",
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                dateinit       = self.conf.datespinup,
                threshold      = self.conf.threshold,
                members        = footprints.util.rangex(members),
                subensemble    = self.conf.subensemble if hasattr(self.conf, "subensemble")  else "E2",
                ntasks         = 40 * int(self.conf.nnodes)
            )
            print t.prompt, 'tb11 =', tb11
            print
            self.component_runner(tbalgo4, tbx3)

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:

            for p, datebegin in enumerate(list_dates_begin_pro):
                dateend = list_dates_end_pro[p]
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
                print t.prompt, 'tb19 =', tb19
                print

                self.sh.title('Toolbox output tb20')
                tb20 = toolbox.output(
                    local          = 'mb[member]/PREP_[date:ymdh].nc',
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
                ),
                print t.prompt, 'tb20 =', tb20
                print

# The following condition does not work. --> Ask leffe how to do
#                 if not (tb02[0] or tb02_a[0]):
                tb21 = toolbox.output(
                    role           = 'SurfexClim',
                    kind           = 'pgdnc',
                    nativefmt      = 'netcdf',
                    local          = 'PGD.nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    model          = 'surfex',
                    namespace      = 'cenvortex.multi.fr',
                ),
                print t.prompt, 'tb21 =', tb21
                print

