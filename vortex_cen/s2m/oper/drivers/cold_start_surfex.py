# -*- coding: utf-8 -*-
'''
'''

from mkjob.nodes import Driver, Task
from vortex_cen.tasks.oper_research_mixin import CENTaskMixIn
import vortex
from vortex_cen.tools.monitoring import InputReportContext, OutputReportContext


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            PrepareForcing(tag='prepareforcing', ticket=t, **kw),
            Monthly_Surfex_Reanalysis(tag='Monthly_Surfex_Reanalysis', ticket=t, **kw),
        ],
        options=kw
    )


class PrepareForcing(CENTaskMixIn, Task):

    filter_execution_error = CENTaskMixIn.s2moper_filter_execution_error

    def process(self):

        t = self.ticket

        datebegin, dateend = self.get_period()
        rundate_forcing = self.get_rundate_forcing()

        list_geometry = self.get_list_geometry()
        source_safran, block_safran = self.get_source_safran()
        alternate_safran, alternate_block, alternate_geometry = self.get_alternate_safran()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            self.sh.title('Toolbox input tb01')
            tb01 = vortex.input(
                role           = 'Forcing',
                local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_[geometry::tag].nc',
                vapp           = self.conf.vapp,
                vconf          = '[geometry:domain]',
                block          = block_safran,
                source_app     = 'arpege' if source_safran == 'safran' else None,
                source_conf    = '4dvarfr' if source_safran == 'safran' else None,
                experiment     = self.conf.forcingid if source_safran == 'safran' else self.conf.xpid,
                geometry       = list_geometry,
                date           = rundate_forcing,
                datebegin      = datebegin,
                dateend        = dateend,
                nativefmt      = 'netcdf',
                namespace      = self.conf.namespace_in,
                kind           = 'MeteorologicalForcing',
                model          = source_safran,
                cutoff         = 'assimilation',
                fatal          = True
            ),
            print((t.prompt, 'tb01 =', tb01))
            print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo Prepare Forcing')
            tb09 = vortex.task(
                engine       = 'algo',
                kind         = 'prepareforcing',
                datebegin    = [datebegin],
                dateend      = [dateend],
                ntasks       = 1,
                geometry_in  = list_geometry,
                geometry_out = self.conf.geometry.tag,
                reprod_info  = self.get_reprod_info,
                role_members = 'Forcing',
            )
            print((t.prompt, 'tb09a =', tb09))
            print()
            tb09.run()

        if 'backup' in self.steps:

            with OutputReportContext(self, t):

                self.sh.title('Toolbox output SURFEX-ready forcing')
                tb10 = vortex.output(
                    local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
                    experiment     = self.conf.xpid,
                    block          = 'meteo',
                    geometry       = self.conf.geometry,
                    date           = self.conf.rundate,
                    datebegin      = datebegin,
                    dateend        = dateend,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = self.conf.namespace_out,
                    cutoff         = 'assimilation',
                    fatal          = True,
                ),
                print((t.prompt, 'tb10 =', tb10))
                print()


class Monthly_Surfex_Reanalysis(CENTaskMixIn, Task):
    '''

    '''

    filter_execution_error = CENTaskMixIn.s2moper_filter_execution_error

    def process(self):

        t = self.ticket

        datebegin, dateend = self.get_period()
        rundate_prep, alternate_rundate_prep = self.get_rundate_prep()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            self.sh.title('Toolbox input tb02')
            tb02 = vortex.input(
                role           = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                geometry       = self.conf.geometry,
                genv           = self.conf.cycle,
                gvar           = 'pgd_[geometry::tag]',
                model          = 'surfex',
                fatal          = True,
            ),
            print((t.prompt, 'tb02 =', tb02))
            print()

            self.sh.title('Toolbox input tb03')
            tb03 = vortex.input(
                role           = 'SnowpackInit',
                local          = 'PREP.nc',
                block          = self.conf.get('prep_block', 'offline'),
                vapp           = self.conf.prep_vapp,
                vconf          = self.conf.prep_vconf,
                experiment     = self.conf.prep_xpid,
                username       = self.conf.prep_user,
                geometry       = self.conf.geometry,
                datevalidity   = self.conf.prep_datevalidity,
                namespace      = 'vortex.multi.fr',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namebuild      = 'flat@cen',
                intent         = 'inout',
                fatal          = True,
            ),
            print((t.prompt, 'tb03 =', tb03))
            print()

            self.sh.title('Toolbox input tb04')
            tb04 = vortex.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapI_covers_param.bin',
                geometry       = self.conf.geometry,
                genv           = self.conf.cycle,
                source         = 'ecoclimap1',
                model          = 'surfex',
            ),
            print((t.prompt, 'tb04 =', tb04))
            print()

            self.sh.title('Toolbox input tb05')
            tb05 = vortex.input(
                role           = 'Surfex cover parameters',
                kind           = 'coverparams',
                nativefmt      = 'bin',
                local          = 'ecoclimapII_eu_covers_param.bin',
                geometry       = self.conf.geometry,
                genv           = self.conf.cycle,
                source         = 'ecoclimap2',
                model          = 'surfex',
            ),
            print((t.prompt, 'tb05 =', tb05))
            print()

            self.sh.title('Toolbox input tb06')
            tb06 = vortex.input(
                role            = 'Parameters for F06 metamorphism',
                kind            = 'ssa_params',
                genv            = self.conf.cycle,
                nativefmt       = 'netcdf',
                local           = 'drdt_bst_fit_60.nc',
                model           = 'surfex',
            )
            print((t.prompt, 'tb06 =', tb06))
            print()

            self.sh.title('Toolbox input tb07')
            tb07 = vortex.input(
                role            = 'Nam_surfex',
                source          = self.conf.monthly_namelist,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                intent          = 'inout',
                model           = 'surfex',
                local           = 'OPTIONS.nam',
            )

            print((t.prompt, 'tb07 =', tb07))
            print()

            self.sh.title('Toolbox executable tb08= tbx1')
            tb08 = tbx1 = vortex.executable(
                role           = 'Binary',
                kind           = 'offline',
                local          = 'OFFLINE',
                model          = 'surfex',
                genv           = self.conf.cycle,
                gvar           = 'master_offline_nompi',
            )

            print((t.prompt, 'tb08 =', tb08))
            print()

        if 'fetch' in self.steps:

            self.sh.title('Toolbox input tb01')
            tb01 = vortex.input(
                role           = 'Forcing',
                local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                block          = 'meteo',
                geometry       = self.conf.geometry,
                date           = self.conf.rundate,
                datebegin      = datebegin,
                dateend        = dateend,
                nativefmt      = 'netcdf',
                namespace      = self.conf.namespace_in,
                kind           = 'MeteorologicalForcing',
                model          = 's2m',
                cutoff         = 'assimilation',
                fatal          = True,
            ),
            print((t.prompt, 'tb01 =', tb01))
            print()

        if 'compute' in self.steps:

            firstforcing = 'FORCING_' + datebegin.strftime("%Y%m%d%H") + "_" + dateend.strftime("%Y%m%d%H") + ".nc"

            self.sh.title('Toolbox algo tb09a')
            tb10 = vortex.task(
                kind         = 'surfex_preprocess',
                datebegin    = datebegin,
                dateend      = dateend,
                forcingname  = firstforcing,
            )
            print((t.prompt, 'tb09a =', tb10))
            print()
            tb10.run()

            self.sh.title('Toolbox algo tb11 = OFFLINE')
            tb11 = tbalgo3 = vortex.task(
                engine         = 'parallel',
                binary         = 'OFFLINE',
                kind           = 'deterministic',
                datebegin      = datebegin,
                dateend        = dateend,
                dateinit       = self.conf.prep_datevalidity,
                threshold      = self.conf.threshold,
                daily          = False,
                reprod_info    = self.get_reprod_info,
            )
            print((t.prompt, 'tb11 =', tb11))
            print()

            self.component_runner(tbalgo3, tbx1)

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:

            with OutputReportContext(self, t):

                self.sh.title('Toolbox output tb11')
                tb11 = vortex.output(
                    local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    block          = 'pro',
                    geometry       = self.conf.geometry,
                    date           = self.conf.rundate,
                    datebegin      = datebegin,
                    dateend        = dateend,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = self.conf.namespace_out,
                    cutoff         = 'assimilation',
                    fatal          = False
                ),
                print((t.prompt, 'tb11 =', tb11))
                print()

                # Prep file is saved directly in the 03h run output of the same day in order to update
                # initial conditions for the next day
                self.sh.title('Toolbox output tb12')
                tb12 = vortex.output(
                    local          = 'PREP_[datevalidity:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    block          = 'prep',
                    geometry       = self.conf.geometry,
                    datevalidity   = dateend,
                    date           = self.conf.rundate.replace(hour = self.nightruntime.hour),
                    member         = 35,
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = self.conf.namespace_out,
                    cutoff         = 'assimilation',
                    fatal          = True
                ),
                print((t.prompt, 'tb12 =', tb12))
                print()
