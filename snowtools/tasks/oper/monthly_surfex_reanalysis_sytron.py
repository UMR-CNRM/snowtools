# -*- coding: utf-8 -*-
'''
Created on 7 nov. 2017

@author: lafaysse
'''

from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import daterange, yesterday, tomorrow, Period
import footprints
from vortex.algo.components import DelayedAlgoComponentError


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            Monthly_Surfex_Reanalysis_Sytron(tag='Monthly_Surfex_Reanalysis_Sytron', ticket=t, **kw),
        ],
        options=kw
    )


class Monthly_Surfex_Reanalysis_Sytron(S2MTaskMixIn, Task):
    '''

    '''

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def process(self):

        t = self.ticket

        datebegin, dateend = self.get_period()
        rundate_forcing = self.get_rundate_forcing()
        rundate_prep, alternate_rundate_prep = self.get_rundate_prep()

        list_geometry = self.get_list_geometry()
        source_safran, block_safran = self.get_source_safran()
        alternate_safran, alternate_block, alternate_geometry = self.get_alternate_safran()
        exceptional_save_forcing = False

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            self.sh.title('Toolbox input tb01')
            tb01 = toolbox.input(
                role           = 'Forcing',
                local          = '[geometry::tag]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc' if len(list_geometry) > 1 else 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                vapp           = self.conf.vapp,
                vconf          = '[geometry:area]',
                block          = block_safran,
                source_app     = 'arpege' if source_safran == 'safran' else None,
                source_conf    = '4dvarfr' if source_safran == 'safran' else None,
                experiment     = self.conf.forcingid  if source_safran == 'safran' else self.conf.xpid,
                geometry       = list_geometry,
                date           = rundate_forcing,
                datebegin      = datebegin,
                dateend        = dateend,
                nativefmt      = 'netcdf',
                kind           = 'MeteorologicalForcing',
                namespace      = 'vortex.multi.fr',
                model          = source_safran,
                cutoff         = 'assimilation',
                fatal          = True
            ),
            print((t.prompt, 'tb01 =', tb01))
            print()

            self.sh.title('Toolbox input tb02')
            tb02 = toolbox.input(
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
            tb03 = toolbox.input(
                role           = 'SnowpackInit',
                local          = 'PREP.nc',
                block          = 'prep',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datevalidity   = datebegin,
                date           = rundate_prep,
                member         = 35,
                namespace      = 'vortex.multi.fr',
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                fatal          = False,
                cutoff         = 'assimilation'
            ),
            print((t.prompt, 'tb03 =', tb03))
            print()

            for i, alternate_prep in enumerate(alternate_rundate_prep):

                # fatal = i == len(alternate_rundate_prep) - 1

                self.sh.title('Toolbox input tb03b')
                tb03b = toolbox.input(
                    alternate      = 'SnowpackInit',
                    local          = 'PREP.nc',
                    block          = 'prep',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datevalidity   = datebegin,
                    date           = alternate_prep[0],
                    member         = 35,
                    namespace      = 'vortex.multi.fr',
                    intent         = 'inout',
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    fatal          = False,
                    cutoff         = alternate_prep[1]
                ),
                print((t.prompt, 'tb03b =', tb03b))
                print()

            # Last chance is the reanalysis if even the deterministic run was stopped:
            self.sh.title('Toolbox input tb03e')
            tb03d = toolbox.input(
                alternate      = 'SnowpackInit',
                local          = 'PREP.nc',
                experiment     = self.ref_reanalysis,
                vconf          = self.conf.geometry.tag,
                geometry       = self.conf.geometry,
                date           = datebegin,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'prep',
                fatal          = True,
            )

            print(t.prompt, 'tb03d =', tb03d)
            print()

            self.sh.title('Toolbox input tb04')
            tb04 = toolbox.input(
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
            tb05 = toolbox.input(
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
            tb06 = toolbox.input(
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
            tb07 = toolbox.input(
                role            = 'Nam_surfex',
                source          = 'OPTIONS_sytron.nam',
                genv            = self.conf.cycle,
                kind            = 'namelist',
                intent          = 'inout',
                model           = 'surfex',
                local           = 'OPTIONS.nam',
            )

            print((t.prompt, 'tb07 =', tb07))
            print()

            self.sh.title('Toolbox executable tb08= tbx1')
            tb08 = tbx1 = toolbox.executable(
                role           = 'Binary',
                kind           = 'offline',
                local          = 'OFFLINE',
                model          = 'surfex',
                genv           = self.conf.cycle,
                gvar           = 'master_surfex_offline_mpi',
            )

            print((t.prompt, 'tb08 =', tb08))
            print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb09a')
            tb09 = tbalgo1 = toolbox.algo(
                engine         = 's2m',
                kind         = 'prepareforcing',
                datebegin    = [datebegin],
                dateend      = [dateend],
                ntasks       = 1,
                geometry_in     = list_geometry,
                geometry_out     = self.conf.geometry.tag
            )
            print((t.prompt, 'tb09a =', tb09))
            print()
            tb09.run()

            firstforcing = 'FORCING_' + datebegin.strftime("%Y%m%d%H") + "_" + dateend.strftime("%Y%m%d%H") + ".nc"

            self.sh.title('Toolbox algo tb09a')
            tb10 = tbalgo2 = toolbox.algo(
                kind         = 'surfex_preprocess',
                datebegin    = datebegin,
                dateend      = dateend,
                forcingname  = firstforcing
            )
            print((t.prompt, 'tb09a =', tb10))
            print()
            tb10.run()

            self.sh.title('Toolbox algo tb11 = OFFLINE')
            tb11 = tbalgo3 = toolbox.algo(
                engine         = 'parallel',
                binary         = 'OFFLINE',
                kind           = 'deterministic',
                datebegin      = datebegin,
                dateend        = dateend,
                dateinit       = datebegin,
                threshold      = self.conf.threshold,
                daily          = False
            )
            print((t.prompt, 'tb11 =', tb11))
            print()

            self.component_runner(tbalgo3, tbx1)

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:
            if source_safran != 's2m' or exceptional_save_forcing:
                self.sh.title('Toolbox output tb10')
                tb10 = toolbox.output(
                    local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    block          = 'meteo_sytron',
                    geometry       = self.conf.geometry,
                    date           = self.conf.rundate,
                    datebegin      = datebegin,
                    dateend        = dateend,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = 'vortex.multi.fr',
                    cutoff         = 'assimilation',
                    fatal          = False
                ),
                print((t.prompt, 'tb10 =', tb10))
                print()

            self.sh.title('Toolbox output tb11')
            tb11 = toolbox.output(
                local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                block          = 'pro_sytron',
                geometry       = self.conf.geometry,
                date           = self.conf.rundate,
                datebegin      = datebegin,
                dateend        = dateend,
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                cutoff         = 'assimilation',
                fatal          = False
            ),
            print((t.prompt, 'tb11 =', tb11))
            print()

            # Prep file is saved directly in the 03h run output of the same day in order to update initial conditions for the next day
            self.sh.title('Toolbox output tb12')
            tb12 = toolbox.output(
                local          = 'PREP_[datevalidity:ymdh].nc',
                role           = 'SnowpackInit',
                experiment     = self.conf.xpid,
                block          = 'prep',
                geometry       = self.conf.geometry,
                datevalidity   = dateend,
                date           = self.conf.rundate.replace(hour = self.nightruntime.hour),
                member         = 36,
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                cutoff         = 'assimilation',
                fatal          = True
            ),
            print((t.prompt, 'tb12 =', tb12))
            print()
