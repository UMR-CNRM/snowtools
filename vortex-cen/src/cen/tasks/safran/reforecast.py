#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex import toolbox
from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn
from bronx.stdtypes.date import Period


def setup(t, **kw):
    return Driver(
        tag='safran',
        ticket=t,
        nodes=[
            Safran(tag='prvsaf', ticket=t, **kw),
        ],
        options=kw,
    )


class Safran(Task, S2MTaskMixIn):
    """
    Safran re-forecast task
    """

    def process(self):
        """Safran"""

        def untar_hook(t, rh):
                sh = t.sh
                tarname = sh.path.basename(rh.container.localpath())
                if sh.is_tarfile(tarname):
                    sh.untar(tarname)

        t = self.ticket
        datebegin = self.conf.datebegin.replace(hour=6)
        dateend = self.conf.dateend.replace(hour=6)

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            self.sh.title('Toolbox input tb01')
            tb01 = toolbox.input(
                role           = 'Ebauche',
                local          = 'ebauches_[geometry:area]_[datebegin:ymdh]_[dateend:ymdh]',
                kind           = 'packedguess',
                experiment     = self.conf.xpid,
                #vconf          = 'common',
                block          = 'guess',
                geometry        = self.conf.geometry[self.conf.vconf],
                nativefmt      = 'tar',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                date           = datebegin.ymdh,
                datebegin      = datebegin.ymdh,
                dateend        = dateend.ymdh,
                # The untar command is done in the Vortex algo component SurfexReforecast because
                # it also provides the list of repositories to consider as members
                #hook_autohook1 = (untar_hook, ),
            ),
            print(t.prompt, 'tb01 =', tb01)
            print()
#
#            rundate = datebegin
#            while rundate <= dateend:
#
#                if isinstance(self.conf.guess_xpid, dict):
#
#                    self.sh.title('Toolbox input tb6h')
#                    tb6h = toolbox.input(
#                        role           = 'Ebauche',
#                        local          = '[date::ymdh]/mb[member%03]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
#                        experiment     = self.conf.xpid,
#                        block          = self.conf.guess_block,
#                        geometry       = self.conf.geometry[self.conf.vconf],
#                        date           = rundate.ymd6h,
#                        cumul          = footprints.util.rangex(self.conf.prv_terms)[:33],
#                        nativefmt      = 'ascii',
#                        kind           = 'guess',
#                        model          = 'safran',
#                        source_app     = self.conf.source_app,
#                        source_conf    = self.conf.eps_conf,
#                        namespace      = 'vortex.cache.fr',
#                        member         = footprints.util.rangex(self.conf.members),
#                    ),
#                    print(t.prompt, 'tb6h =', tb6h)
#                    print()
#
#                    rundate = rundate + Period(days=3)
#
#                    self.sh.title('Toolbox input tb18h')
#                    tb18h = toolbox.input(
#                        role           = 'Ebauche',
#                        local          = '[date::ymdh]/mb[member%03]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
#                        experiment     = self.conf.xpid,
#                        block          = self.conf.guess_block,
#                        geometry       = self.conf.geometry[self.conf.vconf],
#                        date           = '{0:s}/-PT12H'.format(rundate.ymd6h),
#                        cumul          = footprints.util.rangex(self.conf.prv_terms)[4:],
#                        nativefmt      = 'ascii',
#                        kind           = 'guess',
#                        model          = 'safran',
#                        source_app     = self.conf.source_app,
#                        source_conf    = self.conf.eps_conf,
#                        namespace      = 'vortex.cache.fr',
#                        member         = footprints.util.rangex(self.conf.members),
#                    ),
#                    print(t.prompt, 'tb18h =', tb18h)
#                    print()
#
#                    rundate = rundate + Period(days=2)
#
#
#                else:
#
#                    # I-ARPEGE
#                    self.sh.title('Toolbox intput tb01')
#                    tb01 = toolbox.input(
#                        role           = 'Ebauche',
#                        local          = '{0:s}/P[date::yymdh]_[cumul:hour]'.format(rundate.ymd6h),
#                        experiment     = self.conf.xpid,
#                        block          = self.conf.guess_block,
#                        geometry       = self.conf.geometry[self.conf.vconf],
#                        date           = '{0:s}/-PT6H'.format(rundate.ymd6h),
#                        cumul          = footprints.util.rangex(self.conf.prv_terms),
#                        nativefmt      = 'ascii',
#                        kind           = 'guess',
#                        model          = 'safran',
#                        source_app     = self.conf.source_app,
#                        source_conf    = self.conf.arpege_conf,
#                        namespace      = self.conf.namespace,
#                    ),
#                    print(t.prompt, 'tb01 =', tb01)
#                    print()
#
#                    if self.conf.pearp:
#                        # II-PEARP
#                        self.sh.title('Toolbox intput tb01')
#                        tb01 = toolbox.input(
#                            role           = 'Ebauche',
#                            local          = '{0:s}/mb[member]/P[date::yymdh]_[cumul:hour]'.format(rundate.ymd6h),
#                            experiment     = self.conf.xpid,
#                            block          = self.conf.guess_block,
#                            geometry       = self.conf.geometry[self.conf.vconf],
#                            date           = '{0:s}/-PT6H'.format(rundate.ymd6h),
#                            cumul          = footprints.util.rangex(self.conf.prv_terms),
#                            nativefmt      = 'ascii',
#                            kind           = 'guess',
#                            model          = 'safran',
#                            source_app     = self.conf.source_app,
#                            source_conf    = self.conf.eps_conf,
#                            namespace      = self.conf.namespace,
#                            member         = footprints.util.rangex(self.conf.members),
#                            fatal          = False,
#                        ),
#                        print(t.prompt, 'tb01 =', tb01)
#                        print()
#
#
#                    rundate = rundate + Period(days=1)

            self.sh.title('Toolbox input tb03')
            tb03 = toolbox.input(
                role            = 'ListeMassif',
                genv            = self.conf.cycle,
                kind            = 'listem',
                model           = self.conf.model,
                local           = 'listem',
                geometry        = self.conf.geometry[self.conf.vconf],
            )
            print(t.prompt, 'tb03 =', tb03)
            print()

            self.sh.title('Toolbox input tb04')
            tb04 = toolbox.input(
                role            = 'ListeLimitesMassif',
                genv            = self.conf.cycle,
                kind            = 'listeml',
                model           = self.conf.model,
                local           = 'listeml',
                geometry        = self.conf.geometry[self.conf.vconf],
            )
            print(t.prompt, 'tb04 =', tb04)
            print()

            self.sh.title('Toolbox input tb05')
            tb05 = toolbox.input(
                role            = 'ListePost',
                genv            = self.conf.cycle,
                kind            = 'listeo',
                model           = self.conf.model,
                local           = 'listeo',
                geometry        = self.conf.geometry[self.conf.vconf],
            )
            print(t.prompt, 'tb05 =', tb05)
            print()

            if self.conf.vconf in ['alp', 'pyr']:

                self.sh.title('Toolbox input tb06')
                tb06 = toolbox.input(
                    role            = 'MoyRRmensuelles',
                    genv            = self.conf.cycle,
                    kind            = 'NORELmt',
                    model           = self.conf.model,
                    local           = 'NORELmt',
                    geometry        = self.conf.geometry[self.conf.vconf],
                )
                print(t.prompt, 'tb06 =', tb06)
                print()

            self.sh.title('Toolbox input tb07')
            tb07 = toolbox.input(
                role            = 'Clim',
                genv            = self.conf.cycle,
                kind            = 'rsclim',
                model           = self.conf.model,
                local           = 'rsclim.don',
                geometry        = self.conf.geometry[self.conf.vconf],
            )
            print(t.prompt, 'tb07 =', tb07)
            print()

            self.sh.title('Toolbox input tb08')
            tb08 = toolbox.input(
                role            = 'Clim',
                genv            = self.conf.cycle,
                kind            = 'icrccm',
                model           = self.conf.model,
                local           = 'icrccm.don',
                geometry        = self.conf.geometry[self.conf.vconf],
            )
            print(t.prompt, 'tb08 =', tb08)
            print()

            self.sh.title('Toolbox input tb09')
            tb09 = toolbox.input(
                role            = 'Nam_sorties',
                source          = 'namelist_sorties_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'SORTIES',
            )
            print(t.prompt, 'tb09 =', tb09)
            print()

            self.sh.title('Toolbox input tb14')
            tb14 = toolbox.input(
                role            = 'Nam_adapt',
                source          = 'namelist_adapt',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'ADAPT',
            )
            print(t.prompt, 'tb14 =', tb14)
            print()

            self.sh.title('Toolbox input tb10')
            tb10 = toolbox.input(
                role            = 'Nam_melange',
                source          = 'namelist_melange_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'MELANGE',
            )
            print(t.prompt, 'tb10 =', tb10)
            print()

            self.sh.title('Toolbox input tb11')
            tb11 = toolbox.input(
                role            = 'carac_post',
                genv            = self.conf.cycle,
                geometry        = self.conf.geometry[self.conf.vconf],
                kind            = 'carpost',
                model           = self.conf.model,
                local           = 'carpost.tar',
            )
            print(t.prompt, 'tb11 =', tb11)
            print()

            self.sh.title('Toolbox input tb12')
            tb12 = toolbox.input(
                role            = 'Nam_impress',
                source          = 'namelist_impress_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'IMPRESS',
            )
            print(t.prompt, 'tb12 =', tb12)
            print()

            self.sh.title('Toolbox input tb13')
            tb13 = toolbox.input(
                role            = 'Nam_observr',
                source          = 'namelist_observr_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'OBSERVR',
                fatal           = False,
            )
            print(t.prompt, 'tb13 =', tb13)
            print()

            self.sh.title('Toolbox input tb14')
            tb14 = toolbox.input(
                role            = 'Nam_analyse',
                source          = 'namelist_analyse_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'ANALYSE',
                fatal           = False,
            )
            print(t.prompt, 'tb14 =', tb14)
            print()

            self.sh.title('Toolbox input tb16')
            tb16 = toolbox.input(
                role            = 'Nam_ebauche',
                source          = 'namelist_ebauche_[geometry:area]',
                geometry        = self.conf.geometry[self.conf.vconf],
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'EBAUCHE',
                fatal           = False,
            )
            print(t.prompt, 'tb16 =', tb16)
            print()

            self.sh.title('Toolbox executable tb11 = tbx1')
            tb11 = tbx1 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'safrane',
                local          = 'safrane',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb11 =', tb11)
            print()

            self.sh.title('Toolbox executable tb12 = tbx2')
            tb12 = tbx2 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syrpluie',
                local          = 'syrpluie',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb12 =', tb12)
            print()

            self.sh.title('Toolbox executable tb13 = tbx3')
            tb13 = tbx3 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syrmrr',
                local          = 'syrmRR',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb13 =', tb13)
            print()

            self.sh.title('Toolbox executable tb14 = tbx4')
            tb14 = tbx4 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'sytist',
                local          = 'sytist',
                model          = self.conf.model,
            )
            print(t.prompt, 'tb14 =', tb14)
            print()

        if 'compute' in self.steps:

#            rundate = datebegin + Period(hours=12)  # Verrue : les simulations de Thomas commencent le 03/01 à 18h...
#            while rundate <= dateend:
#                self.sh.title('Running date {0:s}'.format(rundate.ymdh))
#                start = rundate.ymdh if rundate.hour == 6 else rundate + Period(hours=12)
#                stop  = start + Period(days=4)

             # datebegin/dateend are automatically detected by the algo component using available ressources
             #----------------------------------------------------------------------------------------------

            self.sh.title('Toolbox algo tb15 = SAFRANE')
            tb15 = tbalgo1 = toolbox.algo(
                engine         = 's2m',
                kind           = 'safrane',
                execution      = 'reforecast',
                ntasks         = self.conf.ntasks,
            )
            print(t.prompt, 'tb15 =', tb15)
            print()

            self.component_runner(tbalgo1, tbx1)

            self.sh.title('Toolbox algo tb16 = SYRPLUIE')
            tb16 = tbalgo2 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syrpluie',
                execution      = 'reforecast',
                ntasks         = self.conf.ntasks,
                #datebegin      = start.ymdh,
                #dateend        = stop.ymdh,
            )
            print(t.prompt, 'tb16 =', tb16)
            print()

            self.component_runner(tbalgo2, tbx2)

            self.sh.title('Toolbox algo tb17 = SYRMRR')
            tb17 = tbalgo3 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syrmrr',
                execution      = 'reforecast',
                ntasks         = self.conf.ntasks,
                #datebegin      = start.ymdh,
                #ateend        = stop.ymdh,
            )
            print(t.prompt, 'tb17 =', tb17)
            print()

            self.component_runner(tbalgo3, tbx3)

            self.sh.title('Toolbox algo tb18 = SYTIST')
            tb18 = tbalgo4 = toolbox.algo(
                engine         = 's2m',
                kind           = 'sytist',
                execution      = 'reforecast',
                ntasks         = self.conf.ntasks,
                #datebegin      = start.ymdh,
                #dateend        = stop.ymdh,
            )
            print(t.prompt, 'tb18 =', tb18)
            print()

            self.component_runner(tbalgo4, tbx4)

#                if isinstance(self.conf.guess_xpid, dict):
#                    # RECYF 2022 reforecast provides forecast every 2 days
#                    rundate = rundate + Period(days=2) + Period(hours=12)
#                else:
#                    rundate = rundate + Period(days=1)

        if 'late-backup' in self.steps:

            rundate = datebegin.replace(hour=18)  # Verrue : les simulations de Tom commencent le 03/01 à 18h...
            while rundate <= dateend:

                start = rundate if rundate.hour == 6 else rundate + Period(hours=12)
                stop  = start + Period(days=4)

                if not isinstance(self.conf.guess_xpid, dict):

                    self.sh.title('Toolbox output tb27')
                    tb27 = toolbox.output(
                        role           = 'Prv_massifs',
                        kind           = 'MeteorologicalForcing',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.arpege_conf,
                        local          = '[datebegin:subPT6H_ymdh]/mb035/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                        experiment     = self.conf.xpid,
                        block          = 'massifs',
                        geometry        = self.conf.geometry[self.conf.vconf],
                        nativefmt      = 'netcdf',
                        model          = self.conf.model,
                        date           = rundate.ymd6h,
                        datebegin      = rundate.ymd6h,
                        dateend        = '{0:s}/+PT96H'.format(rundate.ymd6h),
                        namespace      = 'vortex.multi.fr',
                        namebuild      = 'flat@cen',
                    ),
                    print(t.prompt, 'tb27 =', tb27)
                    print()

                    self.sh.title('Toolbox output tb28')
                    tb28 = toolbox.output(
                        role           = 'Prv_postes',
                        kind           = 'MeteorologicalForcing',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.arpege_conf,
                        local          = '[datebegin:subPT6H_ymdh]/mb035/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                        experiment     = self.conf.xpid,
                        block          = 'postes',
                        geometry        = self.conf.geometry[self.conf.vconf],
                        nativefmt      = 'netcdf',
                        model          = self.conf.model,
                        date           = rundate.ymd6h,
                        datebegin      = rundate.ymd6h,
                        dateend        = '{0:s}/+PT96H'.format(rundate.ymd6h),
                        namespace      = 'vortex.multi.fr',
                        namebuild      = 'flat@cen',
                    ),
                    print(t.prompt, 'tb28 =', tb28)
                    print()

                if isinstance(self.conf.guess_xpid, dict) or self.conf.pearp:

                    self.sh.title('Toolbox output tb27')
                    tb27 = toolbox.output(
                        role           = 'Prv_massifs',
                        kind           = 'MeteorologicalForcing',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.eps_conf,
                        local          = '[date:ymdh]/mb[member]/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                        experiment     = self.conf.xpid,
                        block          = 'massifs',
                        geometry        = self.conf.geometry[self.conf.vconf],
                        nativefmt      = 'netcdf',
                        model          = self.conf.model,
                        date           = rundate.ymdh,
                        datebegin      = start.ymd6h,
                        dateend        = stop.ymd6h,
                        namespace      = 'vortex.multi.fr',
                        member         = footprints.util.rangex(self.conf.members),
                        namebuild      = 'flat@cen',
                    ),
                    print(t.prompt, 'tb27 =', tb27)
                    print()

                    self.sh.title('Toolbox output tb28')
                    tb28 = toolbox.output(
                        role           = 'Prv_postes',
                        kind           = 'MeteorologicalForcing',
                        source_app     = self.conf.source_app,
                        source_conf    = self.conf.eps_conf,
                        local          = '[date:ymdh]/mb[member]/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc',
                        experiment     = self.conf.xpid,
                        block          = 'postes',
                        geometry        = self.conf.geometry[self.conf.vconf],
                        nativefmt      = 'netcdf',
                        model          = self.conf.model,
                        date           = rundate.ymdh,
                        datebegin      = start.ymd6h,
                        dateend        = stop.ymd6h,
                        namespace      = 'vortex.multi.fr',
                        member         = footprints.util.rangex(self.conf.members),
                        namebuild      = 'flat@cen',
                    ),
                    print(t.prompt, 'tb28 =', tb28)
                    print()

                if isinstance(self.conf.guess_xpid, dict):
                    # RECYF 2022 reforecast provides forecast every 2 days
                    rundate = rundate + Period(days=2) + Period(hours=12)
                else:
                    rundate = rundate + Period(days=1)

            print('==================================================================================================')
            print('==================================================================================================')
            raise Exception('INFO :The execution went well, do not take into account the following error')
