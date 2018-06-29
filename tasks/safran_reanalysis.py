#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex import toolbox
from vortex.layout.nodes import Driver
from cen.layout.nodes import S2Mtask

from bronx.stdtypes.date import Period


def setup(t, **kw):
    return Driver(
        tag    = 'safran',
        ticket = t,
        nodes  = [
            Safran(tag='safran', ticket=t, **kw),
        ],
        options = kw,
    )


class Safran(S2Mtask):

    def process(self):
        """Safran analysis"""

        t = self.ticket

#       list_geometry = self.get_list_geometry()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            rundate = self.conf.datebegin
            while rundate <= self.conf.dateend:

                datebegin = rundate
                dateend = rundate + Period(days=1)

                # I- ARPEGE (J-5) -> J ou (J-1) -> J
                # --------------------

                # I.1- EBAUCHE issue des A6 des réseaux 0/6/12/18h (J-n) d'assimilation d'ARPEGE et l'A6 du réseau 0h J si présente pour couvrir (J-n) 6h -> J 6h
                self.sh.title('Toolbox input tb17_a')
                tb07_a = toolbox.input(
                    role           = 'Ebauche',
                    # local          = 'mb035/P[date::yymdh]_[cumul:hour]',
                    local          = 'P[date::yymdh]_[cumul:hour]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.guess_block,
                    geometry        = self.conf.vconf,
                    cutoff         = 'assimilation',
                    date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d)) for d in footprints.util.rangex(6, 30, self.conf.cumul)],
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                    fatal          = False,
                ),
                print t.prompt, 'tb17_a =', tb07_a
                print

    #             # I.2- EBAUCHE issue de la P6 du réseau 0h J de production d'ARPEGE
    #             # Si l'A6 du réseau 0h J n'est pas là (cas du run de 3h) on prend la P6 du réseau 0h J
    #             # RQ : il est fondamental de prendre une P6 pour avoir un cumul des RR sur 6h homogène avec le cumul dans les fichiers d'assimilation
    #             self.sh.title('Toolbox input tb17_b')
    #             tb07_b = toolbox.input(
    #                 alternate      = 'Ebauche',
    #                 local          = 'mb035/P[date::yymdh]_[cumul:hour]',
    #                 experiment     = self.conf.xpid,
    #                 block          = self.conf.guess_block,
    #                 geometry        = self.conf.vconf,
    #                 date           = '{0:s}/-PT[cumul:hour]H'.format(dateend.ymd6h),
    #                 cumul          = self.conf.cumul,
    #                 nativefmt      = 'ascii',
    #                 kind           = 'guess',
    #                 model          = 'safran',
    #                 source_app     = self.conf.source_app,
    #                 source_conf    = self.conf.deterministic_conf,
    #                 namespace      = self.conf.namespace,
    #             ),
    #             print t.prompt, 'tb17_b =', tb07_b
    #             print
    #
    #             # II- PEARP (J-5) -> J
    #             # --------------------
    #
    #             # II.1 EBAUCHE issue des prevision P0/P3/P6/P9/P12/P15/P18/P21/P24 du réseau 6h (J-n) de la PEARP pour couvrir (J-5) 6h -> (J-1) 6h
    #             # RQ : on ne peut pas mélanger des resources issues de runs différents pour conserver des cumuls de précipitations cohérents
    #             self.sh.title('Toolbox input tb18_a')
    #             tb18_a = toolbox.input(
    #                 role           = 'Ebauche',
    #                 # local          = 'mb[member]/P[date:addcumul_yymdh]',
    #                 local          = 'mb[member]/P[date::yymdh]_[cumul:hour]',
    #                 term           = '[cumul]',
    #                 experiment     = self.conf.xpid,
    #                 block          = self.conf.guess_block,
    #                 geometry        = self.conf.vconf,
    #                 cutoff         = 'production',
    #                 date           = ['{0:s}/+PT{1:s}H'.format(datebegin.ymd6h, str(24 * i)) for i in range(ndays)],
    #                 cumul          = footprints.util.rangex('0-24-3'),
    #                 nativefmt      = 'ascii',
    #                 kind           = 'guess',
    #                 model          = 'safran',
    #                 source_app     = self.conf.source_app,
    #                 source_conf    = self.conf.eps_conf,
    #                 namespace      = self.conf.namespace,
    #                 member         = footprints.util.rangex(self.conf.pearp_members),
    #                 fatal          = False,
    #             ),
    #             print t.prompt, 'tb18_a =', tb18_a
    #             print

                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
                    role           = 'ObsSynop',
                    part           = 'synop',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    kind           = 'observations',
                    stage          = 'safrane',
                    nativefmt      = 'ascii',
                    date           = '{0:s}/+PT[term]H'.format(rundate.ymd6h),
                    term           = footprints.util.rangex(self.conf.ana_terms),
                    local          = 'S[date:yymdh]',
                    model          = self.conf.model,
                    namespace      = 'cendev.soprano.fr',
                    storage        = 'guppy.meteo.fr',
                    fatal          = False,
                )
                print t.prompt, 'tb01 =', tb01
                print

                self.sh.title('Toolbox input tb02')
                tb02 = toolbox.input(
                    role           = 'ObsRR',
                    part           = 'precipitation',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    fatal          = False,
                    kind           = 'observations',
                    stage          = 'sypluie',
                    nativefmt      = 'ascii',
                    date           = dateend.ymd6h,
                    local          = 'R[date:yymdh]',
                    model          = self.conf.model,
                    namespace      = 'cendev.soprano.fr',
                    storage        = 'guppy.meteo.fr',
                )
                print t.prompt, 'tb02 =', tb02
                print

                self.sh.title('Toolbox input tb03')
                tb03 = toolbox.input(
                    role           = 'HourlyObs',
                    part           = 'hourlyobs',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    fatal          = False,
                    kind           = 'observations',
                    stage          = 'safrane',
                    nativefmt      = 'ascii',
                    date           = datebegin.ymd6h,
                    local          = 'T[date:yymdh]',
                    model          = self.conf.model,
                    namespace      = 'cendev.soprano.fr',
                    storage        = 'guppy.meteo.fr',
                )
                print t.prompt, 'tb03 =', tb03
                print

                self.sh.title('Toolbox input tb04')
                tb04 = toolbox.input(
                    role           = 'ObsRS',
                    part           = 'radiosondage',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    fatal          = False,
                    kind           = 'observations',
                    stage          = 'safrane',
                    nativefmt      = 'ascii',
                    date           = '{0:s}/-PT24H/+PT[term]H'.format(datebegin.ymd6h),
                    term           = footprints.util.rangex(self.conf.ana_terms),
                    local          = 'A[date:yymdh]',
                    model          = self.conf.model,
                    namespace      = 'cendev.soprano.fr',
                    storage        = 'guppy.meteo.fr',
                )
                print t.prompt, 'tb04 =', tb04
                print

                self.sh.title('Toolbox input tb05')
                tb05 = toolbox.input(
                    role           = 'ObsNeb',
                    part           = 'nebulosity',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    fatal          = False,
                    kind           = 'observations',
                    stage          = 'safrane',
                    nativefmt      = 'ascii',
                    date           = datebegin.ymd6h,
                    local          = 'N[date:yymdh]',
                    model          = self.conf.model,
                    namespace      = 'cendev.soprano.fr',
                    storage        = 'guppy.meteo.fr',
                )
                print t.prompt, 'tb05 =', tb05
                print

                rundate = rundate + Period(days=1)

            self.sh.title('Toolbox input tb07')
            tb07 = toolbox.input(
                role            = 'ListeMassif',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = '[gdomain]',
                kind            = 'listem',
                model           = self.conf.model,
                local           = 'listem',
            )
            print t.prompt, 'tb07 =', tb07
            print

            self.sh.title('Toolbox input tb08')
            tb08 = toolbox.input(
                role            = 'ListeLimitesMassif',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = '[gdomain]',
                kind            = 'listeml',
                model           = self.conf.model,
                local           = 'listeml',
            )
            print t.prompt, 'tb08 =', tb08
            print

            self.sh.title('Toolbox input tb09')
            tb09 = toolbox.input(
                role            = 'ListePost',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = '[gdomain]',
                kind            = 'listeo',
                model           = self.conf.model,
                local           = 'listeo' if self.conf.vconf == 'alp' else 'lysteo',
            )
            print t.prompt, 'tb09 =', tb09
            print

            self.sh.title('Toolbox input tb09')
            tb09 = toolbox.input(
                role            = 'carac_post',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = '[gdomain]',
                kind            = 'carpost',
                model           = self.conf.model,
                local           = 'carpost.tar',
            )
            print t.prompt, 'tb09 =', tb09
            print

            if not self.conf.vconf == 'cor':

                self.sh.title('Toolbox input tb10')
                tb10 = toolbox.input(
                    role            = 'MoyRRmensuelles',
                    genv            = self.conf.cycle,
                    gdomain         = self.conf.vconf,
                    geometry        = '[gdomain]',
                    kind            = 'NORELmt',
                    model           = self.conf.model,
                    local           = 'NORELmt',
                    fatal           = False,
                )
                print t.prompt, 'tb10 =', tb10
                print

                self.sh.title('Toolbox input tb12')
                tb12 = toolbox.input(
                    role            = 'BlackList',
                    genv            = self.conf.cycle,
                    gdomain         = self.conf.vconf,
                    geometry        = '[gdomain]',
                    kind            = 'blacklist',
                    model           = self.conf.model,
                    local           = 'BLACK',
                    fatal           = False,
                )
                print t.prompt, 'tb12 =', tb12
                print

            self.sh.title('Toolbox input tb11')
            tb11 = toolbox.input(
                role            = 'Clim',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = '[gdomain]',
                kind            = 'rsclim',
                model           = self.conf.model,
                local           = 'rsclim.don',
            )
            print t.prompt, 'tb11 =', tb11
            print

            self.sh.title('Toolbox input tb12')
            tb12 = toolbox.input(
                role            = 'Clim',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = '[gdomain]',
                kind            = 'icrccm',
                model           = self.conf.model,
                local           = 'icrccm.don',
            )
            print t.prompt, 'tb12 =', tb12
            print

            self.sh.title('Toolbox input tb13')
            tb13 = toolbox.input(
                role            = 'Nam_sorties',
                source          = 'namelist_sorties_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'SORTIES',
                fatal           = False,
            )
            print t.prompt, 'tb13 =', tb13
            print

            self.sh.title('Toolbox input tb13')
            tb13 = toolbox.input(
                role            = 'Nam_analyse',
                source          = 'namelist_analyse_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'ANALYSE',
                fatal           = False,
            )
            print t.prompt, 'tb13 =', tb13
            print

            self.sh.title('Toolbox input tb14')
            tb14 = toolbox.input(
                role            = 'Nam_melange',
                source          = 'namelist_melange_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'MELANGE',
                fatal           = False,
            )
            print t.prompt, 'tb14 =', tb14
            print

#            self.sh.title('Toolbox input tb15')
#            tb15 = toolbox.input(
#                role            = 'Nam_impress',
#                source          = 'namelist_impress_{0:s}'.format(self.conf.geometry.area),
#                genv            = self.conf.cycle,
#                kind            = 'namelist',
#                model           = self.conf.model,
#                local           = 'IMPRESS',
#                fatal           = False,
#            )
#            print t.prompt, 'tb15 =', tb15
#            print

            self.sh.title('Toolbox input tb16')
            tb16 = toolbox.input(
                role            = 'Nam_observr',
                source          = 'namelist_observr_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'OBSERVR',
                fatal           = False,
            )
            print t.prompt, 'tb16 =', tb16
            print

            self.sh.title('Toolbox input tb16')
            tb16 = toolbox.input(
                role            = 'Nam_ebauche',
                source          = 'namelist_ebauche_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'EBAUCHE',
                fatal           = False,
            )
            print t.prompt, 'tb16 =', tb16
            print

            self.sh.title('Toolbox executable tb17 = tbx1')
            tb17 = tbx1 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'safrane',
                local          = 'safrane',
                model          = self.conf.model,
            )
            print t.prompt, 'tb17 =', tb17
            print

            self.sh.title('Toolbox executable tb18 = tbx2')
            tb18 = tbx2 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syrpluie',
                local          = 'syrpluie',
                model          = self.conf.model,
            )
            print t.prompt, 'tb18 =', tb18
            print

            self.sh.title('Toolbox executable tb18_b = tbx3')
            tb18_b = tbx3 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'sypluie',
                local          = 'sypluie',
                model          = self.conf.model,
            )
            print t.prompt, 'tb18_b =', tb18_b
            print

            self.sh.title('Toolbox executable tb19 = tbx4')
            tb19 = tbx4 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syvapr',
                local          = 'syvapr',
                model          = self.conf.model,
            )
            print t.prompt, 'tb19 =', tb19
            print

            self.sh.title('Toolbox executable tb20 = tbx5')
            tb20 = tbx5 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syvafi',
                local          = 'syvafi',
                model          = self.conf.model,
            )
            print t.prompt, 'tb20 =', tb20
            print

            self.sh.title('Toolbox executable tb21 = tbx6')
            tb21 = tbx6 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'sytist',
                local          = 'sytist',
                model          = self.conf.model,
            )
            print t.prompt, 'tb21 =', tb21
            print

        if 'fetch' in self.steps:
            pass

        if 'compute' in self.steps:

            # NB : La date des executions est fixée à J-1 car l'analyse SAFRAN va de J-1 6h à J 6H
            self.sh.title('Toolbox algo tb22 = SAFRANE')
            tb22 = tbalgo1 = toolbox.algo(
                engine         = 'blind',
                kind           = 'safrane',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                ntasks         = self.conf.ntasks,
            )
            print t.prompt, 'tb22 =', tb22
            print

            self.component_runner(tbalgo1, tbx1)

            self.sh.title('Toolbox algo tb23 = SYRPLUIE')
            tb23 = tbalgo2 = toolbox.algo(
                engine         = 'blind',
                kind           = 'syrpluie',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
            )
            print t.prompt, 'tb23 =', tb23
            print

            self.component_runner(tbalgo2, tbx2)

            self.sh.title('Toolbox algo tb23_b = SYPLUIE')
            tb23 = tbalgo3 = toolbox.algo(
                engine         = 'blind',
                kind           = 'sypluie',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
            )
            print t.prompt, 'tb23 =', tb23
            print

            self.component_runner(tbalgo3, tbx3)

            self.sh.title('Toolbox algo tb24 = SYVAPR')
            tb24 = tbalgo4 = toolbox.algo(
                engine         = 'blind',
                kind           = 'syvapr',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
            )
            print t.prompt, 'tb24 =', tb24
            print

            self.component_runner(tbalgo4, tbx4)

            self.sh.title('Toolbox algo tb25 = SYVAFI')
            tb25 = tbalgo5 = toolbox.algo(
                engine         = 'blind',
                kind           = 'syvafi',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
            )
            print t.prompt, 'tb25 =', tb25
            print

            self.component_runner(tbalgo5, tbx5)

            self.sh.title('Toolbox algo tb26 = SYTIST')
            tb26 = tbalgo6 = toolbox.algo(
                engine         = 'blind',
                kind           = 'sytist',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                execution      = 'analysis',
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
            )
            print t.prompt, 'tb26 =', tb26
            print

            self.component_runner(tbalgo6, tbx6)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            self.sh.title('Toolbox output tb27')
            tb27 = toolbox.output(
                role           = 'Ana_massifs',
                kind           = 'MeteorologicalForcing',
                source_app     = 'arpege',
                source_conf    = '4dvarfr',
                cutoff         = 'assimilation',
                # local          = 'mb035/FORCING_massif.nc',
                local          = 'FORCING_massif.nc',
                experiment     = self.conf.xpid,
                block          = 'massifs',
                geometry        = self.conf.vconf,
                nativefmt      = 'netcdf',
                model          = self.conf.model,
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                namespace      = self.conf.cennamespace,
            ),
            print t.prompt, 'tb27 =', tb27
            print

            self.sh.title('Toolbox output tb28')
            tb27 = toolbox.output(
                role           = 'Ana_postes',
                kind           = 'MeteorologicalForcing',
                source_app     = 'arpege',
                source_conf    = '4dvarfr',
                cutoff         = 'assimilation',
                # local          = 'mb035/FORCING_postes.nc',
                local          = 'FORCING_postes.nc',
                experiment     = self.conf.xpid,
                block          = 'postes',
                geometry        = self.conf.vconf,
                nativefmt      = 'netcdf',
                model          = self.conf.model,
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                namespace      = self.conf.cennamespace,
            ),
            print t.prompt, 'tb28 =', tb27
            print

#             self.sh.title('Toolbox output tb29')
#             tb27 = toolbox.output(
#                 role           = 'Ana_massifs',
#                 kind           = 'MeteorologicalForcing',
#                 source_app     = 'arpege',
#                 source_conf    = 'pearp',
#                 cutoff         = 'assimilation',
#                 local          = 'mb[member]/FORCING_massif.nc',
#                 experiment     = self.conf.xpid,
#                 block          = 'massifs',
#                 geometry        = self.conf.vconf,
#                 nativefmt      = 'netcdf',
#                 model          = self.conf.model,
#                 datebegin      = datebegin.ymd6h,
#                 dateend        = dateend.ymd6h,
#                 namespace      = self.conf.namespace,
#                 member         = footprints.util.rangex(self.conf.pearp_members),
#             ),
#             print t.prompt, 'tb29 =', tb27
#             print
#
#             self.sh.title('Toolbox output tb30')
#             tb27 = toolbox.output(
#                 role           = 'Ana_postes',
#                 kind           = 'MeteorologicalForcing',
#                 source_app     = 'arpege',
#                 source_conf    = 'pearp',
#                 cutoff         = 'assimilation',
#                 local          = 'mb[member]/FORCING_postes.nc',
#                 experiment     = self.conf.xpid,
#                 block          = 'postes',
#                 geometry        = self.conf.vconf,
#                 nativefmt      = 'netcdf',
#                 model          = self.conf.model,
#                 datebegin      = datebegin.ymd6h,
#                 dateend        = dateend.ymd6h,
#                 namespace      = self.conf.namespace,
#                 member         = footprints.util.rangex(self.conf.pearp_members),
#             ),
#             print t.prompt, 'tb30 =', tb27
#             print

            self.sh.title('Toolbox output tb31')
            tb28 = toolbox.output(
                role           = 'Listing',
                block          = 'listing',
                experiment     = self.conf.xpid,
                cutoff         = 'assimilation',
                geometry        = self.conf.vconf,
                format         = 'ascii',
                kind           = 'listing',
                # local          = 'mb035/{glob:a:\w+}.out',
                local          = '{glob:a:\w+}.out',
                namespace      = self.conf.cennamespace,
                task           = '[local]',
                # date           = self.conf.rundate.ymdh,
            )
            print t.prompt, 'tb31 =', tb28
            print

            self.sh.title('Toolbox output tb32')
            tb29 = toolbox.output(
                role           = 'Liste_obs',
                block          = 'listing',
                experiment     = self.conf.xpid,
                geometry        = self.conf.vconf,
                cutoff         = 'assimilation',
                format         = 'ascii',
                kind           = 'listing',
                # local          = 'mb035/liste_obs_{glob:a:\w+}',
                local          = 'liste_obs_{glob:a:\w+}',
                namespace      = self.conf.cennamespace,
                task           = '[local]',
                # date           = self.conf.rundate.ymdh,
            )
            print t.prompt, 'tb32 =', tb29
            print

#             self.sh.title('Toolbox output tb33')
#             tb28 = toolbox.output(
#                 role           = 'Listing',
#                 block          = 'listing',
#                 experiment     = self.conf.xpid,
#                 geometry        = self.conf.vconf,
#                 cutoff         = 'assimilation',
#                 format         = 'ascii',
#                 kind           = 'listing',
#                 local          = 'mb{glob:a:\d+}/{glob:b:\w+}.out',
#                 seta           = '[glob:a]',
#                 member         = '[seta]',
#                 namespace      = self.conf.namespace,
#                 task           = '[local]',
#                 date           = self.conf.rundate.ymdh,
#             )
#             print t.prompt, 'tb33 =', tb28
#             print
#
#             self.sh.title('Toolbox output tb34')
#             tb29 = toolbox.output(
#                 role           = 'Liste_obs',
#                 block          = 'listing',
#                 experiment     = self.conf.xpid,
#                 geometry        = self.conf.vconf,
#                 cutoff         = 'assimilation',
#                 format         = 'ascii',
#                 kind           = 'listing',
#                 local          = 'mb{glob:a:\d+}/liste_obs_{glob:b:\w+}',
#                 seta           = '[glob:a]',
#                 member         = '[seta]',
#                 namespace      = self.conf.namespace,
#                 task           = '[local]',
#                 date           = self.conf.rundate.ymdh,
#             )
#             print t.prompt, 'tb34 =', tb29
#             print

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
