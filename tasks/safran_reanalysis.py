#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex import toolbox
from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn

from bronx.stdtypes.date import Period, Date


def setup(t, **kw):
    return Driver(
        tag    = 'safran_reana',
        ticket = t,
        nodes  = [
            Safran(tag='safran_reana', ticket=t, **kw),
        ],
        options = kw,
    )


class Safran(Task, S2MTaskMixIn):

    def process(self):
        """Safran analysis"""

        t = self.ticket

        rundate = self.conf.datebegin

        if 'early-fetch' in self.steps:

            if self.conf.datebegin < Date(year=2002, month=8, day=1):

                # Avant 2002 on utilise des guess issus de era40
                self.sh.title('Toolbox input tb01')
                tb07_a = toolbox.input(
                    role           = 'Ebauche',
                    # local          = 'mb035/P[date::yymdh]_[cumul:hour]',
                    local          = 'cep_[date:nivologyseason]',
                    remote         = '/home/vernaym/s2m/[local]',
                    hostname       = 'hendrix.meteo.fr',
                    unknownflow    = True,
                    date           = self.conf.datebegin.ymdh,
                    tube           = 'ftp',
                    username       = 'vernaym',
                    kind           = 'guess',
                    model          = 'safran',
                    cutoff         = 'assimilation',
                ),
                print t.prompt, 'tb17_a =', tb07_a
                print

            while rundate <= self.conf.dateend:

                datebegin = rundate
                dateend = rundate + Period(days=1)

                # A partir de la saison 2017-2018 on prend les guess produits par Vortex
                if datebegin >= Date(year=2018, month=8, day=1):
                    # I.1- EBAUCHE issue des A6 des réseaux 0/6/12/18h (J-n) d'assimilation d'ARPEGE et l'A6 du réseau 0h J si présente pour couvrir (J-n) 6h -> J 6h
                    self.sh.title('Toolbox input tb01')
                    tb07_a = toolbox.input(
                        role           = 'Ebauche',
                        # local          = 'mb035/P[date::yymdh]_[cumul:hour]',
                        local          = 'P[date::yymdh]_[cumul:hour]',
                        experiment     = self.conf.xpid,
                        block          = self.conf.guess_block,
                        geometry       = self.conf.vconf,
                        cutoff         = 'assimilation',
                        date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d)) for d in footprints.util.rangex(6, 30, self.conf.cumul)],
                        cumul          = self.conf.cumul,
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = 'arpege',
                        source_conf    = '4dvarfr',
                        namespace      = 'vortex.multi.fr',
                    ),
                    print t.prompt, 'tb17_a =', tb07_a
                    print

                # Entre 2002 et 2017 on utilise des guess ARPEGE archivés
                elif datebegin >= Date(year=2002, month=8, day=1):

                    self.sh.title('Toolbox input tb01')
                    tb07_a = toolbox.input(
                        role           = 'Ebauche',
                        # local          = 'mb035/P[date::yymdh]_[cumul:hour]',
                        local          = 'P[date::yymdh]',
                        geometry        = self.conf.vconf,
                        cutoff         = 'assimilation',
                        date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d)) for d in footprints.util.rangex(6, 30, 6)],
                        cumul          = self.conf.cumul,
                        nativefmt      = 'ascii',
                        kind           = 'guess',
                        model          = 'safran',
                        source_app     = 'arpege',
                        source_conf    = '4dvarfr',
                        namespace      = 's2m.archive.fr',
                    ),
                    print t.prompt, 'tb17_a =', tb07_a
                    print

                # TODO : Gérer le fait de ne pas prendre le fichier 2 de 6h le 01/08
                self.sh.title('Toolbox input tb02')
                tb02 = toolbox.input(
                    role           = 'ObsSynop',
                    part           = 'synop',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    cutoff         = 'assimilation',
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    kind           = 'observations',
                    stage          = 'safrane',
                    nativefmt      = 'ascii',
                    date           = '{0:s}/+PT[term]H'.format(rundate.ymd6h),
                    term           = footprints.util.rangex(self.conf.ana_terms),
                    local          = 'S[date:yymdh]',
                    model          = self.conf.model,
                    namespace      = 's2m.archive.fr',
                    fatal          = False,
                )
                print t.prompt, 'tb02 =', tb02
                print

                self.sh.title('Toolbox input tb03')
                tb02 = toolbox.input(
                    role           = 'ObsRR',
                    part           = 'precipitation',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    cutoff         = 'assimilation',
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    kind           = 'observations',
                    stage          = 'sypluie',
                    nativefmt      = 'ascii',
                    date           = dateend.ymd6h,
                    local          = 'R[date:yymdh]',
                    model          = self.conf.model,
                    namespace      = 's2m.archive.fr',
                )
                print t.prompt, 'tb02 =', tb02
                print

                self.sh.title('Toolbox input tb03')
                tb03 = toolbox.input(
                    role           = 'HourlyObs',
                    part           = 'hourlyobs',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    cutoff         = 'assimilation',
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    kind           = 'observations',
                    stage          = 'safrane',
                    nativefmt      = 'ascii',
                    date           = datebegin.ymd6h,
                    local          = 'T[date:yymdh]',
                    model          = self.conf.model,
                    namespace      = 's2m.archive.fr',
                )
                print t.prompt, 'tb03 =', tb03
                print

                self.sh.title('Toolbox input tb04')
                tb04 = toolbox.input(
                    role           = 'ObsRS',
                    part           = 'radiosondage',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    cutoff         = 'assimilation',
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    kind           = 'observations',
                    stage          = 'safrane',
                    nativefmt      = 'ascii',
                    date           = '{0:s}/-PT24H/+PT[term]H'.format(datebegin.ymd6h),
                    term           = footprints.util.rangex(self.conf.ana_terms),
                    local          = 'A[date:yymdh]',
                    model          = self.conf.model,
                    namespace      = 'cendev.soprano.fr',
                    storage        = 'guppy.meteo.fr',
                    fatal          = False,
                )
                print t.prompt, 'tb04 =', tb04
                print

                self.sh.title('Toolbox input tb05')
                tb05 = toolbox.input(
                    role           = 'ObsNeb',
                    part           = 'nebulosity',
                    block          = 'observations',
                    experiment     = self.conf.xpid,
                    cutoff         = 'assimilation',
                    geometry       = self.conf.vconf,
                    suite          = 'oper',
                    kind           = 'observations',
                    stage          = 'safrane',
                    nativefmt      = 'ascii',
                    date           = datebegin.ymd6h,
                    local          = 'N[date:yymdh]',
                    model          = self.conf.model,
                    namespace      = 'cendev.soprano.fr',
                    storage        = 'guppy.meteo.fr',
                    fatal          = False,
                )
                print t.prompt, 'tb05 =', tb05
                print

                rundate = rundate + Period(days=1)


        if 'fetch' in self.steps:

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
                local           = 'listeo',
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
                role            = 'Nam_adapt',
                source          = 'namelist_adapt',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'ADAPT',
            )
            print t.prompt, 'tb14 =', tb14
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
            )
            print t.prompt, 'tb16 =', tb16
            print

            self.sh.title('Toolbox executable tb17 = tbx1')
            tb17 = tbx1 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'intercep',
                local          = 'intercep_era40',
                model          = self.conf.model,
            )
            print t.prompt, 'tb17 =', tb17
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

        if 'compute' in self.steps:

            rundate = self.conf.datebegin

            while rundate <= self.conf.dateend:

                datebegin = rundate
                dateend = rundate + Period(days=1)

                if datebegin <= Date(year=2002, month=7, day=31):

                    self.sh.title('Toolbox algo tb22 = INTERCEP')
                    tb22 = tbalgo1 = toolbox.algo(
                        engine         = 'blind',
                        kind           = 'intercep',
                        datebegin      = self.conf.datebegin.ymd6h,
                        dateend        = self.conf.dateend.ymd6h,
                        ntasks         = self.conf.ntasks,
                    )
                    print t.prompt, 'tb22 =', tb22
                    print

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

            rundate = self.conf.datebegin

            while rundate <= self.conf.dateend:

                datebegin = rundate
                dateend = rundate + Period(days=1)

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
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
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
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
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

                rundate = dateend

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
