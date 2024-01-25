# -*- coding:Utf-8 -*-

__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex import toolbox
from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn

from vortex.tools.systems import ExecutionError

from bronx.stdtypes.date import Date


def setup(t, **kw):
    return Driver(
        tag='safran_ana',
        ticket=t,
        nodes=[
            Safran(tag='safran_ana', ticket=t, **kw),
        ],
        options=kw,
    )


class Safran(Task, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def process(self):
        """Safran analysis"""

        t = self.ticket

        list_dates = self.get_list_seasons(self.conf.datebegin, self.conf.dateend)
        list_dates[0] = self.conf.datebegin

        if len(list_dates) > 40:
            raise ExecutionError("Too many years")

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            if 'guess_path' in self.conf:
                for rundate in list_dates:
                    datebegin = max(rundate, self.conf.datebegin)
                    dateend = min(datebegin.replace(year=datebegin.year + 1), self.conf.dateend)
                    season = datebegin.nivologyseason
                    y1 = datebegin.year
                    y2 = dateend.year
                    rundir = '{0:d}{1:d}'.format(y1, y2)

                    if 'hendrix:' in self.conf.guess_path or 'hendrix.meteo.fr:' in self.conf.guess_path:
                        hostname = 'hendrix.meteo.fr'
                        if '@' in self.conf.guess_path.split(':')[0]:
                            username = self.conf.guess_path.split(':')[0].split('@')[0]
                        else:
                            username = None
                        y1 = datebegin.year
                        y2 = dateend.year
                        actual_path = self.conf.guess_path.split(':')[1].rstrip('/')

                        self.sh.title('Toolbox input tb01')
                        tb01 = toolbox.input(
                            role           = 'Ebauche',
                            local          = '{0:s}/{0:s}.tar'.format(rundir),
                            remote         = '{0:s}/{1:s}.tar'.format(actual_path, season),
                            hostname       = hostname,
                            username       = username,
                            unknownflow    = True,
                            tube           = 'ftp',
                            kind           = 'guess',
                            model          = 'safran',
                            cutoff         = 'assimilation',
                            now            = True,
                        ),
                        print(t.prompt, 'tb01 =', tb01)
                        print()

                    else:

                        ndays = (dateend - datebegin).days
                        self.sh.title('Toolbox input tb01_a')
                        tb01 = toolbox.input(
                            role           = 'Ebauche',
                            remote         = '{0:s}/P[date:yymdh]'.format(self.conf.guess_path),
                            local          = '{0:s}/P[date:yymdh]'.format(rundir),
                            kind           = 'guess',
                            model          = 'safran',
                            cutoff         = 'assimilation',
                            cumul          = self.conf.cumul,
                            date           = ['{0:s}/-PT{1:s}H'.format(self.conf.dateend.ymd6h, str(d))
                                              for d in footprints.util.rangex(0, 24 * ndays, self.conf.cumul)],
                            unknownflow    = True,
                            now            = True,
                            fatal          = False,
                        ),
                        print(t.prompt, 'tb01_a =', tb01)
                        print()

                        self.sh.title('Toolbox input tb01_b')
                        tb01 = toolbox.input(
                            alternate      = 'Ebauche',
                            remote         = '{0:s}/{1:s}/P[date:yymdh]'.format(self.conf.guess_path, season),
                            local          = '{0:s}/P[date:yymdh]'.format(rundir),
                            kind           = 'guess',
                            model          = 'safran',
                            cutoff         = 'assimilation',
                            cumul          = self.conf.cumul,
                            date           = ['{0:s}/-PT{1:s}H'.format(self.conf.dateend.ymd6h, str(d))
                                              for d in footprints.util.rangex(0, 24 * ndays, self.conf.cumul)],
                            unknownflow    = True,
                            now            = True,
                        ),
                        print(t.prompt, 'tb01_b =', tb01)
                        print()

                        self.sh.title('Toolbox input tb01_c')
                        tb01 = toolbox.input(
                            alternate      = 'Ebauche',
                            remote         = '{0:s}/E[date:yymdh]'.format(self.conf.guess_path),
                            local          = '{0:s}/P[date:yymdh]'.format(rundir),
                            kind           = 'guess',
                            model          = 'safran',
                            cutoff         = 'assimilation',
                            cumul          = self.conf.cumul,
                            date           = ['{0:s}/-PT{1:s}H'.format(self.conf.dateend.ymd6h, str(d))
                                              for d in footprints.util.rangex(0, 24 * ndays, self.conf.cumul)],
                            unknownflow    = True,
                            now            = True,
                        ),
                        print(t.prompt, 'tb01_c =', tb01)
                        print()

                        self.sh.title('Toolbox input tb01_d')
                        tb01 = toolbox.input(
                            alternate      = 'Ebauche',
                            remote         = '{0:s}/{1:s}/E[date:yymdh]'.format(self.conf.guess_path, season),
                            local          = '{0:s}/P[date:yymdh]'.format(rundir),
                            kind           = 'guess',
                            model          = 'safran',
                            cutoff         = 'assimilation',
                            cumul          = self.conf.cumul,
                            date           = ['{0:s}/-PT{1:s}H'.format(self.conf.dateend.ymd6h, str(d))
                                              for d in footprints.util.rangex(0, 24 * ndays, self.conf.cumul)],
                            unknownflow    = True,
                            now            = True,
                        ),
                        print(t.prompt, 'tb01_d =', tb01)
                        print()

                    self.input_obs(t, datebegin, rundir, season)

            else:

                for rundate in list_dates:
                    datebegin = max(rundate, self.conf.datebegin)
                    dateend = min(datebegin.replace(year=datebegin.year + 1), self.conf.dateend)
                    season = datebegin.nivologyseason
                    next_season = dateend.nivologyseason
                    y1 = datebegin.year
                    y2 = dateend.year
                    rundir = '{0:d}{1:d}'.format(y1, y2)

                    if datebegin < Date(year=2002, month=8, day=1):

                        y1 = datebegin.year
                        y2 = dateend.year

                        # Avant 2002 on utilise des guess issus de era40
                        self.sh.title('Toolbox input tb01_a - ' + datebegin.ymdh)
                        tb01_a = toolbox.input(
                            role           = 'Ebauche',
                            # local          = 'mb035/P[date::yymdh]_[cumul:hour]',
                            local          = 'cep{0:d}_{1:d}'.format(y1, y2),
                            remote         = '/home/vernaym/s2m/cep/cep_{0:s}'.format(season),
                            hostname       = 'hendrix.meteo.fr',
                            unknownflow    = True,
                            # date           = rundate.ymd6h,
                            tube           = 'ftp',
                            username       = 'vernaym',
                            kind           = 'guess',
                            model          = 'safran',
                            cutoff         = 'assimilation',
                            now            = True,
                        ),
                        print(t.prompt, 'tb01_a =', tb01_a)
                        print()

                        # On doit prendre les guess de la saison suivante pour y trouver les 01/08 de l'année A+1...
                        # NE PAS REFAIRE CETTE ERREUR LORS D'UNE FUTURE GENERATION DES GUESS
                        if dateend < Date(2002, 8, 1):

                            self.sh.title('Toolbox input tb01_b - ' + dateend.ymdh)
                            tb01_b = toolbox.input(
                                role           = 'Ebauche',
                                # local          = 'mb035/P[date::yymdh]_[cumul:hour]',
                                local          = 'cep{0:d}_{1:d}'.format(y1 + 1, y2 + 1),
                                remote         = '/home/vernaym/s2m/cep/cep_{0:s}'.format(next_season),
                                hostname       = 'hendrix.meteo.fr',
                                unknownflow    = True,
                                # date           = rundate.ymd6h,
                                tube           = 'ftp',
                                username       = 'vernaym',
                                kind           = 'guess',
                                model          = 'safran',
                                cutoff         = 'assimilation',
                                now            = True,
                            ),
                            print(t.prompt, 'tb01_b =', tb01_b)
                            print()

                        elif dateend.ymd == '20020801':

                            self.sh.title('Toolbox input tb01 - ' + dateend.ymdh)
                            tb01_c = toolbox.input(
                                role           = 'Ebauche',
                                local          = '{0:d}/p{2:s}.tar'.format(rundir, next_season),
                                remote         = '/home/vernaym/s2m/[geometry]/{0:s}/p{1:s}.tar'.format(
                                    self.conf.guess_block, next_season),
                                hostname       = 'hendrix.meteo.fr',
                                unknownflow    = True,
                                username       = 'vernaym',
                                tube           = 'ftp',
                                geometry       = self.conf.vconf,
                                cumul          = self.conf.cumul,
                                nativefmt      = 'ascii',
                                kind           = 'guess',
                                model          = 'safran',
                                now            = True,
                                fatal          = False,
                            ),
                            print(t.prompt, 'tb01_c =', tb01_c)
                            print()

    #                 # A partir de la saison 2017-2018 on prend les guess produits par Vortex
    #                 elif datebegin >= Date(year=2018, month=8, day=1):
    #                     # I.1- EBAUCHE issue des A6 des réseaux 0/6/12/18h (J-n) d'assimilation d'ARPEGE et
                    #                     *l'A6 du réseau 0h J si présente pour couvrir (J-n) 6h -> J 6h
    #                     self.sh.title('Toolbox input tb01')
    #                     tb07_a = toolbox.input(
    #                         role           = 'Ebauche',
    #                         # local          = 'mb035/P[date::yymdh]_[cumul:hour]',
    #                         #local          = 'P[date::yymdh]_[cumul:hour]',
    #                         local          = 'p[date:nivologyseason].tar',
    #                         remote         = '/home/vernaym/s2m/cep/[local]',
    #                         hostname       = 'hendrix.meteo.fr',
    #                         unknownflow    = True,
    #                         username       = 'vernaym',
    #                         #experiment     = self.conf.xpid,
    #                         #block          = self.conf.guess_block,
    #                         #geometry       = self.conf.vconf,
    #                         #cutoff         = 'assimilation',
    #                         date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d))
                    #                         for d in footprints.util.rangex(6, 30, self.conf.cumul)],
    #                         cumul          = self.conf.cumul,
    #                         nativefmt      = 'ascii',
    #                         kind           = 'guess',
    #                         model          = 'safran',
    #                         #source_app     = 'arpege',
    #                         #source_conf    = '4dvarfr',
    #                         #namespace      = 'vortex.multi.fr',
    #                     ),
    #                     print t.prompt, 'tb17_a =', tb07_a
    #                     print

                    # Entre 2002 et 2017 on utilise des guess ARPEGE archivés
                    else:
                        self.sh.title('Toolbox input tb01 - ' + datebegin.ymdh)
                        tb01 = toolbox.input(
                            role           = 'Ebauche',
                            local          = '{0:s}/p{1:s}.tar'.format(rundir, season),
                            remote         = '/home/vernaym/s2m/[geometry]/{0:s}/p{1:s}.tar'.format(
                                self.conf.guess_block, season),
                            hostname       = 'hendrix.meteo.fr',
                            unknownflow    = True,
                            username       = 'vernaym',
                            tube           = 'ftp',
                            geometry       = self.conf.vconf,
                            cumul          = self.conf.cumul,
                            nativefmt      = 'ascii',
                            kind           = 'guess',
                            model          = 'safran',
                            now            = True,
                            fatal          = False,
                        ),
                        print(t.prompt, 'tb01 =', tb01)
                        print()

                self.input_obs(t, datebegin, rundir, season)

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
            print(t.prompt, 'tb07 =', tb07)
            print()

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
            print(t.prompt, 'tb08 =', tb08)
            print()

#             self.sh.title('Toolbox input tb08')
#             tb08 = toolbox.input(
#                 role            = 'NormalesClim',
#                 genv            = self.conf.cycle,
#                 gdomain         = self.conf.vconf,
#                 geometry        = '[gdomain]',
#                 kind            = 'NORELm',
#                 model           = self.conf.model,
#                 local           = 'NORELm',
#             )
#             print t.prompt, 'tb08 =', tb08
#             print

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
            print(t.prompt, 'tb09 =', tb09)
            print()

            self.sh.title('Toolbox input tb09')
            tb09 = toolbox.input(
                role            = 'MoyennesMensuellesRR',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = '[gdomain]',
                kind            = 'NORELot',
                model           = self.conf.model,
                local           = 'NORELot',
            )
            print(t.prompt, 'tb09 =', tb09)
            print()

            self.sh.title('Toolbox input tb09')
            tb09 = toolbox.input(
                role            = 'SurfZ',
                genv            = self.conf.cycle,
                gdomain         = self.conf.vconf,
                geometry        = '[gdomain]',
                kind            = 'surfz',
                model           = self.conf.model,
                local           = 'surfz',
                nativefmt       = 'ascii',
            )
            print(t.prompt, 'tb09 =', tb09)
            print()

#             self.sh.title('Toolbox input tb09')
#             tb09 = toolbox.input(
#                 role            = 'MoyennesMensuellesRR_TT',
#                 genv            = self.conf.cycle,
#                 gdomain         = self.conf.vconf,
#                 geometry        = '[gdomain]',
#                 kind            = 'NORELo',
#                 model           = self.conf.model,
#                 local           = 'NORELo',
#             )
#             print t.prompt, 'tb09 =', tb09
#             print

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
            print(t.prompt, 'tb09 =', tb09)
            print()

            if not self.conf.vconf == 'cor':

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
                print(t.prompt, 'tb12 =', tb12)
                print()

                self.sh.title('Toolbox input tb08')
                tb08 = toolbox.input(
                    role            = 'NormalesClimTT',
                    genv            = self.conf.cycle,
                    gdomain         = self.conf.vconf,
                    geometry        = '[gdomain]',
                    kind            = 'NORELmt',
                    model           = self.conf.model,
                    local           = 'NORELmt',
                )
                print(t.prompt, 'tb08 =', tb08)
                print()

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
            print(t.prompt, 'tb11 =', tb11)
            print()

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
            print(t.prompt, 'tb12 =', tb12)
            print()

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
            print(t.prompt, 'tb13 =', tb13)
            print()

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
            print(t.prompt, 'tb13 =', tb13)
            print()

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
            print(t.prompt, 'tb14 =', tb14)
            print()

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
            print(t.prompt, 'tb14 =', tb14)
            print()

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
            print(t.prompt, 'tb16 =', tb16)
            print()

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
            print(t.prompt, 'tb16 =', tb16)
            print()

            executables = self.executables(t)

        if 'compute' in self.steps:

#             if self.conf.datebegin < Date(2002, 8, 1):
# 
#                 self.sh.title('Toolbox algo tb22 = INTERCEP')
#                 tb22 = tbalgo0 = toolbox.algo(
#                     engine         = 's2m',
#                     kind           = 'intercep',
#                     datebegin      = self.conf.datebegin.ymd6h,
#                     dateend        = self.conf.dateend.ymd6h,
#                     ntasks         = self.conf.ntasks,
#                     # execution      = 'reanalysis',
#                     execution      = 'analysis',
#                 )
#                 print t.prompt, 'tb22 =', tb22
#                 print
# 
#                 self.component_runner(tbalgo0, executables['intercep'])

            # NB : La date des executions est fixée à J-1 car l'analyse SAFRAN va de J-1 6h à J 6H
            self.sh.title('Toolbox algo tb22 = SAFRANE')
            tb22 = tbalgo1 = toolbox.algo(
                engine         = 's2m',
                kind           = 'safrane',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                ntasks         = self.conf.ntasks,
                execution      = 'reanalysis',
                # execution      = 'analysis',
            )
            print(t.prompt, 'tb22 =', tb22)
            print()

            self.component_runner(tbalgo1, executables['safrane'])

#             self.sh.title('Toolbox algo tb23 = SYRPLUIE')
#             tb23 = tbalgo2 = toolbox.algo(
#                 engine         = 's2m',
#                 kind           = 'syrpluie',
#                 datebegin      = self.conf.datebegin.ymd6h,
#                 dateend        = self.conf.dateend.ymd6h,
#                 # members        = footprints.util.rangex(self.conf.members),
#                 ntasks         = self.conf.ntasks,
#                 execution      = 'reanalysis',
#                 # execution      = 'analysis',
#             )
#             print t.prompt, 'tb23 =', tb23
#             print
#
#             self.component_runner(tbalgo2, tbx2)

            self.sh.title('Toolbox algo tb23_b = SYPLUIE')
            tb23 = tbalgo3 = toolbox.algo(
                engine         = 's2m',
                kind           = 'sypluie',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
                execution      = 'reanalysis',
                # execution      = 'analysis',
            )
            print(t.prompt, 'tb23 =', tb23)
            print()

            self.component_runner(tbalgo3, executables['sypluie'])

            self.sh.title('Toolbox algo tb24 = SYVAPR')
            tb24 = tbalgo4 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syvapr',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
                execution      = 'reanalysis',
                # execution      = 'analysis',
            )
            print(t.prompt, 'tb24 =', tb24)
            print()

            self.component_runner(tbalgo4, executables['syvapr'])

            self.sh.title('Toolbox algo tb25 = SYVAFI')
            tb25 = tbalgo5 = toolbox.algo(
                engine         = 's2m',
                kind           = 'syvafi',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
                execution      = 'reanalysis',
                # execution      = 'analysis',
            )
            print(t.prompt, 'tb25 =', tb25)
            print()

            self.component_runner(tbalgo5, executables['syvafi'])

            self.sh.title('Toolbox algo tb26 = SYTIST')
            tb26 = tbalgo6 = toolbox.algo(
                engine         = 's2m',
                kind           = 'sytist',
                datebegin      = self.conf.datebegin.ymd6h,
                dateend        = self.conf.dateend.ymd6h,
                # members        = footprints.util.rangex(self.conf.members),
                ntasks         = self.conf.ntasks,
                execution      = 'reanalysis',
                # execution      = 'analysis',
            )
            print(t.prompt, 'tb26 =', tb26)
            print()

            self.component_runner(tbalgo6, executables['sytist'])

        if 'late-backup' in self.steps:

            for rundate in list_dates:
                datebegin = max(rundate, self.conf.datebegin)
                dateend = min(datebegin.replace(year=datebegin.year + 1), self.conf.dateend)
                y1 = datebegin.year
                y2 = dateend.year
                rundir = '{0:d}{1:d}'.format(y1, y2)

                if 'source_app' not in self.conf:
                    if rundate >= Date(2002, 8, 1):
                        source_app = 'arpege'
                        source_conf = '4dvarfr'
                    else:
                        source_app = 'ifs'
                        source_conf = 'era40'
                else:
                    source_app = self.conf.source_app
                    source_conf = self.conf.source_conf

                self.sh.title('Toolbox output tb27')
                tb27 = toolbox.output(
                    role           = 'Ana_massifs',
                    kind           = 'MeteorologicalForcing',
                    source_app     = source_app,
                    source_conf    = source_conf,
                    cutoff         = 'assimilation',
                    # local          = 'mb035/FORCING_massif.nc',
                    local          = '{0:s}/FORCING_massif_[datebegin::ymd6h]_[dateend::ymd6h].nc'.format(rundir),
                    experiment     = self.conf.xpid,
                    block          = 'massifs',
                    geometry        = self.conf.vconf,
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace,
                    namebuild      = 'flat@cen',
                ),
                print(t.prompt, 'tb27 =', tb27)
                print()

                self.sh.title('Toolbox output tb28')
                tb28 = toolbox.output(
                    role           = 'Ana_postes',
                    kind           = 'MeteorologicalForcing',
                    source_app     = source_app,
                    source_conf    = source_conf,
                    cutoff         = 'assimilation',
                    # local          = 'mb035/FORCING_postes.nc',
                    local          = '{0:s}/FORCING_postes_[datebegin::ymd6h]_[dateend::ymd6h].nc'.format(rundir),
                    experiment     = self.conf.xpid,
                    block          = 'postes',
                    geometry        = self.conf.vconf,
                    nativefmt      = 'netcdf',
                    model          = self.conf.model,
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    namespace      = self.conf.namespace,
                    namebuild      = 'flat@cen',
                ),
                print(t.prompt, 'tb28 =', tb28)
                print()

                self.sh.title('Toolbox output tb32')
                tb29 = toolbox.output(
                    role           = 'Liste_obs',
                    block          = 'liste_obs',
                    experiment     = self.conf.xpid,
                    geometry        = self.conf.vconf,
                    cutoff         = 'assimilation',
                    nativefmt      = 'tar',
                    model          = self.conf.model,
                    kind           = 'listobs',
                    datebegin      = datebegin.ymd6h,
                    dateend        = dateend.ymd6h,
                    local          = '{0:s}/liste_obs_[datebegin::ymd6h]_[dateend::ymd6h].tar.gz'.format(rundir),
                    namespace      = self.conf.namespace,
                    task           = '[local]',
                    namebuild      = 'flat@cen',
                )
                print(t.prompt, 'tb32 =', tb29)
                print()

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')

#########################################################################################
#                               END OF THE TASK
#########################################################################################

    def input_obs(self, t, datebegin, rundir, season):

        # TODO : Gérer le fait de ne pas prendre le fichier 2 de 6h le 01/08
        self.sh.title('Toolbox input tb02 - ' + datebegin.ymdh)
        tb02 = toolbox.input(
            role           = 'Observations',
            part           = 'all',
            geometry       = self.conf.vconf,
            kind           = 'observations',
            nativefmt      = 'ascii',
            unknownflow    = True,
            local          = '{0:s}/rs{1:s}.tar'.format(rundir, season),
            remote         = '/home/vernaym/s2m/[geometry]/obs/rs{0:s}.tar'.format(season),
            hostname       = 'hendrix.meteo.fr',
            tube           = 'ftp',
            model          = self.conf.model,
            now            = True,
        )
        print(t.prompt, 'tb02 =', tb02)
        print()

        self.sh.title('Toolbox input tb03 - ' + datebegin.ymdh)
        tb03 = toolbox.input(
            role           = 'Observations',
            part           = 'all',
            geometry       = self.conf.vconf,
            kind           = 'observations',
            nativefmt      = 'ascii',
            unknownflow    = True,
            local          = '{0:s}/n{1:s}.tar'.format(rundir, season),
            remote         = '/home/vernaym/s2m/[geometry]/obs/n{0:s}.tar'.format(season),
            hostname       = 'hendrix.meteo.fr',
            tube           = 'ftp',
            model          = self.conf.model,
            now            = True,
            fatal          = False,
        )
        print(t.prompt, 'tb03 =', tb03)
        print()

    def executables(self, t):

        executables = dict()

        if 'executables' in self.conf:

            for exe in ['safrane', 'syrpluie', 'sypluie', 'syvapr', 'syvafi', 'sytist']:

                self.sh.title('Toolbox executable {0:s}'.format(exe))
                executables[exe] = tbx = toolbox.executable(
                    role           = 'Binary',
                    remote         = '{0:s}/{1:s}'.format(self.conf.executables, exe),
                    kind           = exe,
                    local          = exe,
                )
                print(t.prompt, 'executable {0:s} ='.format(exe), tbx)
                print()

        else:

            self.sh.title('Toolbox executable tb17 = tbx0')
            executables['intercep'] = tbx0 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'intercep',
                local          = 'intercep_era40',
                model          = self.conf.model,
                fatal          = False,
            )
            print(t.prompt, 'tbx0 =', tbx0)
            print()

            self.sh.title('Toolbox executable tb17 = tbx1')
            executables['safrane'] = tbx1 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'safrane',
                local          = 'safrane',
                model          = self.conf.model,
            )
            print(t.prompt, 'tbx1 =', tbx1)
            print()

            self.sh.title('Toolbox executable tb18 = tbx2')
            executables['syrpluie'] = tbx2 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syrpluie',
                local          = 'syrpluie',
                model          = self.conf.model,
            )
            print(t.prompt, 'tbx2 =', tbx2)
            print()

            self.sh.title('Toolbox executable tb18_b = tbx3')
            executables['sypluie'] = tbx3 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'sypluie',
                local          = 'sypluie',
                model          = self.conf.model,
            )
            print(t.prompt, 'tbx3 =', tbx3)
            print()

            self.sh.title('Toolbox executable tb19 = tbx4')
            executables['syvapr'] = tbx4 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syvapr',
                local          = 'syvapr',
                model          = self.conf.model,
            )
            print(t.prompt, 'tbx4 =', tbx4)
            print()

            self.sh.title('Toolbox executable tb20 = tbx5')
            executables['syvafi'] = tbx5 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'syvafi',
                local          = 'syvafi',
                model          = self.conf.model,
            )
            print(t.prompt, 'tbx5 =', tbx5)
            print()

            self.sh.title('Toolbox executable tb21 = tbx6')
            executables['sytist'] = tbx6 = toolbox.executable(
                role           = 'Binary',
                genv           = self.conf.cycle,
                kind           = 'sytist',
                local          = 'sytist',
                model          = self.conf.model,
            )
            print(t.prompt, 'tbx6 =', tbx6)
            print()

        return executables
