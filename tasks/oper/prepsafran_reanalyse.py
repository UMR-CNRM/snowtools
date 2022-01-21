#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

from cen.layout.nodes import S2MTaskMixIn
import footprints
from iga.tools.apps import OpTask
from snowtools.bronx.stdtypes.date import Date
from vortex import toolbox
from vortex.layout.nodes import Driver

logger = footprints.loggers.getLogger(__name__)


def setup(t, **kw):
    return Driver(
        tag    = 'pearp2safran',
        ticket = t,
        nodes  = [
            Reanalyses(tag='prepsafreana', ticket=t, **kw),
        ],
        options = kw,
    )


class Reanalyses(OpTask, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket
        # TODO :
        # La tâche actuelle ne marche pas au changment de saison :
        # On doit effectuer le changement de saison lorsque rundate devient le 01/08
        # On veut alors initialiser une nouvelle archive contenant les guess entre le 31/07 6h et 08/01 6h
        season = Date.nivologyseason.fget(self.conf.rundate)
        datebegin, dateend = self.get_period()

        def tb01_generic_hook1(t, rh):
            sh = t.sh
            tarname = sh.path.basename(rh.container.localpath())
            if sh.is_tarfile(tarname):
                sh.untar(tarname)
            sh.rm(tarname)

        if 'early-fetch' in self.steps:

            if not (self.conf.rundate.month == 8 and self.conf.rundate.day == 1):

                # Récupération de l'archive de la veille (tâche de 12h J-1)
                # TODO :
                # L'archive devrait couvrir la période allant du 31/07 précendent à 6h jusqu'à J-1 6h bien que
                # la réanalyse mensuelle n'utilise les guess que jusqu'à J-4 6h

                # TODO : Ajouter un mode secours en récupérant les archives de J-2, J-3,... et en les complètant
                # avec les guess manquants
                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
                    role           = 'Reanalyse',
                    local          = 'p{0:s}_{1:s}.tar'.format(season, self.conf.vconf),
                    experiment     = self.conf.xpid,
                    block          = 'guess',
                    nativefmt      = 'tar',
                    kind           = 'packedguess',
                    model          = 'safran',
                    hook_autohook1 = (tb01_generic_hook1, ),
                    vapp           = self.conf.vapp,
                    vconf          = self.conf.vconf,
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymdh),
                    begindate      = datebegin.ymd6h,
                    enddate        = '{0:s}/-PT24H'.format(dateend.ymd6h),
                    geometry       = self.conf.vconf,
                    #cutoff         = 'assimilation',
                    intent         = 'inout',
                    fatal          = True,
                )
                print(t.prompt, 'tb01 =', tb01)
                print()

            # Récupération des guess de la veille à ajouter à l'archive
            self.sh.title('Toolbox input tb02')
            tb02 = toolbox.input(
                role           = 'Ebauche',
                local          = 'P[date::addcumul_yymdh]',
                experiment     = self.conf.xpid,
                block          = 'guess',
                geometry       = self.conf.vconf,
                cutoff         = 'assimilation',
                date           = ['{0:s}/+PT96H/-PT{1:s}H'.format(dateend.ymd6h, str(d)) for d in footprints.util.rangex(6, 30, self.conf.cumul)],
                cumul          = self.conf.cumul,
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = 'vortex.cache.fr',
                fatal          = True,
            ),
            print(t.prompt, 'tb02 =', tb02)
            print()

            # Mode secours : on prend la prévision correspondante
            self.sh.title('Toolbox input tb02')
            tb02 = toolbox.input(
                alternate      = 'Ebauche',
                local          = 'P[date::addcumul_yymdh]',
                experiment     = self.conf.xpid,
                block          = 'guess',
                geometry       = self.conf.vconf,
                cutoff         = 'prevision',
                date           = ['{0:s}/+PT96H/-PT{1:s}H'.format(dateend.ymd6h, str(d)) for d in footprints.util.rangex(6, 30, self.conf.cumul)],
                cumul          = self.conf.cumul,
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = 'vortex.cache.fr',
                fatal          = True,
            ),
            print(t.prompt, 'tb02 =', tb02)
            print()

        if 'late-backup' in self.steps:

            # Mise à jour de l'archive,
            # TODO :
            # Elle devrait désormais couvrir au moins la période allant du 31/07 6h jusqu'à J 6h
            self.sh.tar('p{0:s}_{1:s}.tar'.format(season, self.conf.vconf), "P????????")

            self.sh.title('Toolbox output tb03')
            tb03 = toolbox.output(
                role           = 'Reanalyses',
                kind           = 'packedguess',
                local          = 'p{0:s}_{1:s}.tar'.format(season, self.conf.vconf),
                experiment     = self.conf.xpid,
                block          = 'guess',
                nativefmt      = 'tar',
                namespace      = 'vortex.multi.fr',
                geometry       = self.conf.vconf,
                #cutoff         = 'assimilation',
                begindate      = datebegin.ymd6h,
                enddate        = dateend.ymd6h,
                model          = 'safran',
                date           = self.conf.rundate.ymdh,
                vapp           = self.conf.vapp,
                vconf          = self.conf.vconf,
                fatal          = True,
            ),
            print(t.prompt, 'tb03 =', tb03)
            print()
