#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

from cen.layout.nodes import S2MTaskMixIn
import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex import toolbox
from vortex.layout.nodes import Driver

from iga.tools.apps import OpTask
from vortex.tools.actions import actiond as ad
from common.util import usepygram
import iga.tools.op as op
import snowtools
from snowtools.bronx.stdtypes.date import Date


def setup(t, **kw):
    return Driver(
        tag    = 'pearp2safran',
        ticket = t,
        nodes  = [
            Reanalyses(tag='reanalyses', ticket=t, **kw),
        ],
        options = kw,
    )


class Reanalyses(OpTask, S2MTaskMixIn):

    def refill(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket
        season = Date.nivologyseason.fget(self.conf.rundate)
        datebegin, dateend = self.get_period()

        def tb01_generic_hook1(t, rh):
            sh = t.sh
            tarname = sh.path.basename(rh.container.localpath())
            if sh.is_tarfile(tarname):
                sh.untar(tarname)
            sh.rm(tarname)

        if 'refill' in self.steps:

            if not (self.conf.rundate.month == 8 and self.conf.rundate.day == 1):

                # Récupération de l'archive de la veille (tâche de 12h J-1)
                # L'archive doit couvrir la période allant du 31/07 précendent à 6h jusqu'à J-1 6h bien que
                # la réanalyse mensuelle n'utilise les guess que jusqu'à J-4 6h

                # TODO : Ajouter un mode secours en récupérant les archives de J-2, J-3,... et en les completant
                # avec les guess manquants
                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
                    role           = 'Reanalyse',
                    local          = 'p{0:s}_{1:s}.tar'.format(season, self.conf.vconf),
                    experiment     = self.conf.xpid,
                    cutoff         = 'assimilation',
                    block          = 'guess',
                    nativefmt      = 'tar',
                    kind           = 'packedguess',
                    model          = 'safran',
                    hook_autohook1 = (tb01_generic_hook1, ),
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymdh),
                    vapp           = self.conf.vapp,
                    vconf          = self.conf.vconf,
                    begindate      = datebegin.ymd6h,
                    enddate        = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                    geometry       = self.conf.vconf,
                    namespace      = 'vortex.cache.fr',
                    fatal          = True,
                )
                print(t.prompt, 'tb01 =', tb01)
                print()

            # Récupération des guess de la veille à ajouter à l'archive
            # Comme dateend correpond à J-4 on ajoute 5*24h
            self.sh.title('Toolbox input tb02a')
            tb02a = toolbox.input(
                role           = 'Ebauche',
                local          = 'P[date::addcumul_yymdh]',
                experiment     = self.conf.xpid,
                block          = 'guess',
                geometry       = self.conf.vconf,
                cutoff         = 'assimilation',
                date           = ['{0:s}/+PT96H/-PT{1:s}H'.format(dateend.ymd6h, str(d)) for d in footprints.util.rangex(6, 120, self.conf.cumul)],
                cumul          = self.conf.cumul,
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = 'vortex.cache.fr',
                fatal          = False,
            ),
            print(t.prompt, 'tb02a =', tb02a)
            print()

            # Mode secours : on prend la prévision correspondante
            self.sh.title('Toolbox input tb02b')
            tb02b = toolbox.input(
                alternate      = 'Ebauche',
                local          = 'P[date::addcumul_yymdh]',
                experiment     = self.conf.xpid,
                block          = 'guess',
                geometry       = self.conf.vconf,
                cutoff         = 'production',
                date           = ['{0:s}/+PT96H/-PT{1:s}H'.format(dateend.ymd6h, str(d)) for d in footprints.util.rangex(6, 120, self.conf.cumul)],
                cumul          = self.conf.cumul,
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = 'vortex.cache.fr',
                fatal          = True,
            ),
            print(t.prompt, 'tb02b =', tb02b)
            print()

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
                cutoff         = 'assimilation',
                block          = 'guess',
                nativefmt      = 'tar',
                namespace      = self.conf.namespace,
                geometry       = self.conf.vconf,
                begindate      = datebegin.ymd6h,
                enddate        = self.conf.rundate.ymd6h,
                model          = 'safran',
                date           = self.conf.rundate.ymdh,
                vapp           = self.conf.vapp,
                vconf          = self.conf.vconf,
                fatal          = True,
                delayed        = True,
            ),
            print(t.prompt, 'tb03 =', tb03)
            print()
            
            ad.phase(tb07)
