#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

from cen.layout.nodes import S2MTaskMixIn
from common.util import usepygram
import footprints
from iga.tools.apps import OpTask
import snowtools
from snowtools.bronx.stdtypes.date import Date
from vortex import toolbox
from vortex.layout.nodes import Driver
from vortex.tools.actions import actiond as ad

import iga.tools.op as op

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

                # Récupération de l'archive de la veille
                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
                    role           = 'Reanalyse',
                    local          = 'p{0:s}_{1:s}.tar'.format(season, self.conf.vconf),
                    experiment     = self.conf.xpid,
                    block          = 'guess',
                    nativefmt      = 'tar',
                    fatal          = True,
                    kind           = 'packedguess',
                    model          = 'safran',
                    hook_autohook1 = (tb01_generic_hook1, ),
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymdh),
                    vapp           = self.conf.vapp,
                    vconf          = self.conf.vconf,
                    # datebegin renvoie le 01/08, on veut aussi les guess du 31/07
                    begindate      = '{0:s}/-PT24H'.format(datebegin.ymd6h),
                    # dateend renvoie à J-4 (la réanalyse se fait seulement jusqu'à J-4 pour l'initialisation de SURFEX avec le réseau de 3h J+1)
                    enddate        = '{0:s}/+PT72H'.format(dateend.ymd6h),
                    geometry       = self.conf.vconf,
                    intent         = 'inout',
                )
                print t.prompt, 'tb01 =', tb01
                print

            # Récupération des guess de la veille à ajouter à l'archive
            self.sh.title('Toolbox input tb02')
            tb02 = toolbox.input(
                role           = 'Ebauche',
                local          = 'P[date::addcumul_yymdh]',
                experiment     = self.conf.xpid,
                block          = 'guess',
                geometry       = self.conf.vconf,
                cutoff         = 'assimilation',
                date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(6, 30, self.conf.cumul)],
                cumul          = self.conf.cumul,
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = 'vortex.cache.fr',
                fatal          = True,
            ),
            print t.prompt, 'tb02 =', tb02
            print

        if 'late-backup' in self.steps:

            # Mise à jour de l'archive
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
                begindate      = '{0:s}/-PT24H'.format(datebegin.ymd6h),
                enddate        = '{0:s}/+PT96H'.format(dateend.ymd6h),
                model          = 'safran',
                date           = '{0:s}'.format(self.conf.rundate.ymdh),
                vapp           = self.conf.vapp,
                vconf          = self.conf.vconf,
                fatal          = True,
                )
            print t.prompt, 'tb03 =', tb03
            print

