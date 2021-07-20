#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

from cen.layout.nodes import S2MTaskMixIn
import footprints
from vortex import toolbox
from vortex.layout.nodes import Driver, Task

from iga.tools.apps import OpTask
from vortex.tools.actions import actiond as ad
from common.util import usepygram
import iga.tools.op as op
import snowtools

logger = footprints.loggers.getLogger(__name__)


def setup(t, **kw):
    return Driver(
        tag    = 'pearp2safran',
        ticket = t,
        nodes  = [
            PrepSafran(tag='prepsafana', ticket=t, **kw),
        ],
        options = kw,
    )


class PrepSafran(Task, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            tbarp = list()

            if self.conf.rundate.hour == 3:

                # I- Guess ARPEGE (= "36eme membre")
                # ----------------------------------

                # RUN 3h : Recuperation de A6 des réseaux 6H, 12H et 18H (J-1).
                # On récupère aussi le réseau 0H (J-1) qui a normalement déjà été extrait par la tâche prepsaf de 9h de la veille par sécurité
                # SAFRAN utilisera la P6 du réseau 0h J pour le dernier guess en attendant que l'analyse soit disponible (réseau 9h)
                # On cherche d'abord sur le cache inline puis sur hendrix
                # RQ : on ne peut pas utiliser le namespace multi car les fichiers sur hendrix n'ont pas de filtername...
                self.sh.title('Toolbox input arpege assim inline')
                tbarp = toolbox.input(
                    role           = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    filtername     = 'concatenate',
                    suite          = 'oper',
                    cutoff         = 'assimilation',
                    local          = 'ARP_[date:ymdh]/ARPEGE[date::addterm_ymdh]',
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.cache.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tbarp =', tbarp)
                print()

                # Mode secours : On récupère les prévisions 6h correspondantes inline...
                self.sh.title('Toolbox alternate arpege prod inline (secours)')
                tbarp.extend(toolbox.input(
                    alternate      = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    filtername     = 'concatenate',
                    suite          = 'oper',
                    cutoff         = 'production',
                    local          = 'ARP_[date:ymdh]/ARPEGE[date::addterm_ymdh]',
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.cache.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                ))
                print(t.prompt, 'tbarp =', tbarp)
                print()

                # II- Guess PEARP (membres 0 à 34)
                # --------------------------------

                # Récupération des prévisions du réseau 6h (J-1) (utilisée seulement à partir du run de 6h) pour l'analyse (J-1) 6h -> J 6h
                self.sh.title('Toolbox input pearp')
                tbpearp = toolbox.input(
                    role           = 'Gridpoint',
                    block          = 'forecast',
                    suite          = 'oper',
                    cutoff         = 'production',
                    format         = 'grib',
                    geometry       = self.conf.pearp_geometry,
                    kind           = 'gridpoint',
                    local          = 'PEARP_[member]_[term:hour]/PEARP[date::addterm_ymdh]',
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                    term           = footprints.util.rangex(self.conf.ana_terms),
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    namespace      = 'vortex.cache.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.eps_conf,
                    fatal          = True,
                )
                print(t.prompt, 'tb04_a =', tbpearp)
                print()

                # Mode secours : les guess correspondant au prévisions du réseau 18h (J-2) ont déjà été extraites
                # par la tâche prepsafran_analyse du réseau 3h

            else:

                # RUN 9h : Récupération de A6 du réseau d'assimilation d'ARPEGE de 0h inline...
                self.sh.title('Toolbox input arpege assim inline')
                tbarp = toolbox.input(
                    role           = 'Gridpoint',
                    block          = 'forecast',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    filtername     = 'concatenate',
                    suite          = 'oper',
                    cutoff         = 'assimilation',
                    local          = 'ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.cache.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tb01 =', tbarp)
                print()

                tbpearp = list()

            self.sh.title('Toolbox input tb03 = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = self.conf.cycle,
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -a -i IDW -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)]))),
            )
            print(t.prompt, 'tb03 =', tb03)
            print()

        if 'fetch' in self.steps:

            pass

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb04')
            expresso = toolbox.algo(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                interpreter    = 'current',
                # Need to extend pythonpath to be independant of the user environment ($PYTHONPATH)
                # The vortex-build environment already set up the pythonpath (see jobassistant plugin) but the script is 
                # eventually launched in a 'user-defined' environment
		extendpypath = ['/homech/mxpt001/vortex/oper/s2m/alp/eccodes_python'] + [self.sh.path.join(self.conf.rootapp, d) for d in ['vortex/src', 'vortex/site', 'epygram', 'epygram/site']],
                ntasks         = self.conf.ntasks,
                terms          = footprints.util.rangex(self.conf.ana_terms),
            )
            print(t.prompt, 'tb04 =', expresso)
            print()

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            if self.conf.rundate.hour == 3:

                self.sh.title('Toolbox output tb05_a = guess arpege assim')
                tb05a = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'ARP_[date:ymdh]/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                    cutoff         = 'assimilation',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
		    delayed        = True, 
                ),
                print(t.prompt, 'tb05_a =', tb05a)
                print()

                self.sh.title('Toolbox output tb05_b = guess arpege prod (secours)')
                tb05b = toolbox.output(
                    alternate      = 'Ebauche',
                    local          = 'ARP_[date:ymdh]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                    cutoff         = 'production',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                    fatal          = False,
		    delayed        = True,
                ),
                print(t.prompt, 'tb05_b =', tb05b)
                print()

                self.sh.title('Toolbox output tb06 = guess pearp')
                tb06 = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'PEARP_[member]_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    cutoff         = 'production',
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                    cumul          = footprints.util.rangex(self.conf.ana_terms),
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
		    delayed        = True,
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.eps_conf,
                    namespace      = self.conf.namespace,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    fatal          = False,
                ),
                print(t.prompt, 'tb06 =', tb06)
                print()

                ad.phase(tb05a,tb05b,tb06)
            else:

                self.sh.title('Toolbox output tb05 = guess arpege assim')
                tb05 = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
		    delayed        = True,
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                    fatal          = True,
                ),
                print(t.prompt, 'tb05 =', tb05)
                print()

                ad.phase(tb05)

            #from vortex.tools.systems import ExecutionError
            #raise ExecutionError('')
