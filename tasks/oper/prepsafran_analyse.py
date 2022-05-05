#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

from cen.layout.nodes import S2MTaskMixIn
import footprints
from vortex import toolbox
from vortex.layout.nodes import Driver, Task

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

    # Filter of errors to be applied in both oper and dev cases
    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error
    report_execution_warning = S2MTaskMixIn.s2moper_report_execution_warning
    report_execution_error = S2MTaskMixIn.s2moper_report_execution_error

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
                self.sh.title('Toolbox input arpege assim')
                tbarp = toolbox.input(
                    role           = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    #filtername     = 'concatenate',
                    suite          = self.conf.suite,
                    cutoff         = 'assimilation',
                    local          = 'ARP_[date:ymdh]/ARPEGE[date::addterm_ymdh]',
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.multi.fr',
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

                # Mode secours : On récupère les prévisions 6h correspondantes
                self.sh.title('Toolbox alternate arpege prod (secours)')
                tbarp.extend(toolbox.input(
                    alternate      = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    #filtername     = 'concatenate',
                    suite          = self.conf.suite,
                    cutoff         = 'production',
                    local          = 'ARP_[date:ymdh]/ARPEGE[date::addterm_ymdh]',
                    date           = ['{0:s}/-PT{1:s}H'.format(self.conf.rundate.ymd6h, str(d)) for d in footprints.util.rangex(12, 30, self.conf.cumul)],
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.multi.fr',
                    block          = 'forecast',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = False,
                ))
                print(t.prompt, 'tbarp =', tbarp)
                print()

                # II- Guess PEARP (membres 0 à 34)
                # --------------------------------

                # Récupération des prévisions du réseau 6h (J-1) (utilisées seulement à partir du run de 6h) pour l'analyse (J-1) 6h -> J 6h
                # On en profite pour extraire les échéances jusqu'à 102h pour avoir un mode secours pour la prévision de 3h (J+1) qui couvre
                # la période jusqu'à J+4 (6h) (soit une journée de moins qu'une prévision standard)
                self.sh.title('Toolbox input pearp')
                tbpearp = toolbox.input(
                    role           = 'Gridpoint',
                    block          = 'forecast',
                    suite          = self.conf.suite,
                    cutoff         = 'production',
                    format         = 'grib',
                    geometry       = self.conf.pearp_geometry,
                    kind           = 'gridpoint',
                    local          = 'PEARP_[member]_[term:hour]/PEARP[date::addterm_ymdh]',
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                    term           = footprints.util.rangex(self.conf.prv_terms),
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    namespace      = 'vortex.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.eps_conf,
                    fatal          = False,
                )
                print(t.prompt, 'tb04_a =', tbpearp)
                print()

                # Mode secours : les guess correspondant au prévisions du réseau 18h (J-2) ont déjà été extraites
                # par la tâche prepsafran_analyse du réseau 3h

            else:

                # RUN 9h : Récupération de A6 du réseau d'assimilation d'ARPEGE de 0h
                self.sh.title('Toolbox input arpege assim')
                tbarp = toolbox.input(
                    role           = 'Gridpoint',
                    block          = 'forecast',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    #filtername     = 'concatenate',
                    suite          = self.conf.suite,
                    cutoff         = 'assimilation',
                    local          = 'ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'vortex.multi.fr',
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
                # En python 3 l'ordre des arguments a une importance pour que Vortex ne considère pas que les exécutables sont différents
                # Pour éviter de complexifier le code ici, le script s2m_filtering_grib s'occupe désormais de supprimer les doublons.
                rawopts     = ' -o -a -i IDW -f ' + ' '.join(list([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)])),
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
                extendpypath   = [self.sh.path.join('/'.join(self.conf.iniconf.split('/')[:-2]), d) for d in ['vortex/src', 'vortex/site', 'epygram', 'epygram/site', 'epygram/eccodes_python']],
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
                    fatal          = False,
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
                    fatal          = True,
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
                    cumul          = footprints.util.rangex(self.conf.prv_terms),
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.eps_conf,
                    namespace      = self.conf.namespace,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    fatal          = False, # Stay alive if deterministic "member" is OK
                ),
                print(t.prompt, 'tb06 =', tb06)
                print()

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
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                    fatal          = True,
                ),
                print(t.prompt, 'tb05 =', tb05)
                print()

            print('==================================================================================================')
            print('INFO :The execution went well, do not take into account the following error')
            print('==================================================================================================')
            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
