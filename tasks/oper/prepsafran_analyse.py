#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex import toolbox
from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn


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

            if self.conf.rundate.hour == 3:

                # I- Guess ARPEGE (= "36eme membre")
                # ----------------------------------

                # RUN 3h : Recuperation de A6 du réseau H-6 pour H in [6, 12, 18] (J-1) (on utilisera la P6 du réseau 0h J pour le dernier guess)
                self.sh.title('Toolbox input tb01_a')
                tbarp = toolbox.input(
                    role           = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = 'oper',
                    local          = 'mb035/ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT12H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'oper.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                )
                print t.prompt, 'tb01_a =', tbarp
                print

                self.sh.title('Toolbox input tb01_b')
                tbarp.extend(toolbox.input(
                    alternate      = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = 'oper',
                    cutoff         = 'production',
                    local          = 'mb035/ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT12H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'oper.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                ))
                print t.prompt, 'tb01_b =', tbarp
                print

                self.sh.title('Toolbox input tb02_a')
                tbarp.extend(toolbox.input(
                    role           = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = 'oper',
                    local          = 'mb036/ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT18H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'oper.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                ))
                print t.prompt, 'tb02_a =', tbarp
                print

                self.sh.title('Toolbox input tb02_b')
                tbarp.extend(toolbox.input(
                    alternate      = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = 'oper',
                    cutoff         = 'production',
                    local          = 'mb036/ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT18H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'oper.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                ))
                print t.prompt, 'tb02_b =', tbarp
                print

                self.sh.title('Toolbox input tb03_a')
                tbarp.extend(toolbox.input(
                    role           = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = 'oper',
                    local          = 'mb037/ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'oper.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                ))
                print t.prompt, 'tb03_a =', tbarp
                print

                self.sh.title('Toolbox input tb03_b')
                tbarp.extend(toolbox.input(
                    alternate      = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = 'oper',
                    cutoff         = 'production',
                    local          = 'mb037/ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'oper.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                ))
                print t.prompt, 'tb03_b =', tbarp
                print

                # II- Guess PEARP (membres 0 à 34)
                # --------------------------------

                # Récupération du réseau 6h (J-1) (utilisée seulement à partir du run de 6h) pour l'analyse (J-1) 6h -> J 6h
                self.sh.title('Toolbox input tb04_a')
                tbpearp = toolbox.input(
                    role           = 'Gridpoint',
                    block          = 'forecast',
                    suite          = 'oper',
                    cutoff         = 'production',
                    format         = 'grib',
                    geometry       = self.conf.pearp_geometry,
                    kind           = 'gridpoint',
                    local          = 'mb[member]/PEARP[date::addterm_ymdh]',
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                    term           = footprints.util.rangex(self.conf.ana_terms),
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    namespace      = 'vortex.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.eps_conf,
                )
                print t.prompt, 'tb04_a =', tbpearp
                print

                # Alternate : récupération des prévi P12 à P36 du réseau 18h (J-2) pour l'analyse (J-1) 6h -> J 6h
                self.sh.title('Toolbox input tb04_b')
                tbpearp = toolbox.input(
                    alternate      = 'Gridpoint',
                    block          = 'forecast',
                    suite          = 'oper',
                    cutoff         = 'production',
                    format         = 'grib',
                    geometry       = self.conf.pearp_geometry,
                    kind           = 'gridpoint',
                    local          = 'mb[member]/PEARP[date::addterm_ymdh]',
                    date           = '{0:s}/-PT36H'.format(self.conf.rundate.ymd6h),
                    term           = footprints.util.rangex(self.conf.ana_terms, shift=12),
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    namespace      = 'vortex.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.eps_conf,
                )
                print t.prompt, 'tb04_b =', tbpearp
                print

            else:

                # RUN 9h : Récupération de A6 du réseau d'assimilation d'ARPEGE de 0h
                self.sh.title('Toolbox input tb01')
                tbarp = toolbox.input(
                    role           = 'Gridpoint',
                    format         = 'grib',
                    geometry       = self.conf.arpege_geometry,
                    kind           = 'gridpoint',
                    suite          = 'oper',
                    local          = 'mb035/ARPEGE[date::addterm_ymdh]',
                    date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                    # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                    term           = self.conf.cumul,
                    namespace      = 'oper.multi.fr',
                    nativefmt      = '[format]',
                    origin         = 'historic',
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.deterministic_conf,
                    fatal          = True,
                )
                print t.prompt, 'tb01 =', tbarp
                print

                tbpearp = list()

            self.sh.title('Toolbox input tb03 = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = 'uenv:s2m.01@vernaym',
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -a -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)]))),
            )
            print t.prompt, 'tb03 =', tb03
            print

        if 'fetch' in self.steps:

            pass

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb04')
            expresso = toolbox.algo(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                # interpreter    = script[0].resource.language,
                interpreter    = 'current',
                ntasks         = self.conf.ntasks,
                members        = footprints.util.rangex(self.conf.members),
                terms          = footprints.util.rangex(self.conf.ana_terms),
                extendpypath = ['/home/gmap/mrpe/mary/public/eccodes_python'] + [self.sh.path.join(self.conf.rootapp, d) for d in ['vortex/src', 'vortex/site', 'epygram', 'epygram/site']],
            )
            print t.prompt, 'tb04 =', expresso
            print

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            if self.conf.rundate.hour == 3:

                self.sh.title('Toolbox output tb05_a')
                tb05_a = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'mb035/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = '{0:s}/-PT12H'.format(self.conf.rundate.ymd6h),
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                ),
                print t.prompt, 'tb05_a =', tb05_a
                print

                self.sh.title('Toolbox output tb05_b')
                tb05_b = toolbox.output(
                    alternate      = 'Ebauche',
                    local          = 'mb035/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = '{0:s}/-PT12H'.format(self.conf.rundate.ymd6h),
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                ),
                print t.prompt, 'tb05_b =', tb05_b
                print

                self.sh.title('Toolbox output tb06_a')
                tb06_a = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'mb036/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = '{0:s}/-PT18H'.format(self.conf.rundate.ymd6h),
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                ),
                print t.prompt, 'tb06_a =', tb06_a
                print

                self.sh.title('Toolbox output tb06_b')
                tb06_b = toolbox.output(
                    alternate      = 'Ebauche',
                    local          = 'mb036/P[date:yymdh]_[cumul:hour]_[vconf]production',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = '{0:s}/-PT18H'.format(self.conf.rundate.ymd6h),
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                ),
                print t.prompt, 'tb06_b =', tb06_b
                print

                self.sh.title('Toolbox output tb07_a')
                tb07_a = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'mb037/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                ),
                print t.prompt, 'tb07_a =', tb07_a
                print

                self.sh.title('Toolbox output tb07_b')
                tb07_b = toolbox.output(
                    alternate      = 'Ebauche',
                    local          = 'mb037/P[date:yymdh]_[cumul:hour]_[vconf]production',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    date           = '{0:s}/-PT24H'.format(self.conf.rundate.ymd6h),
                    cumul          = self.conf.cumul,
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.deterministic_conf,
                    namespace      = self.conf.namespace,
                ),
                print t.prompt, 'tb07_b =', tb07_b
                print

                self.sh.title('Toolbox output tb08_a')
                tb08_a = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'mb[member]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
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
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.eps_conf,
                    namespace      = self.conf.namespace,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                ),
                print t.prompt, 'tb08_a =', tb08_a
                print

                self.sh.title('Toolbox output tb08_b')
                tb08_b = toolbox.output(
                    alternate      = 'Ebauche',
                    local          = 'mb[member]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                    geometry       = self.conf.domains,
                    vconf          = '[geometry:area]',
                    experiment     = self.conf.xpid,
                    block          = self.conf.block,
                    cutoff         = 'production',
                    date           = '{0:s}/-PT36H'.format(self.conf.rundate.ymd6h),
                    cumul          = footprints.util.rangex(self.conf.ana_terms, shift=12),
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.eps_conf,
                    namespace      = self.conf.namespace,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                ),
                print t.prompt, 'tb08_b =', tb08_b
                print

            else:

                self.sh.title('Toolbox output tb05')
                tb05 = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'mb035/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
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
                ),
                print t.prompt, 'tb05 =', tb05
                print

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
