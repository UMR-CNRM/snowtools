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
            PrepSafran(tag='prepsafprv', ticket=t, **kw),
        ],
        options = kw,
    )


class PrepSafran(Task, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def refill(self):

        pass

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket
        datebegin, dateend = self.get_period()
        t.env.setvar('DATADIR', '/scratch/mtool/vernaym/cache')

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            # I- ARPEGE
            # Récupération des échéances de 6h à 102h du réseau 0h J d'ARPEGE
            # On traite les échéances en 5 membres distincts pour optimiser le temps de calcul
            self.sh.title('Toolbox input tb01_a')
            tbarp = toolbox.input(
                role           = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.arpege_geometry,
                kind           = 'gridpoint',
                suite          = 'oper',
                cutoff         = 'production',
                local          = 'mb035/ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[0:7],
                namespace      = 'oper.multi.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.deterministic_conf,
                fatal          = False,
            )
            print t.prompt, 'tb01 =', tbarp
            print

            self.sh.title('Toolbox input tb01_b')
            tbarp.extend(toolbox.input(
                role           = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.arpege_geometry,
                kind           = 'gridpoint',
                suite          = 'oper',
                cutoff         = 'production',
                local          = 'mb036/ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[7:14],
                namespace      = 'oper.multi.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.deterministic_conf,
                fatal          = False,
            ))
            print t.prompt, 'tb01 =', tbarp
            print

            self.sh.title('Toolbox input tb01_c')
            tbarp.extend(toolbox.input(
                role           = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.arpege_geometry,
                kind           = 'gridpoint',
                suite          = 'oper',
                cutoff         = 'production',
                local          = 'mb037/ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[14:21],
                namespace      = 'oper.multi.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.deterministic_conf,
                fatal          = False,
            ))
            print t.prompt, 'tb01 =', tbarp
            print

            self.sh.title('Toolbox input tb01_d')
            tbarp.extend(toolbox.input(
                role           = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.arpege_geometry,
                kind           = 'gridpoint',
                suite          = 'oper',
                cutoff         = 'production',
                local          = 'mb038/ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[21:27],
                namespace      = 'oper.multi.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.deterministic_conf,
                fatal          = False,
            ))
            print t.prompt, 'tb01 =', tbarp
            print

            self.sh.title('Toolbox input tb01_e')
            tbarp.extend(toolbox.input(
                role           = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.arpege_geometry,
                kind           = 'gridpoint',
                suite          = 'oper',
                cutoff         = 'production',
                local          = 'mb039/ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[27:33],
                namespace      = 'oper.multi.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.deterministic_conf,
                fatal          = False,
            ))
            print t.prompt, 'tb01 =', tbarp
            print

            # II- PEARP
            # Récupération du réseau 18h (J-1) pour couvrir J 6h -> (J+4) 6h
            self.sh.title('Toolbox input tb02_a')
            tbpearp = toolbox.input(
                role           = 'Gridpoint',
                block          = 'forecast',
                suite          = 'oper',
                cutoff         = 'production',
                format         = 'grib',
                geometry       = self.conf.pearp_geometry,
                kind           = 'gridpoint',
                local          = 'mb[member]/PEARP[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[2:17],
                member         = footprints.util.rangex(self.conf.pearp_members),
                namespace      = 'vortex.multi.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.eps_conf,
                fatal          = False,
            )
            print t.prompt, 'tb02 =', tbpearp
            print

            self.sh.title('Toolbox input tb02_b')
            tbpearp.extend(toolbox.input(
                role           = 'Gridpoint',
                block          = 'forecast',
                suite          = 'oper',
                cutoff         = 'production',
                format         = 'grib',
                geometry       = self.conf.pearp_geometry,
                kind           = 'gridpoint',
                local          = 'mb[member]/PEARP[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[18:35:2],
                member         = footprints.util.rangex(self.conf.pearp_members),
                namespace      = 'vortex.multi.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.eps_conf,
                fatal          = False,
            ))
            print t.prompt, 'tb02 =', tbpearp
            print

            self.sh.title('Toolbox input tb04 = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = 'uenv:s2m.01@vernaym',
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -a -i IDW  -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)]))),
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
                terms          = footprints.util.rangex(self.conf.prv_terms),
                interpreter    = script[0].resource.language,
                ntasks         = self.conf.ntasks,
                members        = footprints.util.rangex(self.conf.members),
                extendpypath = ['/home/gmap/mrpe/mary/public/eccodes_python'] + [self.sh.path.join(self.conf.rootapp, d) for d in ['vortex/src', 'vortex/site', 'epygram', 'epygram/site']],
            )
            print t.prompt, 'tb04 =', expresso
            print

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            self.sh.title('Toolbox output tb05a')
            tb05 = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb035/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[0:7],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = self.conf.namespace,
            ),
            print t.prompt, 'tb05a =', tb05
            print

            self.sh.title('Toolbox output tb05b')
            tb05b = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb036/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[7:14],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = self.conf.namespace,
            ),
            print t.prompt, 'tb05b =', tb05b
            print

            self.sh.title('Toolbox output tb05c')
            tb05 = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb037/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[14:21],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = self.conf.namespace,
            ),
            print t.prompt, 'tb05c =', tb05
            print

            self.sh.title('Toolbox output tb05d')
            tb05b = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb038/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[21:27],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = self.conf.namespace,
            ),
            print t.prompt, 'tb05d =', tb05b
            print

            self.sh.title('Toolbox output tb05e')
            tb05b = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb039/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[27:33],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = self.conf.namespace,
            ),
            print t.prompt, 'tb05e =', tb05b
            print

            self.sh.title('Toolbox output tb06a')
            tb06 = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb[member]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[2:17],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.eps_conf,
                namespace      = self.conf.namespace,
                member         = footprints.util.rangex(self.conf.pearp_members),
            ),
            print t.prompt, 'tb06a =', tb06
            print

            self.sh.title('Toolbox output tb06b')
            tb06 = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb[member]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[18:35:2],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.eps_conf,
                namespace      = self.conf.namespace,
                member         = footprints.util.rangex(self.conf.pearp_members),
            ),
            print t.prompt, 'tb06b =', tb06
            print

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
            pass
