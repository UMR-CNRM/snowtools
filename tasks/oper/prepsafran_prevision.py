#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

import vortex
from vortex import toolbox
from vortex.layout.nodes import Driver
from cen.layout.nodes import S2Mtask


def setup(t, **kw):
    return Driver(
        tag    = 'pearp2safran',
        ticket = t,
        nodes  = [
            PrepSafran(tag='prepsafprv', ticket=t, **kw),
        ],
        options = kw,
    )


class PrepSafran(S2Mtask):

    def refill(self):

        pass

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket
        datebegin, dateend = self.get_period()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            # I- ARPEGE
            # Récupération des échéances de 6h à 102h du réseau 0h J d'ARPEGE
            # On traite les échéances en 2 membres distincts pour optimiser le temps de calcul
            self.sh.title('Toolbox input tb01')
            tbarp = toolbox.input(
                alternate      = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.cpl_geometry,
                kind           = 'gridpoint',
                suite          = 'oper',
                cutoff         = 'production',
                local          = 'mb035/ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[2:14],
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

            self.sh.title('Toolbox input tb01')
            tbarp.extend(toolbox.input(
                alternate      = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.cpl_geometry,
                kind           = 'gridpoint',
                suite          = 'oper',
                cutoff         = 'production',
                local          = 'mb036/ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[15:27],
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
            # Récupération du réseau P18 (J-1) pour couvrir J 6h -> (J+4) 6h
            self.sh.title('Toolbox input tb02')
            tbpearp = toolbox.input(
                role           = 'Gridpoint',
                block          = 'forecast',
                suite          = 'oper',
                cutoff         = 'production',
                format         = 'grib',
                geometry       = self.conf.cpl_geometry,
                kind           = 'gridpoint',
                local          = 'mb[member]/PEARP[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[4:28],
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

            self.sh.title('Toolbox input tb04 = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = 'uenv:s2m.01@vernaym',
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)]))),
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
                members        = footprints.util.rangex(self.conf.members)
            )
            print t.prompt, 'tb04 =', expresso
            print

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            self.sh.title('Toolbox output tb05')
            tb05 = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb035/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[2:14],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = self.conf.namespace,
                fatal          = False,
            ),
            print t.prompt, 'tb05 =', tb05
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
                cumul          = footprints.util.rangex(self.conf.prv_terms)[15:27],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = self.conf.namespace,
                fatal          = False,
            ),
            print t.prompt, 'tb05b =', tb05b
            print

            self.sh.title('Toolbox output tb06')
            tb06 = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb[member]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[4:28],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.eps_conf,
                namespace      = self.conf.namespace,
                member         = footprints.util.rangex(self.conf.pearp_members),
                fatal          = False,
            ),
            print t.prompt, 'tb06 =', tb06
            print

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
            pass
