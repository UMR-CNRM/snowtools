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
            PrepSafran(tag='prepsafana', ticket=t, **kw),
        ],
        options = kw,
    )


class PrepSafran(S2Mtask):

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket

        datebegin, dateend = self.get_period()
#       list_geometry = self.get_list_geometry()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            # I- Guess ANALYSE
            # ----------------

            # I.1- ARPEGE( = "36eme membre")
            # Recuperation de A6 du réseau H-6 pour H in [0, 6, 12, 18] (J-1) au run de 3h (on utilisera la P6 du réseau 0h J pour le dernier guess)
            # Récupération seulement de A6 du réseau 0h J au run de 9h
            self.sh.title('Toolbox input tb01_a')
            tbarp = toolbox.input(
                role           = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.cpl_geometry,
                kind           = 'gridpoint',
                suite          = 'oper',
                local          = 'mb035/ARPEGE[date::addterm_ymdh]',
                date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d)) for d in footprints.util.rangex(6, 24, self.conf.cumul_ana)],
                # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                term           = self.conf.cumul_ana,
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

            # RQ : il est fondamental de prendre une P6 pour avoir un cumul des RR sur 6h homogène avec le cumul dans les fichiers d'assimilation
            # Si le réseau d'assim de 0h (J) n'est pas là, on prend comme guess la P6 du réseau de prod 0h (J)
            self.sh.title('Toolbox input tb01_b')
            tbarp.extend(toolbox.input(
                alternate      = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.cpl_geometry,
                kind           = 'gridpoint',
                suite          = 'oper',
                cutoff         = 'production',
                local          = 'mb035/ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/-PT6H'.format(dateend.ymd6h),
                # Utilisation d'une varibale de conf pour assurer la cohérence des cumuls de precip
                term           = self.conf.cumul_ana,
                namespace      = 'oper.multi.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.deterministic_conf,
            ))
            print t.prompt, 'tb01_b =', tbarp
            print

            # 2) Guess PEARP (membres 0 à 34)
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
                date           = '{0:s}/-PT12H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(12, 42, self.conf.cumul),
                member         = footprints.util.rangex(self.conf.pearp_members),
                namespace      = 'vortex.multi.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.eps_conf,
            )
            print t.prompt, 'tb02 =', tbpearp
            print

            # La namelist EBAUCHE est optionelle dans le cas oper
#            self.sh.title('Toolbox input tb03')
#            tb03 = tbebauche =  toolbox.input(
#                role            = 'Nam_ebauche',
#                source          = ['namelist_ebauche_{0:s}'.format(area) for area in self.conf.geometry.area],
#                genv            = self.conf.cycle,
#                kind            = 'namelist',
#                model           = self.conf.model,
#                local           = ['EBAUCHE_{0:s}'.format(area) for area in self.conf.geometry.area],
#                fatal           = True,
#            )
#            print t.prompt, 'tb03 =', tb03
#            print

            self.sh.title('Toolbox input tb04 = PRE-TRAITEMENT FORCAGE script')
            tb04 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = 'uenv:s2m.01@vernaym',
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)]))),
            )
            print t.prompt, 'tb04 =', tb04
            print

        if 'fetch' in self.steps:

            pass

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb05')
            tb05 = expresso = toolbox.algo(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                terms          = footprints.util.rangex(self.conf.ana_terms),
                interpreter    = script[0].resource.language,
                ntasks         = self.conf.ntasks,
                members        = footprints.util.rangex(self.conf.members)
            )
            print t.prompt, 'tb05 =', tb05
            print

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            self.sh.title('Toolbox output tb06')
            tb06 = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb035/P[date:yymdh]_[cumul:hour]_[vconf]_assimilation',
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                date           = ['{0:s}/-PT{1:s}H'.format(dateend.ymd6h, str(d)) for d in footprints.util.rangex(6, 30, self.conf.cumul)],
                cumul          = self.conf.cumul,
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = self.conf.namespace,
                fatal          = False,
            ),
            print t.prompt, 'tb06 =', tb06
            print

            self.sh.title('Toolbox output tb06_b')
            tb06_b = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb035/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                cutoff         = 'production',
                date           = '{0:s}/-PT6H'.format(dateend.ymdh),
                cumul          = self.conf.cumul,
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = self.conf.namespace,
                fatal          = False,
            ),
            print t.prompt, 'tb06_b =', tb06_b
            print

            self.sh.title('Toolbox output tb07')
            tb07 = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb[member]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                cutoff         = 'production',
                date           = '{0:s}/-PT12H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(12, 42, self.conf.cumul),
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.eps_conf,
                namespace      = self.conf.namespace,
                member         = footprints.util.rangex(self.conf.pearp_members),
            ),
            print t.prompt, 'tb07 =', tb07
            print

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
