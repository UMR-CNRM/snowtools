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
            # On traite les échéances en les considérant comme des membres distincts pour paralléliser les calculs

            # On essaye d'abord sur le cache inline
            self.sh.title('Toolbox input tbarp_inline')
            tbarp = toolbox.input(
                role           = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.arpege_geometry,
                kind           = 'gridpoint',
                filtername     = 'concatenate',
                suite          = self.conf.suite,
                cutoff         = 'production',
                local          = 'ARP_[term:hour]/ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[2:35],
                namespace      = 'vortex.cache.fr',
                block          = 'forecast',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.deterministic_conf,
                fatal          = False,
            )
            print t.prompt, 'tb01 =', tbarp
            print

            # En cas de bascule les fichiers ont pu ne pas être phasés, on essaye alors sur Hendrix.
            # Les fichiers sur Hendrix n'ont pas de filtername "concatenate" --> A voir avec IGA
            self.sh.title('Toolbox input tbarp_arch')
            tbarp.extend(toolbox.input(
                alternate      = 'Gridpoint',
                format         = 'grib',
                geometry       = self.conf.arpege_geometry,
                kind           = 'gridpoint',
                suite          = self.conf.suite,
                cutoff         = 'production',
                local          = 'ARP_[term:hour]/ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[2:35],
                namespace      = 'vortex.archive.fr',
                block          = 'forecast',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.deterministic_conf,
                fatal          = True,
            ))
            print t.prompt, 'tb01 =', tbarp
            print

            # II- PEARP
            # Récupération du réseau 18h (J-1) pour couvrir J 6h -> (J+4) 6h
            # On veut donc les échéances de 12h à 108h
            # Désormais toutes les échéances tri-horaire sont disponible
            self.sh.title('Toolbox input tbpearp_inline')
            tbpearp = toolbox.input(
                role           = 'Gridpoint',
                block          = 'forecast',
                suite          = self.conf.suite,
                cutoff         = 'production',
                format         = 'grib',
                geometry       = self.conf.pearp_geometry,
                kind           = 'gridpoint',
                local          = 'PEARP_[member]_[term:hour]/PEARP[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[4:38],
                member         = footprints.util.rangex(self.conf.pearp_members),
                namespace      = 'vortex.cache.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.eps_conf,
                fatal          = False,
            )
            print t.prompt, 'tb02 =', tbpearp
            print

            self.sh.title('Toolbox input tbpearp_archive')
            tbpearp.extend(toolbox.input(
                alternate      = 'Gridpoint',
                block          = 'forecast',
                suite          = self.conf.suite,
                cutoff         = 'production',
                format         = 'grib',
                geometry       = self.conf.pearp_geometry,
                kind           = 'gridpoint',
                local          = 'PEARP_[member]_[term:hour]/PEARP[date::addterm_ymdh]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms)[4:38],
                member         = footprints.util.rangex(self.conf.pearp_members),
                namespace      = 'vortex.archive.fr',
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
                interpreter    = 'current',
                terms          = footprints.util.rangex(self.conf.prv_terms),
                ntasks         = self.conf.ntasks,
            )
            print t.prompt, 'tb04 =', expresso
            print

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            # On ne plante que si les guess issus d'ARPEGE n'ont pas pu être générés

            self.sh.title('Toolbox output tb05')
            tb05 = toolbox.output(
                role           = 'Ebauche',
                local          = 'ARP_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT6H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[2:35],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.deterministic_conf,
                namespace      = self.conf.namespace,
                fatal          = True,
            ),
            print t.prompt, 'tb05a =', tb05
            print

            self.sh.title('Toolbox output tb06a')
            tb06a = toolbox.output(
                role           = 'Ebauche',
                local          = 'PEARP_[member]_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[4:19],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.eps_conf,
                namespace      = self.conf.namespace,
                member         = footprints.util.rangex(self.conf.pearp_members),
                fatal          = False,
            ),
            print t.prompt, 'tb06a =', tb06a
            print

            self.sh.title('Toolbox output tb06b')
            tb06b = toolbox.output(
                role           = 'Ebauche',
                local          = 'PEARP_[member]_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[20:38:2],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.eps_conf,
                namespace      = self.conf.namespace,
                member         = footprints.util.rangex(self.conf.pearp_members),
                fatal          = False,
            ),
            print t.prompt, 'tb06b =', tb06b
            print

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
            pass
