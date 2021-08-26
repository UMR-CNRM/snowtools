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

    # Filter of errors to be applied in both oper and dev cases
    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error
    report_execution_warning = S2MTaskMixIn.s2moper_report_execution_warning
    report_execution_error = S2MTaskMixIn.s2moper_report_execution_error

    def refill(self):

        pass

    def process(self):
        """Preparation of SAFRAN input files"""

        t = self.ticket
        datebegin, dateend = self.get_period()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            # I- ARPEGE
            # Récupération des échéances de 6h à 102h du réseau 0h J d'ARPEGE
            # On traite les échéances en les considérant comme des membres distincts pour paralléliser les calculs

            # On essaye d'abord sur le cache inline
            self.sh.title('Toolbox input arpege prod inline')
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
            print(t.prompt, 'tb01 =', tbarp)
            print()

            # En cas de bascule les fichiers ont pu ne pas être phasés, on essaye alors sur Hendrix.
            # Les fichiers sur Hendrix n'ont pas de filtername "concatenate" --> A voir avec IGA
            self.sh.title('Toolbox input arpege prod archive')
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
            print(t.prompt, 'tb01 =', tbarp)
            print()

            # II- PEARP
            # Récupération du réseau 18h (J-1) pour couvrir J 6h -> (J+4) 6h
            # On veut donc les échéances de 12h à 108h
            # Désormais toutes les échéances tri-horaire sont disponible
            self.sh.title('Toolbox input pearp inline')
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
            print(t.prompt, 'tb02 =', tbpearp)
            print()

            self.sh.title('Toolbox input pearp archive')
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
            print(t.prompt, 'tb02 =', tbpearp)
            print()

            self.sh.title('Toolbox input tb04 = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = self.conf.cycle,
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -a -i IDW  -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbarp + tbpearp)]))),
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
                # Need to extend pythonpath to be independant of the user environment
                # The vortex-build environment already set up the pythonpath (see jobassistant plugin) but the script is 
                # eventually launched in a 'user-defined' environment
                extendpypath   = [self.sh.path.join('/'.join(self.conf.iniconf.split('/')[:-2]), d) for d in ['vortex/src', 'vortex/site', 'epygram', 'epygram/site', 'epygram/eccodes_python']],
                terms          = footprints.util.rangex(self.conf.prv_terms),
                ntasks         = self.conf.ntasks,
            )
            print(t.prompt, 'tb04 =', expresso)
            print()

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            # On ne plante que si les guess issus d'ARPEGE n'ont pas pu être générés

            self.sh.title('Toolbox output guess arpege prod')
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
            print(t.prompt, 'tb05 =', tb05)
            print()

            self.sh.title('Toolbox output guess pearp')
            tb06 = toolbox.output(
                role           = 'Ebauche',
                local          = 'PEARP_[member]_[cumul:hour]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/+PT24H/-PT12H'.format(datebegin.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms)[4:38],
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.eps_conf,
                namespace      = self.conf.namespace,
                member         = footprints.util.rangex(self.conf.pearp_members),
                fatal          = False,
            ),
            print(t.prompt, 'tb06 =', tb06)
            print()


            print '=================================================================================================='
            print 'INFO :The execution went well, do not take into account the following error'
            print '=================================================================================================='
            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
