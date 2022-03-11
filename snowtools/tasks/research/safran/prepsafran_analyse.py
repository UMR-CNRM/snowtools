# -*- coding:Utf-8 -*-


__all__ = []

from cen.layout.nodes import S2MTaskMixIn
import footprints
from vortex import toolbox
from vortex.layout.nodes import Driver, Task


logger = footprints.loggers.getLogger(__name__)


def setup(t, **kw):
    return Driver(
        tag='pearp2safran',
        ticket=t,
        nodes=[
            PrepSafran(tag='prepsafana', ticket=t, **kw),
        ],
        options=kw,
    )


class PrepSafran(Task, S2MTaskMixIn):

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def process(self):
        """Generation of SAFRAN input files"""

        t = self.ticket

###########################################################################
#                               STEP.01 - INPUTS                         
###########################################################################

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            # 1. Situation nominale :
            # récupération de l'échéance 6h du réseau d'assimilation d'ARPEGE de 0H dans 
            # la géométrie GLOB025
            self.sh.title('Toolbox input tb01')
            tbarp = toolbox.input(
                role           = 'Gridpoint',
                block          = 'forecast',
                format         = 'grib',
                geometry       = self.conf.arpege_geometry,
                kind           = 'gridpoint',
                filtername     = 'concatenate',
                suite          = self.conf.suite,
                local          = 'ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                term           = self.conf.cumul,
                # Sur le cache HPC et à défaut sur Hendrix :
                namespace      = 'vortex.multi.fr',
                # Uniquement sur le cache HPC :
                #namespace      = 'vortex.cache.fr',
                # Uniquement sur Hendrix :
                #namespace      = 'vortex.archive.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.deterministic_conf,
                # La ressource n'est peut être pas encore présente, dans ce cas pas de panique
                fatal          = False,
            )
            print(t.prompt, 'tb01 =', tbarp)
            print()

            # 2. Mode secours :
            # si la ressource précédente est absente, l'échéance 6h du réseau de PRODUCTION d'ARPEGE de 0H
            # dans la géométrie GLOB025 fera aussi bien l'affaire
            self.sh.title('Toolbox input tb02')
            tbarp = toolbox.input(
                alternate      = 'Gridpoint',
                cutoff         = 'production',
                block          = 'forecast',
                format         = 'grib',
                geometry       = self.conf.arpege_geometry,
                kind           = 'gridpoint',
                filtername     = 'concatenate',
                suite          = self.conf.suite,
                local          = 'ARPEGE[date::addterm_ymdh]',
                date           = '{0:s}/-PT6H'.format(self.conf.rundate.ymd6h),
                term           = self.conf.cumul,
                namespace      = 'vortex.multi.fr',
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.deterministic_conf,
                # Mainteant on peut paniquer en cas d'absence
                fatal          = True,
            )
            print(t.prompt, 'tb02 =', tbarp)
            print()

            # 3. Récupération de l'exécutable (ici un script python avec options)
            self.sh.title('Toolbox input tb03 = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = 'uenv:s2m.01@vernaym',
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -f ' + ' '.join(list([str(rh[1].container.basename) for rh in enumerate(tbarp)])),
            )
            print(t.prompt, 'tb03 =', tb03)
            print()

###########################################################################
#                               STEP.02 - COMPUTE                         
###########################################################################

        if 'compute' in self.steps:

            # 4. Identification de l'algo component
            # Il s'agit d'un algo spécifique héritant directement de ParaExpresso et qui lance plusieurs plusieurs
            # exécution du script en parallèle.
            self.sh.title('Toolbox algo tb04')
            expresso = toolbox.algo(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                interpreter    = 'current',
                ntasks         = self.conf.ntasks,
                terms          = footprints.util.rangex(self.conf.ana_terms),
            )
            print(t.prompt, 'tb04 =', expresso)
            print()

            self.component_runner(expresso, script, fortran = False)

###########################################################################
#                               STEP.03 - OUTPUTS                         
###########################################################################

        if 'late-backup' in self.steps:


            self.sh.title('Toolbox output tb05')
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

            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
