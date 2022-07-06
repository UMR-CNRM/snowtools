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
            PrepSafran(tag='prepsafprv', ticket=t, **kw, delay_component_errors=True),
        ],
        options=kw,
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
        t.env.setvar('DATADIR', '/scratch/mtool/vernaym/cache')

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            self.sh.title('Toolbox input tb01')
            tbpearp = toolbox.input(
                role           = 'Gridpoint',
                block          = 'forecast',
                suite          = 'oper',
                cutoff         = 'production',
                format         = 'grib',
                geometry       = self.conf.pearp_geometry,
                kind           = 'gridpoint',
                local          = 'mb[member]/PEARP[date::addterm_ymdh]',
                date           = '{0:s}/-PT12H'.format(self.conf.rundate.ymd6h),
                term           = footprints.util.rangex(self.conf.prv_terms),
                member         = footprints.util.rangex(self.conf.pearp_members),
                namespace      = self.conf.namespace_in,
                nativefmt      = '[format]',
                origin         = 'historic',
                model          = '[vapp]',
                vapp           = self.conf.source_app,
                vconf          = self.conf.eps_conf,
                fatal          = False,
            )
            print(t.prompt, 'tb01 =', tbpearp)
            print()

            self.sh.title('Toolbox input tb02')
            tb02 = toolbox.input(
                role            = 'Nam_ebauche',
                source          = 'namelist_ebauche_[geometry]',
                geometry        = self.conf.vconf,
                genv            = self.conf.cycle,
                kind            = 'namelist',
                model           = self.conf.model,
                local           = 'EBAUCHE_prosnow',
            )
            print(t.prompt, 'tb02 =', tb02)
            print()


            self.sh.title('Toolbox input tb04 = PRE-TRAITEMENT FORCAGE script')
            tb03 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = self.conf.cycle,
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -a -d prosnow -i IDW -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbpearp)]))),
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
                terms          = footprints.util.rangex(self.conf.prv_terms),
                extendpypath   = [self.sh.path.join('/'.join(self.conf.iniconf.split('/')[:-2]), d) for d in ['vortex/src', 'vortex/site', 'epygram', 'epygram/site', 'epygram/eccodes_python']],
                interpreter    = 'current',
                ntasks         = self.conf.ntasks,
            )
            print(t.prompt, 'tb04 =', expresso)
            print()

            self.component_runner(expresso, script, fortran=False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            self.sh.title('Toolbox output tb06a')
            tb06 = toolbox.output(
                role           = 'Ebauche',
                local          = 'mb[member]/P[date:yymdh]_[cumul:hour]_[vconf]_production',
                experiment     = self.conf.xpid,
                block          = self.conf.guess_block,
                geometry       = self.conf.domains,
                vconf          = '[geometry::area]',
                date           = '{0:s}/-PT12H'.format(self.conf.rundate.ymd6h),
                cumul          = footprints.util.rangex(self.conf.prv_terms),
                nativefmt      = 'ascii',
                kind           = 'guess',
                model          = 'safran',
                source_app     = self.conf.source_app,
                source_conf    = self.conf.eps_conf,
                namespace      = self.conf.namespace_out,
                member         = footprints.util.rangex(self.conf.pearp_members),
            ),
            print(t.prompt, 'tb06a =', tb06)
            print()

            print('==================================================================================================')
            print('INFO :The execution went well, do not take into account the following error')
            print('==================================================================================================')
            from vortex.tools.systems import ExecutionError
            raise ExecutionError('')
