#!/usr/bin/env python
# -*- coding:Utf-8 -*-


__all__ = []

import footprints
logger = footprints.loggers.getLogger(__name__)

from vortex.tools.systems import ExecutionError

from vortex import toolbox
from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn
from bronx.stdtypes.date import Period


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
        datebegin = self.conf.datebegin.replace(hour=6)
        dateend = self.conf.dateend.replace(hour=6)
        t.env.setvar('DATADIR', '/scratch/mtool/vernaym/cache')

        ndays = (dateend - datebegin).days
        if ndays > 366:
            raise ExecutionError('Periode trop longue')

        day_per_worker = ndays / 4

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            tbpearp = list()
            d = 0
            m = 0
            rundate = datebegin
            while rundate <= dateend:

                # I- PEARP
                # Récupération du réseau P18 (J-1) pour couvrir J 6h -> (J+4) 6h
                self.sh.title('Toolbox input tb01')
                tbpearp.extend(toolbox.input(
                    role           = 'Gridpoint',
                    kind           = 'gridpoint',
                    username       = 'vernaym',
                    cutoff         = 'production',
                    format         = 'grib',
                    nativefmt      = '[format]',
                    experiment     = 'reforecast@vernaym',
                    block          = 'refill',
                    namespace      = 'vortex.cache.fr',
                    geometry       = self.conf.pearp_geometry,
                    local          = 'mb{0:=01d}[member%02]/PEARP[date::ymdh]_[term:fmthour]'.format(m),
                    origin         = 'historic',
                    date           = '{0:s}/-PT12H'.format(rundate.ymd6h),
                    term           = footprints.util.rangex(self.conf.prv_terms),
                    member         = footprints.util.rangex(self.conf.pearp_members),
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.eps_conf,
                ))
                print t.prompt, 'tb01'
                print

                rundate = rundate + Period(days=1)

                if d == day_per_worker:
                    m = m + 1
                    d = 0
                d = d + 1

            self.sh.title('Toolbox input tb02 = PRE-TRAITEMENT FORCAGE script')
            tb02 = script = toolbox.input(
                role        = 'pretraitement',
                local       = 'makeP.py',
                genv        = 'uenv:s2m.01@vernaym',
                kind        = 's2m_filtering_grib',
                language    = 'python',
                rawopts     = ' -o -a -f ' + ' '.join(list(set([str(rh[1].container.basename) for rh in enumerate(tbpearp)]))),
            )
            print t.prompt, 'tb02 =', tb02
            print

        if 'fetch' in self.steps:
            pass

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb03')
            expresso = toolbox.algo(
                vconf          = self.conf.vconf,
                engine         = 'exec',
                kind           = 'guess',
                terms          = footprints.util.rangex(self.conf.prv_terms),
                interpreter    = script[0].resource.language,
                ntasks         = self.conf.ntasks,
            )
            print t.prompt, 'tb03 =', expresso
            print

            self.component_runner(expresso, script, fortran = False)

        if 'backup' in self.steps or 'late-backup' in self.steps:

            pass

        if 'late-backup' in self.steps:

            d = 0
            m = 0
            rundate = datebegin
            while rundate <= dateend:

                self.sh.title('Toolbox output tb04')
                tb04 = toolbox.output(
                    role           = 'Ebauche',
                    local          = 'mb{0:=01d}[member%02]/P[date:yymdh]_[cumul:hour]_[vconf]_reforecast'.format(m),
                    experiment     = self.conf.xpid,
                    block          = self.conf.guess_block,
                    geometry       = self.conf.domains,
                    vconf          = '[geometry::area]',
                    date           = '{0:s}/-PT12H'.format(rundate.ymd6h),
                    cumul          = footprints.util.rangex(self.conf.prv_terms),
                    nativefmt      = 'ascii',
                    kind           = 'guess',
                    model          = 'safran',
                    source_app     = self.conf.source_app,
                    source_conf    = self.conf.eps_conf,
                    namespace      = self.conf.namespace,
                    member         = footprints.util.rangex(self.conf.pearp_members),
                ),
                print t.prompt, 'tb04 =', tb04
                print

                rundate = rundate + Period(days=1)

                if d == day_per_worker:
                    m = m + 1
                    d = 0
                d = d + 1

            raise ExecutionError('')
