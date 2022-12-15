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
        tag='refill',
        ticket=t,
        nodes=[
            Refill(tag='refill', ticket=t, **kw),
        ],
        options=kw,
    )


class Refill(Task, S2MTaskMixIn):

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

            d = 0
            m = 0
            rundate = datebegin
            while rundate <= dateend:

                if d == day_per_worker:
                    m = m + 1
                    d = 0

                # I- PEARP
                # Récupération du réseau P18 (J-1) pour couvrir J 6h -> (J+4) 6h
                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
                    role           = 'Gridpoint',
                    kind           = 'gridpoint',
                    remote         = '/home/bjoly/REFV2.0/[date::ymdh]/fc_[member%03]/EUROC25.[date::ymdh].[term:fmthour]',
                    hostname       = 'hendrix.meteo.fr',
                    tube           = 'ftp',
                    username       = 'vernaym',
                    cutoff         = 'production',
                    format         = 'grib',
                    nativefmt      = '[format]',
                    geometry       = self.conf.pearp_geometry,
                    local          = 'mb{0:=01d}[member%02]/PEARP[date::ymdh]_[term:fmthour]'.format(m),
                    origin         = 'historic',
                    date           = '{0:s}/-PT12H'.format(rundate.ymd6h),
                    term           = footprints.util.rangex(self.conf.prv_terms),
                    member         = footprints.util.rangex(self.conf.members),
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.eps_conf,
                )
                print(t.prompt, 'tb01 =', tb01)
                print()

                self.sh.title('Toolbox output tb02')
                tb02 = toolbox.output(
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
                    member         = footprints.util.rangex(self.conf.members),
                    model          = '[vapp]',
                    vapp           = self.conf.source_app,
                    vconf          = self.conf.eps_conf,
                )
                print(t.prompt, 'tb02 =', tb02)
                print()

                rundate = rundate + Period(days=1)

                d = d + 1
