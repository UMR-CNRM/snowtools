# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.research_task_base import _CenResearchTask


def setup(t, **kw):
    return Driver(
        tag='get_forcing',
        ticket=t,
        nodes=[
            GetForcing(tag='get_single_forcing', ticket=t, **kw),
            GetForcing(tag='get_multiple_forcings', ticket=t, **kw),
        ],
        options=kw
    )


class GetForcing(_CenResearchTask):

    def get_remote_inputs(self):

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')
