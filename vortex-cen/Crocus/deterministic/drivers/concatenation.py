# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.concatenate import ForcingSpatialConcatenation


def setup(t, **kw):
    return Driver(
        tag='concatenation',
        ticket=t,
        nodes=[
            ForcingSpatialConcatenation(tag='concatenation', ticket=t, **kw),
        ],
        options=kw,
    )
