# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.concatenate import ForcingSpatialConcatenation


def setup(t, **kw):
    return Driver(
        tag='concatenate_forcings',
        ticket=t,
        nodes=[
            ForcingSpatialConcatenation(tag='concatenate_forcings', ticket=t, **kw),
        ],
        options=kw,
    )
