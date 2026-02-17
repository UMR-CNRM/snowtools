# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.shadows import Shadows


def setup(t, **kw):
    return Driver(
        tag='shadows',
        ticket=t,
        nodes=[
            Shadows(tag='shadows', ticket=t, **kw),
        ],
        options=kw,
    )
