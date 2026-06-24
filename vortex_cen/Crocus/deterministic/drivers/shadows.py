# -*- coding:Utf-8 -*-
"""
The shadows driver allows to add relief-induced solar masks to a FORCING file in a "station" geometry.
"""

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
