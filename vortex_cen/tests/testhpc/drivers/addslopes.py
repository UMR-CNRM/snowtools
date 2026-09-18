# -*- coding: utf-8 -*-
"""
Test the "AddSlopes" unit task.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.add_slopes import AddSlopes


def setup(t, **kw):
    return Driver(
        tag='addslopes',
        ticket=t,
        nodes=[
            AddSlopes(tag='addslopes', ticket=t, **kw),
        ],
        options=kw,
    )
