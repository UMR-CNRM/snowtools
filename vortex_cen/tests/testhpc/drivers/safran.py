# -*- coding: utf-8 -*-
"""
Test the "Safran" unittask.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.safran.reanalysis import SafranReanalysis


def setup(t, **kw):
    return Driver(
        tag='safran',
        ticket=t,
        nodes=[
            SafranReanalysis(tag='safran', ticket=t, **kw),
        ],
        options=kw,
    )
