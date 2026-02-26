# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.interpol import InterpolateS2MForcing


def setup(t, **kw):
    return Driver(
        tag='interpolforcing',
        ticket=t,
        nodes=[
            InterpolateS2MForcing(tag='interpolates2mforcing', ticket=t, **kw),
        ],
        options=kw,
    )
