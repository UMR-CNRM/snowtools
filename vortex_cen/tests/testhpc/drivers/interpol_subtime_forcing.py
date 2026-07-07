# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.interpol import InterpolateS2MLocalForcing
from vortex_cen.tasks.regrid.extract_subperiod import ExtractSubPeriod



def setup(t, **kw):
    return Driver(
        tag='interpolsubtimeforcing',
        ticket=t,
        nodes=[
            ExtractSubPeriod(tag='extractsubperiod', ticket=t, **kw),
            InterpolateS2MLocalForcing(tag='interpols2msubtimeforcing', ticket=t, **kw),
        ],
        options=kw,
    )
