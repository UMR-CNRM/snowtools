# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.interpol import InterpolateS2MForcing
from vortex_cen.tasks.regrid.extract_subperiod import ExtractSubPeriod



def setup(t, **kw):
    return Driver(
        tag='interpolforcing',
        ticket=t,
        nodes=[
            ExtractSubPeriod(tag='extractsubperiod', ticket=t, **kw),
            InterpolateS2MForcing(tag='interpolates2mforcing', ticket=t, **kw),
        ],
        options=kw,
    )
