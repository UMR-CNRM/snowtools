# -*- coding: utf-8 -*-

from mkjob.nodes import Driver

from vortex_cen.tasks.regrid.extract_subperiod import ExtractSubPeriod


def setup(t, **kw):
    return Driver(
        tag="subtimeforcing",
        ticket=t,
        nodes=[
            ExtractSubPeriod(tag="extractsubperiod", ticket=t, **kw),
        ],
        options=kw,
    )
