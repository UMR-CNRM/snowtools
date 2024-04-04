# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.surfex import OfflineMPI
from snowtools.tasks.research.edelweiss.surfex_preprocess import PrepRefill


def setup(t, **kw):
    return Driver(
        tag='OfflineMPI',
        ticket=t,
        nodes=[
            PrepRefill(tag='preprefill', ticket=t, **kw),
            OfflineMPI(tag='offlinempi', ticket=t, **kw),
        ],
        options=kw
    )
