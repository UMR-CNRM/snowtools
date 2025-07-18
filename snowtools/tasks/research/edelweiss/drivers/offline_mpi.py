# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.surfex import OfflineMPI


def setup(t, **kw):
    return Driver(
        tag='OfflineMPI',
        ticket=t,
        nodes=[
            OfflineMPI(tag='offlinempi', ticket=t, **kw),
        ],
        options=kw
    )
