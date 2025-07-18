# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver
from snowtools.tasks.research.crocO.crocO_soda import Soda_Task


def setup(t, **kw):
    return Driver(
        tag='SODA',
        ticket=t,
        nodes=[
            Soda_Task(tag='soda', ticket=t, **kw)
        ],
        options=kw
    )
