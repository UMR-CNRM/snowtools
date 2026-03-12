# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.safran.reanalysis import Safran


def setup(t, **kw):
    return Driver(
        tag='safran',
        ticket=t,
        nodes=[
            Safran(tag='safran', ticket=t, **kw),
        ],
        options=kw,
    )
