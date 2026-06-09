# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.safran.reforecast import SafranReforecast


def setup(t, **kw):
    return Driver(
        tag='safran',
        ticket=t,
        nodes=[
            SafranReforecast(tag='safran', ticket=t, **kw),
        ],
        options=kw,
    )
