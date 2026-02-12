# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
from tasks.safran.reanalysis import Safran


def setup(t, **kw):
    return Driver(
        tag='safran_reana_era5',
        ticket=t,
        nodes=[
            Safran(tag='safran_reana_era5', ticket=t, **kw),
        ],
        options=kw,
    )
