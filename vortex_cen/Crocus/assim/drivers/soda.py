# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import SodaNamelistPreprocess
from vortex_cen.tasks.surfex.pgd import FetchPgdOrCrash
from vortex_cen.tasks.surfex.soda import Soda, FetchBackgroundOrCrash


def setup(t, **kw):
    return Driver(
        tag='soda',
        ticket=t,
        nodes=[
            SodaNamelistPreprocess(tag='soda_preprocess', ticket=t, **kw),
            FetchPgdOrCrash(tag='fetch_pgd', ticket=t, **kw),
            FetchBackgroundOrCrash(tag='fetch_background', ticket=t, **kw),
            Soda(tag='soda', ticket=t, **kw),
        ],
        options=kw,
    )
