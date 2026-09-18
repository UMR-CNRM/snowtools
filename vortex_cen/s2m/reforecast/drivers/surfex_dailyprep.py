# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrCrash
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrCrash
from vortex_cen.tasks.surfex.offline import OfflineMpiDailyPrep


def setup(t, **kw):
    return Driver(
        tag='offline',
        ticket=t,
        nodes=[
            FetchPgdFileOrCrash(tag='getpgd1d', ticket=t, **kw),
            FetchPrepFileOrCrash(tag='getprep', ticket=t, **kw),
            OfflineMpiDailyPrep(tag='offline_mpi_dailyprep', ticket=t, **kw),
        ],
        options=kw,
    )
