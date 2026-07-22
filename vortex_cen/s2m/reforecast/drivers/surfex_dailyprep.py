# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
import vortex
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrMake
from vortex_cen.tasks.surfex.pgd import FetchPgdOrMake
from vortex_cen.tasks.surfex.offline import OfflineMpiDailyPrep
from vortex_cen.tasks.surfex.pre_process import PreprocessNamelist

def setup(t, **kw):
    return Driver(
        tag='offline',
        ticket=t,
        nodes=[
            PreprocessNamelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
            FetchPgdOrMake(tag='getpgd1d', ticket=t, **kw),
            FetchPrepFileOrMake(tag='getprep', ticket=t, **kw),
            OfflineMpiDailyPrep(tag='offline_mpi_dailyprep', ticket=t, **kw),
        ],
        options=kw,
    )