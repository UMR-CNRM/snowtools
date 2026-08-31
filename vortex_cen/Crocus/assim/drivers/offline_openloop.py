# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import PreprocessNamelist
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrCrash
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrCrash
from vortex_cen.tasks.surfex.offline import OfflineOpenloop


def setup(t, **kw):
    return Driver(
        tag='offline_openloop',
        ticket=t,
        nodes=[
            PreprocessNamelist(tag='preprocess', ticket=t, **kw),
            FetchPgdFileOrCrash(tag='fetch_pgd', ticket=t, **kw),
            FetchPrepFileOrCrash(tag='fetch_prep_file', ticket=t, **kw),
            OfflineOpenloop(tag='offline_openloop', ticket=t, **kw),
        ],
        options=kw,
    )

