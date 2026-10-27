# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver

from vortex_cen.tasks.surfex.offline import OfflineAssim
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrCrash
from vortex_cen.tasks.surfex.prep import FetchMembersPrepFilesOrCrash


def setup(t, **kw):
    return Driver(
        tag="offline_assim",
        ticket=t,
        nodes=[
            FetchPgdFileOrCrash(tag="fetch_pgd", ticket=t, **kw),
            FetchMembersPrepFilesOrCrash(tag="fetch_prep_file_assim", ticket=t, **kw),
            OfflineAssim(tag="offline_assim", ticket=t, **kw),
        ],
        options=kw,
    )
