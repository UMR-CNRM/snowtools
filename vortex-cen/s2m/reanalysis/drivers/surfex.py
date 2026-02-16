# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.add_slopes import AddSlopes
from vortex_cen.tasks.surfex.offline import Offline_MPI_Uenv


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            AddSlopes(tag='addslopes', ticket=t, **kw),
            Offline_MPI_Uenv(tag='offline', ticket=t, **kw),
        ],
        options=kw,
    )
