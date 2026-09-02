# -*- coding:Utf-8 -*-
"""
SURFEX/Crocus reanalysis in the "massif" geometry.
Add slopes to the SAFRAN "flat massif" FORCING files and launch the OFFLINE executable.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrCrash
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrCrash
from vortex_cen.tasks.surfex.offline import Offline_Mpi_Uenv


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            FetchPgdFileOrCrash(tag='getpgd', ticket=t, **kw),
            FetchPrepFileOrCrash(tag='getprep', ticket=t, **kw),
            Offline_Mpi_Uenv(tag='offline', ticket=t, **kw),
        ],
        options=kw,
    )
