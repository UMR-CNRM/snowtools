# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
import vortex
from vortex_cen.tasks.surfex.prep import GetPrep
from vortex_cen.tasks.surfex.pgd import GetPgd1D
from vortex_cen.tasks.surfex.offline import OfflineMPIDailyPrep
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist

def setup(t, **kw):
    return Driver(
        tag='offline',
        ticket=t,
        nodes=[
            Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
            GetPgd1D(tag='getpgd1d', ticket=t, **kw),
            GetPrep(tag='getprep', ticket=t, **kw),
            OfflineMPIDailyPrep(tag='offline_mpi_dailyprep', ticket=t, **kw),
        ],
        options=kw,
    )