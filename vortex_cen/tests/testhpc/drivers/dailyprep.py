# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist
from vortex_cen.tasks.surfex.offline import OfflineMPIDailyPrep
from vortex_cen.tasks.surfex.pgd import GetPgd1D
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrMake
from vortex_cen.tasks.surfex.init_clim_ground_temperature import GetClimGroundTemperatureOrMake


def setup(t, **kw):
    return Driver(
        tag='offline',
        ticket=t,
        nodes=[
            Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
            GetClimGroundTemperatureOrMake(tag='getClimGroundTemperature', ticket=t, **kw),
            GetPgd1D(tag='getpgd1d', ticket=t, **kw),
            FetchPrepFileOrMake(tag='getprep', ticket=t, **kw),
            OfflineMPIDailyPrep(tag='offline_mpi_dailyprep', ticket=t, **kw),
        ],
        options=kw,
    )
