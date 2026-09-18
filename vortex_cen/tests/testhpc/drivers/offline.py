# -*- coding: utf-8 -*-
"""
Test the "Offline_MPI_Uenv" unittask.
The driver also includes the following tasks :
- "GetClimGroundTemperature"
- "GetPgd1D"
- "GetPrep"
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.offline import Offline_Mpi_Uenv
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrMake
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrMake
from vortex_cen.tasks.surfex.init_clim_ground_temperature import FetchClimGroundTemperatureOrMake


def setup(t, **kw):
    return Driver(
        tag='offline',
        ticket=t,
        nodes=[
            FetchClimGroundTemperatureOrMake(tag='fetchclimgroundtemperature_offline', ticket=t, **kw),
            FetchPgdFileOrMake(tag='getpgd1d', ticket=t, **kw),
            FetchPrepFileOrMake(tag='offline_getprep', ticket=t, **kw),
            Offline_Mpi_Uenv(tag='offline', ticket=t, **kw),
        ],
        options=kw,
    )
