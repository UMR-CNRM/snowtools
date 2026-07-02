# -*- coding: utf-8 -*-
"""
Test the "Offline_MPI_Uenv" unittask.
The driver also includes the following tasks :
- "Preprocess_Uenv_Namelist"
- "GetClimGroundTemperature"
- "GetPgd1D"
- "GetPrep"
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist
from vortex_cen.tasks.surfex.offline import Offline_MPI_Uenv
from vortex_cen.tasks.surfex.pgd import GetPgd1D
from vortex_cen.tasks.surfex.prep import GetPrep
from vortex_cen.tasks.surfex.init_clim_ground_temperature import GetClimGroundTemperature


def setup(t, **kw):
    return Driver(
        tag='offline',
        ticket=t,
        nodes=[
            Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
            GetClimGroundTemperature(tag='getClimGroundTemperature', ticket=t, **kw),
            GetPgd1D(tag='getpgd1d', ticket=t, **kw),
            GetPrep(tag='offline_getprep', ticket=t, **kw),
            Offline_MPI_Uenv(tag='offline_mpi_uenv', ticket=t, **kw),
        ],
        options=kw,
    )
