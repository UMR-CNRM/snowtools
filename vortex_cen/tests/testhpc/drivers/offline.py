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
from vortex_cen.tasks.surfex.pre_process import PreprocessUenvNamelist
from vortex_cen.tasks.surfex.offline import Offline_Mpi_Uenv
from vortex_cen.tasks.surfex.pgd import FetchPgdOrMake
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrMake
from vortex_cen.tasks.surfex.init_clim_ground_temperature import FetchClimGroundTemperatureOrMake


def setup(t, **kw):
    return Driver(
        tag='offline',
        ticket=t,
        nodes=[
            PreprocessUenvNamelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
            FetchClimGroundTemperatureOrMake(tag='getClimGroundTemperature', ticket=t, **kw),
            FetchPgdOrMake(tag='getpgd1d', ticket=t, **kw),
            FetchPrepFileOrMake(tag='getprep', ticket=t, **kw),
            Offline_Mpi_Uenv(tag='offline_mpi_uenv', ticket=t, **kw),
        ],
        options=kw,
    )
