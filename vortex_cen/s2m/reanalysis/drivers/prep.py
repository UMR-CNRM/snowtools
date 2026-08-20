# -*- coding:Utf-8 -*-
"""
Generation of SURFEX initial conditions file (PREP.nc)
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import PreprocessUenvNamelist
from vortex_cen.tasks.surfex.prep import MakePrepFile
from vortex_cen.tasks.surfex.init_clim_ground_temperature import FetchClimGroundTemperatureOrCrash
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrCrash


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            PreprocessUenvNamelist(tag='preprocess_uenv_namelist_prep', ticket=t, **kw),
            FetchClimGroundTemperatureOrCrash(tag='fetchClimGroundTemperature', ticket=t, **kw),
            FetchPgdFileOrCrash(tag="fetchpgd", ticket=t, **kw),
            MakePrepFile(tag='make_prep', ticket=t, **kw),
        ],
        options=kw,
    )

