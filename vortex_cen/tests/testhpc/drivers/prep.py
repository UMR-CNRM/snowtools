# -*- coding: utf-8 -*-
"""
Test the "MakePrepFile" unittask. The driver also includes the "Preprocess_Uenv_Namelist" and "GetClimGroundTemperature" tasks.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import PreprocessNamelist
from vortex_cen.tasks.surfex.prep import MakePrepFile
from vortex_cen.tasks.surfex.init_clim_ground_temperature import FetchClimGroundTemperatureOrMake
from vortex_cen.tasks.surfex.pgd import FetchPgdOrCrash


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            PreprocessNamelist(tag='preprocess_uenv_namelist_prep', ticket=t, **kw),
            FetchClimGroundTemperatureOrMake(tag='fetchClimGroundTemperature_prep', ticket=t, **kw),
            FetchPgdOrCrash(tag="fetchpgd_prepjob", ticket=t, **kw),
            MakePrepFile(tag='make_prep', ticket=t, **kw),
        ],
        options=kw,
    )
