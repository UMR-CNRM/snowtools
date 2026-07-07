# -*- coding: utf-8 -*-
"""
Test the "MakePrepFile" unittask. The driver also includes the "Preprocess_Uenv_Namelist" and "GetClimGroundTemperature" tasks.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist
from vortex_cen.tasks.surfex.prep import MakePrepFile
from vortex_cen.tasks.surfex.init_clim_ground_temperature import GetClimGroundTemperature


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
            GetClimGroundTemperature(tag='getClimGroundTemperature', ticket=t, **kw),
            MakePrepFile(tag='make_prep', ticket=t, **kw),
        ],
        options=kw,
    )
