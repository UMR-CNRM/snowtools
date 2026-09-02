# -*- coding: utf-8 -*-
"""
Test the "MakePrepFile" unittask. The driver also includes the "GetClimGroundTemperature" tasks.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.prep import MakePrepFile
from vortex_cen.tasks.surfex.init_clim_ground_temperature import FetchClimGroundTemperatureOrCrash
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrCrash


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            FetchClimGroundTemperatureOrCrash(tag='fetchClimGroundTemperature_prep', ticket=t, **kw),
            FetchPgdFileOrCrash(tag="fetchpgd_prepjob", ticket=t, **kw),
            MakePrepFile(tag='make_prep', ticket=t, **kw),
        ],
        options=kw,
    )
