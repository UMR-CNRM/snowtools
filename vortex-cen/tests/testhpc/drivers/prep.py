# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist
from vortex_cen.tasks.surfex.prep import Prep_Uenv_Prep
from vortex_cen.tasks.surfex.init_clim_ground_temperature import GetClimGroundTemperature
from vortex_cen.tasks.surfex.pgd import GetPgd1D


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
            GetClimGroundTemperature(tag='getClimGroundTemperature', ticket=t, **kw),
            GetPgd1D(tag='getpgd1d', ticket=t, **kw),
            Prep_Uenv_Prep(tag='prep_uenv_prep', ticket=t, **kw),
        ],
        options=kw,
    )
