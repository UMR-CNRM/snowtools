# -*- coding:Utf-8 -*-
"""
The init_TG driver allows to generate an init_TG.nc (initial ground temperature) file required for the
generation of a PREP.nc file (initial conditions)
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.init_clim_ground_temperature import InitClimGroundTemperature


def setup(t, **kw):
    return Driver(
        tag='inittg',
        ticket=t,
        nodes=[
            InitClimGroundTemperature(tag='inittg', ticket=t, **kw),
        ],
        options=kw,
    )
