# -*- coding:Utf-8 -*-

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
