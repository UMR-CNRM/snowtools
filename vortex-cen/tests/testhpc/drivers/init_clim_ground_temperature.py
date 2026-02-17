# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.climatological_ground_temperature import InitClimGroundTemperature


def setup(t, **kw):
    # Manually set the "iniconf" argument for local test
    # Otherwise, this is done by the JobAssistant
    if "iniconf" in kw:
        iniconf = kw.pop("iniconf")
    else:
        iniconf = None
    return Driver(
        tag="init_clim_ground_temperature_driver",
        ticket=t,
        nodes=[
            InitClimGroundTemperature(tag="init_clim_ground_temperature", ticket=t, **kw),
        ],
        options=kw,
        iniconf=iniconf,
    )
