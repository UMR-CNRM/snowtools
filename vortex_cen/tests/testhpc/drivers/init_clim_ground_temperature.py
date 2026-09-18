# -*- coding: utf-8 -*-
"""
Test the "InitClimGroundTemperature" unit task, including a reproductibility test of the output file.
"""

import vortex
from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.init_clim_ground_temperature import \
    InitClimGroundTemperature


def setup(t, **kw):
    # Manually set the "iniconf" argument for local test
    # Otherwise, this is done by the JobAssistant
    if 'iniconf' in kw:
        iniconf = kw.pop('iniconf')
    else:
        iniconf = None
    return Driver(
        tag='initClimGT',
        ticket=t,
        nodes=[
            InitClimGroundTemperature(tag='initClimGroundTemperature', ticket=t, **kw),
        ],
        options=kw,
        iniconf = iniconf
    )

