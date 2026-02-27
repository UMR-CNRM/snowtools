# -*- coding: utf-8 -*-
"""
Created on 7 nov. 2017

@author: lafaysse
"""

from .ensemble_surfex_tasks_forecast import Four_Seasons_Task
from vortex.layout.nodes import Driver, Task


def setup(t, **kw):
    return Driver(
        tag='PPTask',
        ticket=t,
        nodes=[Four_Seasons_Task(tag='S2m_pp_Task', ticket=t, **kw,
                                 delay_component_errors=True, on_error='delayed_fail')],
        options=kw
    )

