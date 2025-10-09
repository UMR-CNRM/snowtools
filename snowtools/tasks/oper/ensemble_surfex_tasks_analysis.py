# -*- coding: utf-8 -*-
"""
Created on 7 nov. 2017

@author: lafaysse
"""

from .ensemble_surfex_tasks_common import Ensemble_Surfex_Task
from .hydro_task import Hydro_Task
from vortex.layout.nodes import Driver


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            Ensemble_Surfex_Task(tag='Ensemble_Surfex_Task', ticket=t, **kw, delay_component_errors=True, on_error='delayed_fail'),
            Hydro_Task(tag='S2M_Hydro_Task', ticket=t, **kw, delay_component_errors=True, on_error='delayed_fail'),
    ],
        options=kw
    )
