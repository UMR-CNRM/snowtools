# -*- coding: utf-8 -*-
"""
Created on 15 oct. 2025

@author: lafaysse
"""
from vortex.layout.nodes import Driver
from .hydro_task import Hydro_Task_Forecast_Replay


def setup(t, **kw):
    return Driver(
        tag='S2M_Hydro',
        ticket=t,
        nodes=[
            Hydro_Task_Forecast_Replay(tag='hydro_task_forecast', ticket=t, **kw,)],
        options=kw
    )
