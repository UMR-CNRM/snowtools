# -*- coding: utf-8 -*-
"""
Created on 15 oct. 2025

@author: lafaysse
"""
from vortex.layout.nodes import Driver
from .hydro_task import _Hydro_Task_Replay, Hydro_Task_Forecast


def setup(t, **kw):
    return Driver(
        tag='S2M_Hydro',
        ticket=t,
        nodes=[
                Hydro_Task_Forecast_Replay(tag='S2M_Hydro_Task', ticket=t, **kw,
                                           delay_component_errors=True, on_error='delayed_fail')],
        options=kw
    )


class Hydro_Task_Forecast_Replay(Hydro_Task_Forecast, _Hydro_Task_Replay):
    pass
