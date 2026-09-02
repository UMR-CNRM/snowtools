# -*- coding: utf-8 -*-
"""
Test the "Four_Seasons_Task" postprocessing task
"""

from mkjob.nodes import Driver
from vortex_cen.s2m.oper.drivers.ensemble_surfex_tasks_forecast import Four_Seasons_Task



def setup(t, **kw):
    return Driver(
        tag='S2M_4saisons',
        ticket=t,
        nodes=[
                Four_Seasons_Task(tag="s2m_4season_task", ticket=t, **kw)],
        options=kw
    )
