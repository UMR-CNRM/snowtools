# -*- coding:Utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.safran.reconstruct_obs_safran import Reconstruct_SAFRAN_Obs


def setup(t, **kw):
    return Driver(
        tag='obs_safran',
        ticket=t,
        nodes=[
            Reconstruct_SAFRAN_Obs(tag='obs_safran', ticket=t, **kw),
        ],
        options=kw,
    )
