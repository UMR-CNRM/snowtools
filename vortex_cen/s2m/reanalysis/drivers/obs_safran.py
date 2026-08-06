# -*- coding:Utf-8 -*-
"""
Produces SAFRAN-compatible surface observation files (S and T files) with reconstructed
hourly temperature observations.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.safran.reconstruct_obs_safran import ReconstructSafranObs


def setup(t, **kw):
    return Driver(
        tag='obs_safran',
        ticket=t,
        nodes=[
            ReconstructSafranObs(tag='obs_safran', ticket=t, **kw),
        ],
        options=kw,
    )
