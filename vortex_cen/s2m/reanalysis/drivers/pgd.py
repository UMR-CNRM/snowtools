# -*- coding:Utf-8 -*-
"""
Generation of ground physiography (PGD.nc file)
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pgd import MakePgd


def setup(t, **kw):
    return Driver(
        tag='pgd',
        ticket=t,
        nodes=[
            MakePgd(tag='makepgd', ticket=t, **kw),
        ],
        options=kw,
    )
