# -*- coding:Utf-8 -*-
"""
This "pgd" driver allows to force the generation of a PGD.nc file (ground physiography).

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
