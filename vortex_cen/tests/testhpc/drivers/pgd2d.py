# -*- coding: utf-8 -*-
"""
Test the "Pgd2D_Uenv_Pgd" unittask.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pgd import MakePgd
# from vortex_cen.tasks.surfex.pgd import GetPgd2D


def setup(t, **kw):
    return Driver(
        tag='pgd2d',
        ticket=t,
        nodes=[
            MakePgd(tag='pgd2d', ticket=t, **kw),
            # GetPgd2D(tag='getpgd2d', ticket=t, **kw),
        ],
        options=kw,
    )
