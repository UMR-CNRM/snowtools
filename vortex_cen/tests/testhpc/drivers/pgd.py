# -*- coding: utf-8 -*-
"""
Test the "Pgd_Uenv_Pgd" unittask.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pgd import MakePgd

# from vortex_cen.tasks.surfex.pgd import GetPgd1D


def setup(t, **kw):
    return Driver(
        tag='pgd',
        ticket=t,
        nodes=[
            MakePgd(tag='pgd_uenv_pgd', ticket=t, **kw),
            # GetPgd1D(tag='getpgd1d', ticket=t, **kw),
        ],
        options=kw,
    )
