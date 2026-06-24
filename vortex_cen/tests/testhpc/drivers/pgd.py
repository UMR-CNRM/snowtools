# -*- coding: utf-8 -*-
"""
Test the "Pgd_Uenv_Pgd" unittask. The driver also includes the "Preprocess_Uenv_Namelist" task.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist
from vortex_cen.tasks.surfex.pgd import Pgd_Uenv_Pgd

# from vortex_cen.tasks.surfex.pgd import GetPgd1D


def setup(t, **kw):
    return Driver(
        tag='pgd',
        ticket=t,
        nodes=[
            Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
            Pgd_Uenv_Pgd(tag='pgd_uenv_pgd', ticket=t, **kw),
            # GetPgd1D(tag='getpgd1d', ticket=t, **kw),
        ],
        options=kw,
    )
