# -*- coding:Utf-8 -*-
"""
Generation of ground physiography (PGD.nc file)
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist
from vortex_cen.tasks.surfex.pgd import Pgd_Uenv_Pgd


def setup(t, **kw):
    return Driver(
        tag='pgd',
        ticket=t,
        nodes=[
            Preprocess_Uenv_Namelist(tag='preprocess', ticket=t, **kw),
            Pgd_Uenv_Pgd(tag='makepgd', ticket=t, **kw),
        ],
        options=kw,
    )
