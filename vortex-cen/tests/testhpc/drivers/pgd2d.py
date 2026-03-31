# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Local_Namelist
from vortex_cen.tasks.surfex.pgd import Pgd2D_Local_Pgd


def setup(t, **kw):
    return Driver(
        tag='pgd2d',
        ticket=t,
        nodes=[
            Preprocess_Local_Namelist(tag='preprocess_local_namelist2d', ticket=t, **kw),
            Pgd2D_Local_Pgd(tag='pgd2d_local_pgd', ticket=t, **kw),
        ],
        options=kw,
    )
