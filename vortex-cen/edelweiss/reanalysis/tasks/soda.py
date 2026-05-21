# -*- coding: utf-8 -*-

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Soda_Namelist_Preprocess
from vortex_cen.tasks.surfex.soda import Soda


def setup(t, **kw):
    return Driver(
        tag='soda',
        ticket=t,
        nodes=[
            Soda_Namelist_Preprocess(tag='soda_preprocess', ticket=t, **kw),
            Soda(tag='soda', ticket=t, **kw),
        ],
        options=kw,
    )
