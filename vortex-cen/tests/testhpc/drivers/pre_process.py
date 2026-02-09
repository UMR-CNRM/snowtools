# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist, Preprocess_Local_Namelist


def setup(t, **kw):
    return Driver(
        tag='preprocess_namelist',
        ticket=t,
        nodes=[
            Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
            Preprocess_Local_Namelist(tag='preprocess_local_namelist', ticket=t, **kw),
        ],
        options=kw,
    )
