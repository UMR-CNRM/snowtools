# -*- coding: utf-8 -*-
"""
Test the "Escroc" unittask. The driver includes the "Preprocess_Uenv_Namelist" and "GetPgd1D" tasks.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist
from vortex_cen.tasks.surfex.pgd import FetchPgdOrMake
from vortex_cen.tasks.surfex.offline_ensemble import Escroc


def setup(t, **kw):
    return Driver(
        tag='escroc',
        ticket=t,
        nodes=[
            Preprocess_Uenv_Namelist(tag='preprocess_uenv_namelist_escroc', ticket=t, **kw),
            FetchPgdOrMake(tag='getpgd1d_escroc', ticket=t, **kw),
            Escroc(tag='escroc', ticket=t, **kw),
        ],
        options=kw,
    )
