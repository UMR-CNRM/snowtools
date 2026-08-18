# -*- coding: utf-8 -*-
"""
Test the "Escroc" unittask. The driver includes the "GetPgd1D" task.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import PreprocessNamelist
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrMake
from vortex_cen.tasks.surfex.offline_ensemble import Escroc


def setup(t, **kw):
    return Driver(
        tag='escroc',
        ticket=t,
        nodes=[
            PreprocessNamelist(tag='preprocess_uenv_namelist_escroc', ticket=t, **kw),
            FetchPgdFileOrMake(tag='getpgd1d_escroc', ticket=t, **kw),
            Escroc(tag='escroc', ticket=t, **kw),
        ],
        options=kw,
    )
