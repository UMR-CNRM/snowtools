# -*- coding: utf-8 -*-
"""
Test the "Preprocess_Uenv_Namelist" unittask.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import PreprocessUenvNamelist


def setup(t, **kw):
    return Driver(
        tag='preprocess_namelist',
        ticket=t,
        nodes=[
            PreprocessUenvNamelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
        ],
        options=kw,
    )
