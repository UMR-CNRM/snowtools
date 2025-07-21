# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.surfex import PreprocessSodaNamelist, SodaTask


def setup(t, **kw):
    return Driver(
        tag='SODA',
        ticket=t,
        nodes=[
            PreprocessSodaNamelist(tag='soda_preprocess', ticket=t, **kw),
            SodaTask(tag='soda', ticket=t, **kw),
        ],
        options=kw
    )
