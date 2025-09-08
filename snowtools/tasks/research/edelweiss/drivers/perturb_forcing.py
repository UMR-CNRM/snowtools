# -*- coding: utf-8 -*-
'''
'''

from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.make_forcing import PerturbForcing


def setup(t, **kw):
    return Driver(
        tag='perturb_forcing',
        ticket=t,
        nodes=[
            PerturbForcing(tag='perturb_forcing', ticket=t, **kw),
        ],
        options=kw
    )
