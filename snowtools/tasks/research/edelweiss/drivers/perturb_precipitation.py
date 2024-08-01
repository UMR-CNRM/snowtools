# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.make_forcing import PerturbPrecipitation


def setup(t, **kw):
    return Driver(
        tag='perturb_precipitation',
        ticket=t,
        nodes=[
            PerturbPrecipitation(tag='perturb_precipitation', ticket=t, **kw),
        ],
        options=kw
    )
