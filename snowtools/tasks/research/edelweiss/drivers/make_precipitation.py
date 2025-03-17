# -*- coding: utf-8 -*-
'''
Created on 28 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.make_forcing import Precipitation


def setup(t, **kw):
    return Driver(
        tag='precipitation',
        ticket=t,
        nodes=[
            Precipitation(tag='precipitation', ticket=t, **kw),
        ],
        options=kw
    )
