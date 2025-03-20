# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.make_forcing import Forcing


def setup(t, **kw):
    return Driver(
        tag='forcing',
        ticket=t,
        nodes=[
            Forcing(tag='forcing', ticket=t, **kw),
        ],
        options=kw
    )
