# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.surfex_postprocessing import Diag_sentinel2


def setup(t, **kw):
    return Driver(
        tag='Diag',
        ticket=t,
        nodes=[
            Diag_sentinel2(tag='diag', ticket=t, **kw),
        ],
        options=kw
    )
