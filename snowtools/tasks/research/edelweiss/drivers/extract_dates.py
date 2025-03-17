# -*- coding: utf-8 -*-
'''
Created on 7 mars 2024
@author: Vernay.M
'''

from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.surfex_postprocessing import ExtractDates


def setup(t, **kw):
    return Driver(
        tag='ExtractDates',
        ticket=t,
        nodes=[
            ExtractDates(tag='extract_dates', ticket=t, **kw),
        ],
        options=kw
    )
