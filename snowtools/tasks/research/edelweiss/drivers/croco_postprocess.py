# -*- coding: utf-8 -*-
'''
Created on july 2024

@author: M.Vernay
Concatenation of crocO outputs.
'''

from vortex.layout.nodes import Driver
from snowtools.tasks.research.crocO.crocO_postprocess import PostProcess


def setup(t, **kw):
    return Driver(
        tag='croco_postprocess',
        ticket=t,
        nodes=[
            PostProcess(tag='croco_postprocess', ticket=t, **kw),
        ],
        options=kw
    )
