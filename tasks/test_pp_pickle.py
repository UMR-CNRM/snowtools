# -*- coding: utf-8 -*-
"""
Created on Tue Jan 21 14:45:03 2020

@author: cluzetb
"""
from vortex.layout.nodes import Driver
from snowtools.tasks.crocO_common import CrocO_Out
def setup(t, **kw):
    return Driver(
        tag    = 'crocO_out',
        ticket = t,
        nodes  = [
            CrocO_Out(tag='crocO_out', ticket=t, **kw),
        ],
        options = kw,
    )