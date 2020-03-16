# -*- coding: utf-8 -*-
"""
Created on Tue Jan 21 14:45:03 2020

@author: cluzetb
"""
from vortex.layout.nodes import Driver
from snowtools.tasks.crampon_common import Crampon_Out
def setup(t, **kw):
    return Driver(
        tag    = 'crampon_out',
        ticket = t,
        nodes  = [
            Crampon_Out(tag='crampon_out', ticket=t, **kw),
        ],
        options = kw,
    )