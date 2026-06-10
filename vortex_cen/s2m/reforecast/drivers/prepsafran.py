# -*- coding:Utf-8 -*-
"""
Generation of SAFRAN guess files from ARPEGE and PEARP forecats from the 0H run.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.safran.prep_reforecast import PrepSafran


def setup(t, **kw):
    return Driver(
        tag='prepsafran',
        ticket=t,
        nodes=[
            PrepSafran(tag='prepsafran', ticket=t, **kw),
        ],
        options=kw,
    )
