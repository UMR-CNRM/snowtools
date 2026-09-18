# -*- coding:Utf-8 -*-
"""
refill_prep.py
--------------

Warm-start a real-time experiment by refilling its cache with a PREP file coming from another experiment.

"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.prep import PrepRefill


def setup(t, **kw):
    return Driver(
        tag='refill',
        ticket=t,
        nodes=[
            PrepRefill(tag='refill_prep', ticket=t, **kw),
        ],
        options=kw,
    )


