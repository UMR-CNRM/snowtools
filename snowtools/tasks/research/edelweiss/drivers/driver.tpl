# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver
from snowtools.tasks.research.edelweiss.surfex import OfflineMPI
from snowtools.tasks.research.edelweiss.prep import PrepRefill


def setup(t, **kw):
    return Driver(
        tag='$DriverTag',
        ticket=t,
        nodes=[
$nodes
        ],
        options=kw
    )
