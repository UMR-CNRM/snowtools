# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver
from snowtools.tasks.research.surfex import surfex, prep


def setup(t, **kw):
    return Driver(
        tag='SurfexMPI',
        ticket=t,
        nodes=[
            prep.PrepRefill(tag='prep', ticket=t, **kw),
            surfex.Offline_mpi(tag='offline', ticket=t, **kw),
        ],
        options=kw
    )
