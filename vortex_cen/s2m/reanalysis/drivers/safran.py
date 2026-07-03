# -*- coding:Utf-8 -*-
"""
Production of SAFRAN-reanalysis FORCING files on "flat massif" and "station" geometries.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.safran.reanalysis import SafranReanalysis


def setup(t, **kw):
    return Driver(
        tag='safran',
        ticket=t,
        nodes=[
            SafranReanalysis(tag='safran', ticket=t, **kw),
        ],
        options=kw,
    )
