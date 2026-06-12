# -*- coding:Utf-8 -*-
"""
SURFEX/Crocus reanalysis in the "massif" geometry.
Add slopes to the SAFRAN "flat massif" FORCING files and launch the OFFLINE executable.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.add_slopes import AddSlopes


def setup(t, **kw):
    return Driver(
        tag='addslopes',
        ticket=t,
        nodes=[
            AddSlopes(tag='addslopes', ticket=t, **kw),
        ],
        options=kw,
    )
