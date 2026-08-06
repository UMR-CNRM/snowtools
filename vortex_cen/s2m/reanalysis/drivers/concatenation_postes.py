# -*- coding:Utf-8 -*-
"""
Concatenate station/postes FORCING files covering different domains and solar relief-induced solar masks.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.shadows import ShadowsPostes
from vortex_cen.tasks.regrid.concatenate import ForcingSpatialConcatenation


def setup(t, **kw):
    return Driver(
        tag='surfex_postes',
        ticket=t,
        nodes=[
            ForcingSpatialConcatenation(tag='concatenation', ticket=t, **kw),
            ShadowsPostes(tag='shadows', ticket=t, **kw),
        ],
        options=kw,
    )

