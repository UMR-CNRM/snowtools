# -*- coding: utf-8 -*-

from vortex.layout.nodes import Driver
from vortex_cen.tasks.forcing.extract_s2m_points import ExtractS2MForcing


def setup(t, **kw):
    return Driver(
        tag='extracts2m',
        ticket=t,
        nodes=[
            ExtractS2MForcing(tag='extract_forcing', ticket=t, **kw),
        ],
        options=kw
    )
