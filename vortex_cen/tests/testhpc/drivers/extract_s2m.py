# -*- coding: utf-8 -*-
"""
Test the "ExtractS2MForcing" unit task, including a reproductibility test of the output file.
"""

import vortex
from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.extract_s2m_points import ExtractS2MForcing


def setup(t, **kw):
    return Driver(
        tag='extracts2m',
        ticket=t,
        nodes=[
            ExtractS2MForcing(tag='extracts2mforcing', ticket=t, **kw),
        ],
        options=kw,
    )
