# -*- coding: utf-8 -*-
"""
Test the "InterpolateS2MRemoteForcing" unit task.
"""

from mkjob.nodes import Driver

from vortex_cen.tasks.regrid.interpol import InterpolateS2MRemoteForcing


def setup(t, **kw):
    return Driver(
        tag="interpolforcing",
        ticket=t,
        nodes=[
            InterpolateS2MRemoteForcing(tag="interpolates2mforcing", ticket=t, **kw),
        ],
        options=kw,
    )
