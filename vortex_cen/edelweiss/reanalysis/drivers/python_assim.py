# -*- coding: utf-8 -*-

from mkjob.nodes import Driver

from vortex_cen.tasks.assim.python_particle_filer import PythonParticleFilter


def setup(t, **kw):
    return Driver(
        tag="python_assim",
        ticket=t,
        nodes=[
            PythonParticleFilter(tag="python_assim", ticket=t, **kw),
        ],
        options=kw,
    )
