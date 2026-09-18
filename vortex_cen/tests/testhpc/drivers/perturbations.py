# -*- coding:Utf-8 -*-
"""
Produce an ensemble of forcing from a deterministic forcing by
applying stochastic perturbations.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.meteo.perturbations import ForcingPerturbations


def setup(t, **kw):
    return Driver(
        tag='perturb',
        ticket=t,
        nodes=[
            ForcingPerturbations(tag='perturbations', ticket=t, **kw),
        ],
        options=kw,
    )
