# -*- coding:Utf-8 -*-
"""
A spinup simulation produces more realistic initial conditions (PREP.nc file)
than a simple execution of the PREP executable.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrCrash
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrCrash
from vortex_cen.tasks.surfex.offline import Spinup


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            FetchPgdFileOrCrash(tag='getpgd', ticket=t, **kw),
            FetchPrepFileOrCrash(tag='getprep', ticket=t, **kw),
            Spinup(tag='spinup', ticket=t, **kw),
        ],
        options=kw,
    )
