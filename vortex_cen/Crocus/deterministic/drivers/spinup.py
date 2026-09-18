# -*- coding:Utf-8 -*-
"""
A spinup simulation produces more realistic initial conditions (PREP.nc file)
than a simple execution of the PREP executable.
"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrMake
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrMake
from vortex_cen.tasks.surfex.init_clim_ground_temperature import MakeClimGroundTemperatureIfNoPrep
from vortex_cen.tasks.surfex.offline import Spinup


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            MakeClimGroundTemperatureIfNoPrep(tag='inittg', ticket=t, **kw),
            FetchPgdFileOrMake(tag='pgd', ticket=t, **kw),
            FetchPrepFileOrMake(tag='prep', ticket=t, **kw),
            Spinup(tag='spinup', ticket=t, **kw),
        ],
        options=kw,
    )
