# -*- coding:Utf-8 -*-

"""
The "surfex" driver allows to launch deterministic SURFEX/Crocus simulations in a research context
based on any meteorological forcing file.
WARNING : It does not guarantee the reproducibility of the simulations due to a loose user control
on the input files : missing inputs files will be looked for on alternate locations and will
enventually be generated if necessary and possible.

"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.offline import OfflineLocalForcing
from vortex_cen.tasks.surfex.pgd import FetchPgdOrMake
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrMake
from vortex_cen.tasks.surfex.init_clim_ground_temperature import MakeClimGroundTemperatureIfNoPrep


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            MakeClimGroundTemperatureIfNoPrep(tag='inittg', ticket=t, **kw),
            FetchPgdOrMake(tag='pgd', ticket=t, **kw),
            FetchPrepFileOrMake(tag='prep', ticket=t, **kw),
            OfflineLocalForcing(tag='offline', ticket=t, **kw),
        ],
        options=kw,
    )
