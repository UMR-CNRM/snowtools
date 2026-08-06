# -*- coding:Utf-8 -*-

"""
The "escroc" driver allows to launch multi-physics SURFEX/Crocus simulations in a research context
based on any meteorological forcing file(s).
WARNING : It does not guarantee the reproductibility of the simulations due to a loose user control
on the input files : missing inputs files will be looked for on alternate locations and will
enventually be generated if necessary and possible.

"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.offline_ensemble import EscrocResearch
from vortex_cen.tasks.surfex.pre_process import PreprocessNamelist
from vortex_cen.tasks.surfex.pgd import FetchPgdFileOrMake
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrMake
from vortex_cen.tasks.surfex.init_clim_ground_temperature import MakeClimGroundTemperatureIfNoPrep


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            PreprocessNamelist(tag='preprocess', ticket=t, **kw),
            MakeClimGroundTemperatureIfNoPrep(tag='inittg', ticket=t, **kw),
            FetchPgdFileOrMake(tag='pgd', ticket=t, **kw),
            FetchPrepFileOrMake(tag='prep', ticket=t, **kw),
            EscrocResearch(tag='escroc_task', ticket=t, **kw),
        ],
        options=kw,
    )

