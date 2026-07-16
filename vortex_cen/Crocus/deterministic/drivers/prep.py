# -*- coding:Utf-8 -*-
"""
This "prep" driver allows to generate a PREP.nc file (initial conditions) from an existing init_TG.nc file.

"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import PreprocessUenvNamelist
from vortex_cen.tasks.surfex.init_clim_ground_temperature import FetchClimGroundTemperatureOrCrash
from vortex_cen.tasks.surfex.prep import MakePrepFile

def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            PreprocessUenvNamelist(tag='preprocess_uenv_namelist', ticket=t, **kw),
            FetchClimGroundTemperatureOrCrash(tag='fetch_clim_ground_temperature', ticket=t, **kw),
            MakePrepFile(tag='makeprep', ticket=t, **kw),
        ],
        options=kw,
    )


