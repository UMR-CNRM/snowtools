# -*- coding:Utf-8 -*-
"""
This "prep" driver allows to generate a PREP.nc file (initial conditions) from an existing init_TG.nc file.

"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.pre_process import Preprocess_Uenv_Namelist
from vortex_cen.tasks.surfex.init_clim_ground_temperature import InitClimGroundTemperature
from vortex_cen.tasks.surfex.prep import _Prep_Construct


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            InitClimGroundTemperature(tag='inittg', ticket=t, **kw),
            Preprocess_Uenv_Namelist(tag='preprocess', ticket=t, **kw),
            MakePrep(tag='makeprep', ticket=t, **kw),
        ],
        options=kw,
    )


class MakePrep(_Prep_Construct):

    def get_prep_executable(self):
        self.get_prep_exe_from_uenv()

    def get_namelist(self):
        # This task must be launched after a namelist pre-process task
        self.get_namelist_from_cache()
