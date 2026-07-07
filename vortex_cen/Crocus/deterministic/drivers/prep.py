# -*- coding:Utf-8 -*-
"""
This "prep" driver allows to generate a PREP.nc file (initial conditions) from an existing init_TG.nc file.

"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.prep import _PrepConstruct


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            MakePrep(tag='makeprep', ticket=t, **kw),
        ],
        options=kw,
    )


class MakePrep(_PrepConstruct):

    def get_init_TG(self):
        self.get_init_TG_from_uenv()

    def get_namelist(self):
        self.get_namelist_from_uenv()

    def get_prep_executable(self):
        self.get_prep_exe_from_uenv()
