# -*- coding:Utf-8 -*-
"""
refill_prep.py
--------------

Warm-start a real-time experiment by refilling its cache with a PREP file coming from another experiment.

"""

import footprints
import vortex
from mkjob.nodes import Driver
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin


def setup(t, **kw):
    return Driver(
        tag='refill',
        ticket=t,
        nodes=[
            PrepRefill(tag='refill_prep', ticket=t, **kw),
        ],
        options=kw,
    )


class PrepRefill(_CenResearchTask, SurfexCommonsMixin):
    """
    **Task : PrepRefill**


    **Input:**

    - A single PREP.nc

    **Output:**

    - A copy of the input PREP.nc file into the cache of all members of the real-time chain

    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "prep_date",
            "prep_xpid",
            "geometry",
            "xpid",
            "rundate+help=Date of run;choices=YYYYMMDD[03 06 09 12];type=str or Date",
            "members",
            "datevalidity",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "prep_user",
            "prep_vapp",
            "prep_vconf",
            "prep_vortex1",
            "prep_block",
            "cutoff+help=Target *cutoff* (refill an analysis or a forecast output);type=str;"
            "choices='assimilation', 'production';default='assimilation'",
        ]
        overwrite = [
            "datebegin",
            "dateend",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES, overwrite=overwrite)

    def get_remote_inputs(self):
        self.get_prep()

    def get_local_inputs(self):
        pass

    def algo(self):
        pass

    def launch_algo(self, algo):
        pass

    def put_outputs(self):

        self.sh.title('Output PREP(s)')
        prep = vortex.output(
            role           = 'SnowpackInit',
            local          = 'PREP.nc',
            block          = 'prep',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datevalidity   = self.conf.datevalidity,
            date           = self.conf.rundate,
            member         = footprints.util.rangex(self.conf.members),
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            vortex1        = self.conf.get('prep_vortex1', False),
            cutoff         = self.conf.get('cutoff', 'assimilation'),
        ),
        print(self.ticket.prompt, 'Prep =', prep)
        print()
