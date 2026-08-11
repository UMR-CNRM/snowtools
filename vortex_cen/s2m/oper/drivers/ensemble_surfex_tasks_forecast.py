# -*- coding: utf-8 -*-
"""
"""

from .ensemble_surfex_tasks_common import Ensemble_Surfex_Task
from .hydro_task import Hydro_Task
from mkjob.nodes import Driver, Task
from vortex_cen.tasks.oper_research_mixin import CENTaskMixIn
import vortex
import footprints


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
                Ensemble_Surfex_Task(tag='Ensemble_Surfex_Task', ticket=t, **kw, delay_component_errors=True, on_error='delayed_fail'),
                Four_Seasons_Task(tag='S2m_pp_Task', ticket=t, **kw, delay_component_errors=True, on_error='delayed_fail'),
                Hydro_Task(tag='S2M_Hydro_Task', ticket=t, **kw, delay_component_errors=True, on_error='delayed_fail')],
        options=kw
    )


class Four_Seasons_Task(CENTaskMixIn, Task):
    """
    Task : Four_Seasons_Task
    ========================

    Post-processing task for the 4 seasons bulletin. Uses S2m ensemble forecasts based on PEARP.
    Calculates ensemble deciles for the 12hourly and 1day snow accumulation for the moment.

    Inputs
    ------
    - PRO.nc : Crocus forecast

    Outputs
    -------
    - Post-processed file

    """

    MANDATORY_CONFIGURATION_VARIABLES = [
        "xpid",
        "geometry",
        "rundate+help=Date of run;choices=YYYYMMDD[03 06 09 12];type=str or Date",  # used in the "get_period" method
        "previ+help=Activate forecast mode;type=bool",  # used in the "get_period" method
        "namespace_in+help=Where to look for nwp files;type=str",
        "namespace_out+help=Where to store output guess files;type=str",
    ]
    OPTIONAL_CONFIGURATION_VARIABLES = [
    ]

    def process(self):

        t = self.ticket

        datebegin, dateend = self.get_period()
        pearpmembers, members = self.get_list_members(sytron=False)

        if 'fetch' in self.steps:

            if True:  # In order to have an indentation and facilitate the comparison with IGA Task

                self.sh.title('Toolbox input tb01')
                tb01 = vortex.input(
                    role        = 'CrocusForecast',
                    local       = 'mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment  = self.conf.xpid,
                    block       = 'pro',
                    geometry    = self.conf.geometry,
                    date        = self.conf.rundate,
                    datebegin   = datebegin,
                    dateend     = dateend,
                    member      = members,
                    nativefmt   = 'netcdf',
                    kind        = 'SnowpackSimulation',
                    model       = 'surfex',
                    namespace   = self.conf.namespace_in,
                    cutoff      = 'production',
                    fatal       = False
                ),
                print(t.prompt, 'tb01 =', tb01)
                print()

        if 'compute' in self.steps:
            self.sh.title('Toolbox algo tb02 = Postprocessing')

            tb02 = tbalgo1 = vortex.task(
                kind        = "s2m_postproc",
                varnames    = ['SD_12H_ISBA', 'SD_1DY_ISBA'],
                dateinit    = datebegin,
                engine      = 's2m',
                members     = footprints.util.rangex(members),
            ),
            print(t.prompt, 'tb02 =', tb02)
            print()

            self.component_runner(tbalgo1[0])

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:

            if True:  # In order to have an indentation and facilitate the comparison with IGA Task

                self.sh.title('Toolbox output tb03')
                tb03 = vortex.output(
                    role        = 'Postproc_output',
                    intent      = 'out',
                    local       = 'PRO_post_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment  = self.conf.xpid,
                    block       = 'postproc',
                    geometry    = self.conf.geometry,
                    date        = self.conf.rundate,
                    datebegin   = datebegin,
                    dateend     = dateend,
                    nativefmt   = 'netcdf',
                    kind        = 'SnowpackSimulation',
                    model       = 'postproc',
                    namespace   = self.conf.namespace_out,
                    cutoff      = 'production',
                    fatal       = True
                ),
                print(t.prompt, 'tb03 =', tb03)
                print()
