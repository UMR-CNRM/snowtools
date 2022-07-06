# -*- coding: utf-8 -*-
"""
Created on 7 nov. 2017

@author: lafaysse
"""

from .ensemble_surfex_tasks_common import Ensemble_Surfex_Task
from .ensemble_surfex_tasks_bdpe import Rapatrie_Forcing, Rapatrie_Prep, Rapatrie_Pro, Rapatrie_Postproc, Rapatrie_Forcing_Deterministic, Rapatrie_Pro_Deterministic
from vortex.layout.nodes import Driver, Task
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
import footprints


def setup(t, **kw):
    return Driver(
        tag = 'Surfex_Parallel',
        ticket = t,
        nodes = [
                Ensemble_Surfex_Task(tag='Ensemble_Surfex_Task', ticket=t, **kw, delay_component_errors=True, on_error='delayed_fail'),
                Rapatrie_Forcing_Deterministic(tag='Rapatrie_Forcing_Deterministic', ticket=t, **kw),
                Rapatrie_Pro_Deterministic(tag='Rapatrie_Pro_Deterministic', ticket=t, **kw),
                Four_Seasons_Task(tag='S2m_pp_Task', ticket=t, **kw),
                Rapatrie_Postproc(tag='Rapatrie_Postproc', ticket=t, **kw),
                Rapatrie_Forcing(tag='Rapatrie_Forcing', ticket=t, **kw),
                Rapatrie_Pro(tag='Rapatrie_Pro', ticket=t, **kw),
                Rapatrie_Prep(tag='Rapatrie_Prep', ticket=t, **kw),
        ],
        options=kw
    )


class Four_Seasons_Task(S2MTaskMixIn, Task):
    """
    Post-processing task for the 4 seasons bulletin. Uses S2m ensemble forecasts based on PEARP.
    Calculates ensemble deciles for the 12hourly and 1day snow accumulation for the moment.
    """

    def process(self):

        t = self.ticket

        datebegin, dateend = self.get_period()
        pearpmembers, members = self.get_list_members(sytron=False)

        if 'fetch' in self.steps:

            with op.InputReportContext(self, t):

                self.sh.title('Toolbox input tb01')
                tb01 = toolbox.input(
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

            tb02 = tbalgo1 = toolbox.algo(
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

            with op.OutputReportContext(self, t):

                self.sh.title('Toolbox output tb03')
                tb03 = toolbox.output(
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
