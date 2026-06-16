# -*- coding: utf-8 -*-

from mkjob.nodes import Driver, Task
from vortex_cen.layout.nodes import S2MTaskMixIn
import vortex

def setup(t, **kw):
    return Driver(
        tag    = 'refillprep',
        ticket = t,
        nodes  = [
            RefillPrep(tag='refillprep', ticket=t, delay_component_errors=True, on_error='delayed_fail', **kw),
        ],
        options = kw,
    )


class RefillPrep(Task, S2MTaskMixIn):
    """
    Task : RefillPrep
    =================

    Task to get a PREP file from another experiment to "cold start" a new experiment.

    Input:
    ------
    - PREP.nc file

    Output:
    -------
    - PREP.nc file
    """

    MANDATORY_CONFIGURATION_VARIABLES = [
        "xpid",
        "prep_xpid+help=Experiment identifier from which to refill the PREP file;type=str",
        "geometry",
        "rundate+help=Date of run;choices=YYYYMMDD[03 06 09];type=str or Date",
        "datevalidity+help=The validity date of the PREP file",
    ]
    OPTIONAL_CONFIGURATION_VARIABLES = [
        "prep_user",
        "prep_vortex1",
    ]

    def process(self):

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            self.sh.title('Input PREP')
            prep = vortex.input(
                role           = 'SnowpackInit',
                local          = 'PREP.nc',
                block          = 'prep',
                experiment     = self.conf.prep_xpid,
                username       = self.conf.get('prep_user', None),
                geometry       = self.conf.geometry,
                datevalidity   = self.conf.datevalidity,
                date           = self.conf.rundate,
                member         = 35,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                cutoff         = 'assimilation',
                vortex1        = self.conf.get('prep_vortex1', False),
            ),
            print(self.ticket.prompt, 'prep =', prep)
            print()

        if 'backup' in self.steps or 'late-backup' in self.steps:

            self.sh.title('Output PREP')
            prep = vortex.output(
                role           = 'SnowpackInit',
                local          = 'PREP.nc',
                block          = 'prep',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datevalidity   = self.conf.datevalidity,
                date           = self.conf.rundate,
                member         = 35,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                cutoff         = 'assimilation',
            ),
            print(self.ticket.prompt, 'prep =', prep)
            print()
