# -*- coding: utf-8 -*-
'''
'''

from snowtools.tasks.vortex_task_base import _VortexTask
from vortex.layout.nodes import Driver
from vortex import toolbox
import snowtools.algo  # Mandatory import, do not remove !


def setup(t, **kw):
    return Driver(
        tag='extract_massifs',
        ticket=t,
        nodes=[
            Extract_massifs(tag='extract_massifs', ticket=t, **kw),
        ],
        options=kw
    )


class Extract_massifs(_VortexTask):
    '''
    Extract a list of massifs from a forcing file
    '''

    def get_remote_inputs(self):

        # TODO : check output geometry to crash now rather than at the end

        self.sh.title('Toolbox input FORCING')
        forcing_in = toolbox.input(
            role           = 'Forcing',
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            source_app     = self.conf.source_app if hasattr(self.conf, 'source_app') else None,
            source_conf    = self.conf.source_conf if hasattr(self.conf, 'source_conf') else None,
            cutoff         = 'assimilation',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_IN.nc',
            block          = 'meteo',
            intent         = 'in',
        ),
        print(self.ticket.prompt, 'Input forcing =', forcing_in)
        print()

    def algo(self):

        self.sh.title('Toolbox algo')
        algo = toolbox.algo(
            kind         = 'ExtractMassifs',
            massifs      = self.conf.massifs,
            role_members = 'Forcing',
            engine       = 'algo',
        )
        print(self.ticket.prompt, 'algo =', algo)
        print()
        algo.run()

    def put_remote_outputs(self):

        self.sh.title('Toolbox input FORCING')
        forcing_out = toolbox.output(
            role           = 'Forcing',
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            source_app     = self.conf.source_app,
            source_conf    = self.conf.source_conf,
            cutoff         = 'assimilation',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
            block          = 'meteo',
            geometry       = self.conf.geometry,
            experiment     = f'{self.conf.xpid.split("@")[0]}@{self.ticket.sh.getlogname()}',
        ),
        print(self.ticket.prompt, 'Output forcing =', forcing_out)
        print()
