# -*- coding: utf-8 -*-

import vortex
from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.shadows import Shadows


def setup(t, **kw):
    return Driver(
        tag='shadows',
        ticket=t,
        nodes=[
            TestShadows(tag='shadows', ticket=t, **kw),
        ],
        options=kw,
    )


class TestShadows(Shadows):

    def unittest(self):
        """
        Reproductibility test : compare output to reference.
        """

        self.sh.title('Diff FORCING')
        forcing_diff = vortex.diff(
            kind           = 'MeteorologicalForcing',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            geometry       = self.conf.geometry,
            experiment     = 'reference',
            username       = 'vernaym',
            namebuild      = 'flat@cen',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            block          = 'shadows',
            model          = 'safran',
        ),
        print(self.ticket.prompt, 'diff forcing =', forcing_diff)
        print()
