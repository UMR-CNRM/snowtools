# -*- coding: utf-8 -*-
"""
Test the "AddSlopes" unit task, including a reproductibility test of the output file.
"""

import vortex
from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.add_slopes import AddSlopes


def setup(t, **kw):
    return Driver(
        tag='addslopes',
        ticket=t,
        nodes=[
            TestAddSlopes(tag='addslopes', ticket=t, **kw),
        ],
        options=kw,
    )


class TestAddSlopes(AddSlopes):

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
            local          = '[datebegin:ymdh]_[dateend:ymdh]/FORCING_OUT.nc',
            block          = 'addslopes',
            model          = 'safran',
        ),
        print(self.ticket.prompt, 'diff forcing =', forcing_diff)
        print()
