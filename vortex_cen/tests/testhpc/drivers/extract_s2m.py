# -*- coding: utf-8 -*-
"""
Test the "ExtractS2MForcing" unit task, including a reproductibility test of the output file.
"""

import vortex
from mkjob.nodes import Driver
from vortex_cen.tasks.regrid.extract_s2m_points import ExtractS2MForcing


def setup(t, **kw):
    return Driver(
        tag='extracts2m',
        ticket=t,
        nodes=[
            TestExtractS2MForcing(tag='extracts2mforcing', ticket=t, **kw),
        ],
        options=kw,
    )


class TestExtractS2MForcing(ExtractS2MForcing):

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
            block          = 'meteo',
            model          = 'safran',
        ),
        print(self.ticket.prompt, 'diff forcing =', forcing_diff)
        print()
