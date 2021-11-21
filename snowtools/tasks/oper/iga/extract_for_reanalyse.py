'''
Created on 7 nov. 2017

@author: lafaysse

'''


import iga.tools.op as op
from iga.tools.apps import OpTask
from vortex.tools.actions import actiond as ad

from vortex.layout.nodes import Driver
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox


def setup(t, **kw):
    return Driver(
        tag = 'Surfex_Parallel',
        ticket = t,
        nodes = [
            Monthly_Surfex_Reanalysis_GetInit(tag='Monthly_Surfex_Reanalysis', ticket=t, **kw),
        ],
        options=kw
    )


class Monthly_Surfex_Reanalysis_GetInit(S2MTaskMixIn, OpTask):
    """
    Get from BDPE the PREP file of initial conditions for the monthly reanalysis
    """

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def refill(self):

        t = self.ticket

        datebegin, dateend = self.get_period()
        rundate_prep, alternate_rundate_prep = self.get_rundate_prep()

        # This product is written in BDPE once a year by ensemble_surfex_tasks_bdpe.py and read here once a month.
        self.sh.title('Toolbox input tb01')
        tb01 = toolbox.input(
            role           = 'SnowpackInitForMonthlyReanalysis',
            local          = 'PREP.nc',
            block          = 'prep',
            experiment     = 'oper',#self.conf.xpid,
            geometry       = self.conf.geometry,
            datevalidity   = datebegin,
            date           = rundate_prep,
            member         = 35,
            namespace      = 'bdpe.archive.fr',
            bdpeid         = self.conf.num_bdpe_initrea[self.conf.xpid],
            intent         = 'inout',
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            fatal          = False,
            cutoff         = 'assimilation'
        ),
        print((t.prompt, 'tb01 =', tb01))
        print()

        # Alternates (3 previous days)
        for i, alternate_prep in enumerate(alternate_rundate_prep):

            fatal = i == len(alternate_rundate_prep) - 1

            tb01 = toolbox.input(
                alternate='SnowpackInitForMonthlyReanalysis',
                local='PREP.nc',
                block='prep',
                experiment='oper',  # self.conf.xpid,
                geometry=self.conf.geometry,
                datevalidity=datebegin,
                date=alternate_prep[0],
                member=35,
                namespace='bdpe.archive.fr',
                bdpeid=self.conf.num_bdpe_initrea[self.conf.xpid],
                intent='inout',
                nativefmt='netcdf',
                kind='PREP',
                model='surfex',
                fatal=fatal,
                cutoff='assimilation'
            ),
            print((t.prompt, 'tb01 =', tb01))
            print()

        self.sh.title('Toolbox input tb02')
        tb02 = toolbox.output(
                role           = 'SnowpackInit',
                local          = 'PREP.nc',
                block          = 'prep',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datevalidity   = datebegin,
                date           = rundate_prep,
                member         = 35,
                namespace      = 'vortex.cache.fr',
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                fatal          = True,
                cutoff         = 'assimilation'
        ),
        print((t.prompt, 'tb02 =', tb02))
        print()
