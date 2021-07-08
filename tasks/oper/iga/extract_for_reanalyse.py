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
from bronx.stdtypes.date import daterange, yesterday, tomorrow, Period
import footprints
from vortex.algo.components import DelayedAlgoComponentError

import snowtools
import re



def setup(t, **kw):
    return Driver(
        tag = 'Surfex_Parallel',
        ticket = t,
        nodes = [
            Monthly_Surfex_Reanalysis(tag='Monthly_Surfex_Reanalysis', ticket=t, **kw),
        ],
        options=kw
    )


class Monthly_Surfex_Reanalysis(S2MTaskMixIn, OpTask):
    '''

    '''

    filter_execution_error = S2MTaskMixIn.s2moper_filter_execution_error

    def refill(self):

        t = self.ticket

        datebegin, dateend = self.get_period()
        rundate_forcing = self.get_rundate_forcing()
        rundate_prep, alternate_rundate_prep = self.get_rundate_prep()

        list_geometry = self.get_list_geometry()
        source_safran, block_safran = self.get_source_safran()
        alternate_safran, alternate_block, alternate_geometry = self.get_alternate_safran()
        exceptional_save_forcing = False


        self.sh.title('Toolbox input tb01')
        tb01 = toolbox.input(
        	role           = 'SnowpackInit',
                local          = 'PREP.nc',
                block          = 'prep',
                experiment     = 'oper',#self.conf.xpid,
                geometry       = self.conf.geometry,
                datevalidity   = datebegin,
                date           = rundate_prep,
                member         = 35,
                namespace      = 'vortex.multi.fr',
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                fatal          = True,
                cutoff         = 'assimilation'
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
