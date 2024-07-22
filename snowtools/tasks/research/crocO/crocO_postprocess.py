# -*- coding: utf-8 -*-
'''
Created on july 2024

@author: M.Vernay
Concatenation of crocO outputs.
'''

from vortex import toolbox

from vortex.layout.nodes import Driver
from snowtools.tasks.research.crocO.crocO_common import _CrocO_Task


def setup(t, **kw):
    return Driver(
        tag='croco_postprocess',
        ticket=t,
        nodes=[
            PostPocess(tag='croco_postprocess', ticket=t, **kw),
        ],
        options=kw
    )


class PostPocess(_CrocO_Task):
    '''
    '''

    def process(self):
        t = self.ticket

        if 'early-fetch' in self.steps:

            for i, dateend in enumerate(self.conf.stopdates):

                if i == 0:
                    datebegin = self.conf.datebegin
                else:
                    datebegin = self.conf.stopdates[i - 1]

                self.sh.title('Toolbox input PRO')
                tbin = toolbox.input(
                    role           = 'SnowpackSimulation',
                    local          = 'mb[member%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin,
                    dateend        = dateend,
                    member         = self.conf.members,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'pro',
                ),
                print(t.prompt, 'tbin =', tbin)
                print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo')
            tbalgo = toolbox.algo(
                kind          = 'croco_postprocess',
                datebegin     = self.conf.datebegin,
                dateend       = self.conf.dateend,
                engine        = 'algo',  # _CENTaylorRun algo component "family" to execution a piece of python code
                ntasks        = max(2, len(tbin)),  # Do not forget to set the number of tasks for parallelisation
                role_members  = 'SnowpackSimulation',
            )
            print(t.prompt, 'tbalgo =', tbalgo)
            print()
            tbalgo.run()

        if 'late-backup' in self.steps:

            self.sh.title('Toolbox output PRO')
            tbout = toolbox.output(
                #local          = 'mb[(member+1)%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                local          = 'mb[member%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                #member         = [mb - 1 for mb in self.conf.members],
                member         = self.conf.members,
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'pro',
            ),
            print(t.prompt, 'tbout =', tbout)
            print()
