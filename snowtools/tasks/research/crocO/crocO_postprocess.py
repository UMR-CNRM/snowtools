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
            PostProcess(tag='croco_postprocess', ticket=t, **kw),
        ],
        options=kw
    )


class PostProcess(_CrocO_Task):
    '''
    '''

    def process(self):
        t = self.ticket

        if 'early-fetch' in self.steps:

            if not hasattr(self.conf, 'stopdates'):
                stopdates = self.conf.assimdates + [str(self.conf.dateend)]
            else:
                stopdates = self.conf.stopdates

            datebegin = self.conf.datebegin
            for i, dateend in enumerate(stopdates):

                self.sh.title('Toolbox input PRO')
                tbin = toolbox.input(
                    role           = 'SnowpackSimulation',
                    local          = 'mb[member%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = datebegin,
                    dateend        = dateend,
                    date           = '[dateend]',
                    member         = self.conf.members,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'pro',
                    vapp           = 's2m',
                ),
                print(t.prompt, 'tbin =', tbin)
                print()

                datebegin = dateend

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
                date           = '[dateend]',
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
