# -*- coding: utf-8 -*-
'''
'''

from snowtools.tasks.vortex_task_base import _VortexTask
from vortex.layout.nodes import Driver
from vortex import toolbox
from vortex.tools.env import Environment

import snowtools.algo  # noqa
import snowtools.data.obs  # noqa


def setup(t, **kw):
    return Driver(
        tag='safran_obs',
        ticket=t,
        nodes=[
            Reconstruct_SAFRAN_Obs(tag='safran_obs', ticket=t, **kw),
        ],
        options=kw,
    )


class Reconstruct_SAFRAN_Obs(_VortexTask):
    '''
    Task to build SAFRAN-compatible hourly observations files from reconstructed
    observation series.
    '''

    def get_remote_inputs(self):

        t = self.ticket

        self.sh.title('Reconstructed Observations')
        new_obs = toolbox.input(
            kind='SurfaceObservation',
            nativefmt   = 'netcdf',
            model       = 'safran',
            datebegin   = self.list_dates_begin,
            dateend     = self.dict_dates_end,
            date        = '[dateend]',
            geometry    = 'SinglePoint',
            vapp        = 'safran',
            vconf       = 'france',
            experiment  = self.conf.xpid,
            block       = 'observations',
            filename    = '[datebegin:ymdh]_[dateend:ymdh]/NEW_OBSERVATIONS.nc',
            namespace   = 'vortex.multi.fr',
            namebuild   = 'flat@cen'
        )
        print(t.prompt, 'New Obs = ', new_obs)
        print()

        # Get yearly observation packed files
#        rundate = self.conf.datebegin
#        list_dates = self.get_list_seasons(self.conf.datebegin, self.conf.dateend)
#        for rundate in list_dates:
#            datebegin = rundate
#            dateend = rundate.replace(year = rundate.year + 1)
        self.sh.title('Raw Observations')
        self.obs = toolbox.input(
            role           = 'Observations',
            part           = 'all',
            geometry       = self.conf.geometry,
            kind           = 'packedobs',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/OBSERVATIONS.tar',
            namespace      = 's2m.archive.fr',
            date           = self.conf.dateend.ymdh,
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            model          = 'safran',
            source         = 'surfaceobs',
            nativefmt      = 'tar',
            now            = True,
        )
        print(t.prompt, 'Raw Obs = ', self.obs)
        print()

        self.sh.title('Toolbox input listeo')
        listeo = toolbox.input(
            role            = 'ListePost',
            genv            = self.conf.cycle,
            gdomain         = '[geometry]',
            geometry        = self.conf.geometry,
            kind            = 'listeo',
            model           = 'safran',
            local           = 'listeo_reanalyse',
        )
        print(t.prompt, 'listeo =', listeo)
        print()

    def get_local_inputs(self):

        pass

    def algo(self):

        self.sh.title('Toolbox algo')
        algo = toolbox.algo(
            kind         = 'reconstruct_observations',
            role_members = 'Observations',
            engine       = 'algo',
            ntasks       = len(self.obs),
        )
        print(self.ticket.prompt, 'algo =', algo)
        print()
        algo.run()

    def put_remote_outputs(self):

        if '@' not in self.conf.xpid:
            user = Environment()['logname']
            experiment = f'{self.conf.xpid}@{user}'
        else:
            experiment = self.conf.xpid

        self.sh.title('Reconstructed Observations')
        out = toolbox.output(
            kind           = 'packedobs',
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            date           = '[dateend:ymdh]',
            experiment     = experiment,
            geometry       = self.conf.geometry,
            vapp           = 'safran',
            vconf          = self.conf.vconf,
            local          = '[datebegin:ymd6h]_[dateend:ymd6h]/OBSERVATIONS.tar',
            namespace      = 'vortex.archive.fr',
            model          = 'safran',
            source         = 'surfaceobs',
            namebuild      = 'flat@cen',
            block          = 'observations',
            nativefmt      = 'tar',
            cutoff         = 'assimilation',
            now            = True,
        )
        print(self.ticket.prompt, 'Output observations =', out)
        print()

        if 'debug' in self.conf and self.conf.debug:
            print('==================================================================================================')
            print('==================================================================================================')
            raise Exception('INFO :The execution went well, do not take into account the following error')
