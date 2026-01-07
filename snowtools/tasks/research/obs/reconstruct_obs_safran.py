# -*- coding: utf-8 -*-
'''
'''

from snowtools.tasks.vortex_task_base import _VortexTask
from vortex.layout.nodes import Driver
from vortex import toolbox

import snowtools.algo  # noqa


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

        self.sh.title('METADATA')
        metadata = toolbox.input(
            role        = 'Metadata',
            local       = 'bdclim_SAFRAN_t_table_H_1950-2024_metadata.nc',
            genv        = self.conf.cycle,
            gvar        = 'METADATA',
            unknown     = True,
        )
        print(t.prompt, 'METADATA = ', metadata)
        print()

        self.sh.title('Predictors')
        metadata = toolbox.input(
            local       = 'predictors.json',
            genv        = self.conf.cycle,
            gvar        = 'PREDICTORS',
            unknown     = True,
        )
        print(t.prompt, 'METADATA = ', metadata)
        print()

        # TODO : this should be yearly flow ressources
        self.sh.title('Reconstructed Observations')
        new_obs = toolbox.input(
            local       = 'reconstructed_data_GRIN.pkl',
            genv        = self.conf.cycle,
            gvar        = 'RECONSTRUCTED_OBS',
            unknown     = True,
        )
        print(t.prompt, 'New observations = ', new_obs)
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

        rundate = self.conf.datebegin
        list_dates = self.get_list_seasons(self.conf.datebegin, self.conf.dateend)
        for rundate in list_dates:
            datebegin = rundate
            dateend = rundate.replace(year = rundate.year + 1)

            self.sh.title(f'Reconstructed Observations {datebegin.ymdh}-{dateend.ymdh}')
            out = toolbox.output(
                kind           = 'packedobs',
                datebegin      = datebegin.ymdh,
                dateend        = dateend.ymdh,
                date           = dateend.ymdh,
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                vapp           = 'safran',
                vconf          = self.conf.vconf,
                local          = f'{datebegin.ymd6h}_{dateend.ymd6h}/OBSERVATIONS.tar',
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

            datebegin = dateend

        if self.conf.debug:
            print('==================================================================================================')
            print('==================================================================================================')
            raise Exception('INFO :The execution went well, do not take into account the following error')
