# -*- coding: utf-8 -*-
'''
Created on 7 nov. 2017

@author: lafaysse
'''

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
import footprints


def setup(t, **kw):
    return Driver(
        tag='Surfex_Parallel',
        ticket=t,
        nodes=[
            Escroc_Eval_Task(tag='Escroc_Eval_Task', ticket=t, **kw),
        ],
        options=kw
    )


class Escroc_Eval_Task(Task):

    def process(self):

        t = self.ticket
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)

        startmember = int(self.conf.startmember) if hasattr(self.conf, "startmember") else 1
        members = list(range(startmember, int(self.conf.nmembers) + startmember)) \
            if hasattr(self.conf, "nmembers") else list(range(1, 36))

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            self.sh.title('Toolbox input tb01')
            tb01 = toolbox.input(
                role           = 'Snow reference data',
                local          = 'obs_insitu.nc',
                vapp           = 'ESM-SnowMIP',
                experiment     = 'snow@lafaysse',
                geometry       = self.conf.geometry,
                datebegin      = list_dates_begin_pro[0],
                dateend        = list_dates_end_pro[0],
                nativefmt      = 'netcdf',
                kind           = 'SnowObservations',
                model          = 'obs',
                namespace      = 'cenvortex.multi.fr',
            ),

            print(t.prompt, 'tb01 =', tb01)
            print()

            self.sh.title('Toolbox output tb02')
            tb02 = toolbox.input(
                local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh]_mb[member].nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = list_dates_begin_pro,
                dateend        = dict_dates_end_pro,
                member         = members,
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = 'cenvortex.multi.fr',
            ),
            print(t.prompt, 'tb02 =', tb02)
            print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb03 = scores')
            tb03 = tbalgo1 = toolbox.algo(
                engine         = 'blind',
                kind           = "scores_escroc",
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                list_var       = ["snowdepth", "snowswe", "snowdepthman", "snowsweman", "tsurf", "albedo"],
                list_scores    = ["bias", "rmse", "mae"],
                members        = footprints.util.rangex(members),
                ntasks         = 40 * int(self.conf.nnodes)
            )
            print(t.prompt, 'tb03 =', tb03)
            print()
            tb03.run()

        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:

            self.sh.title('Toolbox output tb04')
            tb04 = toolbox.output(
                local          = 'scores.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                datebegin      = self.conf.datebegin,
                dateend        = self.conf.dateend,
                nativefmt      = 'netcdf',
                kind           = 'ScoresSnow',
                model          = 'surfex',
                namespace      = 'cenvortex.multi.fr',
            ),
            print(t.prompt, 'tb04 =', tb04)
            print()
