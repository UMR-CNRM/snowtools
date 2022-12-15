# -*- coding: utf-8 -*-
'''
Created on 27 mars 2019

@author: cluzetb
Vortex task performing up to 40 offline runs in parallel on a single node

'''
import os

from vortex import toolbox
from snowtools.tasks.research.crocO.crocO_common import _CrocO_Task


class Soda_Task(_CrocO_Task):
    '''
    Task for 1 assimilation cycle
    '''

    def process(self):
        t = self.ticket

        # ##### PREPARE common stuff with the offline task ###########

        firstloop, _, assDate, = self.prepare_common()

        if 'early-fetch' in self.steps or 'fetch' in self.steps:

            self.get_common_consts(firstloop, self.conf.members)  # soda gets ALL members, not only membersnode.

            # ################# FETCH EXECUTABLE ###############
            self.sh.title('Toolbox executable tb08_s= tbx4 (soda)')
            tb08_s = tbx4 = toolbox.executable(
                role           = 'Binary',
                kind           = 'soda',
                local          = 'SODA',
                model          = 'surfex',
                remote         = self.conf.exesurfex + "/SODA"
            )

            print(t.prompt, 'tb08_s =', tb08_s)
            print()

            # ############### FETCH OBSERVATIONS  ##########
            self.sh.title('Toolbox input tobs (obs)')
            tobs = toolbox.input(
                geometry        = self.conf.geometry,
                model           = 'surfex',
                nativefmt       = 'netcdf',
                datevalidity    = assDate,
                block           = self.conf.sensor,
                scope           = self.conf.scope,
                kind            = 'SnowObservations',
                namespace       = 'vortex.multi.fr',
                namebuild       = 'flat@cen',
                experiment      = self.conf.obsxpid,
                local           = 'OBSERVATIONS_[datevalidity:ymdHh].nc',
                fatal           = True
            )
            print(t.prompt, 'tobs =', tobs)
            print()

        if 'fetch' in self.steps:

            self.get_common_fetch() # get the namelist

            # ################# FETCH PREP FILES ################
            # put it in a filetree(/mb0001 etc.) inside the soda task rep for backward comp.
            self.sh.title('Toolbox input tb03_SODA (background)')
            tb03_soda = toolbox.input(
                role           = 'SnowpackInit',
                member         = self.conf.members,
                local          = 'mb[member%04d]/PREP_[date:ymdh].nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                date           = assDate,
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.cache.fr',  # get it on the cache from last loop
                namebuild      = 'flat@cen',
                block          = 'bg',          # get it on cache @mb****/bg
                fatal          = True,
            ),
            print(t.prompt, 'tb03_SODA =', tb03_soda)
            print()
            # ############### FETCH conf file ? ################
            # TODO : get the actualized version of the conf file.

        if 'compute' in self.steps:

            # ################## SODA toolbox.algo
            # test of obs exists/successfully downloaded
            if any([s.stage == 'get' for s in tobs]):

                # soda
                self.sh.title('Toolbox algo tb11_s = SODA (soda)')

                tb11_s = tbalgo4s = toolbox.algo(
                    engine         = 'parallel',
                    binary         = 'SODA',
                    kind           = "s2m_soda",
                    dateassim      = assDate,
                )
                print(t.prompt, 'tb11_s =', tb11_s)
                print()
                self.component_runner(tbalgo4s, tbx4, mpiopts=dict(nnodes=1, nprocs=1, ntasks=1))

        if 'backup' in self.steps or 'late-backup' in self.steps:

            # if uploading to sxcen, must be done file/file to prevent from having too many simultaneous transfers
            storage = ['hendrix.meteo.fr']
            enforcesync = dict(storage={'hendrix.meteo.fr': False, 'sxcen.cnrm.meteo.fr': True})
            if hasattr(self.conf, 'writesx'):
                if self.conf.writesx:
                    storage.append('sxcen.cnrm.meteo.fr')

            if os.path.exists('OBSERVATIONS_' + assDate.ymdHh + '.nc'):

                self.sh.title('Toolbox output tb12_bk (analysis backup)')
                tb20 = toolbox.output(
                    local          = 'mb[member%04d]/PREP_[date:ymdh].nc',
                    role           = 'SnowpackInit',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    date           = assDate,
                    member         = self.conf.members,  # BC 21/03/19 probably replace by mbids
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    storage        = storage,
                    enforcesync    = enforcesync,
                    namebuild      = 'flat@cen',
                    block          = 'an',
                    fatal          = True
                ),
                print(t.prompt, 'tb20 =', tb20)
                print()

        if 'late-backup' in self.steps:

            if os.path.exists('OBSERVATIONS_' + assDate.ymdHh + '.nc'):

                # ########## PF DIAGNOSTICS ################################################

                diags_kind = [k for k in  ('PART', 'BG_CORR', 'IMASK', 'ALPHA')
                              if self.sh.path.exists(k + '_' + assDate.ymdh + '.txt')]
                if 'PART' not in diags_kind:
                    print('PART_' + assDate.ymdh + '.txt does not exist')

                self.sh.title('Toolbox output tbdiags')
                tb242 = toolbox.output(
                    kind           = diags_kind,
                    model          = 'soda',
                    block          = 'soda',
                    namebuild      = 'flat@cen',
                    namespace      = 'vortex.multi.fr',
                    storage        = storage,
                    enforcesync    = enforcesync,
                    fatal          = True,
                    dateassim      = assDate,
                    experiment     = self.conf.xpid,
                    local          = '[kind]_[dateassim:ymdh].txt',
                )
                print(t.prompt, 'tb242 =', tb242)
                print()

