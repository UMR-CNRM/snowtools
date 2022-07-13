# -*- coding: utf-8 -*-
'''
Created on 27 mars 2019

@author: cluzetb
Vortex task performing up to 40 offline runs in parallel on a single node

'''
import os

from vortex import toolbox
from bronx.stdtypes.date import Date
from snowtools.tasks.research.crocO.crocO_common import _CrocO_Task


class Soda_Task(_CrocO_Task):
    '''
    classdocs
    '''

    def process(self):
        t = self.ticket
        # ##### PREPARE common stuff with the offline task ###########

        firstloop, _, assDate, = self.prepare_common()
        # if 'early-fetch' in self.steps or 'fetch' in self.steps:
        if 'early-fetch' in self.steps:
            # ################# FETCH CONSTANT FILES #############
            # 17/04/19 -> keep a workSODA rep for backward compatibility
            # AND sake of simplicity, even though it is not needed anymore.

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
            nature = self.conf.sensor + "_" + self.conf.geometry.tag # to be changed later
            tobs = toolbox.input(
                geometry        = self.conf.geometry,
                nativefmt       = 'netcdf',
                datevalidity    = assDate,
                model           = 'obs',
                block           = self.conf.sensor,
                nature          = nature,
                kind            = 'SnowObservations',
                namespace       = 'vortex.multi.fr',
                namebuild       = 'flat@cen',
                experiment      = self.conf.obsxpid,
#                local           = 'OBSERVATIONS.nc',
                local           = 'workSODA/OBSERVATIONS_[datevalidity:ymdHh].nc',
                stage           = '1date',
                fatal           = True
            )
            print(t.prompt, 'tobs =', tobs)
            print()

        if 'fetch' in self.steps:
            # ################# FETCH PREP FILES ################
            # put it in a filetree(/mb0001 etc.) inside the soda task rep for backward comp.
            dmembers = {str(mb): mb for mb in self.conf.members}
            dlocal_names = {str(mb): 'mb{0:04d}'.format(mb) + '/PREP_[date:ymdh].nc'
                            for mb in self.conf.members}
            self.sh.title('Toolbox input tb03_SODA (background)')
            tb03_soda = toolbox.input(
                alternate      = 'SnowpackInit',
                realmember     = self.conf.members,
                member         = dict(realmember= dmembers),
                local          = dict(realmember= dlocal_names),
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                date           = assDate,
                intent         = 'inout',
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.cache.fr',  # get it on the cache from last loop
                namebuild      = 'flat@cen',
                block          = 'bg',          # get it on cache @mb****/bg
                stage          = '_bg',
                fatal          = True,
            ),
            print(t.prompt, 'tb03_SODA =', tb03_soda)
            print()
            # ############### FETCH conf file ? ################
            # TODO : get the actualized version of the conf file.
        if 'compute' in self.steps:
            # ################## SODA toolbox.algo
            # test of obs exists/successfully downloaded
            if os.path.exists('workSODA/OBSERVATIONS_' + assDate.ymdHh + '.nc'):
                # soda
                self.sh.title('Toolbox algo tb11_s = SODA (soda)')

                tb11_s = tbalgo4s = toolbox.algo(
                    engine         = 'parallel',
                    binary         = 'SODA',
                    kind           = "s2m_soda",
                    dateassim      = assDate,
                    members        = self.conf.members,  # no need for mbids in SODA !
                )
                print(t.prompt, 'tb11_s =', tb11_s)
                print()
                self.component_runner(tbalgo4s, tbx4, mpiopts=dict(nnodes=1, nprocs=1, ntasks=1))
            """
            else:  # in case of SODA, if obs file doesn't exist, we need to remove PREP.nc in every mbdir
                (this is done by soda)
                for mb in self.conf.members:
                    if os.path.exists('mb{0:04d}'.format(mb) + '/PREP.nc'):
                        os.remove('mb{0:04d}'.format(mb) + '/PREP.nc')
            """

        if 'backup' in self.steps:

            # ################### output
            # (deprecated, to update, now renaming is not done remotely, files are stored in separate bg and an blocks
            #                                 local              -->     remote
            # SODA        obs | analysis    PREP_YYYYMMDDHH.nc        PREP_YYYYMMDDHH_an.nc
            # ----            | background  PREP_YYYYMMDDYHH_bg.nc    PREP_YYYYMMDDHH_bg.nc
            # ----      noobs | analysis    NONE                      NONE
            # ----            | background  PREP_YYYYMMDDHH.nc        PREP_YYYYMMDDHH_bg.nc
            # OPENLOOP          analysis    NONE                      NONE
            # -------           background  PREP_YYYYMMDDHH.nc        PREP_YYYYMMDDHH_bg.nc
            if self.conf.openloop == 'on' or not os.path.exists('workSODA/OBSERVATIONS_' + Date(assDate).ymdHh + '.nc'):  # openloop or noobs/final step
                localan = 'NONE'
                # localbg = 'mb[member%04d]/PREP_[date:ymdh].nc'
            else:  # usual soda step
                localan = 'mb[member%04d]/PREP_[date:ymdh].nc'
                # localbg = 'mb[member%04d]/PREP_[date:ymdh]_bg.nc'

            self.sh.title('Toolbox output tb12_bk (analysis backup)')
            tb20 = toolbox.output(
                local          = localan,
                role           = 'SnowpackInit',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                date           = assDate,
                period         = assDate,
                member         = self.conf.members,  # BC 21/03/19 probably replace by mbids
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.cache.fr',
                namebuild      = 'flat@cen',
                block          = 'an',
                stage          = '_an',
                fatal          = False  # doesn't exist if openloop
            ),
            print(t.prompt, 'tb20 =', tb20)
            print()

            # ############# PUT CONF-file to the cache
            # @TODO for the actualization of the conf file.

        if 'late-backup' in self.steps:
            # if fetchnig to sxcen, must be done file/file to prevent from having too many simultaneous transfers
            storage = ['hendrix.meteo.fr']
            enforcesync = dict(storage={'hendrix.meteo.fr': False, 'sxcen.cnrm.meteo.fr': True})
            if self.conf.writesx == 'on':
                storage.append('sxcen.cnrm.meteo.fr')
            if os.path.exists('workSODA/OBSERVATIONS_' + Date(assDate).ymdHh + '.nc'):
                if self.conf.pickleit == 'off':
                    # ########### PUT PREP FILES HENDRIX ######################################
                    self.sh.title('Toolbox output tb12_ar (analysis archive)')
                    tb20 = toolbox.output(
                        local          = localan,
                        role           = 'SnowpackInit',
                        experiment     = self.conf.xpid,
                        geometry       = self.conf.geometry,
                        date           = assDate,
                        period         = assDate,
                        member         = self.conf.members,  # BC 21/03/19 probably replace by mbids
                        nativefmt      = 'netcdf',
                        kind           = 'PREP',
                        model          = 'surfex',
                        namespace      = 'vortex.multi.fr',
                        storage        = storage,
                        enforcesync    = enforcesync,
                        namebuild      = 'flat@cen',
                        block          = 'an',
                        stage          = '_an',
                        fatal          = False  # doesn't exist if openloop
                    ),
                    print(t.prompt, 'tb20 =', tb20)
                    print()

                # ########## RESAMPLE FILES HENDRIX ########################################
                if not os.path.exists('workSODA/PART_' + Date(assDate).ymdh + '.txt'):
                    print('workSODA/PART_' + Date(assDate).ymdh + '.txt doesnot exist')
                else:
                    self.sh.title('Toolbox output tb24 (part archive)')
                    tb24 = toolbox.output(
                        model           = 'PART',
                        namebuild       = 'flat@cen',
                        namespace      = 'vortex.multi.fr',
                        storage        = storage,
                        enforcesync    = enforcesync,
                        storetrack     = False,
                        fatal           = True,
                        dateassim       = assDate,
                        block           = 'workSODA',
                        experiment      = self.conf.xpid,
                        filename        = 'workSODA/PART_' + Date(assDate).ymdh + '.txt',
                    )
                    print(t.prompt, 'tb24 =', tb24)
                    print()
                # ########## BG_CORR FILE ON HENDRIX (klocal case only) ########################################
                if os.path.exists('workSODA/BG_CORR_' + Date(assDate).ymdh + '.txt'):
                    self.sh.title('Toolbox output tb242 (bg_corr archive)')
                    tb242 = toolbox.output(
                        model           = 'BG_CORR',
                        namebuild       = 'flat@cen',
                        namespace      = 'vortex.multi.fr',
                        storage        = storage,
                        enforcesync    = enforcesync,
                        fatal           = True,
                        dateassim       = assDate,
                        block           = 'workSODA',
                        experiment      = self.conf.xpid,
                        filename        = 'workSODA/BG_CORR_' + Date(assDate).ymdh + '.txt',
                    )
                    print(t.prompt, 'tb242 =', tb242)
                    print()
                # ########## IMASK FILE ON HENDRIX (klocal case only) ########################################
                if os.path.exists('workSODA/IMASK_' + Date(assDate).ymdh + '.txt'):
                    self.sh.title('Toolbox output tb243 (imask archive)')
                    tb243 = toolbox.output(
                        model           = 'IMASK',
                        namebuild       = 'flat@cen',
                        namespace      = 'vortex.multi.fr',
                        storage        = storage,
                        enforcesync    = enforcesync,
                        storetrack     = False,
                        fatal           = True,
                        dateassim       = assDate,
                        block           = 'workSODA',
                        experiment      = self.conf.xpid,
                        filename        = 'workSODA/IMASK_' + Date(assDate).ymdh + '.txt',
                    )
                    print(t.prompt, 'tb243 =', tb243)
                    print()
                # ########## inflation FILE ON HENDRIX ########################################
                if os.path.exists('workSODA/ALPHA_' + Date(assDate).ymdh + '.txt'):
                    self.sh.title('Toolbox output tb244 (alpha archive)')
                    tb244 = toolbox.output(
                        model           = 'ALPHA',
                        namebuild       = 'flat@cen',
                        namespace      = 'vortex.multi.fr',
                        storage        = storage,
                        enforcesync    = enforcesync,
                        fatal           = False,
                        dateassim       = assDate,
                        block           = 'workSODA',
                        experiment = self.conf.xpid,
                        filename = 'workSODA/ALPHA_' + Date(assDate).ymdh + '.txt',
                    )
                    print(t.prompt, 'tb244 =', tb244)
                    print()
