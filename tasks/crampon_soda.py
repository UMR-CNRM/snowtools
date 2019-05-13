#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 27 mars 2019

@author: cluzetb
Vortex task performing up to 40 offline runs in parallel on a single node

'''
from vortex import toolbox
from bronx.stdtypes.date import Date
import os
from tasks.crampon_common import Crampon_Task


class Soda_Task(Crampon_Task):
    '''
    classdocs
    '''

    def process(self):

        # ##### PREPARE common stuff with the offline task ###########

        t, firstloop, lastloop, assDate, = self.prepare_common()
        # if 'early-fetch' in self.steps or 'fetch' in self.steps:
        if 'early-fetch' in self.steps:

            # ################# FETCH CONSTANT FILES #############
            # 17/04/19 -> keep a workSODA rep for backward compatibility
            # AND sake of simplicity, even though it is not needed anymore.

            self.get_common_consts(firstloop, self.conf.membersnode)

            # ################# FETCH EXECUTABLE ###############
            self.sh.title('Toolbox executable tb08_s= tbx4')
            tb08_s = tbx4 = toolbox.executable(
                role           = 'Binary',
                kind           = 'soda',
                local          = 'SODA',
                model          = 'surfex',
                remote          = self.conf.exesurfex + "/SODA"
            )

            print(t.prompt, 'tb08_s =', tb08_s)
            print()

            # ############### FETCH OBSERVATIONS  ##########
            self.sh.title('Toolbox input tobs')
            tobs = toolbox.input(
                geometry        = self.conf.geometry,
                nativefmt       = 'netcdf',
                datebegin       = assDate,
                dateend         = assDate,
                model           = 'obs',
                block           = self.conf.sensor,
                part            = self.conf.sensor,
                kind            = 'SnowObservations',
                namespace       = 'vortex.multi.fr',
                namebuild       = 'flat@cen',
                experiment      = 'obs@' + os.environ['USER'],
                local           = 'workSODA/OBSERVATIONS_[datebegin:ymdHh].nc',
                stage           = '1date',
                fatal           = False
            )
            print(t.prompt, 'tobs =', tobs)
            print()

        if 'fetch' in self.steps:
            # ################# FETCH PREP FILES ################
            # put it in a filetree(/mb0001 etc.) inside the soda task rep for backward comp.
            for mb in self.conf.membersnode:
                self.sh.title('Toolbox input tb03_SODA')
                tb03_soda = toolbox.input(
                    alternate      = 'SnowpackInit',
                    member         = mb,
                    local          = 'mb{0:04d}'.format(mb) + '/PREP_[date:ymdh].nc',
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
            if os.path.exists('workSODA/OBSERVATIONS_' + assDate.ymdHh + '.nc'):  # test of obs exists/successfully downloaded

                # soda
                self.sh.title('Toolbox algo tb11_s = SODA')

                tb11_s = tbalgo4s = toolbox.algo(
                    engine         = 'parallel',
                    binary         = 'SODA',
                    kind           = "s2m_soda",
                    dateassim      = assDate,
                    members        = self.conf.membersnode,  # no need for mbids in SODA !
                )
                print(t.prompt, 'tb11_s =', tb11_s)
                print()
                self.component_runner(tbalgo4s, tbx4, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))
            """
            else:  # in case of SODA, if obs file doesn't exist, we need to remove PREP.nc in every mbdir (this is done by soda)
                for mb in self.conf.membersnode:
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

            self.sh.title('Toolbox output tb20an')
            tb20 = toolbox.output(
                local          = localan,
                role           = 'SnowpackInit',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                date           = assDate,
                period         = assDate,
                member         = self.conf.membersnode,  # BC 21/03/19 probably replace by mbids
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
            