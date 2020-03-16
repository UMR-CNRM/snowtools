#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 27 mars 2019

@author: cluzetb
Vortex task performing up to 40 offline runs in parallel on a single node

'''

import os

from vortex import toolbox

from bronx.stdtypes.date import Date
from tasks.crampon_common import _Crampon_Task
from utils.dates import get_list_dates_files, check_and_convert_date


class Offline_Task(_Crampon_Task):
    '''
    classdocs
    '''

    def process(self):
        t = self.ticket

        # ##### PREPARE common stuff with the offline task ###########
        firstloop, lastloop, _, = self.prepare_common()

        # ####  set members configurations ##################
        nmembersnode = len(self.conf.membersnode)
        idsnode = list(map(int, self.conf.idsnode))

        # BC 31/07/19 this produces noise deteriorating the spread so remove it.
        # # in case of synthetic assimilation, shuffle it to prevent the truth to appear.
        # # within each node only, not btw them.
        # if self.conf.openloop == 'off' and self.conf.sensor == 'SYNTH':
        #     shuffle(idsnode)
        # ################## STEP.01 #################################################
        # separate early-fetch (constant files, get at the beginning of the simulation)
        # and fetch (get it as the simulation goes along)
        # common consts are handled with symbolic links in the common directory

        # ############################################################################
        if 'early-fetch' in self.steps:
            # ############ FETCH COMMON CONST #####################################
            # -> duplicated with soda task -< make a class method
            self.get_common_consts(firstloop, self.conf.membersnode)

            if firstloop:  # if firstloop, get it early on hendrix

                self.sh.title('Toolbox input tb03_s')
                tb03_s = toolbox.input(
                    alternate      = 'SnowpackInit',
                    local          = 'PREP.nc',
                    experiment     = 'spinup@' + os.environ['USER'],
                    geometry       = self.conf.geometry,
                    date           = self.conf.datespinup,
                    intent         = 'inout',
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'prep',
                    fatal          = True,
                ),
                print(t.prompt, 'tb03_s =', tb03_s)
                print()

            # ####### FETCH BINARY FILES ############################
            if hasattr(self.conf, "exesurfex"):
                self.sh.title('Toolbox executable tb06= tbx1')
                tb06 = tbx3 = toolbox.executable(
                    role           = 'Binary',
                    kind           = 'offline',
                    local          = 'OFFLINE',
                    model          = 'surfex',
                    remote          = self.conf.exesurfex + "/OFFLINE"
                )

                print(t.prompt, 'tb06 =', tb06)
                print()

            else:
                print('you fool ! you should prescribe a --exesurfex path to your s2m command !')

        if 'fetch' in self.steps:

            # ############# FETCH PREPS : either 1 spinup (firstloop=True, could be moved to early-fetch) or a list of PREPS ##########
            if not firstloop:  # else, got it in the early step on hendrix
                dmembersnode = {str(mb): mb for mb in self.conf.membersnode}
                dlocal_names = {str(mb): 'mb{0:04d}'.format(mb) + '/PREP.nc'
                                for mb in self.conf.membersnode}
                self.sh.title('Toolbox input tb03_lan')
                tb03_lan = toolbox.input(
                    alternate      = 'SnowpackInit',
                    realmember     = self.conf.membersnode,
                    member         = dict(realmember=dmembersnode),
                    local          = dict(realmember = dlocal_names),  # local prep name does not hold the date at the moment
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    date           = self.conf.stopdate_prev,
                    intent         = 'inout',
                    nativefmt      = 'netcdf',
                    kind           = 'PREP',
                    model          = 'surfex',
                    namespace      = 'vortex.cache.fr',  # get it on the cache from last loop
                    namebuild      = 'flat@cen',
                    block          = 'an',          # get it on cache @mb****/an
                    stage          = '_an',
                    fatal          = False,
                ),
                print(t.prompt, 'tb03_lan =', tb03_lan)
                print()
                # if the analysis didn't succeed/openloop (no PREP in analysis rep.), get the BG preps.
                if not tb03_lan[0]:

                    self.sh.title('Toolbox input tb03_lbg')
                    tb03_lbg = toolbox.input(
                        alternate      = 'SnowpackInit',
                        realmember     = self.conf.membersnode,
                        member         = dict(realmember=dmembersnode),
                        local          = dict(realmember = dlocal_names),  # local prep name does not hold the date at the moment
                        experiment     = self.conf.xpid,
                        geometry       = self.conf.geometry,
                        date           = self.conf.stopdate_prev,
                        intent         = 'inout',
                        nativefmt      = 'netcdf',
                        kind           = 'PREP',
                        model          = 'surfex',
                        namespace      = 'vortex.cache.fr',  # get it on the cache from last loop
                        namebuild      = 'flat@cen',
                        block          = 'bg',          # get it on cache @mb****/bg
                        stage          = '_bg',
                        fatal          = True,          # this must succeed.
                    ),
                    print(t.prompt, 'tb03_lbg =', tb03_lbg)
                    print()

        if 'compute' in self.steps:
            # force first forcing to the first forcing of first member 0001 doesn't work on several nodes...
            date_begin_forc, date_end_forc, _, _ = \
                get_list_dates_files(self.conf.datebegin, Date(check_and_convert_date(self.conf.stopdate)), self.conf.duration)  # each one of these items has only one item
            date_begin_forc = date_begin_forc[0]
            date_end_forc = date_end_forc[0]  # replace one-item list by item.
            print('yolo', os.getcwd())
            firstforcing = '../../../common/mb{0:04d}'.format(self.conf.membersnode[0]) + '/FORCING_' + date_begin_forc.strftime("%Y%m%d%H") +\
                "_" + date_end_forc.strftime("%Y%m%d%H") + ".nc"
            self.sh.title('Toolbox algo tb09a')

            tb09a = toolbox.algo(
                kind         = 'surfex_preprocess',
                datebegin    = self.conf.stopdate_prev,
                dateend      = self.conf.stopdate,
                forcingname  = firstforcing,
                # nmembers = self.conf.nmembers,  # BC 06/05/19 : WTF nmembersnode ?? -> delete this shit
            )
            print(t.prompt, 'tb09a =', tb09a)
            print()
            tb09a.run()

            # ########### ASSIMILATION LOOP ###############

            # deal with the case when we use a PREP spinup from a different year.
            if firstloop is True:
                dateinit = Date(self.conf.datespinup)
                threshold = self.conf.threshold
            else:
                dateinit = self.conf.stopdate_prev
                threshold = -999.  # no threshold otherwise.
            stopstep = 1  # useless ?
            self.sh.title('Toolbox algo tb11 = OFFLINE')
            tb11 = tbalgo4 = toolbox.algo(
                engine         = 's2m',
                binary         = 'OFFLINE',
                kind           = "crampon",
                verbose        = True,
                datebegin      = self.conf.stopdate_prev,
                dateend        = self.conf.stopdate,
                dateinit       = dateinit,
                threshold      = threshold,
                members        = idsnode,  # mbids (=ESCROC IDS in case of prescribed draw) = members node in case draw without prescription
                startmbnode    = self.conf.membersnode[0],  # necessary
                geometry       = [self.conf.geometry.area],
                subensemble    = self.conf.subensemble,
                # ntasks         = ntasksEsc,  # not consistent with the self.conf.membersnode
                ntasks         = nmembersnode,  # BC 18/04/19 more consistent
                nforcing       = self.conf.nforcing,
            )
            print(t.prompt, 'tb11 =', tb11)
            print()
            self.component_runner(tbalgo4, tbx3)
            stopstep += 1

        if 'backup' in self.steps:
            # ########### PUT PREP FILES FOR SODA ###########
            localbg = 'mb[member%04d]/PREP_[date:ymdh].nc'
            self.sh.title('Toolbox output tb21bg')
            tb21 = toolbox.output(
                local          = localbg,
                role           = 'SnowpackInit',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                date           = self.conf.stopdate,
                period         = self.conf.stopdate,  # BC 27/03 wtf
                member         = self.conf.membersnode,  # BC 21/03/19 probably replace by mbids
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.cache.fr',
                namebuild      = 'flat@cen',
                block          = 'bg',
                stage          = '_bg',
                fatal          = False
            ),
            print(t.prompt, 'tb21 =', tb21)
            print()

            if self.conf.pickleit == 'on':

                # bc displaced because now we pickle the files instead of transferring it.
                # ########### PUT PRO FILE ###########
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.output(
                    local          = 'mb[member%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = self.conf.stopdate_prev,
                    dateend        = self.conf.stopdate,
                    member         = self.conf.membersnode,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.cache.fr',
                    namebuild      = 'flat@cen',
                    block          = 'pro',
                ),
                print(t.prompt, 'tb19 =', tb19)
                print()

        if 'late-backup' in self.steps:
            # if fetchnig to sxcen, must be done file/file to prevent from having too many simultaneous transfers
            storage = ['hendrix.meteo.fr']
            enforcesync = dict(storage={'hendrix.meteo.fr': False, 'sxcen.cnrm.meteo.fr': True})
            if self.conf.writesx == 'on':
                storage.append('sxcen.cnrm.meteo.fr')

            if self.conf.pickleit == 'off':

                # bc displaced because now we pickle the files instead of transferring it.
                # ########### PUT PRO FILE ###########
                self.sh.title('Toolbox output tb19')
                tb19 = toolbox.output(
                    local          = 'mb[member%04d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    geometry       = self.conf.geometry,
                    datebegin      = self.conf.stopdate_prev,
                    dateend        = self.conf.stopdate,
                    member         = self.conf.membersnode,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    block          = 'pro',
                ),
                print(t.prompt, 'tb19 =', tb19)
                print()

            # fetch namelist &conf file only if lastloop
            if lastloop:
                self.sh.title('Toolbox output tb23')
                tb23 = toolbox.output(
                    role            = 'Nam_surfex',
                    namespace       = 'vortex.multi.fr',
                    storage        = storage,
                    enforcesync    = enforcesync,
                    namebuild       = 'flat@cen',
                    block           = 'conf',
                    experiment      = self.conf.xpid,
                    kind            = 'namelist',
                    model           = 'surfex',
                    local           = 'OPTIONS.nam',
                    fatal           = False,
                ),
                print(t.prompt, 'tb23 =', tb23)
                print()

                # conf file to hendrix
                self.sh.title('Toolbox output tbconf')
                tbconf = toolbox.output(
                    kind           = 'ini_file',
                    namespace      = 'vortex.multi.fr',
                    storage        = storage,
                    enforcesync    = enforcesync,
                    namebuild      = 'flat@cen',
                    block          = 'conf',
                    experiment     = self.conf.xpid,
                    local          = self.conf.vapp + '_' + self.conf.vconf + '.ini',
                    vapp           = self.conf.vapp,
                    vconf          = self.conf.vconf,
                    fatal          = False,
                ),
                print(t.prompt, 'tbconf =', tbconf)
                print()
