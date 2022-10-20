#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""
Created on 17 avr. 2019

@author: cluzetb
"""
from bronx.stdtypes.date import Date
from snowtools.utils.dates import check_and_convert_date, get_list_dates_files
from vortex import toolbox
from vortex.layout.nodes import Task
from cen.layout.nodes import S2MTaskMixIn


class _CrocO_Task(Task, S2MTaskMixIn):
    """
    Task used to fetch common consts btw OFFLINE dates (forcings) and SODA
    """

    def prepare_common(self):
        # set dates.
        # /!\ No handling of sentinel2 dates for the moment (should be put up to the familyloop)

        firstloop = False
        lastloop = False
        if self.conf.stopdate_prev is None:
            firstloop = True
            self.conf.stopdate_prev = self.conf.datebegin
            assDate = Date(check_and_convert_date(self.conf.stopdate))
        elif self.conf.stopdate_next is None:
            lastloop = True
            assDate = None  # if lastloop, assDate = None
        else:
            assDate = Date(check_and_convert_date(self.conf.stopdate))

        return firstloop, lastloop, assDate

    def get_common_consts(self, firstloop, members):
        t = self.ticket

        if not hasattr(self.conf, "genv"):
            self.conf.genv = 'uenv:cen.06@CONST_CEN'

        # #################### FETCH CONSTANT FILES ##########################
        self.sh.title('Toolbox input tb02_s (PGD)')  # this step should work if PGD properly in spinup on hendrix
        tb02_s = toolbox.input(
            alternate      = 'SurfexClim',
            kind           = 'pgdnc',
            nativefmt      = 'netcdf',
            local          = 'PGD.nc',
            # member         = '{0:04d}'.format(forcingdir),
            experiment     = self.conf.spinup_xpid,
            geometry       = self.conf.geometry,
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'pgd',
            fatal          = True,
        ),
        print(t.prompt, 'tb02_s =', tb02_s)
        print()

        self.sh.title('Toolbox input tb03b (ecoclimapI)')
        tb03b = toolbox.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapI_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.genv,
            source         = 'ecoclimap1',
            model          = 'surfex',
        ),
        print(t.prompt, 'tb03b =', tb03b)
        print()

        self.sh.title('Toolbox input tb03c (ecoclimapII)')
        tb03c = toolbox.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapII_eu_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.genv,
            source         = 'ecoclimap2',
            model          = 'surfex',
        ),
        print(t.prompt, 'tb03c =', tb03c)
        print()

        self.sh.title('Toolbox input tb04 (F06 .nc)')
        tb04 = toolbox.input(
            role            = 'Parameters for F06 metamorphism',
            kind            = 'ssa_params',
            genv            = self.conf.genv,
            nativefmt       = 'netcdf',
            local           = 'drdt_bst_fit_60.nc',
            model          = 'surfex',
        )
        print(t.prompt, 'tb04 =', tb04)
        print()

    def get_common_fetch(self):
        t = self.ticket

        self.sh.title('Toolbox input tb04 (soda namelist)')
        tb04 = toolbox.input(
            role='Nam_surfex',
            kind='namelist',
            model='surfex',
            local='OPTIONS.nam',
            experiment=self.conf.xpid,
            namespace='vortex.cache.fr',
            block='namelist',
            intent='inout',
            nativefmt='nam',
        )
        print(t.prompt, 'tb04 =', tb04)
        print()

    def get_forcings(self):
        t = self.ticket
        # ############## FETCH FORCINGS #######################################
        # we fetch forcing files into the members directories using the remainder function %
        # since we usually have more members than forcing files, we loop over forcing files
        date_begin_forc, date_end_forc, _, _ = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        date_begin_forc = date_begin_forc[0]
        date_end_forc = date_end_forc[0]  # replace one-item list by item.
        forcExp = self.conf.forcingid
        meteo_members = {str(m): ((m - 1) % int(self.conf.nforcing)) + 1 for m in self.conf.members}

        if hasattr(self.conf, 'synth'):
            if self.conf.synth is not None:
                # case when the synth member comes from a larger openloop (ex 160) than current experiment (ex 40)
                print(self.conf.synth)
                print(self.conf.nmembers)
                if int(self.conf.synth) <= int(self.conf.nmembers):
                    synth = str(int(self.conf.synth))
                    meteo_members[synth] = self.conf.meteo_draw
        local_names = {str(m): 'mb{0:04d}'.format(m) + '/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc'
                       for m in self.conf.members}
        self.sh.title('Toolbox input tb01 (forcings)')
        tb01 = toolbox.input(
            role='Forcing',
            realmember=self.conf.members,
            local=dict(realmember=local_names),
            vapp=self.conf.meteo,
            experiment=forcExp,
            member=dict(realmember=meteo_members),
            geometry=self.conf.geometry,
            datebegin=date_begin_forc,
            dateend=date_end_forc,
            nativefmt='netcdf',
            kind='MeteorologicalForcing',
            model='safran',
            namespace='vortex.multi.fr',
            namebuild='flat@cen',  # ???
            block='meteo'
        ),
        print(t.prompt, 'tb01 =', tb01)
        print()


class CrocO_In(_CrocO_Task):
    """
    Task used to fetch common consts btw OFFLINE dates (forcings) and SODA
    """

    def process(self):
        t = self.ticket

        if 'early-fetch' in self.steps or 'fetch' in self.steps:
            self.get_forcings()
            self.sh.title('Toolbox input tb02 (namelist)')
            tb02 = toolbox.input(
                role            = 'Nam_surfex',
                remote          = self.conf.namelist,
                kind            = 'namelist',
                model           = 'surfex',
                local           = 'OPTIONS.nam',
                nativefmt='nam',
            )
            print(t.prompt, 'tb02 =', tb02)
            print()

        if 'compute' in self.steps:

            self.sh.title('Toolbox algo tb03 (soda preprocess)')

            tb03 = toolbox.algo(
                kind         = 'soda_preprocess',
                members      = self.conf.members,
            )
            print(t.prompt, 'tb03 =', tb03)
            print()
            tb03.run()

        if 'backup' in self.steps:

            self.sh.title('Toolbox output tb04 (namelist)')
            tb04 = toolbox.output(
                role            = 'Nam_surfex',
                kind            = 'namelist',
                model           = 'surfex',
                local           = 'OPTIONS.nam',
                experiment            = self.conf.xpid,
                namespace       = 'vortex.cache.fr',
                block           = 'namelist',
                nativefmt       = 'nam',
            )
            print(t.prompt, 'tb04 =', tb04)
            print()


class CrocO_Out(Task, S2MTaskMixIn):
    """Final outputs after several offline-soda sequences"""

    def process(self):
        t = self.ticket

        storage = ['hendrix.meteo.fr']

        if 'early-fetch' in self.steps or 'fetch' in self.steps:
            # get conf file for archive purpose
            self.sh.title('Toolbox input tbconfin (conf archive)')
            tbconfin = toolbox.input(
                kind='config',
                nativefmt='ini',
                scope='history',
                source='mkjob',
                remote=self.conf.iniconf,
                local='conf_for_archive.ini',
                role='archive',
                fatal=True,
            ),
            print(t.prompt, 'tbconfin =', tbconfin)
            print()

        if 'backup' in self.steps or 'late-backup' in self.steps:
            # archive conf file
            self.sh.title('Toolbox output tbconfout (conf archive)')
            tbconfout = toolbox.output(
                kind='config',
                nativefmt='ini',
                scope='history',
                source='mkjob',
                namespace='vortex.multi.fr',
                storage=storage,
                namebuild='flat@cen',
                block='conf',
                experiment=self.conf.xpid,
                local='conf_for_archive.ini',
                fatal=True,
            ),
            print(t.prompt, 'tbconfout =', tbconfout)
            print()

# Matthieu Lafaysse: comment this because pickle is not the appropriate solution for post-processing
# class CrocO_Out(Task):
#     '''
#     BC 21/01/20
#     (Dirty) Post-Processing Task to concatenate/compress Pro files and PREP into 2 Pickle file.
#     This solution drastically reduces the number of transfered files and the volume of transfers.
#     '''
#
#     def process(self):
#         t = self.ticket
#         if 'early-fetch' in self.steps:
#             self.sh.title('Toolbox input tb02_s (pickle PGD)')
#             tb02_s = toolbox.input(
#                 alternate      = 'SurfexClim',
#                 kind           = 'pgdnc',
#                 nativefmt      = 'netcdf',
#                 local          = 'crocO/pickle/PGD.nc',
#                 # member         = '{0:04d}'.format(forcingdir),
#                 experiment     = 'spinup@' + t.env.getvar('USER'),
#                 geometry       = self.conf.geometry,
#                 model          = 'surfex',
#                 namespace      = 'vortex.cache.fr',
#                 namebuild      = 'flat@cen',
#                 block          = 'pgd',
#                 fatal          = False,
#             )
#             print(t.prompt, 'tb02_s =', tb02_s)
#             print()
#             # each task has its specific conf file on /scratch to avoid overwriting.
#             takeConf = self.conf.workingdir + '/conf/' + self.conf.vapp + '_' + self.conf.vconf +\
#                 '_' + self.conf.confcomplement + '.ini'
#             self.sh.title('Toolbox input tbconf (pickle conf)')
#             tbconf = toolbox.input(
#                 kind           = 'ini_file',
#                 local          = self.conf.vapp + '_' + self.conf.vconf + '.ini',
#                 vapp           = self.conf.vapp,
#                 vconf          = self.conf.vconf,
#                 remote         = takeConf,
#                 model          ='surfex',
#                 role           = 'Conf_file',
#                 intent = 'inout',
#                 fatal = True,
#             )
#             print(t.prompt, 'tbCONFIN =', tbconf)
#             print()
#             self.sh.title('Toolbox input tb05 (pickle namelist)')
#             tb05 = toolbox.input(
#                 role            = 'Nam_surfex',
#                 remote          = self.conf.namelist,
#                 kind            = 'namelist',
#                 model           = 'surfex',
#                 local           = 'conf/OPTIONS.nam',
#             )
#             print(t.prompt, 'tb05 =', tb05)
#             print()
#         if 'fetch' in self.steps:
#             # dirty loops to start with
#
#             self.sh.title('Toolbox input PP1 (pickle pro)')
#
#             self.conf.begprodates = [self.conf.datedeb] + self.conf.stopdates
#             self.conf.endprodates = self.conf.stopdates
#             for datebegin, dateend in zip(self.conf.begprodates, self.conf.endprodates):
#                 for mb in self.conf.members:
#
#                     tbinpp1 = toolbox.input(
#                         local          = 'mb[member%04d]/pro/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
#                         experiment     = self.conf.xpid,
#                         geometry       = self.conf.geometry,
#                         datebegin      = datebegin,
#                         date           = datebegin,
#                         dateend        = dateend,
#                         member         = mb,
#                         nativefmt      = 'netcdf',
#                         kind           = 'SnowpackSimulation',
#                         model          = 'surfex',
#                         namespace      = 'vortex.multi.fr',
#                         intent         = 'inout',
#                         namebuild      = 'flat@cen',
#                         block          = 'pro',
#                         Fatal = True,
#                     ),
#                     print(t.prompt, 'tbinpp1 = ', tbinpp1)
#                     print()
#                 dmembers = {str(mb): mb for mb in self.conf.members}
#                 dlocal_names_bg = {str(mb): 'mb{0:04d}'.format(mb) + '/bg/PREP_[date:ymdh].nc'
#                                    for mb in self.conf.members}
#
#                 self.sh.title('Toolbox input PP2_ol (pickle background)')
#                 tbinpp2 = toolbox.input(
#                     alternate      = 'SnowpackInit',
#                     realmember     = self.conf.members,
#                     member         = dict(realmember= dmembers),
#                     local          = dict(realmember= dlocal_names_bg),
#                     experiment     = self.conf.xpid,
#                     geometry       = self.conf.geometry,
#                     date           = dateend,
#                     intent         = 'inout',
#                     nativefmt      = 'netcdf',
#                     kind           = 'PREP',
#                     model          = 'surfex',
#                     namespace      = 'vortex.cache.fr',  # get it on the cache from last loop
#                     namebuild      = 'flat@cen',
#                     block          = 'bg',          # get it on cache @mb****/bg
#                     stage          = '_bg',
#                     fatal          = True,
#                 ),
#                 print(t.prompt, 'tbinpp2 = ', tbinpp2)
#                 print()
#                 dlocal_names_an = {str(mb): 'mb{0:04d}'.format(mb) + '/an/PREP_[date:ymdh].nc'
#                                    for mb in self.conf.members}
#                 if not self.conf.openloop:
#                     if dateend != self.conf.stopdates[-1]:
#                         self.sh.title('Toolbox input PP2_an (pickle analysis)')
#                         tbinpp2 = toolbox.input(
#                             alternate      = 'SnowpackInit',
#                             realmember     = self.conf.members,
#                             member         = dict(realmember= dmembers),
#                             local          = dict(realmember= dlocal_names_an),
#                             experiment     = self.conf.xpid,
#                             geometry       = self.conf.geometry,
#                             date           = dateend,
#                             intent         = 'inout',
#                             nativefmt      = 'netcdf',
#                             kind           = 'PREP',
#                             model          = 'surfex',
#                             namespace      = 'vortex.cache.fr',  # get it on the cache from last loop
#                             namebuild      = 'flat@cen',
#                             block          = 'an',          # get it on cache @mb****/bg
#                             stage          = '_an',
#                             fatal          = True,
#                         ),
#                         print(t.prompt, 'tbinpp2 = ', tbinpp2)
#                         print()
#         if 'compute' in self.steps:
#             self.sh.title('Toolbox Algo pp (pickle algo)')
#             tbalgopp1 = toolbox.algo(
#                 kind = 'picklepro',
#                 engine = 's2m',
#                 vapp           = self.conf.vapp,
#                 vconf          = self.conf.vconf,
#                 dates          = self.conf.stopdates,
#             )
#             print(t.prompt, 'tbalgopp1 = ', tbalgopp1)
#             print()
#             tbalgopp1.run()
#         if 'late-backup' in self.steps:
#             # if fetchnig to sxcen, must be done file/file to prevent from having too many simultaneous transfers
#             storage = ['hendrix.meteo.fr']
#             enforcesync = dict(storage={'hendrix.meteo.fr': False, 'sxcen.cnrm.meteo.fr': True})
#             if self.conf.writesx == 'on':
#                 storage.append('sxcen.cnrm.meteo.fr')
#             # FETCHING PRO pickle FILES
#             self.sh.title('Toolbox OUTPUT TB30 (pickle ensPro archive)')
#             tb_outpppro = toolbox.output(
#                 model          = 'ensProOl' if self.conf.openloop else 'ensProAn',
#                 namebuild      = 'flat@cen',
#                 namespace      = 'vortex.archive.fr',
#                 storage        = storage,
#                 enforcesync    = enforcesync,
#                 storetrack     = False,
#                 fatal          = True,
#                 block          = 'crocO/beaufix',
#                 experiment     = self.conf.xpid,
#                 filename       = 'crocO/pickle/ensPro' + ('Ol' if self.conf.openloop else 'An') + '.pkl',
#             )
#             print(t.prompt, 'tboutpro =', tb_outpppro)
#             print()
#
#             # # FETCHING PREP pickle FILES
#             for assDate in self.conf.stopdates:
#                 if self.conf.openloop:
#                     self.sh.title('Toolbox OUTPUT TB31 (pickle ensPrepOl archive)')
#                     tb_outppol = toolbox.output(
#                         model          = 'ol',
#                         namebuild      = 'flat@cen',
#                         namespace      = 'vortex.archive.fr',
#                         storage        = storage,
#                         enforcesync    = enforcesync,
#                         storetrack     = False,
#                         fatal          = True,
#                         dateassim      = assDate,
#                         block          = 'crocO/beaufix',
#                         experiment     = self.conf.xpid,
#                         filename       = 'crocO/pickle/ol_{0}.pkl'.format(assDate),
#                     )
#                     print(t.prompt, 'tboutol =', tb_outppol)
#                     print()
#                 else:
#                     self.sh.title('Toolbox OUTPUT TB32 (pickle ensPrepBg archive)')
#                     tb_outppbg = toolbox.output(
#                         model          = 'bg',
#                         namebuild      = 'flat@cen',
#                         namespace      = 'vortex.archive.fr',
#                         storage        = storage,
#                         enforcesync    = enforcesync,
#                         storetrack     = False,
#                         fatal          = True,
#                         dateassim      = assDate,
#                         block          = 'crocO/beaufix',
#                         experiment     = self.conf.xpid,
#                         filename       = 'crocO/pickle/bg_{0}.pkl'.format(assDate),
#                     )
#                     print(t.prompt, 'tboutbg =', tb_outppbg)
#                     print()
#                     # no final analysis
#                     if assDate != self.conf.stopdates[-1]:
#                         self.sh.title('Toolbox OUTPUT TB33 (pickle ensPrepAn archive)')
#                         tb_outppan = toolbox.output(
#                             model          = 'an',
#                             namebuild      = 'flat@cen',
#                             namespace      = 'vortex.archive.fr',
#                             storage        = storage,
#                             enforcesync    = enforcesync,
#                             storetrack     = False,
#                             fatal          = True,
#                             dateassim      = assDate,
#                             block          = 'crocO/beaufix',
#                             experiment     = self.conf.xpid,
#                             filename       = 'crocO/pickle/an_{0}.pkl'.format(assDate),
#                         )
#                         print(t.prompt, 'tboutan =', tb_outppan)
#                         print()
