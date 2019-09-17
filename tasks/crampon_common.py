#! /usr/bin/env python
# -*- coding: utf-8 -*-
'''
Created on 17 avr. 2019

@author: cluzetb
'''
from bronx.stdtypes.date import Date
import os
from utils.dates import check_and_convert_date, get_list_dates_files

from vortex import toolbox
from vortex.layout.nodes import Task


class _Crampon_Task(Task):
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

        # #################### FETCH CONSTANT FILES ##########################
        self.sh.title('Toolbox input tb02_s')  # this step should work if PGD properly in spinup on hendrix
        tb02_s = toolbox.input(
            alternate      = 'SurfexClim',
            kind           = 'pgdnc',
            nativefmt      = 'netcdf',
            local          = 'PGD.nc',
            # member         = '{0:04d}'.format(forcingdir),
            experiment     = 'spinup@' + os.environ['USER'],
            geometry       = self.conf.geometry,
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'pgd',
            fatal          = False,
        ),
        print(t.prompt, 'tb02_s =', tb02_s)
        print()
        self.sh.title('Toolbox input tb03b')
        tb03b = toolbox.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapI_covers_param.bin',
            geometry       = 'alp_allslopes',
            genv           = 'uenv:cen.01@CONST_CEN',
            source         = 'ecoclimap1',
            model          = 'surfex',
        ),
        print(t.prompt, 'tb03b =', tb03b)
        print()

        self.sh.title('Toolbox input tb03c')
        tb03c = toolbox.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapII_eu_covers_param.bin',
            geometry       = 'alp_allslopes',
            genv           = 'uenv:cen.01@CONST_CEN',
            source         = 'ecoclimap2',
            model          = 'surfex',
        ),
        print(t.prompt, 'tb03c =', tb03c)
        print()

        self.sh.title('Toolbox input tb04')
        tb04 = toolbox.input(
            role            = 'Parameters for F06 metamorphism',
            kind            = 'ssa_params',
            genv            = 'uenv:cen.01@CONST_CEN',
            nativefmt       = 'netcdf',
            local           = 'drdt_bst_fit_60.nc',
            model          = 'surfex',
        )
        print(t.prompt, 'tb04 =', tb04)
        print()

        self.sh.title('Toolbox input tb05')
        tb05 = toolbox.input(
            role            = 'Nam_surfex',
            remote          = self.conf.namelist,
            kind            = 'namelist',
            model           = 'surfex',
            local           = 'OPTIONS.nam',
        )
        print(t.prompt, 'tb05 =', tb05)
        print()
        # each task has its specific conf file on /scratch to avoid overwriting.
        takeConf = self.conf.workingdir + '/conf/' + self.conf.vapp + '_' + self.conf.vconf +\
            '_' + self.conf.confcomplement + '.ini'
        self.sh.title('Toolbox input tbconf')
        tbconf = toolbox.input(
            kind           = 'ini_file',
            local          = self.conf.vapp + '_' + self.conf.vconf + '.ini',
            vapp           = self.conf.vapp,
            vconf          = self.conf.vconf,
            remote         = takeConf,
            model          ='surfex',
            role           = 'Conf_file',
            intent = 'inout',
            fatal = True,
        )
        print(t.prompt, 'tbCONFIN =', tbconf)
        print()


class Crampon_In(_Crampon_Task):
    """
    Task used to fetch common consts btw OFFLINE dates (forcings) and SODA


    """

    def process(self):
        t = self.ticket
        if 'early-fetch' in self.steps:
            # ############## FETCH FORCINGS #######################################
            # we fetch forcing files into the members directories using the remainder function %
            # since we usually have more members than forcing files, we loop over forcing files
            date_begin_forc, date_end_forc, _, _ = \
                get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)  # each one of these items has only one item
            date_begin_forc = date_begin_forc[0]
            date_end_forc = date_end_forc[0]  # replace one-item list by item.
            forcExp = self.conf.forcing + '@' + os.environ['USER']
            meteo_members = {str(m): ((m - 1) % int(self.conf.nforcing)) + 1 for m in self.conf.members}

            if hasattr(self.conf, 'synth'):
                synth = str(int(self.conf.synth))
                meteo_members[synth] = self.conf.meteo_draw
            local_names = {str(m): 'mb{0:04d}'.format(m) + '/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc'
                           for m in self.conf.members}
            self.sh.title('Toolbox input tb01')
            tb01 = toolbox.input(
                role           = 'Forcing',
                realmember     = self.conf.members,
                local          = dict(realmember= local_names),
                vapp           = self.conf.meteo,
                experiment     = forcExp,
                member         = dict(realmember= meteo_members),
                geometry       = self.conf.geometry,
                datebegin      = date_begin_forc,
                dateend        = date_end_forc,
                nativefmt      = 'netcdf',
                kind           = 'MeteorologicalForcing',
                model          = 'safran',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',  # ???
                block          = 'meteo'
            ),
            print(t.prompt, 'tb01 =', tb01)
            print()
