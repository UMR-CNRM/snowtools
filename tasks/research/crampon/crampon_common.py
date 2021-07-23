# -*- coding: utf-8 -*-
'''
Created on 17 avr. 2019

@author: cluzetb
'''
from vortex.layout.nodes import Task
from vortex import toolbox
from bronx.stdtypes.date import Date
from utils.dates import check_and_convert_date
import os


class Crampon_Task(Task):
    def prepare_common(self):
        t = self.ticket

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

        return t, firstloop, lastloop, assDate

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

        takeConf = '/scratch/work/'  + os.environ['USER'] + '/' + self.conf.vapp + '/' + self.conf.vconf + '/conf/' + self.conf.vapp + '_' + self.conf.vconf + '.ini'
        self.sh.title('Toolbox input tbconf')
        tbconf = toolbox.input(
            kind           = 'ini_file',
            # namespace      = 'vortex.sxcen.fr',
            # namebuild      = 'flat@cen',
            # block          = 'conf',
            # storage        = 'sxcen.cnrm.meteo.fr',
            # rootpath       = self.conf.writesx,
            # experiment     = self.conf.xpid,
            local          = self.conf.vapp + '_' + self.conf.vconf + '.ini',
            vapp           = self.conf.vapp,
            vconf          = self.conf.vconf,
            remote         = takeConf,
            model          ='surfex',
            role           = 'Conf_file',
            intent = 'inout',
            # fatal          = False,
        )
        print(t.prompt, 'tbCONFIN =', tbconf)
        print()


