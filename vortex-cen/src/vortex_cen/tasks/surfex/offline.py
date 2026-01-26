# -*- coding: utf-8 -*-
'''
Created on 7 nov. 2017

@author: lafaysse
'''

#from vortex.layout.nodes import Task
from vortex import toolbox
from vortex_cen.tasks.research_task_base import _CenResearchTask
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import Date, daterange, tomorrow

class Abstract_Offline_Task(_CenResearchTask):
    '''
    Abstract task for offline step.
    '''
    def get_remote_inputs(self):
        """
        Get ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        Get drdt_bst_fit_60.nc, PGD.nc, PREP.nc, FORCING.nc
        """
        # Binary ECOCLIMAP I files are mandatory to run PREP and taken from the uenv
        self.sh.title('Toolbox input ecoclimap1')
        ecoclimap1_tbi = toolbox.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapI_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.genv,
            source         = 'ecoclimap1',
            model          = 'surfex',
        ),
        print(self.ticket.prompt, 'ecoclimap1 =', ecoclimap1_tbi)
        print()

        # Binary ECOCLIMAP II files are mandatory to run PREP and taken from the uenv
        self.sh.title('Toolbox input ecoclimap2')
        ecoclimap2_tbi = toolbox.input(
            role           = 'Surfex cover parameters',
            kind           = 'coverparams',
            nativefmt      = 'bin',
            local          = 'ecoclimapII_eu_covers_param.bin',
            geometry       = self.conf.geometry,
            genv           = self.conf.genv,
            source         = 'ecoclimap2',
            model          = 'surfex',
        ),
        print(self.ticket.prompt, 'ecoclimap2 =', ecoclimap2_tbi)
        print()

        # Crocus metamorphism parameters mandatory to run PREP and taken from the uenv
        self.sh.title('Toolbox input drdt_bst_fit_60')
        drdt_bst_fit_tbi = toolbox.input(
            role            = 'Parameters for F06 metamorphism',
            kind            = 'ssa_params',
            genv            = self.conf.genv,
            nativefmt       = 'netcdf',
            local           = 'drdt_bst_fit_60.nc',
            model           = 'surfex',
        )
        print(self.ticket.prompt, 'drdt_bst_fit_60 =', drdt_bst_fit_tbi)
        print()

        # PGD.nc mandatory to run OFFLINE
        self.sh.title('Toolbox input PGD.nc')
        pgd_tbi = toolbox.input(
            local          = 'PGD.nc',
            role           = 'SurfexClim',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            nativefmt      = 'netcdf',
            kind           = 'pgdnc',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'pgd',
            member         = self.conf.member if hasattr(self.conf, 'member') else None,
        ),
        print(self.ticket.prompt, 'pgd =', pgd_tbi)
        print()

        # PREP.nc mandatory to run OFFLINE
        self.sh.title('Toolbox input PREP.nc')
        prep_tbi = toolbox.input(
            local          = 'PREP.nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'prep',
            member         = self.conf.member if hasattr(self.conf, 'member') else None,
        ),
        print(self.ticket.prompt, 'prep_tbi =', prep_tbi)
        print()

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')


    def get_local_inputs(self):
        """
        Get OPTIONS.nam which is always in cache
        """
        # Namelist mandatory to run OFFLINE and taken from the cache
        self.sh.title('Toolbox input Namelist after modification')
        namelist_tbi = toolbox.input(
            role         = 'Nam_surfex',
            kind         = 'namelist',
            model        = 'surfex',
            local        = 'OPTIONS.nam',
            experiment   = self.conf.xpid,
            namespace    = 'vortex.cache.fr',
            block        = 'namelist',
            nativefmt    = 'nam',
        ),
        print(self.ticket.prompt, 'namelist =', namelist_tbi)
        print()


    def algo(self):
        """
        Algo component to execute OFFLINE
        """
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        self.sh.title('Toolbox algo OFFLINE')
        offline_tba = toolbox.algo(
            engine         = 'parallel',
            binary         = 'OFFLINE',
            kind           = 'deterministic',
            datebegin      = self.conf.datebegin,
            dateend        = self.conf.dateend,
            dateinit       = Date(self.conf.datespinup),
            threshold      = self.conf.threshold,
            daily          = self.conf.dailyprep,
            drhookprof     = self.conf.drhook,
            reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, 'offline_tba =', offline_tba)
        print()

            
    def put_remote_outputs(self):
        """
        Save the CUMUL, DIAG, PREP and PRO files (yearly only)
        """
        #######################################################################
        #                               Backup                                #
        #######################################################################
        if 'backup' in self.steps:
            pass

        if 'late-backup' in self.steps:
            namespace = 'vortex.multi.fr'
            if hasattr(self.conf, 'save_pro') and self.conf.save_pro in ['cache', 'archive', 'multi']:
                namespace = 'vortex.' + self.conf.save_pro + '.fr'

        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)

        self.sh.title('Toolbox output CUMUL')
        cumul_tbo = toolbox.output(
            local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
            dateend        = dict_dates_end_pro if not self.conf.dailyprep else
                                list(daterange(tomorrow(base=datebegin), dateend)),
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'cumul',
            member         = self.conf.member if hasattr(self.conf, 'member') else None,
            fatal          = False,
        ),
        print(self.ticket.prompt, 'cumul_tbo =', cumul_tbo)
        print()

        self.sh.title('Toolbox output DIAG')
        diag_tbo = toolbox.output(
            local          = 'DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
            dateend        = dict_dates_end_pro if not self.conf.dailyprep else
                                list(daterange(tomorrow(base=datebegin), dateend)),
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'diag',
            member         = self.conf.member if hasattr(self.conf, 'member') else None,
            fatal          = False,
        ),
        print(self.ticket.prompt, 'diag_tbo =', diag_tbo)
        print()

        self.sh.title('Toolbox output PREP')
        prep_tbo = toolbox.output(
            local          = 'PREP_[date:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            date           = list_dates_end_pro if not self.conf.dailyprep else
                                list(daterange(tomorrow(base=datebegin), dateend)),
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'prep',
            member         = self.conf.member if hasattr(self.conf, 'member') else None,
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()

        self.sh.title('Toolbox output PRO')
        pro_tbo = toolbox.output(
            local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
            dateend        = dict_dates_end_pro if not self.conf.dailyprep else
                                list(daterange(tomorrow(base=datebegin), dateend)),
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = namespace,
            namebuild      = 'flat@cen',
            block          = 'pro',
            member         = self.conf.member if hasattr(self.conf, 'member') else None,
        ),
        print(self.ticket.prompt, 'pro_tbo =', pro_tbo)
        print()


class Offline_Task_Uenv_Offline(Abstract_Offline_Task):
    '''
    Get OFFLINE executable from Uenv
    '''
    def get_remote_inputs(self):
        """
        Get OFFLINE executable from Uenv
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input OFFLINE executable from uenv')
        OFFLINE_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_surfex_offline_mpi',
        )
        print(self.ticket.prompt, 'OFFLINE_tbx =', OFFLINE_tbx)
        print()


class Offline_Task_Local_Offline(Abstract_Offline_Task):
    '''
    Get OFFLINE executable locally
    '''
    def get_remote_inputs(self):
        """
        Get OFFLINE executable locally
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input OFFLINE executable from local')
        OFFLINE_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/OFFLINE"
        )
        print(self.ticket.prompt, 'OFFLINE_tbx =', OFFLINE_tbx)
        print()