# -*- coding: utf-8 -*-
'''
Created on 7 nov. 2017

@author: lafaysse
'''

#from vortex.layout.nodes import Task
from vortex import toolbox
from vortex_cen.tasks.research_task_base import _CenResearchTask

class Abstract_Prep_Construct_Task(_CenResearchTask):
    '''
    Abstract task for prep step.
    '''
    def get_remote_inputs(self):
        """
        Get ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        Get drdt_bst_fit_60.nc, PGD.nc
        Do not get init_TG.nc because there are 2 possibilities
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

        # PGD.nc mandatory to run PREP and taken from the cache ?
        self.sh.title('Toolbox input PGD.nc')
        pgd_tbi = toolbox.input(
            role           = 'SurfexClim',
            kind           = 'pgdnc',
            nativefmt      = 'netcdf',
            local          = 'PGD.nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'pgd',
            member         = self.conf.member if hasattr(self.conf, 'member') else None,
        ),
        print(self.ticket.prompt, 'pgd =', pgd_tbi)
        print()


def get_local_inputs(self):
        """
        Get OPTIONS.nam which is always in cache
        """
        # Namelist mandatory to run PREP and taken from the cache
        self.sh.title('Toolbox input Namelist')
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
        Algo component to produce the PREP file if not found in the inputs
        """
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        self.sh.title('Toolbox algo PREP')
        PREP_tba = toolbox.algo(
            engine     = 'parallel',
        )
        print(self.ticket.prompt, 'Toolbox algo prep=', PREP_tba)
        print()
        #self.component_runner(tbalgo3, tbx2, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))
        # ntasks = 1 !!!! WTF !!!
        return PREP_tba

            
    def put_remote_outputs(self):
        """
        Save the PREP file
        """
        #######################################################################
        #                               Backup                                #
        #######################################################################
        self.sh.title('Toolbox Output PREP')
        prep_tbo = toolbox.output(
            local      = 'PREP_[date:ymdh].nc',
            role       = 'SnowpackInit',
            experiment = self.conf.xpid,
            geometry   = self.conf.geometry,
            date       = self.conf.date,
            nativefmt  = 'netcdf',
            kind       = 'PREP',
            model      = 'surfex',
            namespace  = 'vortex.multi.fr',
            namebuild  = 'flat@cen',
            block      = 'prep',
            member     = self.conf.member if hasattr(self.conf, 'member') else None,
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()


class Prep_Task_Uenv_TG_Uenv_Prep(Abstract_Prep_Construct_Task):
    '''
    Get init_TG.nc and PREP executable both from Uenv
    '''
    def get_remote_inputs(self):
        """
        Get init_TG.nc and PREP executable both from Uenv
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input init_TG from uenv')
        initTG_tbi = toolbox.input(
            role           = 'initial values of ground temperature',
            kind           = 'climTG',
            nativefmt      = 'netcdf',
            local          = 'init_TG.nc',
            geometry       = self.conf.geometry,
            genv           = self.conf.genv,
            gvar           = 'climtg_[geometry::tag]',
            model          = 'surfex',
        ),
        print(self.ticket.prompt, 'initTG_tbi =', initTG_tbi)
        print()

        self.sh.title('Toolbox input PREP executable from uenv')
        PREP_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_prep_mpi',
        )
        print(self.ticket.prompt, 'PREP_tbx =', PREP_tbx)
        print()


class Prep_Task_Local_TG_Uenv_Prep(Abstract_Prep_Construct_Task):
    '''
     Get init_TG.nc locally and PREP executable from Uenv
    '''
    def get_remote_inputs(self):
        """
        Get init_TG.nc locally and PREP executable from Uenv
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input init_TG from local')
        initTG_tbi = toolbox.input(
            alternate      = 'initial values of ground temperature',
            kind           = 'climTG',
            nativefmt      = 'netcdf',
            local          = 'init_TG.nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'prep',
        ),
        print(self.ticket.prompt, 'initTG_tbi =', initTG_tbi)
        print()

        self.sh.title('Toolbox input PREP executable from uenv')
        PREP_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_prep_mpi',
        )
        print(self.ticket.prompt, 'PREP_tbx =', PREP_tbx)
        print()


class Prep_Task_Uenv_TG_Local_Prep(Abstract_Prep_Construct_Task):
    '''
    Get init_TG.nc from Uenv and PREP executable locally
    '''
    def get_remote_inputs(self):
        """
        Get init_TG.nc from Uenv and PREP executable locally
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input init_TG from local')
        initTG_tbi = toolbox.input(
            alternate      = 'initial values of ground temperature',
            kind           = 'climTG',
            nativefmt      = 'netcdf',
            local          = 'init_TG.nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'prep',
        ),
        print(self.ticket.prompt, 'initTG_tbi =', initTG_tbi)
        print()

        self.sh.title('Toolbox input PREP executable from local')
        PREP_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PREP"
        )
        print(self.ticket.prompt, 'PREP_tbx =', PREP_tbx)
        print()


class Prep_Task_Local_TG_Local_Prep(Abstract_Prep_Construct_Task):
    '''
    Get init_TG.nc and PREP executable both locally
    '''
    def get_remote_inputs(self):
        """
        Get init_TG.nc and PREP executable both locally
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input init_TG from local')
        initTG_tbi = toolbox.input(
            alternate      = 'initial values of ground temperature',
            kind           = 'climTG',
            nativefmt      = 'netcdf',
            local          = 'init_TG.nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'prep',
        ),
        print(self.ticket.prompt, 'initTG_tbi =', initTG_tbi)
        print()

        self.sh.title('Toolbox input PREP executable from local')
        PREP_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PREP"
        )
        print(self.ticket.prompt, 'PREP_tbx =', PREP_tbx)
        print()