# -*- coding: utf-8 -*-
'''
Created on 7 nov. 2017

@author: lafaysse
'''
#from vortex.layout.nodes import Task
from vortex import toolbox
from vortex_cen.tasks.research_task_base import _CenResearchTask


class Abstract_Preprocess_Task( _CenResearchTask):
    '''
    Abstract task for pre-processing namelist.
    [Add infos like points and dates from forcing to namelist]
    '''
    def get_remote_inputs(self):
        """
        Get forcings file and namelist in order to transform the namelist
        """

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def algo(self):
        """
        Change the namelist when forcings and namelist are here
        """
        t=self.ticket
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        avail_forcings = t.context.sequence.effective_inputs(role='Forcing')
        if len(avail_forcings) > 0:
            firstforcing = avail_forcings[0]
        else:
            firstforcing = []
            print('Exception à gérer')

        # Algo component to preprocess the namelist (adjust dates, etc.)
        self.sh.title('Toolbox algo preprocess')
        preprocess_tba = toolbox.algo(
            kind         = 'surfex_preprocess',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            forcingname  = firstforcing.rh.resource.basename,
        )
        print(self.ticket.prompt, 'Toolbox algo preprocess =', preprocess_tba)
        print()
        return preprocess_tba
            
    def put_remote_outputs(self):
        """
        Save the changed namelist
        """
        #######################################################################
        #                               Backup                                #
        #######################################################################
        self.sh.title('Toolbox Namelist Output (Preprocess)')
        namelist_tbo = toolbox.output(
            role         = 'Nam_surfex',
            kind         = 'namelist',
            model        = 'surfex',
            local        = 'OPTIONS.nam',
            experiment   = self.conf.xpid,
            namespace    = 'vortex.cache.fr',
            block        = 'namelist',
            nativefmt    = 'nam',
        ),
        print(self.ticket.prompt, 'namelist_tbo =', namelist_tbo)
        print()


class Preprocess_Task_Uenv_Namelist(Abstract_Preprocess_Task):
    '''
    Task for pre-processing the namelist from uenv.
    [Add infos like points and dates from forcing to namelist]
    '''
    def get_remote_inputs(self):
        """
        Get forcings file and namelist in order to transform the namelist
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        if not hasattr(self.conf, "genv"):
            self.conf.genv = 'uenv:cen.14@CONST_CEN'

        if hasattr(self.conf, "uenv") and 'OPTIONS.nam' in self.conf.udata.items():
            # The OPTION.nam namelist will be retrived by the 'tbuenv' toolbox
            pass
        else:
            # If not provided, standard namelist taken from the uenv
            namelist_tbi = toolbox.input(
                role     = 'Nam_surfex',
                source   = 'OPTIONS_default.nam',
                genv     = self.conf.genv,
                kind     = 'namelist',
                model    = 'surfex',
                local    = 'OPTIONS.nam',
            )
        print(self.ticket.prompt, 'namelist_tbi =', namelist_tbi)
        print()


class Preprocess_Task_Local_Namelist(Abstract_Preprocess_Task):
    '''
    Task for pre-processing namelist.
    [Add infos like points and dates from forcing to namelist]
    '''
    def get_remote_inputs(self):
        """
        Get forcings file and namelist in order to transform the namelist
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        # Use the path provided in the configuration file for the SURFEX namelist
        self.sh.title('Toolbox input tb05')
        namelist_tbi = toolbox.input(
            role     = 'Nam_surfex',
            remote   = self.conf.namelist,
            kind     = 'namelist',
            model    = 'surfex',
            local    = 'OPTIONS.nam',
        )
        print(self.ticket.prompt, 'namelist_tbi =', namelist_tbi)
        print()