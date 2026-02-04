# -*- coding: utf-8 -*-
'''
'''
from vortex import toolbox
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex.util.helpers import InputCheckerError


class _Preprocess(_CenResearchTask):
    '''
    Abstract task for pre-processing namelist:
    add infos like points and dates from forcing to namelist.

    Inputs:
    -------
    - SURFEX namelist (OPTIONS.nam)
    - FORCING file

    Outputs:
    --------
    - Modified and ready-to-use SURFEX namelist

    Mandatory configuration variables:
    ----------------------------------
    :param datebegin: *datebegin* of the forcing file(s)
    :type datebegin: str, footprints.stdtypes.FPList
    :param dateend: *dateend* of the forcing files(s)
    :type dateend: str, footprints.stdtypes.FPList
    :param geometry: *geometry* of the forcing file(s)
    :type geometry: str, footprints.stdtypes.FPList
    :param xpid: Experiment identifier (format "experiment_name@user")
    :type xpid: str
    '''
    def get_remote_inputs(self):
        """
        Get forcing file(s) and namelist in order to transform the namelist
        """

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def algo(self):
        """
        Change the namelist when forcings and namelist are here
        """
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        avail_forcings = self.ticket.context.sequence.effective_inputs(role='Forcing')
        if len(avail_forcings) > 0:
            firstforcing = avail_forcings[0]
        else:
            raise InputCheckerError('No FORCING file present, the task can not run properly')

        # Algo component to preprocess the namelist (adjust dates, etc.)
        self.sh.title('Toolbox algo preprocess')
        preprocess_tba = toolbox.algo(
            kind         = 'surfex_preprocess',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            # Le nom local de la ressource est fourni par le "container"
            forcingname  = firstforcing.rh.container.basename,
        )
        print(self.ticket.prompt, 'Toolbox algo preprocess =', preprocess_tba)
        print()
        return preprocess_tba

    def put_local_outputs(self):
        """
        Save the changed namelist in cache -> put_LOCAL_output
        """
        #######################################################################
        #                               Backup                                #
        #######################################################################
        self.sh.title('Toolbox output Namelist after modification (local cache only)')
        namelist_tbo = toolbox.output(
            role         = 'Nam_surfex',
            kind         = 'namelist',
            model        = 'surfex',
            local        = 'OPTIONS.nam',
            experiment   = self.conf.xpid,
            namespace    = 'vortex.cache.fr',  # Never put a namelist on Hendrix
            block        = 'namelist',
            nativefmt    = 'nam',
        ),
        print(self.ticket.prompt, 'namelist_tbo =', namelist_tbo)
        print()


class Preprocess_Uenv_Namelist(_Preprocess):
    '''
    Task for pre-processing a namelist coming from a User Environment.

    NB : This is the task to use to guarantee the simulation's reproductibility

    Supplementary mandatory configuration variables:
    ------------------------------------------------
    :param uenv: User Environment in which the namelist is to be retrieved.
                 Format : uenv:{uenv_name}@{user}
    :type uenv: str
    :param source: The name of the specific namelist to retrieve from the namelist
                   ".tar" archive containing all available namelists.
    :type source: str

    '''
    def get_remote_inputs(self):
        """
        Get namelist from a User Environment.
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input Namelist')
        namelist_tbi = toolbox.input(
            role     = 'Nam_surfex',
            # Dans un UEnv, plusieurs namelistes peuvent être stockées dans une archive ".tar",
            # le footprint *source* permet de définir le nom exact de la nameliste à récupérer.
            source   = self.conf.source,  # ex : OPTIONS_default.nam
            genv     = self.conf.uenv,
            kind     = 'namelist',
            model    = 'surfex',
            local    = 'OPTIONS.nam',
            # MV : la nameliste va être modifiée, il faut s'assurer du droit d'écriture (<==> intent='inout')
            intent   = 'inout',
        )
        print(self.ticket.prompt, 'namelist_tbi =', namelist_tbi)
        print()


class Preprocess_Local_Namelist(_Preprocess):
    '''
    Task for pre-processing a namelist coming from any user-defined absolute path.

    WARNING : The simulation's reproductibility can not be guaranteed with this task !

    Supplementary mandatory configuration variables:
    ------------------------------------------------
    :param namelist: Absolute path pointing to the namelist to be used.
    :type namelist: str
    '''
    def get_remote_inputs(self):
        """
        Get namelist from a user-provided absolute path.
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        # Use the path provided in the configuration file for the SURFEX namelist
        self.sh.title('Toolbox input Namelist')
        namelist_tbi = toolbox.input(
            role     = 'Nam_surfex',
            remote   = self.conf.namelist,
            kind     = 'namelist',
            model    = 'surfex',
            local    = 'OPTIONS.nam',
            # MV : la nameliste va être modifiée, il faut s'assurer du droit d'écriture (<==> intent='inout')
            intent   = 'inout',
        )
        print(self.ticket.prompt, 'namelist_tbi =', namelist_tbi)
        print()
