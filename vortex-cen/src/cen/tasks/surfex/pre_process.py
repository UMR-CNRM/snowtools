# -*- coding: utf-8 -*-
"""
"""
import vortex
# from vortex import toolbox
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex.util.helpers import InputCheckerError


class _Preprocess(_CenResearchTask):
    """
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
    * ``datebegin`` *datebegin* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``dateend`` *dateend* of the forcing files(s)
     type: str, footprints.stdtypes.FPList
    * ``geometry`` *geometry* of the forcing file(s)
     type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
     type: str

    """
    def get_remote_inputs(self):
        """
        Get forcing file(s) and namelist in order to transform the namelist
        """

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def get_local_inputs(self):
        pass

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
        preprocess_tba = vortex.task(
            kind         = 'surfex_preprocess',
            datebegin    = self.conf.datebegin,
            dateend      = self.conf.dateend,
            # Le nom local de la ressource est fourni par le "container"
            forcingname  = firstforcing.rh.container.basename,
        )
        print(self.ticket.prompt, 'Toolbox algo preprocess =', preprocess_tba)
        print()
        return preprocess_tba

    def launch_algo(self, algo):
        """
        Launch an algo component.
        :param algo: algo component
        """
        self.launch_python_algo(algo=algo)

    def put_outputs(self):
        """
        Save the changed namelist in cache -> namespace = 'vortex.CACHE.fr'
        """
        #######################################################################
        #                               Backup                                #
        #######################################################################
        self.sh.title('Toolbox output Namelist after modification (local cache only)')
        namelist_tbo = vortex.output(
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
    """
    Task for pre-processing a namelist coming from a User Environment.

    NB : This is the task to use to guarantee the simulation's reproductibility

    Supplementary mandatory configuration variables:
    ------------------------------------------------
    * ``genv`` User Environment in which the namelist is to be retrieved.
                 Format : uenv:{uenv_name}@{user}
     type: str
    * ``namelist_source`` The name of the specific namelist to retrieve from the namelist
                   ".tar" archive containing all available namelists.
      type: str

    """
    def get_remote_inputs(self):
        """
        Get namelist from a User Environment.
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input Namelist')
        namelist_tbi = vortex.input(
            role     = 'Nam_surfex',
            # Dans un UEnv, plusieurs namelistes peuvent être stockées dans une archive ".tar",
            # le footprint *source* permet de définir le nom exact de la nameliste à récupérer.
            source   = self.conf.namelist_source,  # ex : OPTIONS_default.nam
            genv     = self.conf.genv,
            kind     = 'namelist',
            model    = 'surfex',
            local    = 'OPTIONS.nam',
            # MV : la nameliste va être modifiée, il faut s'assurer du droit d'écriture (<==> intent='inout')
            intent   = 'inout',
        )
        print(self.ticket.prompt, 'namelist_tbi =', namelist_tbi)
        print()


class Preprocess_Local_Namelist(_Preprocess):
    """
    Task for pre-processing a namelist coming from any user-defined absolute path.

    WARNING : The simulation's reproductibility can not be guaranteed with this task !

    Supplementary mandatory configuration variables:
    ------------------------------------------------
    * ``namelist`` Absolute path pointing to the namelist to be used.
     type: str
    """
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
        namelist_tbi = vortex.input(
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


class Soda_Namelist_Preprocess(_CenResearchTask):
    """
    Pre-process SURFEX namelist for SODA executable

    Inputs:
    -------
    - SODA namelist (OPTIONS.nam)

    Outputs:
    --------
    - SODA namelist (OPTIONS.nam)

    Mandatory configuration variables
    ---------------------------------
    * ``genv`` User Environment in which the namelist is to be retrieved
     type: str
    * ``namelist_source`` Name of the namelist in the user environment
     type: str
    * ``nmembers`` Number of ensemble members in the background state
     type: int
    * ``xpid`` Experiment Identifier
      type: str
    """

    def get_remote_inputs(self):

        self.sh.title('Input Namelist')
        namelist_tbi = vortex.input(
            role     = 'Nam_surfex',
            # Dans un UEnv, plusieurs namelistes peuvent être stockées dans une archive ".tar",
            # le footprint *source* permet de définir le nom exact de la nameliste à récupérer.
            source   = self.conf.namelist_source,  # ex : OPTIONS_default.nam
            genv     = self.conf.genv,
            kind     = 'namelist',
            model    = 'surfex',
            local    = 'OPTIONS.nam',
            # MV : la nameliste va être modifiée, il faut s'assurer du droit d'écriture (<==> intent='inout')
            intent   = 'inout',
        )
        print(self.ticket.prompt, 'namelist_tbi =', namelist_tbi)
        print()

    def get_local_inputs(self):
        pass

    def algo(self):

        self.sh.title('Algo soda preprocess')
        algo = vortex.task(
            kind         = 'soda_preprocess',
            nmembers      = self.conf.nmembers,
        )
        print(self.ticket.prompt, 'Algo soda preprocess =', algo)
        print()

        return algo

    def launch_algo(self, algo):
        """
        Launch a soda namelist preprocess algorithm.
        :param algo: algorithm to launch
        """
        self.launch_python_algo(algo=algo)

    def put_outputs(self):

        self.sh.title('Output namelist')
        namelist = vortex.output(
            role            = 'Nam_surfex',
            kind            = 'namelist',
            model           = 'surfex',
            local           = 'OPTIONS.nam',
            experiment      = self.conf.xpid,
            namespace       = 'vortex.cache.fr',
            block           = 'namelist',
            nativefmt       = 'nam',
        )
        print(self.ticket.prompt, 'Namelist = ', namelist)
        print()
