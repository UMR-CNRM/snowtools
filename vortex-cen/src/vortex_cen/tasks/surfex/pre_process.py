# -*- coding: utf-8 -*-
'''
'''
from vortex import toolbox
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex.util.helpers import InputCheckerError


# MV : Pas de soucis pour moi pour laisser "Abstract" dans le nome de la classe,
# mais la convention python dans ce cas est plutot de nommer la classe avec un "_"
# initial pour faire plus court 'ex : "_Preprocess_Task"
class Abstract_Preprocess_Task(_CenResearchTask):
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
        t = self.ticket
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        avail_forcings = t.context.sequence.effective_inputs(role='Forcing')
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


class Preprocess_Task_Uenv_Namelist(Abstract_Preprocess_Task):
    '''
    Task for pre-processing a namelist coming from a User Environment.

    NB : This is the task to use to guarantee the simulation's reproductibility

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

        # MV : Pour cette tâche le user DOIT dire dans quel uenv prendre la nameliste pour
        # assurer le reproductibilité.
        # La possibilité de récupérer la namelist dans un Environnement par défaut peut être laissée
        # dans la tâche "Preprocess_Task_Local_Namelist" (ne garatissant pas la reproductibilité)
        # si besoin.

        #if not hasattr(self.conf, "genv"):
        #    self.conf.genv = 'uenv:cen.14@CONST_CEN'

        #if hasattr(self.conf, "uenv") and 'OPTIONS.nam' in self.conf.udata.items():
        #    # The OPTION.nam namelist will be retrived by the 'tbuenv' toolbox
        #    pass
        #else:
            # If not provided, standard namelist taken from the uenv

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


class Preprocess_Task_Local_Namelist(Abstract_Preprocess_Task):
    '''
    Task for pre-processing a namelist coming from any user-defined absolute path.

    WARNING : The simulation's reproductibility can not be guaranteed with this task !

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


class SODA_namelist_preprocess(_CenResearchTask):
    """
    Pre-processing of SODA namelist to add informations such as the date of the run.
    """

    def get_remote_inputs(self):

        self.get_forcings()

        self.sh.title('Input namelist')
        namelist_tbi = toolbox.input(
            role    = 'Namelist_soda',
            genv    = self.conf.uenv,
            gvar    = 'surfex_namelist',
            kind    = 'namelist',
            model   = 'surfex',
            local   = 'OPTIONS.nam',
        )
        print(self.ticket.prompt, 'Namelist =', namelist_tbi)
        print()

    def algo(self):

        self.sh.title('Algo : soda namelist preprocess')
        algo = toolbox.algo(
            kind    = 'soda_preprocess',
            members = self.conf.members,
        )
        print(self.ticket.prompt, 'Algo =', algo)
        print()
        return algo

    def put_local_outputs(self):

        self.sh.title('Output namelist')
        namelist_tbo = toolbox.output(
            role            = 'Nam_surfex',
            kind            = 'namelist',
            model           = 'surfex',
            local           = 'OPTIONS.nam',
            experiment      = self.conf.xpid,
            namespace       = 'vortex.cache.fr',
            block           = 'namelist',
            nativefmt       = 'nam',
        )
        print(self.ticket.prompt, 'Namelist =', namelist_tbo)
        print()
