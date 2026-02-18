# -*- coding: utf-8 -*-
'''
'''

from vortex import toolbox
from vortex_cen.tasks.research_task_base import _CenResearchTask
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import Date  # , daterange, tomorrow

# MV :
# TODO Il faudra réfléchir au traitement des cas ensemblistes (parallélisation sur les membres de simulation uniquement):
# - soit tout le monde hérite d'une classe "OFFLINE" abstraite dans laquelle et il faut gérer dans chaque cas le fait que la notion de *membre* est obligatoire ou optionnelle
# - soit on fait 2 classes abstraites distinctes (1 pour chaque algo) avec duplication des inputs communs
class _Offline_MPI(_CenResearchTask):
    '''
    Abstract task for OFFLINE binary execution.

    SURFEX/OFFLINE documentation : https://umr-cnrm.github.io/snowtools-doc/misc/surfex.html

    Inputs:
    -------
    - FORCING.nc files(s) (near-surface meteorological conditions during the simulation period)
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD.nc (Ground physiography)
    - PREP.nc (initial conditions)

    Outputs:
    --------
    - PRO.nc Snowpack simulations covering the entire simulation period
    - PREP.nc SURFEX/Crocus model state variables at the end of the simulation
    - CUMUL.nc TODO   Compléter et CHECKER la doc
    - DIAG.nc TODO    Compléter et CHECKER la doc

    Mandatory configuration variables:
    ----------------------------------
    :param datebegin: *datebegin* of the forcing file(s)
    :type datebegin: str, footprints.stdtypes.FPList
    :param dateend: *dateend* of the forcing files(s)
    :type dateend: str, footprints.stdtypes.FPList
    :param geometry: *geometry* of the forcing file(s)
    :type geometry: str, footprints.stdtypes.FPList
    :param xpid: User-defined Experiment identifier
    :type xpid: str
    :param genv: User Environment in which the following resources are to be retrieved :
                 - ecoclimapI_covers_param.bin
                 - ecoclimapII_eu_covers_param.bin
                 - drdt_bst_fit_60.nc
                 - OFFLINE executable
                 Format : uenv:{uenv_name}@{user}
    :type genv: str
    :param nprocs: Number of process to allocate to the execution of the MPI binary
    :type nprocs: int
    :param ntasks: Number of tasks to allocate to the execution of the MPI binary
    :type nprocs: int

    Optionnal configuration variables (other than forcing-specific ones):
    ---------------------------------------------------------------------
    :param member: Simulation member.
                   NB : This is a deterministic task, only one single member value can be provided
    :type member: int
    :param pgd_xpid: Experiment Identifier of the PGD file, if different from the task's XPID
    :type pgd_xpid: str
    :param pgd_user: User who produced the target PGD file.
    :type pgd_user: str
    :param pgd_vapp: *vapp* of the PGD file, if different from the task's *vapp*
    :type pgd_vapp: str
    :param pgd_vconf: *vconf* of the PGD file, if different from the task's *vconf*
    :type pgd_vconf: str
    :param prep_xpid: Experiment Identifier of the PREP file, if different from the task's XPID
    :type prep_xpid: str
    :param prep_user: User who produced the target PREP file.
    :type prep_user: str
    :param prep_member: Member associated to the PREP file if it comes from an ensemble (after a SODA run)
                        NB : This is a deterministic task, only one single member value can be provided
    :type prep_member: int
    :param prep_vapp: *vapp* of the PREP file, if different from the task's *vapp*
    :type prep_vapp: str
    :param prep_vconf: *vconf* of the PREP file, if different from the task's *vconf*
    :type prep_vconf: str
    :param prep_date: Validity date of the PREP file (if different from *datebegin*)
    :type prep_date: str
    :param prep_vortex1: Boolean to identify resources produced with vortex1 (filename without geometry)
    :type prep_vortex1: bool
    :param datespinup: Date of validity of the spinup file (default: *datebegin*)
    :type datespinup: str, footprints.stdtypes.FPList
    :param threshold: Threshold to apply to the snow water equivalent (in kg/m2) each 1st August (default: -999)
    :type threshold: int
    :param dailyprep: TODO :comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
    :type dailyprep: bool
    :param drhook: Activate / deactivate the profiling with DRHOOK (default: False)
    :type drhook: bool
    :param namespace_out: Force specific namespace for output files (default: 'vortex.multi.fr')
    :type namespace_out: str
    :param nnodes: Number of available nodes for MPI parallelisation
    :type nnodes: int
    :param nprocs: Number of available processors for MPI parallelisation
    :type nprocs: int
    :param ntasks: Number of MPI tasks
    :type ntasks: int
    :param io_duration: Argument similar to the one of the `get_list_dates_files` method in
                        snowtools/utils/dates.py.
                        Used to retrieve the list of *datebegin* and *dateend* for IO covering sub-periods.
                        Possible values : "yearly", "monthly" or "full"
    :type io_duration: str
    '''

    def get_ecoclimap(self):
        """
        Get ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        Get drdt_bst_fit_60.nc, PGD.nc, PREP.nc, FORCING.nc
        """
        # Binary ECOCLIMAP I files are mandatory to run OFFLINE and taken from the uenv
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

        # Binary ECOCLIMAP II files are mandatory to run OFFLINE and taken from the uenv
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

    def get_drdt_bst_fit(self):

        # Crocus metamorphism parameters mandatory to run OFFLINE and taken from the uenv
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

    def get_pgd(self):
        """
        A PGD.nc file is mandatory to run OFFLINE.
        In the general research case, the PGD comes from the vortex cache.
        For "stable" configurations such as the reanalysis, it comes from a UEnv/GEnv.
        """

        self.sh.title('Toolbox input PGD')
        pgd_tbi = toolbox.input(
            local          = 'PGD.nc',
            role           = 'SurfexClim',
            # MV : pour permettre de récupérer le PGD depuis une expérience indépendante
            # --> possibilité de renseigner 'pgd_xpid' dans le fichier de conf
            experiment     = self.conf.get('pgd_xpid', self.conf.xpid),
            username       = self.conf.get('pgd_user', None),
            # MV : Pour prévoir les cas où le PGD vient d'un vapp / vconf différent
            # de ceux de la tâche
            vapp           = self.conf.get('pgd_vapp', self.conf.vapp),
            vconf          = self.conf.get('pgd_vconf', self.conf.vconf),
            geometry       = self.conf.geometry,
            nativefmt      = 'netcdf',
            kind           = 'pgdnc',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'pgd',
            vortex1        = self.conf.get('pgd_vortex1', False),
            # MV : La notion de "membre" n'a pasde sens pour le PGD
        ),
        print(self.ticket.prompt, 'pgd =', pgd_tbi)
        print()

    def get_prep(self):

        # PREP.nc mandatory to run OFFLINE
        self.sh.title('Toolbox input PREP')
        prep_tbi = toolbox.input(
            local          = 'PREP.nc',
            role           = 'SnowpackInit',
            # MV : pour permettre de récupérer le PREP depuis une expérience indépendante
            # --> possibilité de renseigner 'prep_xpid' dans le fichier de conf
            experiment     = self.conf.get('prep_xpid', self.conf.xpid),
            username       = self.conf.get('prep_user', None),
            # MV : il faut définir la date de validité du fichier PREP qui par défaut
            # est la *datebegin* de simulation mais peut être arbitraire si 'date_prep' est renseigné
            date           = self.conf.get('prep_date', self.conf.datebegin),
            # MV : Pour prévoir les cas où le PREP vient d'un vapp / vconf différent
            # de ceux de la tâche
            vapp           = self.conf.get('prep_vapp', self.conf.vapp),
            vconf          = self.conf.get('prep_vconf', self.conf.vconf),
            geometry       = self.conf.geometry,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            vortex1        = self.conf.get('prep_vortex1', False),
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'prep',
            # MV : La notion de "membre" pour le PREP est particulière dans le cas déterministe
            # - dans le cas général, le PREP n'est associé à aucun *membre*
            # - dans une simulation avec assimilation: la première initialisation est faite
            #   avec un unique fichier PREP pour tous les membres de simulation et les initialisations
            #   suivantes dépendent des membres sélectionnés par SODA.
            # Le cas ensembliste (parralélisation sur les membres, 1 PREP / membre)
            # doit être traité dans une tâche spécifique
            member         = self.conf.get('prep_member', self.conf.get('member', None)),
            intent         = 'inout',
        ),
        print(self.ticket.prompt, 'prep_tbi =', prep_tbi)
        print()

    def get_namelist(self):
        """
        OPTIONS.nam always comes from the local cache because it comes from
        a previous execution of a "pre_process" task.
        """
        # Namelist mandatory to run OFFLINE and taken from the cache
        self.sh.title('Toolbox input SURFEX-ready namelist')
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

    def get_executable(self):
        """
        Get OFFLINE executable, either from a UEnv/GEnv or from a path depending on the task
        Either call get_executable_from_uenv or get_executable_from_path methods
        """
        pass

    def get_executable_from_uenv(self):
        """
        Get OFFLINE executable from Uenv
        """
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input OFFLINE executable from uenv')
        OFFLINE_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            # MV : Il faudra peut être utiliser une variable de conf différente de *genv* à terme pour permettre
            # de récupérer les autres "constantes" dans un genv commun et le binaire dans un environement géré par
            # le user
            genv           = self.conf.genv,
            gvar           = 'master_surfex_offline_mpi',
        )
        print(self.ticket.prompt, 'OFFLINE_tbx =', OFFLINE_tbx)
        print()

    def get_executable_from_path(self):
        """
        Get OFFLINE executable locally
        """
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

    def get_remote_inputs(self):

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_pgd()
        self.get_prep()
        self.get_executable()

    def get_local_inputs(self):
        self.get_remote_inputs()
        self.get_namelist()

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
            # MV : *dateinit* correspond à la date de validité du fichier PREP
            dateinit       = self.ticket.context.sequence.effective_inputs(role='SnowpackInit')[0].rh.resource.date,
            # MV : la valeur par défaut de "threshold" dans la commande s2m est -999
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            threshold      = self.conf.get('threshold', -999),
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
            #daily          = self.conf.dailyprep,
            # MV la valeur par défaut de 'drhook' dans la commande s2m est False
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            drhookprof     = self.conf.get('drhook', False),
            # MV : on traitera les question de reproductibilité dans un 2nd temps.
            #reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, 'offline_tba =', offline_tba)
        print()
        return offline_tba

    def launch_algo(self, algo, **kw):
        """
        Run OFFLINE MPI algo component.
        """
        # Pour un exécution de binaire, il faut donner l'objet "exécutable" associé (récupéré par la commande
        # toolbox.executable(...))
        # Il est possible de récupérer cet objet avec la ligne suivante :
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]

        # MV : Il faudra également pouvoir fournir le nombre de process et le nombre de tâches via le fichier de conf
        # TODO : réfléchir à la procédure pour définir des valeurs par défaut en fonction du domaine comme c'est
        # le cas actuellement
        self.component_runner(algo, executable,
                mpiopts=dict(nnodes=self.conf.nnodes, nprocs=self.conf.nprocs, ntasks=self.conf.ntasks))

    def put_remote_outputs(self):
        """
        Save the CUMUL, DIAG, PREP and PRO files (yearly only)
        """
        #######################################################################
        #                               Backup                                #
        #######################################################################
        _, _, list_dates_begin_pro, list_dates_end_pro = get_list_dates_files(
            Date(self.conf.datebegin),
            Date(self.conf.dateend),
            self.conf.get('io_duration', 'yearly'))
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)

        # Define a namespace_out variable to apply to all outputs set as the *namespace_out*
        # configuration variable if provided by the user or 'vortex.multi.fr' by default
        namespace_out = self.conf.get('namespace_out', 'vortex.multi.fr')

        self.sh.title('Toolbox output CUMUL')
        cumul_tbo = toolbox.output(
            local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
#            datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
#            dateend        = dict_dates_end_pro if not self.conf.dailyprep else
#                                list(daterange(tomorrow(base=datebegin), dateend)),
            datebegin      = list_dates_begin_pro,
            dateend        = dict_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'cumul',
            member         = self.conf.get('member', None),
            fatal          = False,
        ),
        print(self.ticket.prompt, 'cumul_tbo =', cumul_tbo)
        print()

        self.sh.title('Toolbox output DIAG')
        diag_tbo = toolbox.output(
            local          = 'DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
#            datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
#            dateend        = dict_dates_end_pro if not self.conf.dailyprep else
#                                list(daterange(tomorrow(base=datebegin), dateend)),
            datebegin      = list_dates_begin_pro,
            dateend        = dict_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'diag',
            member         = self.conf.get('member', None),
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
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
#            date           = list_dates_end_pro if not self.conf.dailyprep else
#                                list(daterange(tomorrow(base=datebegin), dateend)),
            date           = list_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'prep',
            member         = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()

        self.sh.title('Toolbox output PRO')
        pro_tbo = toolbox.output(
            local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
#            datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
#            dateend        = dict_dates_end_pro if not self.conf.dailyprep else
#                                list(daterange(tomorrow(base=datebegin), dateend)),
            datebegin      = list_dates_begin_pro,
            dateend        = dict_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'pro',
            member         = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'pro_tbo =', pro_tbo)
        print()


class Offline_MPI_Uenv(_Offline_MPI):
    '''
    Get OFFLINE executable from a User Environment.

    NB : This is the task to use to guarantee the simulation's reproductibility
    '''

    def get_executable(self):
        self.get_executable_from_uenv()



class Offline_MPI_Local(_Offline_MPI):
    '''
    Get an OFFLINE executable from any user-defined absolute path locally.

    WARNING : The simulation's reproductibility can not be guaranteed with this task !

    Supplementary mandatory configuration variables:
    ------------------------------------------------
    :param exesurfex: Absolute path pointing the a local directory containing the target OFFLINE executable
    :type exesurfex: str
    '''
    # MV : dans ce cas le binaire doit être présent localement sur HPC,
    # pas besoin de le récupérer sur un noeud de transfert
    def get_executable(self):
        self.get_executable_from_path()
