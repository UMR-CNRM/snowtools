# -*- coding: utf-8 -*-
"""
"""

import vortex
from vortex.layout.dataflow import SectionFatalError
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.params import SurfexParamsMixin
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import daterange, tomorrow

# MV :
# TODO Il faudra réfléchir au traitement des cas ensemblistes (parallélisation sur les membres de simulation uniquement):
# - soit tout le monde hérite d'une classe "OFFLINE" abstraite dans laquelle et il faut gérer dans chaque cas le
#   fait que la notion de *membre* est obligatoire ou optionnelle
# - soit on fait 2 classes abstraites distinctes (1 pour chaque algo) avec duplication des inputs communs


class _Offline_MPI(SurfexParamsMixin, _CenResearchTask):
    """
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
    * ``datebegin`` *datebegin* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``dateend` *dateend* of the forcing files(s)
      type: str, footprints.stdtypes.FPList
    * ``geometry` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` User-defined Experiment identifier (WARNING : 4-digit strings prohibited)
      type: str
    * ``genv`` User Environment in which the following resources are to be retrieved :
                 - ecoclimapI_covers_param.bin
                 - ecoclimapII_eu_covers_param.bin
                 - drdt_bst_fit_60.nc
                 - OFFLINE executable
                 Format : uenv:{uenv_name}@{user}
      type: str
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int

    Optionnal configuration variables (other than forcing-specific ones):
    ---------------------------------------------------------------------
    * ``member`` Simulation member.
                   NB : This is a deterministic task, only one single member value can be provided
     type: int
    * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID
     type: str
    * ``pgd_user`` User who produced the target PGD file.
     type: str
    * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*
     type: str
    * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*
     type: str
    * ``prep_xpid`` Experiment Identifier of the PREP file, if different from the task's XPID
     type: str
    * ``prep_user`` User who produced the target PREP file.
     type: str
    * ``prep_member`` Member associated to the PREP file if it comes from an ensemble (after a SODA run)
                        NB : This is a deterministic task, only one single member value can be provided
     type: int
    * ``prep_vapp`` *vapp* of the PREP file, if different from the task's *vapp*
     type: str
    * ``prep_vconf`` *vconf* of the PREP file, if different from the task's *vconf*
     type: str
    * ``prep_date`` Validity date of the PREP file (if different from *datebegin*)
     type: str
    * ``prep_block`` *block* of the PREP file (default 'prep', but can be different after an assimilation step)
     type: str
    * ``prep_vortex1`` Boolean to identify resources produced with vortex1 (filename without geometry)
     type: bool
    * ``datespinup`` Date of validity of the spinup file (default: *datebegin*)
     type: str, footprints.stdtypes.FPList
    * ``threshold`` Threshold to apply to the snow water equivalent (in kg/m2) each 1st August (default: -999)
     type: int
    * ``dailyprep`` TODO :comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
     type: bool
    * ``drhook`` Activate / deactivate the profiling with DRHOOK (default: False)
     type: bool
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
     type: str
    * ``nnodes`` Number of available nodes for MPI parallelisation
     type: int
    * ``nprocs`` Number of available processors for MPI parallelisation
     type: int
    * ``ntasks`` Number of MPI tasks
     type: int
    * ``io_duration`` Argument similar to the one of the `get_list_dates_files` method in
                        snowtools/utils/dates.py.
                        Used to retrieve the list of *datebegin* and *dateend* for IO covering sub-periods.
                        Possible values : "yearly", "monthly" or "full"
     type: str
    """

    def get_prep(self):

        try:
            # PREP.nc mandatory to run OFFLINE
            self.sh.title('Input PREP')
            prep_tbi = vortex.input(
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
                namespace      = 'vortex.cache.fr',
                vortex1        = self.conf.get('prep_vortex1', False),
                namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
                block          = self.conf.get('prep_block', 'prep'),
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
        except SectionFatalError as e:
            print('Unable to get PREP.nc from cache. Make sure that your driver '
                  'has a node corresponding to the GetPrep task '
                  'before executing the offline task and that the prep_xpid values in the '
                  'corresponding configuration sections match. '
                  'Or that the PrepUenvPrep or PrepLocalPrep task '
                  'has been run recently for the given experiment (prep_xpid).')
            raise e

    def get_namelist(self):
        raise NotImplementedError()

    def get_namelist_from_cache(self):
        """
        OPTIONS.nam always comes from the local cache because it comes from
        a previous execution of a "pre_process" task.
        """
        # Namelist mandatory to run OFFLINE and taken from the cache
        self.sh.title('Input SURFEX-ready namelist')
        namelist_tbi = vortex.input(
            role         = 'Nam_surfex',
            kind         = 'namelist',
            model        = 'surfex',
            local        = 'OPTIONS.nam',
            experiment   = self.conf.xpid,
            namespace    = 'vortex.cache.fr',
            block        = 'namelist',
            nativefmt    = 'nam',
            intent = 'inout',
        ),
        print(self.ticket.prompt, 'namelist =', namelist_tbi)
        print()

    def get_namelist_from_uenv(self):
        """
        Get namelist from UEnv
        """
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

    def get_executable(self):
        """
        Get OFFLINE executable, either from a UEnv/GEnv or from a path depending on the task
        Either call get_executable_from_uenv or get_executable_from_path methods
        """
        raise NotImplementedError

    def get_executable_from_uenv(self):
        """
        Get OFFLINE executable from Uenv
        """
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Input OFFLINE executable from uenv')
        OFFLINE_tbx = vortex.executable(
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
        self.sh.title('Input OFFLINE executable from local')
        OFFLINE_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/OFFLINE"
        )
        print(self.ticket.prompt, 'OFFLINE_tbx =', OFFLINE_tbx)
        print()

    def get_remote_inputs(self):

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                         alternate=self.conf.get("forcing_alternate", True))
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_executable()

    def get_local_inputs(self):
        self.get_namelist_from_cache()
        self.get_pgd_from_cache()
        self.get_prep()

    def algo(self):
        """
        Algo component to execute OFFLINE
        """
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        self.sh.title('Algo OFFLINE-MPI')
        offline_tba = vortex.task(
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
        # # Pour un exécution de binaire, il faut donner l'objet "exécutable" associé (récupéré par la commande
        # # vortex.executable(...))
        # # Il est possible de récupérer cet objet avec la ligne suivante :
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        #
        # # MV : Il faudra également pouvoir fournir le nombre de process et le nombre de tâches via le fichier de conf
        # # TODO : réfléchir à la procédure pour définir des valeurs par défaut en fonction du domaine comme c'est
        # # le cas actuellement
        # print("in launch algo:", self.conf.geometry, self.conf.nprocs)
        self.component_runner(
            algo,
            executable,
            mpiopts=dict(
                nnodes=self.conf.nnodes,
                nprocs=self.conf.nprocs[self.conf.geometry.area],
                ntasks=self.conf.ntasks[self.conf.geometry.area],
            )
        )

    def put_outputs(self):
        """
        Save SURFEX/OFFLINE relevant output files
        """
        self.put_pro()
        self.put_prep()
        self.put_cumul()
        self.put_diag()

    def put_cumul(self):

        self.sh.title('Output CUMUL')
        cumul_tbo = vortex.output(
            local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
#            datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
#            dateend        = dict_dates_end_pro if not self.conf.dailyprep else
#                                list(daterange(tomorrow(base=datebegin), dateend)),
            datebegin      = self.list_dates_begin_pro,
            dateend        = self.dict_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'cumul',
            member         = self.conf.get('member', None),
            fatal          = False,
        ),
        print(self.ticket.prompt, 'cumul_tbo =', cumul_tbo)
        print()

    def put_diag(self):

        self.sh.title('Output DIAG')
        diag_tbo = vortex.output(
            local          = 'DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
#            datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
#            dateend        = dict_dates_end_pro if not self.conf.dailyprep else
#                                list(daterange(tomorrow(base=datebegin), dateend)),
            datebegin      = self.list_dates_begin_pro,
            dateend        = self.dict_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'diag',
            member         = self.conf.get('member', None),
            fatal          = False,
        ),
        print(self.ticket.prompt, 'diag_tbo =', diag_tbo)
        print()

    def put_prep(self):

        self.sh.title('Output PREP')
        prep_tbo = vortex.output(
            local          = 'PREP_[date:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
#            date           = list_dates_end_pro if not self.conf.dailyprep else
#                                list(daterange(tomorrow(base=datebegin), dateend)),
            date           = self.list_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'prep',
            member         = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()

    def put_pro(self):

        self.sh.title('Output PRO')
        pro_tbo = vortex.output(
            local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
#            datebegin      = list_dates_begin_pro if not self.conf.dailyprep else '[dateend]/-PT24H',
#            dateend        = dict_dates_end_pro if not self.conf.dailyprep else
#                                list(daterange(tomorrow(base=datebegin), dateend)),
            datebegin      = self.list_dates_begin_pro,
            dateend        = self.dict_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'pro',
            member         = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'pro_tbo =', pro_tbo)
        print()


class Offline_MPI_Uenv(_Offline_MPI):
    """
    Get OFFLINE executable from a User Environment.

    NB : This is the task to use to guarantee the simulation's reproductibility
    """

    def get_executable(self):
        self.get_executable_from_uenv()


class Offline_MPI_Local(_Offline_MPI):
    """
    Get an OFFLINE executable from any user-defined absolute path locally.

    WARNING : The simulation's reproductibility can not be guaranteed with this task !

    Supplementary mandatory configuration variables:
    ------------------------------------------------
    * ``exesurfex`` Absolute path pointing a local directory containing the target OFFLINE executable
     type: str
    """
    # MV : dans ce cas le binaire doit être présent localement sur HPC,
    # pas besoin de le récupérer sur un noeud de transfert
    def get_executable(self):
        self.get_executable_from_path()


class OfflineMPIDailyPrep(_Offline_MPI):
    """
    Do a surfex simulation with daily prep file output
    """
    def get_executable(self):
        self.get_executable_from_uenv()

    def algo(self):
        """
        Algo component to execute OFFLINE with daily prep output
        """
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        self.sh.title('Algo OFFLINE-MPI')
        offline_tba = vortex.task(
            engine         = 'parallel',
            binary         = 'OFFLINE',
            kind           = 'deterministic',
            datebegin      = self.conf.forcing_datebegin,
            dateend        = self.conf.forcing_dateend,
            # MV : *dateinit* correspond à la date de validité du fichier PREP
            dateinit       = self.ticket.context.sequence.effective_inputs(role='SnowpackInit')[0].rh.resource.date,
            # MV : la valeur par défaut de "threshold" dans la commande s2m est -999
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            threshold      = self.conf.get('threshold', -999),
            daily          = True,
            # MV la valeur par défaut de 'drhook' dans la commande s2m est False
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            drhookprof     = self.conf.get('drhook', False),
            # MV : on traitera les question de reproductibilité dans un 2nd temps.
            #reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, 'offline_tba =', offline_tba)
        print()
        return offline_tba

    def put_cumul(self):
        self.sh.title('Output CUMUL')
        cumul_tbo = vortex.output(
            local='CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment=self.conf.xpid,
            geometry=self.conf.geometry,
            # MV : comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
            # et faire une tâche spécifique à ces cas là.
            datebegin      = '[dateend]/-PT24H',
            dateend        = list(daterange(tomorrow(base=self.conf.datebegin), self.conf.dateend)),
            nativefmt='netcdf',
            kind='SnowpackSimulation',
            model='surfex',
            namespace=self.namespace_out,
            namebuild='flat@cen',  # TODO : passer en variable de configuration
            block='cumul',
            member=self.conf.get('member', None),
            fatal=False,
        ),
        print(self.ticket.prompt, 'cumul_tbo =', cumul_tbo)
        print()

    def put_diag(self):
        self.sh.title('Output DIAG')
        diag_tbo = vortex.output(
            local='DIAG_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment=self.conf.xpid,
            geometry=self.conf.geometry,
            datebegin = '[dateend]/-PT24H',
            dateend = list(daterange(tomorrow(base=self.conf.datebegin), self.conf.dateend)),
            nativefmt='netcdf',
            kind='SnowpackSimulation',
            model='surfex',
            namespace=self.namespace_out,
            namebuild='flat@cen',  # TODO : passer en variable de configuration
            block='diag',
            member=self.conf.get('member', None),
            fatal=False,
        ),
        print(self.ticket.prompt, 'diag_tbo =', diag_tbo)
        print()

    def put_prep(self):
        self.sh.title('Output PREP')
        prep_tbo = vortex.output(
            local='PREP_[date:ymdh].nc',
            role='SnowpackInit',
            experiment=self.conf.xpid,
            geometry=self.conf.geometry,
            date = list(daterange(tomorrow(base=self.conf.datebegin), self.conf.dateend)),
            nativefmt='netcdf',
            kind='PREP',
            model='surfex',
            namespace=self.namespace_out,
            namebuild='flat@cen',  # TODO : passer en variable de configuration
            block='prep',
            member=self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()

    def put_pro(self):
        self.sh.title('Output PRO')
        pro_tbo = vortex.output(
            local='PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment=self.conf.xpid,
            geometry=self.conf.geometry,
            datebegin = '[dateend]/-PT24H',
            dateend = list(daterange(tomorrow(base=self.conf.datebegin), self.conf.dateend)),
            nativefmt='netcdf',
            kind='SnowpackSimulation',
            model='surfex',
            namespace=self.namespace_out,
            namebuild='flat@cen',  # TODO : passer en variable de configuration
            block='pro',
            member=self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'pro_tbo =', pro_tbo)
        print()