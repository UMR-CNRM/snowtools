# -*- coding: utf-8 -*-
"""
"""

import vortex
from vortex.layout.dataflow import SectionFatalError
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend
from bronx.stdtypes.date import daterange, tomorrow

class OfflineCommonsMixin(SurfexCommonsMixin):

    def get_executable_from_uenv(self, mpi=True):
        """
        Get OFFLINE executable from Uenv
        """
        if mpi:
            gvar = 'master_surfex_offline_mpi'
        else:
            gvar = 'master_surfex_offline_nompi'
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
            gvar           = gvar,
        )
        print(self.ticket.prompt, 'OFFLINE_tbx =', OFFLINE_tbx)
        print()

    def get_executable_from_path(self):
        """
        Get OFFLINE executable locally
        """
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


class _Offline(OfflineCommonsMixin, _CenResearchTask):
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
    * ``august_threshold`` Threshold to apply to the snow water equivalent (in kg/m2) each 1st August (default: -999)
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

    def get_namelist(self):
        raise NotImplementedError("A namelist is exepected for to launch an OFFLINE executable")

    def get_executable(self):
        """
        Get OFFLINE executable, either from a UEnv/GEnv or from a path depending on the task
        Either call get_executable_from_uenv or get_executable_from_path methods
        """
        raise NotImplementedError("An OFFLINE executable is exepected")

    def get_remote_inputs(self):

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                         alternate=self.conf.get("forcing_alternate", True))
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_executable()


    def get_local_inputs(self):
        self.get_namelist_from_cache()
        try:
            self.get_prep()
        except SectionFatalError as e:
            print('Unable to get PREP.nc.')
            # MV : la tâche 'GetPrep' est une tâche de secours, on ne doit pas
            # compter dessus par defaut mais plutot chercher un PREP existant
            #      'Make sure that your driver '
            #      'has a node corresponding to the GetPrep task '
            #      'before executing the offline task and that the prep_xpid values in the '
            #      'corresponding configuration sections match. '
            #      'Or that the PrepUenvPrep or PrepLocalPrep task '
            #      'has been run recently for the given experiment (prep_xpid).')
            raise e
        self.get_pgd()

    def get_pgd(self):
        self.get_pgd_from_cache()

    def put_outputs(self):
        """
        Save SURFEX/OFFLINE relevant output files
        """
        self.put_pro()
        self.put_prep()
        self.put_cumul()
        self.put_diag()

    def put_prep(self):

        self.sh.title('Output PREP')
        prep_tbo = vortex.output(
            local          = 'PREP_[date:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            # TODO : faire une tâche spécifique "reforecast" pour la production de PREP quotidiens
            # date           = list_dates_end_pro if not self.conf.dailyprep else
            #                       list(daterange(tomorrow(base=datebegin), dateend)),
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

    def put_cumul(self):

        self.sh.title('Output CUMUL')
        cumul_tbo = vortex.output(
            local          = 'CUMUL_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
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


class _Offline_MPI(_Offline):
    """
    Abstract task for the execution of OFFLINE binary with MPI parallelisation.

    Additional mandatory configuration variables:
    ---------------------------------------------
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int or dict
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int or dict
    """

    def algo(self):
        """
        Algo component to execute OFFLINE with MPI parallelisation
        """

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
            threshold      = self.conf.get('august_threshold', -999),
            # daily          = self.conf.dailyprep,
            # MV la valeur par défaut de 'drhook' dans la commande s2m est False
            # TODO : cette valeur par défaut pourrait être codée directement dans l'algo
            drhookprof     = self.conf.get('drhook', False),
            # MV : on traitera les question de reproductibilité dans un 2nd temps.
            # reprod_info    = self.get_reprod_info,
        )
        print(self.ticket.prompt, 'Algo =', offline_tba)
        print()
        return offline_tba

    def launch_algo(self, algo, **kw):
        """
        Run OFFLINE MPI algo component.
        """
        # Pour un exécution de binaire, il faut donner l'objet "exécutable" associé (récupéré par la commande
        # vortex.executable(...))
        # Il est possible de récupérer cet objet avec la ligne suivante :
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]

        self.component_runner(
            algo,
            executable,
            mpiopts=dict(
                nnodes=self.conf.nnodes,  # Redondant avec la valeur par défaut dans mkjob
                nprocs=self.conf.nprocs,  # Redondant avec la valeur par défaut dans mkjob
                ntasks=self.conf.ntasks,  # Redondant avec la valeur par défaut dans mkjob
            )
        )


class Offline_MPI_Uenv(_Offline_MPI):
    """
    Get OFFLINE executable from a User Environment.

    NB : This is the task to use to guarantee the simulation's reproductibility
    """

    def get_executable(self):
        self.get_executable_from_uenv(mpi=True)


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