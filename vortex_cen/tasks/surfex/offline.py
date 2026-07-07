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

    def get_executable_from_uenv(self, mpi=True, fatal=True):
        """
        Get OFFLINE executable from Uenv
        """
        if mpi:
            default_gvar = 'master_offline_mpi'
        else:
            default_gvar = 'master_offline_nompi'

        self.sh.title('Input OFFLINE executable from uenv')
        OFFLINE_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
            gvar           = self.conf.get('offline_gvar', default_gvar),
            fatal          = fatal,
        )
        print(self.ticket.prompt, 'OFFLINE_tbx =', OFFLINE_tbx)
        print()

    def get_executable_from_path(self, fatal=True):
        """
        Get OFFLINE executable locally
        """
        self.sh.title('Input OFFLINE executable from local')
        OFFLINE_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'offline',
            local          = 'OFFLINE',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/OFFLINE",
            fatal          = fatal,
        )
        print(self.ticket.prompt, 'OFFLINE_tbx =', OFFLINE_tbx)
        print()


class _Offline(OfflineCommonsMixin, _CenResearchTask):
    """
    Task : _Offline
    ===============

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
    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
            "datebegin",
            "dateend",
            "xpid",
            "geometry",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "prep",
            "pgd_cache",
            "member",
            "io_duration",
            "namespace_out",
            "august_threshold",
            "offline_gvar",
            "out_block+default=offline/[pro,prep] ",
            "diff_xpid",
            "diff_user",
            "diff_block+default=offline/prep",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                         alternate=self.conf.get("forcing_alternate", True))
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_executable()


    def get_local_inputs(self):
        self.get_namelist()
        try:
            _ = self.get_prep_file_from_cache_or_archive(fatal=True, cache_only=True)
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

    def get_namelist(self):
        self.get_namelist_from_cache()

    def get_executable(self):
        """
        Get OFFLINE executable, either from a UEnv/GEnv or from a path depending on the task
        Either call get_executable_from_uenv or get_executable_from_path methods
        """
        raise NotImplementedError("An OFFLINE executable is exepected")

    def get_pgd(self):
        """
        The PGD.nc file can come from the output of a "PGD" Task in standard cases.
        It comes from a User Environment in reanalysis tasks.
        """
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
            block          = self.conf.get('out_block', 'offline/prep'),
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
            block          = self.conf.get('out_block', 'offline/pro'),
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
            block          = self.conf.get('out_block', 'offline/cumul'),
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
            block          = self.conf.get('out_block', 'offline/diag'),
            member         = self.conf.get('member', None),
            fatal          = False,
        ),
        print(self.ticket.prompt, 'diag_tbo =', diag_tbo)
        print()

    def diff(self):
        """
        Test output reproductibility [OPTIONAL]
        """
        # Diff of PRO files always fails because netcdf can not properly read them.
        # --> check reproductibility on PREP file
        self.sh.title("Reproductibility check : PREP")
        diff = vortex.diff(
            local          = 'PREP_[date:ymdh].nc',
            role           = 'SnowpackInit',
            experiment     = self.conf.diff_xpid,
            username       = self.conf.diff_user,
            geometry       = self.conf.geometry,
            date           = self.list_dates_end_pro,
            nativefmt      = 'netcdf',
            kind           = 'PREP',
            model          = 'surfex',
            namespace      = self.namespace_out,
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = self.conf.get('diff_block', 'offline/prep'),
            member         = self.conf.get('member', None),
        ),
        print(self.ticket.prompt, 'diff =', diff)
        print()


class _Offline_MPI(_Offline):
    """
    Task : _Offline_MPI
    ===================

    Abstract task for the execution of OFFLINE binary with MPI parallelisation.

    """

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = [
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "drhook",
            "august_threshold",
            "ntasks",
            "nnodes",
            "nprocs",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

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
    Task : Offline_MPI_Uenv
    =======================

    Get OFFLINE executable from a User Environment.

    NB : This is the task to use to guarantee the simulation's reproductibility
    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "prep",
            "pgd_cache",
            "member",
            "io_duration",
            "namespace_out",
            "august_threshold",
            "offline_gvar",
        ]
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_executable(self):
        self.get_executable_from_uenv(mpi=True)


class Offline_MPI_Local(_Offline_MPI):
    """
    Task : Offline_MPI_Local
    ========================

    Get an OFFLINE executable from any user-defined absolute path locally.

    WARNING : The simulation's reproductibility can not be guaranteed with this task !
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = []
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "exesurfex",
        ]
        super().__init__(**kw)

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

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