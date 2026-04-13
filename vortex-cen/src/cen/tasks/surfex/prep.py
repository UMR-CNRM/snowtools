# -*- coding: utf-8 -*-
"""
"""

#from vortex.layout.nodes import Task
import vortex
from vortex.layout.dataflow import SectionFatalError
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.params import SurfexParamsMixin

class _Prep_Construct(SurfexParamsMixin, _CenResearchTask):
    """
    Abstract task for PREP step.

    Inputs:
    -------

    * ``OPTIONS.nam`` ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    * ``ecoclimapI_covers_param.bin`` and ``ecoclimapII_eu_covers_param.bin`` (binaries for vegetation generation)
    * ``drdt_bst_fit_60.nc`` (Crocus metamorphism parameters)
    * ``Init_TG.nc`` Initial values of ground temperature coming from the cache
      (put there by an execution of an InitClimGroundTemperature or GetClimGroundTemperature task)
    * ``PGD.nc`` Ground physiography coming from the cache (put ther by an execution of a Pgd* task or GetPgd1D task

    Outputs:
    --------
    - PREP.nc (initial conditions)

    Mandatory configuration variables:
    ----------------------------------

    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier (format "{experiment_name}@{user}")
      type: str
    * ``genv`` User Environment in which the following resources are to be retrieved :
                 - ecoclimapI_covers_param.bin
                 - ecoclimapII_eu_covers_param.bin
                 - drdt_bst_fit_60.nc
                 - PREP executable
                 Format : uenv:{uenv_name}@{user}
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int

    Optionnal configuration variables:
    ---------------------------------------------------------------------
    * ``pgd_xpid`` Experiment Identifier of the PGD file, if different from the task's XPID
      type: str
    * ``pgd_vapp`` *vapp* of the PGD file, if different from the task's *vapp*
      type: str
    * ``pgd_vconf`` *vconf* of the PGD file, if different from the task's *vconf*
      type: str
    * ``dailyprep`` TODO :comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
      type: bool
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str
    """
    def get_remote_inputs(self):
        """
        Get ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        Get drdt_bst_fit_60.nc, PGD.nc
        """
        self.get_ecoclimap()
        self.get_drdt_bst_fit()


    def get_local_inputs(self):
        """
        Get OPTIONS.nam which is always in cache and
        init_TG.nc that should be in cache as well at this point.
        """
        # Namelist mandatory to run PREP and taken from the cache
        self.sh.title('Toolbox input Namelist after modification')
        namelist_tbi = vortex.input(
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

        try:
            self.sh.title('Toolbox input init_TG from Cache')
            init_tg_cache_tbi = vortex.input(
                role="InitialValuesOfGroundTemperature",
                kind='climTG',
                nativefmt='netcdf',
                local='init_TG.nc',
                experiment=self.conf.get('xpid_tg', self.conf.xpid),
                geometry=self.conf.geometry,
                model='surfex',
                namespace='vortex.cache.fr',
                namebuild='flat@cen',  # TODO : passer en variable de configuration
                block='prep',
                fatal=True,
            ),
            print(self.ticket.prompt, 'initTG_tbi =', init_tg_cache_tbi)
            print()
        except SectionFatalError as e:
            print('Unable to get init_TG.nc from cache. Make sure that your driver '
                  'has a node corresponding to the GetClimGroundTemperature task '
                  'before executing the Prep task and that the xpid_tg values in the '
                  'corresponding configuration sections match. '
                  'Or that the InitClimGroundTemperature task '
                  'has been run recently for the given experiment (xpid_tg).')
            raise e

        # PGD.nc mandatory to run PREP
        self.get_pgd_from_cache()


    def algo(self):
        """
        Algo component to produce the PREP file if not found in the inputs
        """
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        self.sh.title('Toolbox algo PREP')
        PREP_tba = vortex.task(
            engine     = 'parallel',
        )
        print(self.ticket.prompt, 'Toolbox algo prep=', PREP_tba)
        print()
        return PREP_tba

    def launch_algo(self, algo, **kw):
        """
        Run PREP algo component.
        """
        self.launch_executable(algo)


    def put_outputs(self):
        """
        Save the PREP file
        """
        #######################################################################
        #                               Backup                                #
        #######################################################################
        self.sh.title('Toolbox Output PREP')
        prep_tbo = vortex.output(
            local      = 'PREP.nc',
            role       = 'SnowpackInit',
            experiment = self.conf.xpid,
            geometry   = self.conf.geometry,
            date       = self.conf.get('date', self.conf.datebegin),
            nativefmt  = 'netcdf',
            kind       = 'PREP',
            model      = 'surfex',
            namespace  = 'vortex.multi.fr',
            namebuild  = 'flat@cen',  # TODO : passer en variable de configuration
            block      = 'prep',
            member     = self.conf.member if hasattr(self.conf, 'member') else None,
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()


class PrepUenvPrep(_Prep_Construct):
    """
    Get init_TG.nc and PREP executable both from Uenv
    """
    def get_remote_inputs(self):
        """
        Get init_TG.nc and PREP executable both from Uenv
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################

        self.sh.title('Toolbox input PREP executable from uenv')
        PREP_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            # MV : Il faudra peut être utiliser une variable de conf différente de *genv* à terme pour permettre
            # de récupérer les autres "constantes" dans un genv commun et le binaire dans un environement géré par
            # le user
            genv           = self.conf.genv,
            gvar           = 'master_prep_mpi',
        )
        print(self.ticket.prompt, 'PREP_tbx =', PREP_tbx)
        print()



class PrepLocalPrep(_Prep_Construct):
    """
    Get init_TG.nc from Uenv and PREP executable locally

    Supplementary mandatory configuration variables:
    ------------------------------------------------

    * ``exesurfex`` Absolute path pointing the a local directory containing the target PREP executable
        type: str
    """
    def get_remote_inputs(self):
        """
        Get init_TG.nc from Uenv and PREP executable locally
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################


        self.sh.title('Toolbox input PREP executable from local')
        prep_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PREP"
        )
        print(self.ticket.prompt, 'PREP_tbx =', prep_tbx)
        print()


class GetPrep(_Prep_Construct):
    """
    Look if the requested PREP.nc file is available in the cache or archive. If not,
    calculate it.

    Mandatory configuration variables:
    ----------------------------------
    * ``prep_xpid`` or ``xpid`` Experiment id the prep file should be searched for or put in cache.
    * ``prep_date`` or ``datebegin`` Validity date of the prep file. Default is ``datebegin`` but can be any date.
    * ``prep_vapp`` or ``vapp`` Application name to search the PREP.nc file.
    * ``prep_vconf`` or ``vconf`` Configuration name to search the PREP.nc file.
    * ``geometry`` Geometry of the PREP.nc file.

    Additional configuration variables needed if PREP is calculated:
    ----------------------------------------------------------------

    * ``xpid`` Experiment identifier (format "{experiment_name}@{user}")
      type: str
    * ``genv`` User Environment in which the following resources are to be retrieved:
                 - ecoclimapI_covers_param.bin
                 - ecoclimapII_eu_covers_param.bin
                 - drdt_bst_fit_60.nc
                 - PREP executable (if the executable is not given via a local path using the ``exesurfex`` configuration variable)
                 Format : uenv:{uenv_name}@{user}
    * ``nnodes`` Number of nodes to allocate to the execution of the MPI binary. In general 1.
    * ``nprocs`` Number of process to allocate to the execution of the MPI binary
      type: int
    * ``ntasks`` Number of tasks to allocate to the execution of the MPI binary
      type: int
    * ``openmp`` Number of threads to use for multithreading. Usually 1, since we don't do multithreading.

    Optional configuration variables:
    ---------------------------------
    * ``prep_user`` username under which the experiment is archived.
    * ``prep_vortex1`` type: bool. True if the requested PREP.nc file was produced with vortex 1 and thus uses
      vortex 1 naming conventions. Default is ``False``.
    * ``prep_block`` block part of the data tree to search for the PREP.nc file. Default is ``prep``.
    * ``prep_member`` or ``member`` If the PREP.nc file comes from an ensemble. Default is ``None``.
    * ``exesurfex`` path to the Surfex executables if PREP.nc is calculated and if the PREP binary is in a local directory
      not in an uenv.
    * ``xpid_tg`` Experiment id the init_TG.nc file comes from, if different from ``xpid``.
    * ``pgd_xpid`` Experiment id the PGD.nc file comes from, if different from ``xpid``.
    * ``pgd_vapp`` Application name to search the PGD.nc file. In case the PREP.nc is calculated. Default is ``vapp``.
    * ``pgd_vconf`` Configuration name to search the PGD.nc file. In case the PREP.nc is calculated. Default is ``vconf``.
    """
    def get_prep_exe(self):
        """
        get PGD executable from uenv or local path
        """
        if hasattr(self.conf, 'exesurfex'):
            self.sh.title('Toolbox input PREP executable from local')
            prep_local_tbx = vortex.executable(
                role='Binary',
                kind='prep',
                local='PREP',
                model='surfex',
                remote=self.conf.exesurfex + "/PREP"
            )
            print(self.ticket.prompt, 'PREP_local_tbx =', prep_local_tbx)
            print()

        else:
            self.sh.title('Toolbox input PREP executable from uenv')
            prep_uenv_tbx = vortex.executable(
                role='Binary',
                kind='prep',
                local='PREP',
                model='surfex',
                genv=self.conf.genv,
                gvar='master_prep_mpi',
            )
            print(self.ticket.prompt, 'PREP_uenv_tbx =', prep_uenv_tbx)
            print()

    def get_remote_inputs(self):

        self.sh.title('Get PREP from cache or archive')
        prep_tbi = vortex.input(
            local='PREP.nc',
            role='SnowpackInit',
            # MV : pour permettre de récupérer le PREP depuis une expérience indépendante
            # --> possibilité de renseigner 'prep_xpid' dans le fichier de conf
            experiment=self.conf.get('prep_xpid', self.conf.xpid),
            username=self.conf.get('prep_user', None),
            # MV : il faut définir la date de validité du fichier PREP qui par défaut
            # est la *datebegin* de simulation mais peut être arbitraire si 'date_prep' est renseigné
            date=self.conf.get('prep_date', self.conf.datebegin),
            # MV : Pour prévoir les cas où le PREP vient d'un vapp / vconf différent
            # de ceux de la tâche
            vapp=self.conf.get('prep_vapp', self.conf.vapp),
            vconf=self.conf.get('prep_vconf', self.conf.vconf),
            geometry=self.conf.geometry,
            nativefmt='netcdf',
            kind='PREP',
            model='surfex',
            namespace='vortex.multi.fr',
            vortex1=self.conf.get('prep_vortex1', False),
            namebuild='flat@cen',  # TODO : passer en variable de configuration
            block=self.conf.get('prep_block', 'prep'),
            # MV : La notion de "membre" pour le PREP est particulière dans le cas déterministe
            # - dans le cas général, le PREP n'est associé à aucun *membre*
            # - dans une simulation avec assimilation: la première initialisation est faite
            #   avec un unique fichier PREP pour tous les membres de simulation et les initialisations
            #   suivantes dépendent des membres sélectionnés par SODA.
            # Le cas ensembliste (parralélisation sur les membres, 1 PREP / membre)
            # doit être traité dans une tâche spécifique
            member=self.conf.get('prep_member', self.conf.get('member', None)),
            intent='inout',
            fatal=False,
        ),
        print(self.ticket.prompt, 'prep_tbi =', prep_tbi)
        print()

        if not prep_tbi[0]:
            super().get_remote_inputs()
            self.get_prep_exe()

    def get_local_inputs(self):
        if len(self.ctx.sequence.effective_inputs(role="SnowpackInit")) > 0:
            pass
        else:
            super().get_local_inputs()

    def algo(self):
        if len(self.ctx.sequence.effective_inputs(role="SnowpackInit")) > 0:
            pass
        else:
            myalgo = super().algo()
            return myalgo

    def launch_algo(self, algo, **kwargs):
        if len(self.ctx.sequence.effective_inputs(role="SnowpackInit")) > 0:
            pass
        else:
            super().launch_algo(algo, **kwargs)

    def put_outputs(self):

        self.sh.title('Put PREP to cache')
        prep_tbo = vortex.output(
            local='PREP.nc',
            role='SnowpackInit',
            experiment=self.conf.get('prep_xpid', self.conf.xpid),
            username=self.conf.get('prep_user', None),
            date=self.conf.get('prep_date', self.conf.datebegin),
            vapp=self.conf.get('prep_vapp', self.conf.vapp),
            vconf=self.conf.get('prep_vconf', self.conf.vconf),
            geometry=self.conf.geometry,
            nativefmt='netcdf',
            kind='PREP',
            model='surfex',
            namespace='vortex.cache.fr',
            namebuild='flat@cen',  # TODO : passer en variable de configuration
            block=self.conf.get('prep_block', 'prep'),
            member=self.conf.get('prep_member', self.conf.get('member', None)),
        ),
        print(self.ticket.prompt, 'prep_tbo =', prep_tbo)
        print()



