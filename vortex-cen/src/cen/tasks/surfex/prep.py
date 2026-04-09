# -*- coding: utf-8 -*-
"""
"""

#from vortex.layout.nodes import Task
import vortex
from vortex.layout.dataflow import SectionFatalError
from vortex_cen.tasks.research_task_base import _CenResearchTask


class _Prep_Construct(_CenResearchTask):
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

    Optionnal configuration variables (other than forcing-specific ones):
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
        Do not get init_TG.nc because there are 2 possibilities
        """
        # Binary ECOCLIMAP I files are mandatory to run PREP and taken from the uenv
        self.sh.title('Toolbox input ecoclimap1')
        ecoclimap1_tbi = vortex.input(
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
        ecoclimap2_tbi = vortex.input(
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
        drdt_bst_fit_tbi = vortex.input(
            role            = 'Parameters for F06 metamorphism',
            kind            = 'ssa_params',
            genv            = self.conf.genv,
            nativefmt       = 'netcdf',
            local           = 'drdt_bst_fit_60.nc',
            model           = 'surfex',
        )
        print(self.ticket.prompt, 'drdt_bst_fit_60 =', drdt_bst_fit_tbi)
        print()


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
            initTG_cache_tbi = vortex.input(
                role="InitialValuesOfGroundTemperature",
                kind='climTG',
                nativefmt='netcdf',
                local='init_TG.nc',
                experiment=self.conf.xpid_tg,
                geometry=self.conf.geometry,
                model='surfex',
                namespace='vortex.cache.fr',
                namebuild='flat@cen',  # TODO : passer en variable de configuration
                block='prep',
                fatal=True,
            ),
            print(self.ticket.prompt, 'initTG_tbi =', initTG_cache_tbi)
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
        try:
            self.sh.title('Toolbox input PGD from cache')
            pgd_tbi = vortex.input(
                local='PGD.nc',
                role='SurfexClim',
                experiment=self.conf.get('pgd_xpid', self.conf.xpid),
                vapp=self.conf.get('pgd_vapp', self.conf.vapp),
                vconf=self.conf.get('pgd_vconf', self.conf.vconf),
                geometry=self.conf.geometry,
                nativefmt='netcdf',
                kind='pgdnc',
                model='surfex',
                namespace='vortex.cache.fr',
                namebuild='flat@cen',  # TODO : passer en variable de configuration
                block='pgd',
                fatal=True,
            ),
            print(self.ticket.prompt, 'pgd =', pgd_tbi)
            print()
        except SectionFatalError as e:
            print('Unable to get PGD.nc from cache. Make sure that your driver '
                  'has a node corresponding to the GetPgd1D task '
                  'before executing the Prep task and that the pgd_xpid values in the '
                  'corresponding configuration sections match. '
                  'Or that the Pgd_Uenv_Pgd or Pgd_Local_Pgd task '
                  'has been run recently for the given experiment (pgd_xpid).')
            raise e

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
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        self.component_runner(algo, executable)
        #self.component_runner(tbalgo3, tbx2, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))
        # ntasks = 1 !!!! WTF !!!

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


class Prep_Uenv_Prep(_Prep_Construct):
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



class Prep_Local_Prep(_Prep_Construct):
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
        PREP_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PREP"
        )
        print(self.ticket.prompt, 'PREP_tbx =', PREP_tbx)
        print()

