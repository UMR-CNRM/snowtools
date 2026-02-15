# -*- coding: utf-8 -*-
'''
'''

#from vortex.layout.nodes import Task
from vortex import toolbox
from vortex_cen.tasks.research_task_base import _CenResearchTask


class _Prep_Construct(_CenResearchTask):
    '''
    Abstract task for PREP step.

   Inputs:
    -------
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD.nc (Ground physiography coming from the cache ?)

    Outputs:
    --------
    - PREP.nc (initial conditions)

    Mandatory configuration variables:
    ----------------------------------
    :param geometry: *geometry* of the forcing file(s)
    :type geometry: str, footprints.stdtypes.FPList
    :param xpid: Experiment identifier (format "{experiment_name}@{user}")
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
    :param pgd_xpid: Experiment Identifier of the PGD file, if different from the task's XPID
    :type pgd_xpid: str
    :param pgd_vapp: *vapp* of the PGD file, if different from the task's *vapp*
    :type pgd_vapp: str
    :param pgd_vconf: *vconf* of the PGD file, if different from the task's *vconf*
    :type pgd_vconf: str
    :param dailyprep: TODO :comprendre avec Matthieu L les cas d'usages avec "dailyprep" (reforecast ?)
    :type dailyprep: bool
    :param namespace_out: Force specific namespace for output files (default: 'vortex.multi.fr')
    :type namespace_out: str
    '''
    def get_remote_inputs(self):
        """
        Get ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        Get drdt_bst_fit_60.nc, PGD.nc
        Do not get init_TG.nc because there are 2 possibilities
        """
        # Binary ECOCLIMAP I files are mandatory to run PREP and taken from the uenv
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

        # Binary ECOCLIMAP II files are mandatory to run PREP and taken from the uenv
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

        # Crocus metamorphism parameters mandatory to run PREP and taken from the uenv
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

        # PGD.nc mandatory to run PREP
        self.sh.title('Toolbox input PGD')
        pgd_tbi = toolbox.input(
            local          = 'PGD.nc',
            role           = 'SurfexClim',
            experiment     = self.conf.get('pgd_xpid', self.conf.xpid),
            vapp           = self.conf.get('pgd_vapp', self.conf.vapp),
            vconf          = self.conf.get('pgd_vconf', self.conf.vconf),
            geometry       = self.conf.geometry,
            nativefmt      = 'netcdf',
            kind           = 'pgdnc',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'pgd',
        ),
        print(self.ticket.prompt, 'pgd =', pgd_tbi)
        print()

    def get_local_inputs(self):
        """
        Get OPTIONS.nam which is always in cache
        """
        # Namelist mandatory to run PREP and taken from the cache
        self.sh.title('Toolbox input Namelist after modification')
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

    def algo(self):
        """
        Algo component to produce the PREP file if not found in the inputs
        """
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        self.sh.title('Toolbox algo PREP')
        PREP_tba = toolbox.algo(
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

    def put_remote_outputs(self):
        """
        Save the PREP file
        """
        #######################################################################
        #                               Backup                                #
        #######################################################################
        self.sh.title('Toolbox Output PREP')
        prep_tbo = toolbox.output(
            local      = 'PREP_[date:ymdh].nc',
            role       = 'SnowpackInit',
            experiment = self.conf.xpid,
            geometry   = self.conf.geometry,
            date       = self.conf.date,
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


class Prep_Uenv_TG_Uenv_Prep(_Prep_Construct):
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
        self.sh.title('Toolbox input init_TG from uenv')
        initTG_tbi = toolbox.input(
            role           = 'initial values of ground temperature',
            kind           = 'climTG',
            nativefmt      = 'netcdf',
            local          = 'init_TG.nc',
            geometry       = self.conf.geometry,
            # MV : Il faudra peut être utiliser une variable de conf différente de *genv* à terme pour permettre
            # de récupérer les autres "constantes" dans un genv commun et le binaire dans un environement géré par
            # le user
            genv           = self.conf.genv,
            gvar           = 'climtg_[geometry::tag]',
            model          = 'surfex',
        ),
        print(self.ticket.prompt, 'initTG_tbi =', initTG_tbi)
        print()

        self.sh.title('Toolbox input PREP executable from uenv')
        PREP_tbx = toolbox.executable(
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


class Prep_Local_TG_Uenv_Prep(_Prep_Construct):
    '''
     Get init_TG.nc locally and PREP executable from Uenv
    '''
    def get_remote_inputs(self):
        """
        Get init_TG.nc locally and PREP executable from Uenv
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################

        # MV : Comme discuté avec MF, il serait peut être plus pertinent de faire une tâche
        # spécifique pour la génération de init_TG qui check si un 'init_TG.nc' existe déjà pour
        # cet xpid et géométrie et le mette dans le cache local ou en génère un dans le cas contraire.
        # --> cela permettrait de simplifier les tâches de génération du PREP en allant systématiquement
        # chercher le fichier init_TG sur le cache

        self.sh.title('Toolbox input init_TG from local')
        initTG_tbi = toolbox.input(
            alternate      = 'initial values of ground temperature',
            kind           = 'climTG',
            nativefmt      = 'netcdf',
            local          = 'init_TG.nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'prep',
        ),
        print(self.ticket.prompt, 'initTG_tbi =', initTG_tbi)
        print()

        self.sh.title('Toolbox input PREP executable from uenv')
        PREP_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_prep_mpi',
        )
        print(self.ticket.prompt, 'PREP_tbx =', PREP_tbx)
        print()


class Prep_Uenv_TG_Local_Prep(_Prep_Construct):
    '''
    Get init_TG.nc from Uenv and PREP executable locally

    Supplementary mandatory configuration variables:
    ------------------------------------------------
    :param exesurfex: Absolute path pointing the a local directory containing the target PREP executable
    :type exesurfex: str
    '''
    def get_remote_inputs(self):
        """
        Get init_TG.nc from Uenv and PREP executable locally
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################

        self.sh.title('Toolbox input init_TG from local')
        initTG_tbi = toolbox.input(
            alternate      = 'initial values of ground temperature',
            kind           = 'climTG',
            nativefmt      = 'netcdf',
            local          = 'init_TG.nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'prep',
        ),
        print(self.ticket.prompt, 'initTG_tbi =', initTG_tbi)
        print()

        self.sh.title('Toolbox input PREP executable from local')
        PREP_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PREP"
        )
        print(self.ticket.prompt, 'PREP_tbx =', PREP_tbx)
        print()


class Prep_Local_TG_Local_Prep(_Prep_Construct):
    '''
    Get init_TG.nc and PREP executable both locally

    Supplementary mandatory configuration variables:
    ------------------------------------------------
    :param exesurfex: Absolute path pointing the a local directory containing the target PREP executable
    :type exesurfex: str
    '''
    def get_remote_inputs(self):
        """
        Get init_TG.nc and PREP executable both locally
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input init_TG from local')
        initTG_tbi = toolbox.input(
            alternate      = 'initial values of ground temperature',
            kind           = 'climTG',
            nativefmt      = 'netcdf',
            local          = 'init_TG.nc',
            experiment     = self.conf.xpid,
            geometry       = self.conf.geometry,
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',  # TODO : passer en variable de configuration
            block          = 'prep',
        ),
        print(self.ticket.prompt, 'initTG_tbi =', initTG_tbi)
        print()

        self.sh.title('Toolbox input PREP executable from local')
        PREP_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'prep',
            local          = 'PREP',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PREP"
        )
        print(self.ticket.prompt, 'PREP_tbx =', PREP_tbx)
        print()
