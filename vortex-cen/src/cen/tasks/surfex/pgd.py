# -*- coding: utf-8 -*-
'''
'''

from vortex import toolbox
from vortex.util.helpers import InputCheckerError
from vortex_cen.tasks.research_task_base import _CenResearchTask


class _Pgd_Construct(_CenResearchTask):
    '''
    Abstract task for the generation of ground physiography (PGD.nc file).

   Inputs:
    -------
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)

    Outputs:
    --------
    - PGD.nc (Ground physiography)

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
    :param namespace_out: Force specific namespace for output files (default: 'vortex.multi.fr')
    :type namespace_out: str
    '''
    def get_remote_inputs(self):
        """
        Get forcing file(s) and namelist in order to transform the namelist
        """
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

        """
        Get ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        Get drdt_bst_fit_60.nc, PGD.nc
        Do not get init_TG.nc because there are 2 possibilities
        """
        # Binary ECOCLIMAP I files are mandatory to run PGD and taken from the uenv
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

        # Binary ECOCLIMAP II files are mandatory to run PGD and taken from the uenv
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

        # Crocus metamorphism parameters mandatory to run PGD and taken from the uenv
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

    def get_local_inputs(self):
        """
        Get OPTIONS.nam which is always in cache
        """
        # Namelist mandatory to run PGD and taken from the cache
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
        Algo component to produce the PGD file if not found in the inputs
        """
        #######################################################################
        #                            Compute step                             #
        #######################################################################
        avail_forcings = self.ticket.context.sequence.effective_inputs(role='Forcing')
        if len(avail_forcings) > 0:
            firstforcing = avail_forcings[0]
        else:
            raise InputCheckerError('No FORCING file present, the task can not run properly')

        self.sh.title('Toolbox algo PGD')
        PGD_tba = toolbox.algo(
            kind         = 'pgd_from_forcing',
            # Le nom local de la ressource est fourni par le "container"
            forcingname  = firstforcing.rh.container.basename,
        )
        print(self.ticket.prompt, 'Toolbox algo pgd=', PGD_tba)
        print()
        return PGD_tba

    def launch_algo(self, algo, **kw):
        """
        Run PGD algo component.
        """
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        self.component_runner(algo, executable)
        #self.component_runner(tbalgo3, tbx2, mpiopts = dict(nnodes=1, nprocs=1, ntasks=1))
        # ntasks = 1 !!!! WTF !!!

    def put_outputs(self):
        """
        Save the PGD file
        """
        #######################################################################
        #                               Backup                                #
        #######################################################################
        self.sh.title('Toolbox Output PGD')
        pgd_tbo = toolbox.output(
            local      = 'PGD.nc',
            role       = 'SurfexClim',
            experiment = self.conf.xpid,
            geometry   = self.conf.geometry,
            nativefmt  = 'netcdf',
            kind       = 'pgdnc',
            model      = 'surfex',
            namespace  = 'vortex.multi.fr',
            namebuild  = 'flat@cen',  # TODO : passer en variable de configuration
            block      = 'pgd',
        ),
        # MF: in surfex_task.py:       member = self.conf.member if hasattr(self.conf, 'member') else None,
        # MV : c'était un bug introduit par mon commit #21915d5748ec0f80095edced4fc7ee6790a8faa4
        print(self.ticket.prompt, 'pgd_tbo =', pgd_tbo)
        print()


class _Pgd2D_Construct(_Pgd_Construct):
    '''
    Abstract task for 2D PGD step.

   Inputs:
   :param genv2D: 2D Environment in which the following resources are to be retrieved :
        - Sand_DB.bin and Sand_DB.hdr
        - Clay_DB.bin and Clay_DB.hdr
        - ECOCLIMAP_II_EUROP.dir and ECOCLIMAP_II_EUROP.hdr

    # MF: in surfex_task.py, self.conf.genv2D = uenv:pgd.003@SURFEX_CEN
    :type genv2D: str
    '''
    def get_remote_inputs(self):
        """
        Get PGD executable from Uenv
        """
        super().get_remote_inputs()
        # Binary Sand files are mandatory to run SURFEX for PGD construction in simu2D
        self.sh.title('Toolbox input sand')
        sand_tbi = toolbox.input(
            role           = 'SandDB',
            format         = 'dir/hdr',
            genv           = self.conf.genv2D,
            model          = 'surfex',
            kind           = 'sand',  # 'database'
            local          = 'sand_DB.tgz',
            source         = 'sand_DB',
            gvar           = 'sand_DB',
        )
        print(self.ticket.prompt, 'sand_tbi =', sand_tbi)
        print()

        # Binary Clay files are mandatory to run SURFEX for PGD construction in simu2D
        self.sh.title('Toolbox input clay')
        clay_tbi = toolbox.input(
            role           = 'ClayDB',
            format         = 'dir/hdr',
            genv           = self.conf.genv2D,
            model          = 'surfex',
            kind           = 'clay',
            local          = 'clay_DB.tgz',
            source         = 'clay_DB',
            gvar           = 'clay_DB',
        )
        print(self.ticket.prompt, 'clay_tbi =', clay_tbi)
        print()

        # EcoclimapII_europ files are mandatory to run SURFEX for PGD construction in simu2D
        self.sh.title('Toolbox input ecoclimap2_europ')
        ecoclimap2_europ_tbi = toolbox.input(
            role           = 'EcoclimapIIEurop',
            format         = 'dir/hdr',
            genv           = self.conf.genv2D,
            model          = 'surfex',
            kind           = 'coverparams',
            local          = 'ECOCLIMAP_II_EUROP.tgz',
            source         = 'ecoclimap2',
            gvar           = 'ECOCLIMAP_II_EUROP',
        )
        print(self.ticket.prompt, 'ecoclimap2_europ_tbi =', ecoclimap2_europ_tbi)
        print()


class Pgd_Uenv_Pgd(_Pgd_Construct):
    '''
    Get PGD executable from Uenv
    '''
    def get_remote_inputs(self):
        """
        Get PGD executable from Uenv
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input PGD executable from uenv')
        PGD_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_pgd_mpi',
        )
        print(self.ticket.prompt, 'PGD_tbx =', PGD_tbx)
        print()


class Pgd_Local_Pgd(_Pgd_Construct):
    '''
    Get PGD executable locally

    Supplementary mandatory configuration variables:
    ------------------------------------------------
    :param exesurfex: Absolute path pointing the a local directory containing the target PGD executable
    :type exesurfex: str
    '''
    def get_remote_inputs(self):
        """
        Get PGD executable locally
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input PGD executable from local')
        PGD_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PGD"
        )
        print(self.ticket.prompt, 'PGD_tbx =', PGD_tbx)
        print()


class Pgd2D_Uenv_Pgd(_Pgd2D_Construct):
    '''
    Get PGD executable from Uenv
    '''
    def get_remote_inputs(self):
        """
        Get PGD executable from Uenv
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input PGD executable from uenv')
        PGD_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_pgd_mpi',
        )
        print(self.ticket.prompt, 'PGD_tbx =', PGD_tbx)
        print()


class Pgd2D_Local_Pgd(_Pgd2D_Construct):
    '''
    Get PGD executable locally
    '''
    def get_remote_inputs(self):
        """
        Get PGD executable locally
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input PGD executable from local')
        PGD_tbx = toolbox.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PGD"
        )
        print(self.ticket.prompt, 'PGD_tbx =', PGD_tbx)
        print()
