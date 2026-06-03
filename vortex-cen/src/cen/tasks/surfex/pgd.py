# -*- coding: utf-8 -*-
"""
"""

import vortex
from vortex.util.helpers import InputCheckerError
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.params import SurfexParamsMixin


class _Pgd_Construct(SurfexParamsMixin, _CenResearchTask):
    """
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

    * ``geometry`` *geometry* of the forcing file(s)
      type: str, footprints.stdtypes.FPList
    * ``xpid`` Experiment identifier
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
    * ``namespace_out`` Force specific namespace for output files (default: 'vortex.multi.fr')
      type: str
    """
    def get_remote_inputs(self):
        """
        Get forcing file(s) and namelist in order to transform the namelist
        """
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                         alternate=self.conf.get("forcing_alternate", True))

        """
        Get ecoclimapI_covers_param.bin, ecoclimapII_eu_covers_param.bin,
        Get drdt_bst_fit_60.nc
        """
        self.get_ecoclimap()
        self.get_drdt_bst_fit()

    def get_local_inputs(self):
        """
        Get OPTIONS.nam which is always in the user's local cache because it comes
        from a namelist pre-processing task.
        """
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
        pgd_tba = vortex.task(
            kind         = 'pgd_from_forcing',
            # Le nom local de la ressource est fourni par le "container"
            forcingname  = firstforcing.rh.container.basename,
        )
        print(self.ticket.prompt, 'Toolbox algo pgd=', pgd_tba)
        print()
        return pgd_tba

    def launch_algo(self, algo):
        """
        Run PGD algo component.
        """
        self.launch_executable(algo)

    def put_outputs(self):
        """
        Save the PGD file
        """
        #######################################################################
        #                               Backup                                #
        #######################################################################
        self.sh.title('Toolbox Output PGD')
        pgd_tbo = vortex.output(
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


class Pgd2DMixin:
    """
    Mixin for 2D PGDs.

    Configuration variables:
    ------------------------
    *``genv2D`` user Environment in which the following 2D specific resources are to be retrieved :
        - Sand_DB.bin and Sand_DB.hdr
        - Clay_DB.bin and Clay_DB.hdr
        - ECOCLIMAP_II_EUROP.dir and ECOCLIMAP_II_EUROP.hdr

    """
    def get_2D_databases(self):
        """
        Get Sand_DB.bin and Sand_DB.hdr, Clay_DB.bin and Clay_DB.hdr,
        ECOCLIMAP_II_EUROP.dir and ECOCLIMAP_II_EUROP.hdr from Uenv
        """
        # Binary Sand files are mandatory to run SURFEX for PGD construction in simu2D
        self.sh.title('Toolbox input sand')
        sand_tbi = vortex.input(
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
        clay_tbi = vortex.input(
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
        ecoclimap2_europ_tbi = vortex.input(
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
    """
    Get PGD executable from Uenv
    """
    def get_remote_inputs(self):
        """
        Get PGD executable from Uenv
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input PGD executable from uenv')
        pgd_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_pgd_mpi',
        )
        print(self.ticket.prompt, 'PGD_tbx =', pgd_tbx)
        print()


class Pgd_Local_Pgd(_Pgd_Construct):
    """
    Get PGD executable locally

    Supplementary mandatory configuration variables:
    ------------------------------------------------
    * ``exesurfex`` Absolute path pointing the local directory containing the target PGD executable
      type: str
    """
    def get_remote_inputs(self):
        """
        Get PGD executable locally
        """
        super().get_remote_inputs()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input PGD executable from local')
        pgd_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PGD"
        )
        print(self.ticket.prompt, 'PGD_tbx =', pgd_tbx)
        print()


class Pgd2D_Uenv_Pgd(Pgd2DMixin, _Pgd_Construct):
    """
    Get PGD executable from Uenv
    """
    def get_remote_inputs(self):
        """
        Get PGD executable from Uenv
        """
        super().get_remote_inputs()
        self.get_2D_databases()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input PGD executable from uenv')
        pgd_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            genv           = self.conf.genv,
            gvar           = 'master_pgd_mpi',
        )
        print(self.ticket.prompt, 'PGD_tbx =', pgd_tbx)
        print()


class Pgd2D_Local_Pgd(Pgd2DMixin, _Pgd_Construct):
    """
    Get PGD executable locally
    """
    def get_remote_inputs(self):
        """
        Get PGD executable locally
        """
        super().get_remote_inputs()
        self.get_2D_databases()
        #######################################################################
        #                             Fetch steps                             #
        #######################################################################
        self.sh.title('Toolbox input PGD executable from local')
        pgd_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PGD"
        )
        print(self.ticket.prompt, 'PGD_tbx =', pgd_tbx)
        print()


class GetPgd1D(_Pgd_Construct):
    """
    If PGD.nc is available in cache or archive for the current experiment fetch it.
    If not, try to get it from an uenv.
    If not either, generate it by calling the methods from the mother class.

    Configuration Parameters:
    -------------------------

    * ``xpid`` experiment id. !!! Do not use ids with 4 letters !!!
    * ``vapp`` application name !!! Implicit, depends on the driver tree !!!
    * ``vconf`` application configuration. !!! Implicit, depends on the driver tree !!!
    * ``geometry`` geometry of the PGD.nc file. Logically the same as for the rest of the simulation.

     Optional Configuration Parameters:
    ----------------------------------

    * ``pgd_xpid`` experiment id the PGD.nc file should be fetched from. Defaults to ``xpid``.
    * ``pgd_user`` name of the user who produced the PGD file
    * ``pgd_vapp`` vapp the PGD.nc file should be fetched from. Defaults to ``vapp``.
    * ``pgd_vconf`` vconf the PGD.nc file should be fetched from. Defaults to ``vconf``.
    * ``genv_pgd`` uenv to look for the PGD.nc file if the PGD.nc file should come from an uenv.
    * ``gvar_pgd`` key to look up the PGD.nc file in the uenv if the file should come from there.
        Defaults to 'pgd_[geometry::tag]'.
    * ``genv`` uenv to look for the ecoclimap Surfex cover parameters, Crocus metamorphism parameters
        and PGD executable in case the PGD.nc needs to be calculated.
    * ``forcing_source_app`` in case the PGD.nc needs to be calculated
        and the forcing comes from the S2M reanalysis
        (example: arpege)
    * ``forcing_source_conf`` in case the PGD.nc needs to be calculated
        and the forcing comes from the S2M reanalysis
        (example: 4dvarfr)
    * ``forcing_source`` in case the PGD.nc needs to be calculated
        and the forcing comes from the S2M reanalysis (yearly sub-periods)
        (example: era5)
    * ``forcing_localname`` in case the PGD.nc needs to be calculated
        and the forcing comes from the S2M reanalysis
        (example: [datebegin:ymdh]_[dateend:ymdh]/FORCING_IN.nc)
    * ``exesurfex`` path to the surfex executable in case the PGD.nc needs to be calculated and the executable comes
        from a local directory.
    * ``nnodes`` number of nodes to use to run the PGD executable with MPI if the PGD.nc needs to be calculated.
    * ``nprocs`` number of processors to be used to run the PGD executable with MPI.
    (example: 60, might be 80 for bigger simulations)
    * ``ntasks`` number of tasks per node to use to run the PGD executable with MPI.
    (in general equals the ``nprocs`` since we are not doing multithreading, 60 in our example)
    * ``openmp`` number of openmp threads to use to run the PGD executable with MPI.
    (normally 1, since we are not doing multithreading)

    """
    def pgd_avail(self):
        """
        Try to get PGD.nc from cache or archive. If not available try to get PGD.nc from uenv.

        :return: True if the PGD.nc file can be fetched from the uenv, cache or archive, False otherwise.
        :rtype: bool
        """
        # try to get PGD.nc from cache or archive
        self.sh.title('Toolbox input PGD from cache or archive')
        pgd_cache_tbi = vortex.input(
            local      = 'PGD.nc',
            role       = 'SurfexClim',
            experiment = self.conf.get('pgd_xpid', self.conf.xpid),
            username   = self.conf.get('pgd_user', None),
            vapp       = self.conf.get('pgd_vapp', self.conf.vapp),
            vconf      = self.conf.get('pgd_vconf', self.conf.vconf),
            geometry   = self.conf.geometry,
            nativefmt  = 'netcdf',
            kind       = 'pgdnc',
            model      = 'surfex',
            namespace  = 'vortex.multi.fr',
            namebuild  = 'flat@cen',  # TODO : passer en variable de configuration
            block      = 'pgd',
            fatal      = False,
        ),
        print(self.ticket.prompt, 'pgd cache or archive =', pgd_cache_tbi)
        print()

        # try to get PGD.nc from uenv
        if not pgd_cache_tbi[0] and hasattr(self.conf, 'genv_pgd'):
            self.sh.title('Toolbox input PGD from uenv')
            pgd_uenv_tbi = vortex.input(
                local='PGD.nc',
                role='SurfexClim',
                geometry=self.conf.geometry,
                nativefmt='netcdf',
                kind='pgdnc',
                model='surfex',
                genv=self.conf.genv_pgd,
                gvar=self.conf.get('gvar_pgd', 'pgd_[geometry::tag]'),
                # TODO: I'm not sure about the "area". It used to be "tag"
                # but "tag" does not exist in geometries_vortex2.ini @vernaym: should it be area, tag or nothing?
                fatal=False,
            ),
            print(self.ticket.prompt, 'pgd uenv =', pgd_uenv_tbi)
            print()

        if len(self.ctx.sequence.effective_inputs(role="SurfexClim")) == 0:
            return False
        else:
            return True

    def get_pgd_exe(self):
        """
        get PGD executable from uenv or local path
        """
        if hasattr(self.conf, 'exesurfex'):
            self.sh.title('Toolbox input PGD executable from local')
            pgd_local_tbx = vortex.executable(
                role='Binary',
                kind='buildpgd',
                local='PGD',
                model='surfex',
                remote=self.conf.exesurfex + "/PGD"
            )
            print(self.ticket.prompt, 'PGD_tbx =', pgd_local_tbx)
            print()

        else:
            self.sh.title('Toolbox input PGD executable from uenv')
            pgd_uenv_tbx = vortex.executable(
                role='Binary',
                kind='buildpgd',
                local='PGD',
                model='surfex',
                genv=self.conf.genv,
                gvar='master_pgd_mpi',
            )
            print(self.ticket.prompt, 'PGD_uenv_tbx =', pgd_uenv_tbx)
            print()

    def get_remote_inputs(self):

        input_pgd = self.pgd_avail()
        if not input_pgd:
            super().get_remote_inputs()
            self.get_pgd_exe()

    def get_local_inputs(self):
        if len(self.ctx.sequence.effective_inputs(role="SurfexClim")) > 0:
            pass
        else:
            super().get_local_inputs()

    def algo(self):
        if len(self.ctx.sequence.effective_inputs(role="SurfexClim")) > 0:
            pass
        else:
            myalgo = super().algo()
            return myalgo

    def launch_algo(self, algo):
        if len(self.ctx.sequence.effective_inputs(role="SurfexClim")) > 0:
            pass
        else:
            super().launch_algo(algo)

    def put_outputs(self):
        self.sh.title('Toolbox Output PGD')
        pgd_tbo = vortex.output(
            local       = 'PGD.nc',
            role        = 'SurfexClim',
            experiment  = self.conf.xpid,
            geometry    = self.conf.geometry,
            nativefmt   = 'netcdf',
            kind        = 'pgdnc',
            model       = 'surfex',
            namespace   = 'vortex.cache.fr',
            namebuild   = 'flat@cen',  # TODO : passer en variable de configuration
            block       = 'pgd',
        ),
        print(self.ticket.prompt, 'pgd_tbo =', pgd_tbo)
        print()


class GetPgd2D(Pgd2DMixin, GetPgd1D):
    """
    Get Pgd file for 2D cases. For further documentation see GetPgd1D.
    """

    def get_remote_inputs(self):

        input_pgd = self.pgd_avail()
        if not input_pgd:
            _Pgd_Construct.get_remote_inputs(self)
            self.get_2D_databases()
            self.get_pgd_exe()
