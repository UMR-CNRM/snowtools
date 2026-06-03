# -*- coding: utf-8 -*-
"""
"""

import vortex
from vortex.util.helpers import InputCheckerError
from vortex_cen.tasks.research_task_base import _CenResearchTask
from vortex_cen.tasks.surfex.commons import SurfexCommonsMixin


class PgdCommonsMixin(SurfexCommonsMixin):
    """
    Mixin methods for PGD binary IOs.

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
            genv           = self.conf.get('genv2D', self.conf.genv),
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
            genv           = self.conf.get('genv2D', self.conf.genv),
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
            genv           = self.conf.get('genv2D', self.conf.genv),
            model          = 'surfex',
            kind           = 'coverparams',
            local          = 'ECOCLIMAP_II_EUROP.tgz',
            source         = 'ecoclimap2',
            gvar           = 'ECOCLIMAP_II_EUROP',
        )
        print(self.ticket.prompt, 'ecoclimap2_europ_tbi =', ecoclimap2_europ_tbi)
        print()

    def get_pgd_exe_from_uenv(self):
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

    def get_pgd_exe_from_local_path(self):
        self.sh.title('Toolbox input PGD executable from local path')
        pgd_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            remote         = self.conf.exesurfex + "/PGD"
        )
        print(self.ticket.prompt, 'PGD_tbx =', pgd_tbx)
        print()


class _Pgd_Construct(PgdCommonsMixin, _CenResearchTask):
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
        self.get_namelist_from_cache()

    def algo(self):
        """
        Algo component to produce the PGD file if not found in the inputs
        """
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
        # Pour un exécution de binaire, il faut donner l'objet "exécutable" associé (récupéré par la commande
        # vortex.executable(...))
        executable = [tbx.rh for tbx in self.ticket.context.sequence.executables()]
        #
        # MV : Il faudra également pouvoir fournir le nombre de process et le nombre de tâches via le fichier de conf
        # TODO : réfléchir à la procédure pour définir des valeurs par défaut en fonction du domaine comme c'est
        # le cas actuellement
        # TODO : S'assurer que ce qui suit fonctionne avec un executable compilé sans MPI,
        # ou prévoir un switch MPI / NOMPI
        self.component_runner(
            algo,
            executable,
            mpiopts=dict(
                nnodes=self.conf.get('nnodes', 1),
                nprocs=self.conf.get('nprocs', 1),
                ntasks=self.conf.get('ntasks', 1),
            )
        )

    def put_outputs(self):
        """
        Save the PGD file
        """
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


class Pgd_Uenv_Pgd(_Pgd_Construct):
    """
    Get PGD executable from Uenv
    """
    def get_remote_inputs(self):
        """
        Get PGD executable from Uenv
        """
        super().get_remote_inputs()
        self.get_pgd_exe_from_uenv()


class Pgd_Local_Pgd(_Pgd_Construct):
    """
    Get PGD executable locally

    WARNING : The simulation's reproductibility can not be guaranteed with this task !

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
        self.get_pgd_exe_from_local_path()


class Pgd2D_Uenv_Pgd(_Pgd_Construct):
    """
    Get PGD executable from Uenv
    """
    def get_remote_inputs(self):
        """
        Get PGD executable from Uenv
        """
        super().get_remote_inputs()
        self.get_2D_databases()
        self.get_pgd_exe_from_uenv()


class Pgd2D_Local_Pgd(_Pgd_Construct):
    """
    Get PGD executable locally

    WARNING : The simulation's reproductibility can not be guaranteed with this task !
    """
    def get_remote_inputs(self):
        """
        Get PGD executable locally
        """
        super().get_remote_inputs()
        self.get_2D_databases()
        self.get_pgd_exe_from_local_path()


class GetPgd1D(_Pgd_Construct):
    """
    If PGD.nc is available in cache or archive for the current experiment fetch it.
    If not, try to get it from an uenv.
    If not either, generate it by calling the methods from the mother class.

    WARNING : The simulation's reproductibility can not be guaranteed with this task !

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
        pgd = self.get_pgd_from_cache_or_archive(fatal=False)

        # try to get PGD.nc from uenv
        if not pgd[0]:
            self.get_pgd_from_uenv(fatal=False)

        if len(self.ctx.sequence.effective_inputs(role="SurfexClim")) == 0:
            return False
        else:
            return True

    def get_pgd_exe(self):
        """
        get PGD executable from uenv or local path
        """
        if hasattr(self.conf, 'exesurfex'):
            self.get_pgd_exe_from_local_path()
        else:
            self.get_pgd_exe_from_uenv()

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


class GetPgd2D(GetPgd1D):
    """
    Get Pgd file for 2D cases. For further documentation see GetPgd1D.

    WARNING : The simulation's reproductibility can not be guaranteed with this task !
    """

    def get_remote_inputs(self):

        input_pgd = self.pgd_avail()
        if not input_pgd:
            _Pgd_Construct.get_remote_inputs(self)
            self.get_2D_databases()
            self.get_pgd_exe()
