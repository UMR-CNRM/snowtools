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
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
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
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
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
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
            model          = 'surfex',
            kind           = 'coverparams',
            local          = 'ECOCLIMAP_II_EUROP.tgz',
            source         = 'ecoclimap2',
            gvar           = 'ECOCLIMAP_II_EUROP',
        )
        print(self.ticket.prompt, 'ecoclimap2_europ_tbi =', ecoclimap2_europ_tbi)
        print()

    def get_pgd_exe_from_uenv(self, mpi=True, fatal=True):

        if mpi:
            default_gvar = 'master_pgd_mpi'
        else:
            default_gvar = 'master_pgd_nompi'

        self.sh.title('Toolbox input PGD executable from uenv')
        pgd_tbx = vortex.executable(
            role           = 'Binary',
            kind           = 'buildpgd',
            local          = 'PGD',
            model          = 'surfex',
            genv           = self.conf.get('surfex_uenv', self.conf.uenv),
            gvar           = self.conf.get('pgd_gvar', default_gvar),
            fatal          = fatal,
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
    Task : _Pgd_Construct
    =====================

    Abstract task for the generation of ground physiography (PGD.nc file).

   Inputs:
    -------
    - FORCING file
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD executable

    Outputs:
    --------
    - PGD.nc (Ground physiography)

    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "geometry",
            "xpid",
            "uenv|surfex_uenv",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
            "ntasks",
            "nnodes",
            "nprocs",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get forcing file(s) and namelist in order to transform the namelist
        """
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

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
    Task : Pgd_Uenv_Pgd
    ===================

    Generation of ground physiography (PGD.nc file).
    Get PGD executable from Uenv

   Inputs:
    -------
    - FORCING file
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD executable

    Outputs:
    --------
    - PGD.nc (Ground physiography)
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "uenv|surfex_uenv",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get PGD executable from Uenv
        """
        super().get_remote_inputs()
        self.get_pgd_exe_from_uenv()


class Pgd_Local_Pgd(_Pgd_Construct):
    """
    Task : Pgd_Local_Pgd
    ====================

    Generation of ground physiography (PGD.nc file).
    Get PGD executable locally
    WARNING : The simulation's reproductibility can not be guaranteed with this task !

    Inputs:
    -------
    - FORCING file
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD executable

    Outputs:
    --------
    - PGD.nc (Ground physiography)
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "exesurfex",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get PGD executable locally
        """
        super().get_remote_inputs()
        self.get_pgd_exe_from_local_path()


class Pgd2D_Uenv_Pgd(_Pgd_Construct):
    """
    Task : Pgd2D_Uenv_Pgd
    =====================

    Generation of ground physiography (PGD.nc file).
    Get PGD executable from Uenv

    Inputs:
    -------
    - FORCING file
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD executable

    Outputs:
    --------
    - PGD.nc (Ground physiography)
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "uenv|surfex_uenv",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get PGD executable from Uenv
        """
        super().get_remote_inputs()
        self.get_2D_databases()
        self.get_pgd_exe_from_uenv()


class Pgd2D_Local_Pgd(_Pgd_Construct):
    """
    Task : Pgd2D_Local_Pgd
    ======================

    Generation of ground physiography (PGD.nc file).
    Get PGD executable locally

    WARNING : The simulation's reproductibility can not be guaranteed with this task !

    Inputs:
    -------
    - FORCING file
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD executable

    Outputs:
    --------
    - PGD.nc (Ground physiography)
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "uenv|surfex_uenv",
            "exesurfex",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        """
        Get PGD executable locally
        """
        super().get_remote_inputs()
        self.get_2D_databases()
        self.get_pgd_exe_from_local_path()


class GetPgd1D(_Pgd_Construct):
    """
    Task : GetPgd1D
    ===============

    Generation of ground physiography (PGD.nc file).
    If PGD.nc is available in cache or archive for the current experiment fetch it.
    If not, try to get it from an uenv.
    If not either, generate it by calling the methods from the mother class.

    WARNING : The simulation's reproductibility can not be guaranteed with this task !

    Inputs:
    -------
    - FORCING file
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD executable

    Outputs:
    --------
    - PGD.nc (Ground physiography)

    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "exesurfex",
            "pgdnc_gvar",
            "pgd_cache",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

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
    Task : GetPgd2D
    ===============

    Generation of ground physiography (PGD.nc file).
    Get Pgd file for 2D cases. For further documentation see GetPgd1D.

    WARNING : The simulation's reproductibility can not be guaranteed with this task !

    Inputs:
    -------
    - FORCING file
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from an execution of a "Preprocess_Task")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD executable

    Outputs:
    --------
    - PGD.nc (Ground physiography)
    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "uenv|surfex_uenv",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "exesurfex",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        input_pgd = self.pgd_avail()
        if not input_pgd:
            _Pgd_Construct.get_remote_inputs(self)
            self.get_2D_databases()
            self.get_pgd_exe()
