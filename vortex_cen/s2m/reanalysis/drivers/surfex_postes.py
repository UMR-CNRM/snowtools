# -*- coding:Utf-8 -*-
"""
SURFEX/Crocus reanalysis in the "postes" geometry.
Launch the OFFLINE executable with forcing files produced by the "concatenation_postes" driver.
"""

from mkjob.nodes import Driver
import vortex
from vortex_cen.tasks.surfex.offline import Offline_Mpi_Uenv
from vortex_cen.tasks.surfex.pre_process import PreprocessNamelist


def setup(t, **kw):
    return Driver(
        tag='surfex_postes',
        ticket=t,
        nodes=[
            PreprocessNamelist(tag='preprocess', ticket=t, **kw),
            Offline_reanalysis_postes(tag='offline', ticket=t, **kw),
        ],
        options=kw,
    )


class Offline_reanalysis_postes(Offline_Mpi_Uenv):
    """
    Task : Offline_reanalysis_postes
    ================================
    - Get all constant inputs (including the PGD file) from a User Environment.
    - Get forcing file(s) on a compute node (step.02) because it comes from the
      output of a previous execution of the "Shadows" task.

    Inputs:
    -------
    - FORCING.nc files ('postes' geometry)
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from the execution of the "PreProcess" task)
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD.nc (Ground physiography)
    - PREP.nc (initial conditions)

    Outputs:
    --------
    - PRO.nc Snowpack simulations covering the entire simulation period
    - PREP.nc SURFEX/Crocus model state variables at the end of the simulation
    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "prep",
            "pgd_cache",
            "member",
            "io_duration",
            "namespace_out",
            "august_threshold",
            "offline_gvar",
        ]
        overwrite = [
            "forcing",
        ]
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES, overwrite=overwrite)

    def get_remote_inputs(self):

        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        _ = self.get_pgd_file_from_uenv()
        self.get_executable()
        _ = self.get_prep_file_from_cache_or_archive(fatal=True, cache_only=False)

    def get_local_inputs(self):
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')
        self.get_namelist_from_cache()

    def get_pgd(self):
        """
        Get PGD file from a User Environment (reanalysis case only !)
        """
        self.sh.title('Input PGD')
        pgd = vortex.input(
            role           = 'SurfexClim',
            kind           = 'pgdnc',
            nativefmt      = 'netcdf',
            model          = 'surfex',
            local          = 'PGD.nc',
            geometry       = self.conf.geometry,
            genv           = self.conf.uenv,
            gvar           = 'PGD_[geometry:tag]',
        ),
        print(self.ticket.prompt, 'PGD =', pgd)
        print()
