# -*- coding:Utf-8 -*-
"""
SURFEX/Crocus reanalysis in the "massif" geometry.
Add slopes to the SAFRAN "flat massif" FORCING files and launch the OFFLINE executable.
"""

from mkjob.nodes import Driver
import vortex
from vortex_cen.tasks.surfex.offline import Offline_MPI_Uenv
from vortex_cen.tasks.surfex.pre_process import _Preprocess


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            PreProcess(tag='preprocess', ticket=t, **kw),
            Offline_reanalysis(tag='offline', ticket=t, **kw),
        ],
        options=kw,
    )


class PreProcess(_Preprocess):
    """
    Task : PreProcess
    =================
    Pre-process SURFEX namelist.

    Inputs:
    -------
    - OPTIONS.nam : raw SURFEX namelist

    Outputs:
    --------
    - OPTIONS.nam : pre-processed SURFEX namelist
    """

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
            "surfex_uenv|uenv",
            "namelist_source",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
        ]
        overwrite = [
            "forcing",
        ]
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES, overwrite=overwrite)

    def get_remote_inputs(self):
        self.get_namelist_from_uenv()

    def get_local_inputs(self):
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')


class Offline_reanalysis(Offline_MPI_Uenv):
    """
    Task : Offline_reanalysis
    =========================
    Get all constant inputs (including the PGD file) from a User Environment.

    Inputs:
    -------
    - FORCING.nc files ('massif allslopes' geometry)
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

        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')
        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_pgd()
        self.get_executable()
        self.get_prep()

    def get_local_inputs(self):
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
