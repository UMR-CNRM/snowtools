# -*- coding:Utf-8 -*-
"""
Generation of SURFEX initial conditions file (PREP.nc)
"""

import vortex
from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.prep import _PrepConstruct


def setup(t, **kw):
    return Driver(
        tag='prep',
        ticket=t,
        nodes=[
            MakePrep(tag='makeprep', ticket=t, **kw),
        ],
        options=kw,
    )


class MakePrep(_PrepConstruct):
    """
    Task : MakePrep
    ===============

    Force generation of initial conditions (PREP.nc file).
    All input file come from a UEnv, so that they must have been properly generated and archived.

    Inputs:
    -------

    * ``OPTIONS.nam`` SURFEX namelist
    * ``ecoclimapI_covers_param.bin`` and ``ecoclimapII_eu_covers_param.bin`` (binaries for vegetation generation)
    * ``drdt_bst_fit_60.nc`` (Crocus metamorphism parameters)
    * ``Init_TG.nc`` Initial values of ground temperature
    * ``PGD.nc`` Ground physiography

    Outputs:
    --------
    - PREP.nc (initial conditions)

    """

    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "geometry",
            "uenv|surfex_uenv",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "namelist_source",
        ]
        overwrite = [
            "pgd_cache",
        ]
        super().__init__(**kw)
        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES,
                overwrite=overwrite)

    def get_init_TG(self):
        self.get_init_TG_from_uenv()

    def get_namelist(self):
        self.get_namelist_from_uenv()

    def get_prep_executable(self):
        self.get_prep_exe_from_uenv()

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
