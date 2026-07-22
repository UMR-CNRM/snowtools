# -*- coding:Utf-8 -*-

"""
The "escroc" driver allows to launch multi-physics SURFEX/Crocus simulations in a research context
based on any meteorological forcing file(s).
WARNING : It does not guarantee the reproductibility of the simulations due to a loose user control
on the input files : missing inputs files will be looked for on alternate locations and will
enventually be generated if necessary and possible.

"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.offline_ensemble import Escroc
from vortex_cen.tasks.surfex.pre_process import PreprocessNamelist
from vortex_cen.tasks.surfex.pgd import FetchPgdOrMake
from vortex_cen.tasks.surfex.prep import FetchPrepFileOrMake
from vortex_cen.tasks.surfex.init_clim_ground_temperature import MakeClimGroundTemperatureIfNoPrep


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            PreprocessNamelist(tag='preprocess', ticket=t, **kw),
            MakeClimGroundTemperatureIfNoPrep(tag='inittg', ticket=t, **kw),
            FetchPgdOrMake(tag='pgd', ticket=t, **kw),
            FetchPrepFileOrMake(tag='prep', ticket=t, **kw),
            EscrocResearch(tag='escroc_task', ticket=t, **kw),
        ],
        options=kw,
    )



class EscrocResearch(Escroc):
    """
    Task : EscrocResearch
    =====================

    SURFEX/OFFLINE documentation : https://umr-cnrm.github.io/snowtools-doc/misc/surfex.html

    Inputs:
    -------
    - FORCING.nc files(s) (near-surface meteorological conditions during the simulation period)
    - OPTIONS.nam ready-to-use SURFEX namelist (coming from the execution of the "PreProcess")
    - ecoclimapI_covers_param.bin and ecoclimapII_eu_covers_param.bin (binaries for vegetation generation)
    - drdt_bst_fit_60.nc (Crocus metamorphism parameters)
    - PGD.nc (Ground physiography) retrieved or produced by the GetPgd1D task
    - PREP.nc (initial conditions) retrieved or produced by the GetPrep task

    Outputs:
    --------
    - PRO.nc Snowpack simulations covering the entire simulation period
    - PREP.nc SURFEX/Crocus model state variables at the end of the simulation
    """

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = [
            "surfex_uenv|uenv",
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "exesurfex",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        self.get_ecoclimap()
        self.get_drdt_bst_fit()
        self.get_executable()

    def get_local_inputs(self):
        # Get PGD and PREP locally because they have been retrieved or produced by a previous task
        self.get_pgd()
        self.get_prep()
        # Get namelist from the preprocess task output
        self.get_namelist_from_cache()
        # Get FORCING locally because they have already been retrieved by the preprocess task
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')

    def get_executable(self):

        if "exesurfex" in self.conf:
            self.get_executable_from_path()
        else:
            self.get_executable_from_uenv()
