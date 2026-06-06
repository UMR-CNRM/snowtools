# -*- coding:Utf-8 -*-

"""
The "surfex" driver allows to launch deterministic SURFEX/Crocus simulations in a research context
based on any meteorological forcing file.
It does not guarantee the reproductibility of the simulations due to a loose user control
on the input files : missing inputs files will be looked for on alternate locations and will
enventually be generated if necessary and possible.

"""

from mkjob.nodes import Driver
from vortex_cen.tasks.surfex.offline import _Offline_MPI
from vortex_cen.tasks.surfex.pre_process import _Preprocess
from vortex_cen.tasks.surfex.pgd import GetPgd1D
from vortex_cen.tasks.surfex.prep import GetPrep
from vortex_cen.tasks.surfex.init_clim_ground_temperature import GetClimGroundTemperature
from vortex_cen.tasks.configuration_variables import forcing, prep, pgd_cache, pgd_uenv


def setup(t, **kw):
    return Driver(
        tag='surfex',
        ticket=t,
        nodes=[
            PreProcess(tag='preprocess', ticket=t, **kw),
            MakeClimGroundTemperature(tag='inittg', ticket=t, **kw),
            GetPgd1D(tag='pgd', ticket=t, **kw),
            GetPrep(tag='prep', ticket=t, **kw),
            Offline(tag='offline', ticket=t, **kw),
        ],
        options=kw,
    )


class MakeClimGroundTemperature(GetClimGroundTemperature):

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "climground",  # TODO : ajouter les dépendances
        ] + prep

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def process(self):
        if self.conf.get('climground', False):
            # Check if a PREP file already exists
            prep = self.get_prep(fatal=False)
            # If no PREP file found, launch the generation of init_TG file
            if not prep[0]:
                super().process()
        else:
            pass


class PreProcess(_Preprocess):

    def __init__(self, **kw):

        super().__init__(**kw)
        MANDATORY_CONFIGURATION_VARIABLES = [
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "namelist_path",
            "surfex_uenv",
            "uenv",
        ]

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):
        if 'namelist_path' in self.conf:
            self.get_namelist_from_path()
        else:
            self.get_namelist_from_uenv()
        self.get_forcing(localname='FORCING_[datebegin:ymdh]_[dateend:ymdh].nc')


class Offline(_Offline_MPI):

    def __init__(self, **kw):

        super().__init__(**kw)

        MANDATORY_CONFIGURATION_VARIABLES = [
            "uenv",
            # "uenv|surfex_uenv",  # Gérer la syntaxe "OR" dans mkjob-help
        ]

        OPTIONAL_CONFIGURATION_VARIABLES = [
            "surfex_uenv",
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
