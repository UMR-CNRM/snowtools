# -*- coding: utf-8 -*-
"""
reanalysis.py
--------------

.. autoclass:: ForcingPerturbations
   :no-members:
   :class-doc-from: class
   :show-inheritance:
"""

import vortex
import footprints
from vortex_cen.tasks.research_task_base import _CenResearchTask


class ForcingPerturbations(_CenResearchTask):
    """
    **Task : ForcingPerturbations**

    Generate an ensemble of FORCING files by perturbing a reference FORCING file.

    **Input:**

    - reference FORCING file

    **Output:**

    - ensemble of perturbed FORCING files

    **Configuration variables:**

    **Mandatory**

    * ``forcing_datebegin`` *datebegin* footprint, default self.conf.datebegin
      type forcing_datebegin: str, footprints.stdtypes.FPList
    * ``forcing_dateend`` *dateend* footprint, default self.conf.dateend
      type forcing_dateend: str, footprints.stdtypes.FPList
    * ``forcing_xpid`` Experiment identifier, default self.conf.xpid
      type forcing_xpid: str
    * ``forcing_geometry`` geometry of input file
      type: str, footprints.stdtypes.FPList
    * ``forcing_block`` *block* footprint, default "meteo"
      type forcing_block: str

    **Optional**

    * ``forcing_*`` Any additional footprint description of the input forcing file
      type forcing_*: str

    """
    def __init__(self, **kw):

        MANDATORY_CONFIGURATION_VARIABLES = [
            "forcing_datebegin|datebegin",
            "forcing_dateend|dateend",
            "forcing_xpid|xpid",
            "forcing_geometry|geometry",
            "forcing_block",
            "members+help=List of output perturbed ensemble members;format='first-last-step'",
        ]
        OPTIONAL_CONFIGURATION_VARIABLES = [
            "forcing",
        ]
        super().__init__(**kw)

        self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

    def get_remote_inputs(self):

        self.get_forcing(localname='[datebegin:ymdh]_[dateend:ymdh]/FORCING.nc')

    def get_local_inputs(self):
        pass

    def algo(self):

        self.sh.title('Algo perturbforcing')
        algo = vortex.task(
            kind         = 'perturbforcing',
            engine       = 'algo',
            members      = footprints.util.rangex(self.conf.members),
            role_members = 'Forcing',
            # reprod_info  = self.get_reprod_info,
        )
        print(self.ticket.prompt, 'Algo =', algo)
        print()
        return algo

    def launch_algo(self, algo):

        self.launch_python_algo(algo=algo)

    def put_outputs(self):

        self.sh.title('Output FORCINGs')
        out = vortex.output(
            role           = 'Forcing',
            kind           = 'MeteorologicalForcing',
            local          = '[datebegin:ymdh]_[dateend:ymdh]/mb[member%04d]/FORCING.nc',
            experiment     = self.conf.xpid,
            member         = footprints.util.rangex(self.conf.members),
            geometry       = self.conf.geometry,
            datebegin      = self.list_dates_begin,
            dateend        = self.dict_dates_end,
            nativefmt      = 'netcdf',
            namespace      = 'vortex.multi.fr',
            namebuild      = 'flat@cen',
            block          = 'perturbations'
        ),
        print(self.ticket.prompt, 'Output forcings =', out)
        print()
