# -*- coding: utf-8 -*-
"""
prep.py
-------

Algo component to launch the SURFEX PREP executable.

.. inheritance-diagram:: vortex_cen.algo.prep
   :top-classes: vortex_cen.algo.components._CenParaBlindRun, vortex_cen.algo.components._CenTaylorRun,
                 vortex_cen.algo.components._CenTaylorVortexWorker, vortex_cen.algo.components._CenWorkerBlindRun
                 vortex.algo.components.AlgoComponent, vortex.algo.components.Parallel, vortex.algo.components.TaylorRun
   :private-bases:

.. autoclass:: Prep
   :no-members:
   :show-inheritance:
"""

from bronx.fancies import loggers
from vortex.algo.components import Parallel
from vortex.syntax.stdattrs import a_date
from snowtools.tools.change_prep import prep_tomodify
from vortex_cen.algo.deterministic import SurfexMixIn

logger = loggers.getLogger(__name__)


class Prep(Parallel, SurfexMixIn):
    _footprint = dict(
        info = 'AlgoComponent that runs the PREP executable',
        attr = dict(
            kind = dict(
                values = ['make_prep']  # value "prep" already used
            ),
            date = a_date,
        )
    )

    def execute(self, rh, opts):
        firstforcing = self.context.sequence.effective_inputs(role='Forcing')[0]
        # retrieve FORCING information for namelist pre-processing
        datebegin = firstforcing.rh.resource.datebegin
        dateend = firstforcing.rh.resource.dateend
        forcingname = firstforcing.rh.container.basename
        self.modify_namelist(datebegin, dateend, forcingname)
        super().execute(rh, opts)

    def postfix(self, rh, opts):
        """
        Set the date of the PREP file to the prescribed date
        """
        print("CHANGE DATE OF THE PREP FILE.")
        prep = prep_tomodify("PREP.nc")
        prep.change_date(self.date)
        prep.close()
        super().postfix(rh, opts)
