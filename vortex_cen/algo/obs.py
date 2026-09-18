# -*- coding: utf-8 -*-

"""
obs.py
------

Algo Components for the manipulation of any observation data.

.. inheritance-diagram:: vortex_cen.algo.obs
   :top-classes: vortex_cen.algo.components._CenParaBlindRun, vortex_cen.algo.components._CenTaylorRun,
                 vortex_cen.algo.components._CenTaylorVortexWorker, vortex_cen.algo.components._CenWorkerBlindRun
                 vortex.algo.components.AlgoComponent, vortex.algo.components.Parallel, vortex.algo.components.TaylorRun
   :private-bases:

.. autoclass:: ReconstructObservations
   :no-members:
   :show-inheritance:

.. autoclass:: ReconstructObservationsWorker
   :no-members:
   :show-inheritance:
"""
from bronx.fancies import loggers
from vortex_cen.algo.components import _CenTaylorRun, _CenTaylorVortexWorker
from snowtools.scripts.observations.create_new_SAFRAN_observations import replace_obs_tar

logger = loggers.getLogger(__name__)


class ReconstructObservations(_CenTaylorRun):
    """
    Generate an ensemble of `ReconstructObservationsWorker`
    """

    _footprint = dict(
        info = 'TODO',
        attr = dict(
            kind  = dict(
                values     = ['reconstruct_observations'],
            ),
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                values   = ['Observations'],
            ),
        ),
    )


class ReconstructObservationsWorker(_CenTaylorVortexWorker):
    """
    Update SAFRAN-ready observation files (R*, S* and T* files) with reconstructed hourly temperature observations.
    """

    _footprint = dict(
        attr = dict(
            kind    = dict(
                values = ['reconstruct_observations']
            ),
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                values   = ['Observations'],
            ),
        )
    )

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        """
        Method called by the main **vortex_task** method of the **_CenMixIn** class
        """
        self.link_in('../listeo_reanalyse', 'listeo_reanalyse')
        replace_obs_tar('OBSERVATIONS.tar')
