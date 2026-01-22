#!/usr/bin/env python

"""
Algo Components for FORCING generation.
"""
from bronx.fancies import loggers
from snowtools.algo.ensemble import _CENTaylorRun, _CENTaylorVortexWorker
from snowtools.scripts.observations.create_new_SAFRAN_observations import replace_obs_tar

logger = loggers.getLogger(__name__)


class ReconstructObservations(_CENTaylorRun):
    """
    TODO
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


class ReconstructObservationsWorker(_CENTaylorVortexWorker):
    """
    TODO
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
        Method called by the main **vortex_task** method of the **_S2MWorkerMixIn** class
        """
        replace_obs_tar('OBSERVATIONS.tar')
