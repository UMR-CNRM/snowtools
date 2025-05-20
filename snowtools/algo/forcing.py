#!/usr/bin/env python

"""
Algo Components for FORCING generation.
"""
from bronx.fancies import loggers
from snowtools.algo.ensemble import _CENTaylorRun, _CENTaylorVortexWorker
from snowtools.scripts.create_forcing import extract_massifs

import footprints

logger = loggers.getLogger(__name__)


class ExtractMassifs(_CENTaylorRun):
    """
    Algo component to extract a list of massifs from a set of FORCING files
    """

    _footprint = dict(
        info = 'AlgoComponent that runs several diagnostics in parallel.',
        attr = dict(
            kind  = dict(
                values     = ['ExtractMassifs'],
            ),
            massifs = dict(
                info    = 'List of massifs to be extracted',
                type    = footprints.FPList,
            ),
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                values   = ['Forcing'],
            ),
        ),
    )


class ExtractMassifsWorker(_CENTaylorVortexWorker):
    """
    Worker to extract a list of massifs from a given FORCING file
    """

    _footprint = dict(
        attr = dict(
            kind    = dict(
                values = ['ExtractMassifs']
            ),
            massifs = dict(
                info    = 'List of massifs to be extracted',
                type    = footprints.FPList,
            ),
        )
    )

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        """
        Method called by the main **vortex_task** method of the **_S2MWorkerMixIn** class
        """
        extract_massifs.extract(self.massifs)
