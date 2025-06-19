#!/usr/bin/env python

"""
Algo Components for FORCING generation.
"""
from bronx.fancies import loggers
from snowtools.algo.ensemble import _CENTaylorRun, _CENTaylorVortexWorker
from snowtools.scripts.create_forcing import extract_forcing

import footprints

logger = loggers.getLogger(__name__)


class ExtractForcing(_CENTaylorRun):
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
                info     = 'List of massifs to be extracted',
                type     = footprints.FPList,
                optional = True,
            ),
            elevations = dict(
                info     = 'List of elevations to be extracted',
                type     = footprints.FPList,
                optional = True,
            ),
            slopes = dict(
                info     = 'List of slopes to be extracted',
                type     = footprints.FPList,
                optional = True,
            ),
            aspects = dict(
                info     = 'List of aspects to be extracted',
                type     = footprints.FPList,
                optional = True,
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
                info     = 'List of massifs to be extracted',
                type     = footprints.FPList,
                optional = True,
            ),
            elevations = dict(
                info     = 'List of elevations to be extracted',
                type     = footprints.FPList,
                optional = True,
            ),
            slopes = dict(
                info     = 'List of slopes to be extracted',
                type     = footprints.FPList,
                optional = True,
            ),
            aspects = dict(
                info     = 'List of aspects to be extracted',
                type     = footprints.FPList,
                optional = True,
            ),
        )
    )

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        """
        Method called by the main **vortex_task** method of the **_S2MWorkerMixIn** class
        """
        extract_forcing.extract(massif_number=self.massifs, ZS=self.elevations, aspect=self.aspects, slope=self.slopes)
