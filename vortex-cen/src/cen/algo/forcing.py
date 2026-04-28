#!/usr/bin/env python

"""
Algo Components generating a FORCING file.
"""
from bronx.fancies import loggers
from vortex_cen.algo.ensemble import _CENTaylorRun, _CENTaylorVortexWorker
from snowtools.scripts.create_forcing import extract_forcing

logger = loggers.getLogger(__name__)


class ExtractForcing(_CENTaylorRun):
    """
    Algo component to extract a list of points from a set of S2M FORCING files in the "massif" geometry.
    """

    _footprint = dict(
        info = 'AlgoComponent that runs several diagnostics in parallel.',
        attr = dict(
            kind  = dict(
                values     = ['ExtractMassifs'],
            ),
            massifs = dict(
                info     = 'List of massifs to be extracted',
                optional = True,
            ),
            elevations = dict(
                info     = 'List of elevations to be extracted',
                optional = True,
            ),
            slopes = dict(
                info     = 'List of slopes to be extracted',
                optional = True,
            ),
            aspects = dict(
                info     = 'List of aspects to be extracted',
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
    Worker to extract a list of points from a given S2M FORCING file in the "massif" geometry.
    """

    _footprint = dict(
        attr = dict(
            kind    = dict(
                values = ['ExtractMassifs']
            ),
            massifs = dict(
                info     = 'List of massifs to be extracted',
                optional = True,
            ),
            elevations = dict(
                info     = 'List of elevations to be extracted',
                optional = True,
            ),
            slopes = dict(
                info     = 'List of slopes to be extracted',
                optional = True,
            ),
            aspects = dict(
                info     = 'List of aspects to be extracted',
                optional = True,
            ),
        )
    )

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        """
        Method called by the main **vortex_task** method of the **_CenWorkerMixIn** class
        """
        extract_forcing.extract(massif_num=self.massifs, ZS=self.elevations, aspect=self.aspects, slope=self.slopes)
