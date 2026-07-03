# -*- coding: utf-8 -*-

"""
forcing.py
----------

Algo Components for the generation or the modification of FORCING files.

.. inheritance-diagram:: vortex_cen.algo.forcing
   :top-classes: vortex_cen.algo.components._CenParaBlindRun, vortex_cen.algo.components._CenTaylorRun,
                 vortex_cen.algo.components._CenTaylorVortexWorker, vortex_cen.algo.components._CenWorkerBlindRun
                 vortex.algo.components.AlgoComponent, vortex.algo.components.Parallel, vortex.algo.components.TaylorRun
   :private-bases:
   :parts: 2

.. autoclass:: ExtractMassifs
   :no-members:
   :show-inheritance:

.. autoclass:: ExtractMassifsWorker
   :no-members:
   :show-inheritance:

.. autoclass:: ConcatForcings
   :no-members:
   :show-inheritance:

.. autoclass:: ConcatForcingsWorker
   :no-members:
   :show-inheritance:

"""
from bronx.fancies import loggers
from vortex_cen.algo.components import _CenTaylorRun, _CenTaylorVortexWorker
from snowtools.scripts.create_forcing import extract_forcing

import xarray as xr
from snowtools.utils import xarray_snowtools  # noqa

logger = loggers.getLogger(__name__)


class ExtractMassifs(_CenTaylorRun):
    """
    Algo component to extract a list of points from a set of S2M FORCING files in the "massif" geometry.
    """

    _footprint = dict(
        info = 'AlgoComponent that runs several extractions in parallel.',
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


class ExtractMassifsWorker(_CenTaylorVortexWorker):
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


class ConcatForcings(_CenTaylorRun):
    """
    Concatenation of a set of FORCING files into a single forcing.
    """

    _footprint = dict(
        info = 'AlgoComponent that runs several concatenations in parallel.',
        attr = dict(
            kind  = dict(
                values     = ['ConcatForcings'],
            ),
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                values   = ['Forcing'],
            ),
            concat_dim = dict(
                info     = 'Name of the spatial dimension to concatenate files along',
                type     = str,
                default  = 'Number_of_points',
                optional = True,
            ),
        ),
    )

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super()._default_common_instructions(rh, opts)
        avail_forcings = self.context.sequence.effective_inputs(role=self.role_members)
        list_forcings = [forcing.rh.container.basename for forcing in avail_forcings]
        ddict['list_forcings'] = list_forcings
        return ddict


class ConcatForcingsWorker(_CenTaylorVortexWorker):
    """
    Concatenation of a set of FORCING files into a single forcing.
    """

    _footprint = dict(
        attr = dict(
            kind    = dict(
                values = ['ConcatForcings']
            ),
            list_forcings = dict(
                info     = 'Names of the FORCING files to concatenate',
                type = list,
            ),
            concat_dim = dict(
                info     = 'Name of the spatial dimension to concatenate files along',
                type     = str,
            ),
        )
    )

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        """
        Method called by the main **vortex_task** method of the **_CenWorkerMixIn** class
        """
        ds = xr.open_mfdataset(
            self.list_forcings,
            combine='nested',
            concat_dim=self.concat_dim,
            chunks='auto',  # Activates dask for automatic data slicing
            # parallel=True,  # multi-threads  --> crash (TODO : understand and fix)
            engine='snowtools',  # Apply snowtools-specific pre-processing
        )
        ds.to_netcdf("FORCING_OUT.nc")
