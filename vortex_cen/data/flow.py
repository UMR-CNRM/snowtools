# -*- coding: utf-8 -*-
"""
flow.py
-------

Abstract base classes for all SURFEX/Crocus IO flow resources.

.. inheritance-diagram:: vortex_cen.data.flow
   :top-classes: vortex.data.resources.Resource
   :private-bases:
   :parts: 1

.. autoclass:: SurfaceIO
   :members:
   :show-inheritance:

.. autoclass:: SurfaceIOVortex1
   :no-members:
   :show-inheritance:
"""

from bronx.fancies import loggers
from vortex_cen.syntax.stdattrs import cendateperiod_deco
from vortex.data.flow import GeoFlowResource
from vortex.data.geometries import HorizontalGeometry
from vortex.syntax.stddeco import namebuilding_delete


#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)


@namebuilding_delete('src')
class SurfaceIO(GeoFlowResource):
    """
    Abstract base classe for all surface IO flow resources used in snowpack simulations.
    `SurfaceIO` resources are NetCDF files covering a time period defined by the `datebegin` and `dateend` footprints.
    Note that the `date` and `cutoff` footprints comonly used in NWP applications are optional and used only in
    an operational context.
    """

    _abstract = True
    _footprint = [
        cendateperiod_deco,
        dict(
            info = 'SURFEX input or output file',
            attr = dict(
                nativefmt = dict(
                    values  = ['netcdf', 'nc'],
                    default = 'netcdf',
                    remap   = dict(nc='netcdf'),
                ),
                geometry = dict(
                    info = "The resource's massif geometry.",
                    type = HorizontalGeometry,
                ),
                datebegin = dict(
                    info = "First date of the forcing file",
                ),
                dateend = dict(
                    info = "Last date of the forcing file",
                ),
                # This notion does not mean anything in our case (and seems to be
                # rather ambiguous also in other cases)
                cutoff = dict(
                    optional = True,
                ),
                date = dict(
                    optional = True,
                ),
            )
        )
    ]

    _extension_remap = dict(netcdf='nc')

    @property
    def realkind(self):
        return self.kind


@namebuilding_delete('geo')
class SurfaceIOVortex1(SurfaceIO):
    """
    This class is here for retro-compatibilty with resources produced before the migration to vortex2
    """

    _abstract = True
    _footprint = [
        dict(
            info = 'SURFEX input or output file',
            attr = dict(
                vortex1 = dict(
                    type = bool,
                    optional=False,
                    values=[True, ]
                ),
            )
        )
    ]
