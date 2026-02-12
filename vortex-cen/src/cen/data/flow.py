"""
Abstract base class for all SURFEX/Crocus related flow resources.
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
@namebuilding_delete('geo')
class SurfaceIO(GeoFlowResource):

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
            )
        )
    ]

    _extension_remap = dict(netcdf='nc')

    @property
    def realkind(self):
        return self.kind
