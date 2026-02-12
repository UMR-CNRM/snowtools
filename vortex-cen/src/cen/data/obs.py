"""
Snow observation resources
"""

from bronx.stdtypes.date import Date
from vortex_cen.syntax.stdattrs import cendateperiod_deco
from vortex.data.flow import GeoFlowResource
from vortex.data.geometries import HorizontalGeometry
from vortex.syntax.stddeco import namebuilding_insert


class SnowObs(GeoFlowResource):
    """Abstract class for snow observations in netcdf format (unknown time management)"""

    _abstract = True
    _footprint = [
        dict(
            info = 'Observations of snow',
            attr = dict(
                kind = dict(
                    values = ['SnowObservations'],
                ),
                model = dict(
                    values  = ['surfex'],
                    default = 'surfex',

                ),
                nativefmt = dict(
                    values  = ['netcdf', 'nc'],
                    default = 'netcdf',
                    remap   = dict(nc='netcdf'),
                ),
                geometry = dict(
                    info = "The resource's massif geometry.",
                    type = HorizontalGeometry,
                ),
                scope = dict(
                    optional    = True,
                    alias       = ('nature', ),
                    info        = "Free description of the obs (var, sensor, location...)",
                ),
                # This notion does not mean anything in our case (and seems to be rather
                # ambiguous also in other cases)
                cutoff = dict(
                    optional = True
                )
            )
        )
    ]

    _extension_remap = dict(netcdf='nc')

    @property
    def realkind(self):
        id = 'insitu' if self.scope is None else self.scope
        return "obs_" + id


@namebuilding_insert('cen_period', lambda self: [self.datebegin.y, self.dateend.y])
class SnowObsPeriod(SnowObs):
    """Snow observations covering a time period in netcdf format"""

    _footprint = [
        cendateperiod_deco,
        dict(
            info = 'Time series of snow observations of snow for model evaluation',
            attr = dict(
                datebegin=dict(
                    info="First date of the observation file",
                ),
                dateend=dict(
                    info="Last date of the observation file",
                ),
            )
        )
    ]


@namebuilding_insert('cen_period', lambda self: [self.datevalidity.ymdh, ])
class SnowObsOneDate(SnowObs):
    """Snow observations covering at a given date in netcdf format"""

    _footprint = [
        dict(
            info = 'Instantaneous snow observations for assimilation',
            attr = dict(
                datevalidity = dict(
                    info     = "Validity date of the observation file",
                    type     = Date,
                    default  = '[date]',  # TODO : Why don't we use *date* directly in this case ?
                    # TODO : use an alias for retro-compatibility ?
                    optional = True,  # No need to set a default otherwise
                ),
            )
        )
    ]
