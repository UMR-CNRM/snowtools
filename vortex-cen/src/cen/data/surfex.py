"""
SURFEX/Crocus output resources.
"""

from bronx.stdtypes.date import Date
from vortex.nwp.data.modelstates import InitialCondition
from vortex.data.geometries import HorizontalGeometry
from vortex.syntax.stddeco import namebuilding_delete, namebuilding_insert
from vortex_cen.data.flow import SurfaceIO, SurfaceIOVortex1


class Pro(SurfaceIO):
    """Class for the SURFEX/Crocus output files."""
    _footprint = [
        dict(
            info = 'Surfex-simulated snowpack files',
            attr = dict(
                kind = dict(
                    values = ['SnowpackSimulation', 'PRO', 'pro'],
                ),
                model = dict(
                    values = ['surfex'],
                    default = 'surfex',
                    optional = True,
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return "PRO"


@namebuilding_delete('src')
@namebuilding_insert('cen_period', lambda self: [self.datevalidity.ymdh, ])
class Prep(InitialCondition):
    """Class for the SURFEX-Crocus initialisation of the snowpack state."""

    _footprint = [
        dict(
            info = 'Instant SURFEX-Crocus Snowpack state',
            attr = dict(
                kind = dict(
                    values  = ['PREP'],
                ),
                nativefmt = dict(
                    values = ['ascii', 'netcdf', 'nc'],
                    default = 'netcdf',
                    remap = dict(nc='netcdf'),
                ),
                origin = dict(
                    default = None,
                    optional = True,
                ),
                geometry = dict(
                    info = "The resource's massif geometry.",
                    type = HorizontalGeometry,
                ),
                filling = dict(
                    value = ['surf', ],
                    default = 'surf',
                ),
                # In operational applications, date is used to refer to the run time
                # but the validity date of the file can be different.
                # In research applications, there is only the validity date which makes sense.
                datevalidity = dict(
                    optional = True,
                    type = Date,
                    default = '[date]',
                ),
                # This notion does not mean anything in our case (and seems to be rather
                # ambiguous also in other cases)
                cutoff = dict(
                    optional = True
                )
            )
        )
    ]

    _extension_remap = dict(netcdf='nc', ascii='txt')

    @property
    def realkind(self):
        return 'PREP'


class ProVortex1(SurfaceIOVortex1):
    """Class for the SURFEX/Crocus output files."""
    _footprint = [
        dict(
            info = 'Surfex-simulated snowpack files',
            attr = dict(
                kind = dict(
                    values = ['SnowpackSimulation', 'PRO', 'pro'],
                ),
                model = dict(
                    values = ['surfex'],
                    default = 'surfex',
                    optional = True,
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return "PRO"


@namebuilding_delete('src')
@namebuilding_delete('geo')
@namebuilding_insert('cen_period', lambda self: [self.datevalidity.ymdh, ])
class PrepVortex1(InitialCondition):
    """Class for the SURFEX-Crocus initialisation of the snowpack state."""

    _footprint = [
        dict(
            info = 'Instant SURFEX-Crocus Snowpack state',
            attr = dict(
                kind = dict(
                    values  = ['PREP'],
                ),
                nativefmt = dict(
                    values = ['ascii', 'netcdf', 'nc'],
                    default = 'netcdf',
                    remap = dict(nc='netcdf'),
                ),
                origin = dict(
                    default = None,
                    optional = True,
                ),
                geometry = dict(
                    info = "The resource's massif geometry.",
                    type = HorizontalGeometry,
                ),
                filling = dict(
                    value = ['surf', ],
                    default = 'surf',
                ),
                # In operational applications, date is used to refer to the run time
                # but the validity date of the file can be different.
                # In research applications, there is only the validity date which makes sense.
                datevalidity = dict(
                    optional = True,
                    type = Date,
                    default = '[date]',
                ),
                # This notion does not mean anything in our case (and seems to be rather
                # ambiguous also in other cases)
                cutoff = dict(
                    optional = True
                ),
                vortex1 = dict(
                    type = bool,
                    optional=False,
                    values=[True, ]
                ),
            )
        )
    ]

    _extension_remap = dict(netcdf='nc', ascii='txt')

    @property
    def realkind(self):
        return 'PREP'
