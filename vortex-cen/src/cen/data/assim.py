"""
SODA execution flow resources
"""

from bronx.stdtypes.date import Date
from vortex.data.flow import GeoFlowResource
from vortex.syntax.stddeco import namebuilding_insert


@namebuilding_insert('cen_period', lambda self: [self.dateassim.ymdh, ])
class PfSample(GeoFlowResource):
    """
    Class for SODA particle filter outputs (text files at each assim step)
    """

    _footprint = [
        dict(
            info = 'pf sample file',
            attr = dict(
                # This notion does not mean anything in our case (and seems to be rather ambiguous also in other cases)
                cutoff = dict(
                    optional = True
                ),
                kind = dict(
                    values = ['PART', 'BG_CORR', 'IMASK', 'ALPHA']
                ),
                nativefmt=dict(
                    values=['ascii', 'netcdf', 'nc'],
                    default='ascii',
                    remap=dict(nc='netcdf'),
                ),
                model = dict(
                    values = ['soda']
                ),
                # This seems redundant with the *date* footrprint
                dateassim = dict(
                    info = "date of the analysis",
                    type = Date,
                    default='[date]',
                ),
                date = dict(
                    optional = True,
                ),
            )
        )
    ]

    _extension_remap = dict(ascii='txt', netcdf='nc')

    @property
    def realkind(self):
        return self.kind
