"""
Packed resources including several small files.
"""

from vortex_cen.syntax.stdattrs import cendateperiod_deco
from vortex.data.flow import GeoFlowResource
from vortex.syntax.stddeco import namebuilding_delete


class CenPackedFiles(GeoFlowResource):
    """
    Abstract class for CEN archives to reduce the number of small files.
    """

    _abstract = True,
    _footprint = [
        cendateperiod_deco,
        dict(
            info = 'Any packed file covering a given period',
            attr = dict(
                nativefmt = dict(
                    values = ['tar', 'tar.gz'],
                    default = 'tar'
                ),
                datebegin = dict(
                    info = "First date of the archive",
                ),
                dateend =dict(
                    info = "Last date of the archive",
                ),
                cutoff=dict(
                    optional=True,
                ),
            ),
        )
    ]

    @property
    def realkind(self):
        return self.kind


@namebuilding_delete('src')
@namebuilding_delete('geo')
class SurfexPackedFiles(CenPackedFiles):
    """
    Class for Surfex packed files.
    """
    # TODO : Is this really used ?

    _footprint = [
        dict(
            info='Surfex packed files covering a given period',
            attr=dict(
                kind=dict(
                    values=['FORCING', 'PREP', 'PRO'],
                ),
                model = dict(
                    values=['surfex', 's2m'],
                ),
            )
        )
    ]
