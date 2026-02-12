"""
Resources related to SURFEX/Crocus meteorological FORCINGs
"""

from vortex.syntax.stddeco import namebuilding_append
from vortex_cen.data.flow import SurfaceIO


@namebuilding_append('src', lambda self: self.source_conf, none_discard=True)
@namebuilding_append('src', lambda self: self.source_app, none_discard=True)
class SurfaceForcing(SurfaceIO):
    """
    Class for all kind of meteorological forcing files.
    Files containing only some meteorological forcing variables can be identified with a
    different kind, which may be usefull for the developpement of EDELWEISS while forcing
    variables come from different sources (SAFRAN, PE-AROME, ANTILOPE,...)
    """
    _footprint = [
        dict(
            info = 'Full or incomplete forcing files',
            attr = dict(
                # TODO : This needs further collective reflexion
                kind = dict(
                    values = ['MeteorologicalForcing', 'FORCING', 'forcing',
                        'Precipitation', 'Wind', 'ISO_TPW', 'ISO_WETBT', 'Profile_TPW', 'FORCING', 'forcing'],
                ),
                # TODO : this also needs further collective reflexion
                model = dict(
                    optional = True,  # The 'model' footprint of FORCING files is mostly unknown/undefined (and unused)
                    deafult  = 'unknown',
                    values   = ['safran', 'obs', 's2m', 'adamont', 'edelweiss', 'devine', 'arome', 'antilope',
                        'pearome', 'unknown'],
                ),
                source_app = dict(
                    optional = True
                ),
                source_conf = dict(
                    optional = True
                ),
            )
        )
    ]

    @property
    def realkind(self):
        if self.kind == 'MeteorologicalForcing':
            return 'FORCING'
        else:
            return self.kind
