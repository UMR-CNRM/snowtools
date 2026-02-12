"""
SAFRAN-specific flow resources.
"""

from bronx.stdtypes.date import Time
from footprints.util import rangex
from vortex.data.flow import GeoFlowResource
from vortex.data.geometries import UnstructuredGeometry
from vortex.syntax.stddeco import namebuilding_delete, namebuilding_insert
from vortex.nwp.data.obs import ObsRaw
from vortex_cen.data.flow import CenPackedFiles
import footprints


class SafranObsDateError(ValueError):
    """General content error."""

    def __init__(self, allowedhours):
        super().__init__(
            'SAFRAN guess are synoptic, therefore the hour must be in {!s}'.
            format(rangex(allowedhours))
        )


@namebuilding_insert('src', lambda s: [s.source_app, s.source_conf])
@namebuilding_insert('term', lambda s: s.cumul.fmthour)
@namebuilding_delete('geo')
class SafranGuess(GeoFlowResource):
    """Class for the guess file (P ou E file) that is used by SAFRAN."""

    _footprint = [
        dict(
            info = 'Safran guess',
            attr = dict(
                kind = dict(
                    values = ['guess'],
                ),
                nativefmt = dict(
                    values  = ['ascii', 'txt'],
                    default = 'ascii',
                ),
                model = dict(
                    values = ['safran'],
                    optional = True,
                ),
                source_app = dict(
                    values = ['arpege', 'arome', 'ifs'],
                ),
                source_conf = dict(
                    values = ['4dvarfr', 'pearp', '3dvarfr', 'pefrance', 'eps', 'pearo', 'era40'],
                ),
                geometry = dict(
                    info = "The resource's massif geometry.",
                    type = UnstructuredGeometry,
                ),
                cumul = dict(
                    info     = "The duration of cumulative fields (equivalent to the initial"
                               " model resource term).",
                    type     = Time,
                ),
            )
        )
    ]

    _extension_remap = dict(ascii='txt')

    @property
    def realkind(self):
        return 'guess'

    def reanalysis_basename(self):
        # guess files are named PYYMMDDHH in cen re-analysis database
        if self.source_app == 'arpege':
            if self.date.hour in [0, 6, 12, 18]:
                return 'P' + self.date.yymdh
            else:
                raise SafranObsDateError('SAFRAN guess are synoptic, therefore the hour must be 0, 6, 12 or 18')
        elif self.conf.source_app == 'cep':
            return 'cep_' + self.data.nivologyseason


class SafranObsRaw(ObsRaw):

    _footprint = dict(
        info = 'SAFRAN observation files (SYNOP observations)',
        attr = dict(
            part = dict(
                values  = ['synop', 'precipitation', 'hourlyobs', 'radiosondage', 'nebulosity', 'all'],
            ),
            model = dict(
                values  = ['safran'],
            ),
            stage = dict(
                values = ['safrane', 'sypluie', 'safran']
            ),
            cendev_map = dict(
                type     = footprints.FPDict,
                optional = True,
                default  = footprints.FPDict({'precipitation': 'R',
                                              'hourlyobs': 'T',
                                              'radiosondage': 'A'}),
            ),
            cendev_hours = dict(
                type     = footprints.FPDict,
                optional = True,
                default  = footprints.FPDict({'default': '0-18-6',
                                              'precipitation': '6',
                                              'hourlyobs': '6',
                                              'nebulosity': '6'}),
            ),
        ),
    )

    def cendev_basename(self):
        prefix = self.cendev_map.get(self.part, self.part[0].upper())
        allowed = rangex(self.cendev_hours.get(self.part, self.cendev_hours['default']))
        if self.part == 'nebulosity':
            return '{:s}{:s}.tgz'.format(prefix, self.date.yymd)
        elif self.date.hour in allowed:
            return prefix + self.date.yymdh
        else:
            raise SafranObsDateError(allowed)

    def reanalysis_basename(self):
        return self.cendev_basename()


class SafranPackedFiles(CenPackedFiles):
    """
    Class for SAFRAN archives.
    """

    _footprint = [
        dict(
            info='SAFRAN packed files covering a given period',
            attr = dict(
                kind = dict(
                    values=['packedobs', 'listobs', 'packedguess', 'packedlisting'],
                ),
                model = dict(
                    values=['safran'],
                ),
                source=dict(
                    values=['arpege', 'cep', 'era5', 'surfaceobs', 'neb'],
                    default=None,
                    optional=True,
                ),
            )
        )
    ]

    def reanalysis_basename(self):
        """
        Basename of input files for SAFRAN reanalysis.
        Since v1.8.3 and the introduction of SafranPackedFiles resources,
        the reanalysis also use this type of resources.
        """
        if self.source == 'arpege':
            return 'p' + self.datebegin.strftime('%y') + self.dateend.strftime('%y') + '.' + self.nativefmt
        elif self.source == 'cep':
            return 'cep_' + self.datebegin.strftime('%y') + self.dateend.strftime('%y')
        elif self.source == 'era5':
            return 'e' + self.datebegin.strftime('%y') + self.dateend.strftime('%y') + '.' + self.nativefmt
        elif self.source == 'surfaceobs':
            return 'rs' + self.datebegin.strftime('%y') + self.dateend.strftime('%y') + '.' + self.nativefmt
        elif self.source == 'neb':
            return 'n' + self.datebegin.strftime('%y') + self.dateend.strftime('%y') + '.' + self.nativefmt
        else:
            print('ERROR : Missing "source" information to build resource file name')
