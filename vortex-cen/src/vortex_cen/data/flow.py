"""
S2M executions flow resources.
"""

from bronx.fancies import loggers
from bronx.stdtypes.date import Date, Time
from cen.syntax.stdattrs import cendateperiod_deco
from common.data.modelstates import InitialCondition
from common.data.obs import ObsRaw
import footprints
from footprints.util import rangex
from vortex.data.flow import GeoFlowResource
from vortex.data.geometries import UnstructuredGeometry, HorizontalGeometry
from vortex.syntax.stddeco import namebuilding_append, namebuilding_delete, namebuilding_insert


#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)


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
                kind = dict(
                    values = ['MeteorologicalForcing', 'Precipitation', 'Wind'],  # To be extended
                ),
                model = dict(
                    values = ['safran', 'obs', 's2m', 'adamont', 'edelweiss', 'devine'],
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


class Pro(SurfaceIO):
    """Class for the SURFEX/Crocus output files."""
    _footprint = [
        dict(
            info = 'Surfex-simulated snowpack files',
            attr = dict(
                kind = dict(
                    values = ['SnowpackSimulation'],
                ),
                model = dict(
                    values = ['surfex'],
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return "PRO"


class Postproc(SurfaceIO):
    """Class for post-processed SURFEX/Crocus output files."""
    _footprint = [
        dict(
            info = 'Post-processed Surfex-simulated snowpack files',
            attr = dict(
                kind = dict(
                    values = ['SnowpackSimulation'],
                ),
                model = dict(
                    values = ['postproc'],
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return "POSTPROC"


@namebuilding_delete('src')
@namebuilding_delete('geo')
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
                    values = ['surfex'],
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
            info='Instantaneous snow observations for assimilation',
            attr=dict(
                datevalidity=dict(
                    info="Validity date of the observation file",
                    type=Date,
                    default='[date]',
                ),
            )
        )
    ]


@namebuilding_insert('cen_period', lambda self: [self.dateassim.ymdh, ])
class PfSample(GeoFlowResource):
    """
    Class for SODA particle filter outputs (text files at each assim step)
    @author : B. Cluzet
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
                dateassim = dict(
                    info = "date of the analysis",
                    type = Date,
                    default='[date]',
                ),
            )
        )
    ]

    _extension_remap = dict(ascii='txt', netcdf='nc')

    @property
    def realkind(self):
        return self.kind


class ScoresSnow(SurfaceIO):
    """Class for scores of snow simulations."""

    _footprint = [
        dict(
            info = 'Safran-produced forcing file',
            attr = dict(
                kind = dict(
                    values = ['ScoresSnow'],
                ),
                model = dict(
                    values = ['surfex'],
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return "scores"


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
class S2MPackedFiles(CenPackedFiles):
    """
    Class for S2M archives.
    """

    _footprint = [
        dict(
            info='S2M packed files covering a given period',
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
            return 'p' + self.datebegin.yy + self.dateend.yy + '.' + self.nativefmt
        elif self.source == 'cep':
            return 'cep_' + self.datebegin.yy + self.dateend.yy
        elif self.source == 'era5':
            return 'e' + self.datebegin.yy + self.dateend.yy + '.' + self.nativefmt
        elif self.source == 'surfaceobs':
            return 'rs' + self.datebegin.yy + self.dateend.yy + '.' + self.nativefmt
        elif self.source == 'neb':
            return 'n' + self.datebegin.yy + self.dateend.yy + '.' + self.nativefmt
        else:
            print('ERROR : Missing "source" information to build resource file name')
