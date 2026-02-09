"""
Specific CEN "genv" resources.
"""

from bronx.fancies import loggers

from vortex.nwp.data.consts import GenvModelGeoResource, GenvModelResource
from vortex.nwp.syntax.stdattrs import gdomain

#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)


class List(GenvModelGeoResource):

    _footprint = [
        gdomain,
        dict(
            info = 'Config file used by  S2M models.',
            attr = dict(
                kind = dict(
                    values = ['listem', 'lystem', 'listeo', 'lysteo', 'listeml', 'lysteml',
                              'carpost', 'rsclim', 'icrccm', 'NORELot', 'NORELmt', 'blacklist',
                              'metadata', 'NORELo', 'NORELm', 'shapefile'],
                ),
                nativefmt = dict(
                    values  = ['ascii', 'shp'],
                    default = 'ascii',
                ),
                gvar = dict(
                    default = '[kind]_[gdomain]',
                ),
            )
        )
    ]

    @property
    def realkind(self):
        return 'safran_namelist'


class Params(GenvModelGeoResource):

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['ssa_params', 'surfz'],
            ),
            nativefmt = dict(
                values  = ['netcdf', 'nc', 'ascii'],
                default = 'netcdf',
                remap   = dict(nc='netcdf'),
            ),
            gvar = dict(
                default = '[kind]',
            ),
        )
    )

    @property
    def realkind(self):
        return self.kind


class climTG(GenvModelGeoResource):
    """
    Ground temperature climatological resource.
    """

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ["climTG"],
            ),
            nativefmt = dict(
                values  = ['netcdf', 'nc'],
                default = 'netcdf',
                remap   = dict(nc='netcdf'),
            ),
            gvar = dict(
                default = '[kind]',
            ),
        )
    )

    _extension_remap = dict(netcdf='nc')

    @property
    def realkind(self):
        return 'init_TG'

    def namebuilding_info(self):

        nbi = super().namebuilding_info()
        nbi.update(
            # will work only with the @cen namebuilder:
            cen_rawbasename=(self.realkind + "." + self._extension_remap.get(self.nativefmt, self.nativefmt)),
            # With the standard provider, the usual keys will be used.
        )
        return nbi


class GridTarget(GenvModelGeoResource):
    """
    Resource describing a grid for interpolation of data based on massifs geometry
    """

    _footprint = [
        gdomain,
        dict(
            attr = dict(
                kind = dict(
                    values = ["interpolgrid", 'relief', 'surfhydro'],
                ),
                nativefmt = dict(
                    values  = ['netcdf', 'nc', 'grib'],
                    default = 'netcdf',
                    remap   = dict(nc='netcdf'),
                ),
                gvar = dict(
                    default = '[kind]_[gdomain]',
                ),
            )
        )
    ]


class Prosnow_SetUp_Global(GenvModelResource):
    """Prosnow general setup file.

    This class was implemented by C. Carmagnola in April 2019 (PROSNOW project).
    """

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['prep_fillup_5', 'prep_fillup_50', 'list_updated_variables'],
            ),
            nativefmt = dict(
                values  = ['ascii', 'netcdf'],
            ),
            gvar = dict(
                default = '[kind]',
            ),
        ),
    )

    @property
    def realkind(self):
        return self.kind


class Prosnow_SetUp_Resort(GenvModelResource):
    """
    Prosnow ski-resort setup file.

    This class was implemented by C. Carmagnola in April 2019 (PROSNOW project).
    """

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['sru', 'sru_flat', 'pgd_spinup', 'prep_spinup', 'water', 'snow_nogro', 'snow_nosm',
                          'snow_noobs', 'obs_empty'],
            ),
            nativefmt = dict(
                values  = ['ascii', 'netcdf'],
            ),
            resort = dict(
                info    = "The ski resort name.",
                values  = ['saisies', 'plagne', 'soldeu', 'peyra', 'saetde'],
            ),
            gvar = dict(
                default = '[kind]_[resort]',
            ),
        ),
    )

    @property
    def realkind(self):
        return self.kind
