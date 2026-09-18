# -*- coding: utf-8 -*-
"""
consts.py
---------

Specific CEN "genv" resources.

.. inheritance-diagram:: vortex_cen.data.consts
   :top-classes: vortex.nwp.data.consts.GenvModelGeoResource, vortex.nwp.data.consts.GenvModelResource
   :private-bases:
   :parts: 4

.. autoclass:: SAFRANList
   :show-inheritance:

.. autoclass:: Params
   :show-inheritance:

.. autoclass:: climTG
   :show-inheritance:

.. autoclass:: GridTarget
   :show-inheritance:

.. autoclass:: Prosnow_SetUp_Global
   :show-inheritance:

.. autoclass:: Prosnow_SetUp_Resort
   :show-inheritance:
"""

from bronx.fancies import loggers

from vortex.nwp.data.consts import GenvModelGeoResource, GenvModelResource
from vortex.nwp.syntax.stdattrs import gdomain

#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)


class SAFRANList(GenvModelGeoResource):

    _footprint = [
        gdomain,
        dict(
            info = 'Config file used by the SAFRAN model.',
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
            model = dict(
                optional = True,
                default = 'surfex',
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
            model = dict(
                optional = True,
                default = 'surfex',
            ),
        )
    )

    _extension_remap = dict(netcdf='nc')

    @property
    def realkind(self):
        return 'init_TG'

#    def namebuilding_info(self):
#
#        nbi = super().namebuilding_info()
#        nbi.update(
#            # will work only with the @cen namebuilder:
#            cen_rawbasename=(self.realkind + "." + self._extension_remap.get(self.nativefmt, self.nativefmt)),
#            # With the standard provider, the usual keys will be used.
#        )
#        return nbi


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
                model = dict(
                    optional = True,
                    default = 'surfex',
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


class EmosCoeffs(GenvModelResource):
    """
    Class of a .csv file of emos coefficients. A Genvkey can be given.
    """

    _footprint = dict(
        info="Set of EMOS  coefficients",
        attr=dict(
            kind=dict(values=["emos_pars", "emos_pars_clim"]),
            nativefmt=dict(
                values=['ascii'],
            ),
        ),
    )

    @property
    def realkind(self):
        return self.kind
