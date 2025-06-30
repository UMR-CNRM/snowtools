
import numpy as np
import xarray as xr

from xarray.core.types import Self

from collections.abc import (
    Iterable,
    Mapping,
)

from typing import Any

"""

Introduction:
^^^^^^^^^^^^^
This module aims at wrapping and extending the xarray module for snowtools-specific usage. The wrapping of
existing methods is designed to reduce dependency to native xarray method changes (in order to
centralise required adaptations).

Following the xarray project's recomandations, it is based on the use of accessor :
https://tutorial.xarray.dev/advanced/accessors/01_accessor_examples.html


Usage examples:
^^^^^^^^^^^^^^^

code-block:: python

     import xarray as xr

     from snowtools.tools import xarray_snowtools_backend
     from snowtools.tools import xarray_snowtools_accessor

     ds = xr.open_dataset('INPUT.nc', engine='snowtools')

1. Select subset of points from a S2M file in the massif geometry, based on the massif number,
   elevation, slope and aspect

code-block:: python

    ds.s2m.get_points(massif_num=3, ZS=[900, 1800, 2700, 3600], slope=40)

2. Project gridded data from Lambert-93 to lat/lon :

code-block:: python

    ds.edelweiss.proj(crs_in="EPSG:2154", crs_out="EPSG:4326")

3. Interpret time-like variable or dimension as datetime :

code-block:: python

    ds.surfex.decode_time_variable(varname)


New features integration rules:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Any native xarray function/method NOT part of xarray’s public API can be overwritten in these accessors in order to
centralise required adaptations in case of any change of behavior of the native method.

Informations on the list of xarray function/method considered public API can be found in the xarray documentation :
- https://docs.xarray.dev/en/v2023.09.0/getting-started-guide/faq.html (section "What parts of xarray are considered
public API?")
- https://docs.xarray.dev/en/v2023.09.0/api.html#api

"""


@xr.register_dataset_accessor("snowtools")
class SnowtoolsAccessor:
    """
    Common snowtools-specific additionnal methods for xarray
    """

    def __init__(self, xarray_obj):
        self.ds = xarray_obj

    # def sel(self, indexers=None, method=None, tolerance=None, drop=False, **indexers_kwargs):
    def sel(
        self,
        indexers: Mapping[Any, Any] | None = None,
        method: str | None = None,
        tolerance: int | float | Iterable[int | float] | None = None,
        drop: bool = False,
        **indexers_kwargs: Any,
    ) -> Self:
        """
        xarray "sel" method wrapper to reduce dependency to changes in the native method.

        This custom method is to be used systematically within snowtools:

        use

        code-block:: python

            ds.snowtools.sel(time=slice('2025-06-12 00', '2025-06-14 12'), xx=slice(6.5, 8.), yy=slice(43., 45.))

        instead of

        code-block:: python

            ds.sel(time=slice('2025-06-12 00', '2025-06-14 12'), xx=slice(6.5, 8.), yy=slice(43., 45.))

        See xarray's documentation for more information on the arguments:
        https://docs.xarray.dev/en/stable/generated/xarray.DataArray.sel.html
        """

        return self.ds.sel(indexers=indexers, method=method, tolerance=tolerance, drop=drop, **indexers_kwargs)


@xr.register_dataset_accessor("s2m")
class S2MAccessor(SnowtoolsAccessor):
    """
    S2M-specific additionnal methods for xarray

    Usage example:

    code-block:: python

         import xarray as xr

         from snowtools.tools import xarray_snowtools_backend
         from snowtools.tools import xarray_snowtools_accessor

         ds = xr.open_dataset('INPUT.nc', engine='snowtools')
         ds.s2m.get_points(massif_num=3, ZS=[900, 1800, 2700, 3600], slope=40)
    """

    def get_points(self, massif_num=None, ZS=None, slope=None, aspect=None):

        sel_dict = dict()
        for var in ['massif_num', 'ZS', 'slope', 'aspect']:
            if eval(var) is None:
                sel_dict[var] = np.unique(self.ds[var].data)
            elif not isinstance(eval(var), list):
                sel_dict[var] = [eval(var)]
            else:
                sel_dict[var] = eval(var)

        out = self.ds.where((self.ds.massif_num.isin(sel_dict['massif_num'])) & (self.ds.ZS.isin(sel_dict['ZS'])) &
                (self.ds.slope.isin(sel_dict['slope'])) & (self.ds.aspect.isin(sel_dict['aspect'])), drop=True)

        return out


@xr.register_dataset_accessor("edelweiss")
class EDELWEISSAccessor(SnowtoolsAccessor):
    """
    EDELWEISS-specific additionnal methods for xarray

    Usage example:

    code-block:: python

         import xarray as xr

         from snowtools.tools import xarray_snowtools_backend
         from snowtools.tools import xarray_snowtools_accessor

         ds = xr.open_dataset('INPUT.nc', engine='snowtools')
         ds.edelweiss.proj("EPSG:4326", "EPSG:2154")
    """

    def proj(self, crs_in="EPSG:4326", crs_out="EPSG:2154"):
        """
        Projection of an xarray dataset or dataarray into a new CRS.

        :param ds: xarray object to preprocess
        :param ds: xarray Dataset or Dataarray
        :param crs_in: CRS of the input object
        :type crs_in: str
        :param crs_out: CRS of the output object
        :type crs_out: str
        """
        # WARNING : rioxarray not available on HPC yet
        # TODO : check crs_in ?
        self.ds.rio.write_crs(crs_in, inplace=True)
        out = self.ds.rio.reproject(crs_out)
        return out


@xr.register_dataset_accessor("surfex")
class SurfexAccessor(SnowtoolsAccessor):
    """
    Accessor designed to deal with SURFEX output files.

    Usage example:

    code-block:: python

         import xarray as xr

         from snowtools.tools import xarray_snowtools_backend
         from snowtools.tools import xarray_snowtools_accessor

         ds = xr.open_dataset('PRO.nc', engine='snowtools')
         ds.surfex.decode_time_variable('time')
    """

    def decode_time_variable(self, varname):
        """
        Manually decode any time-like variable from a SURFEX output

        :param varname: Name of the variable to decode
        :param ds: str
        """

        timevar = xr.Dataset({varname: self.ds[varname]})
        timevar = xr.decode_cf(timevar)
        self.ds[varname] = timevar[varname]
