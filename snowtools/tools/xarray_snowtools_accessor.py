
import numpy as np
import xarray as xr

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

    ds.semidistributed.get_points(massif_num=3, ZS=[900, 1800, 2700, 3600], slope=40)

2. Project gridded data from Lambert-93 to lat/lon :

code-block:: python

    ds.distributed.proj(crs_in="EPSG:2154", crs_out="EPSG:4326")

3. Interpret time-like variable or dimension as datetime :

code-block:: python

    ds.surfex.decode_time_variable(varname)

4. Compute 24-hour precipitation accumulations from 6h J to 6h J+1 from hourly precipitation dataset :

code-block:: python

    ds.Precipitation.meteo.daily_accumulation()


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

    def transpose(self):
        """
        Put time dimension as first dimension in case of data processing through numpy arrays

        """

        if 'time' in self.ds.dims:
            return self.ds.transpose('time', ...)
        else:
            return self.ds

    def squeeze(self):
        """
        Drop dimensions with only 1 coordinate (ex : Number_of_patches / Number_of_tiles)
        """
        return self.ds.squeeze()

    def daily_accumulation(self, start_hour=6):
        """
        Compute 24-hour accumulation starting at *strat_hour*, with an output label set at the end of the
        accumulation period.

        Usage example:
        ^^^^^^^^^^^^^^

        Compute 24-hour precipitation accumulations from 6h J to 6h J+1 from hourly precipitation dataset,
        so that the new index 'YYYY-MM-DD O6' contains the sum of all indices between
        'YYYY-MM-{DD-1} 07' and 'YYYY-MM-DD 06' (corresponding to precipitation accumulations between
        'YYYY-MM-{DD-1} 06' and 'YYYY-MM-DD 06')

        code-block:: python

            tmp = ds.snowtools.daily_accumulation()
            out = tmp.sel(time='2021-12-05 06')

        is equivalent to :

        code-block:: python

            out = ds.sel({'time': slice('2021-12-04 07', '2021-12-05 06')}).sum('time')

        :param start_hour: Hour of the day from which the 24-hour accumulation is to be computed
        :param start_hour: int

        """
        return self.ds.resample(time='D', offset=f'{start_hour}h', closed='right', label='right').sum()


@xr.register_dataset_accessor("meteo")
class MeteoAccessor(SnowtoolsAccessor):
    """
    Accessor designed to deal with meteorological files.

    Usage example:

    code-block:: python

         import xarray as xr

         from snowtools.tools import xarray_snowtools_backend
         from snowtools.tools import xarray_snowtools_accessor

         ds = xr.open_dataset('FORCING.nc', engine='meteo')
         ds.meteo.[...]
    """


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


@xr.register_dataset_accessor("semidistributed")
class SemiDistributedAccessor(SurfexAccessor):
    """
    Additionnal methods in semi-distributed geometry (ex: S2M simulaitions)

    Usage example:

    code-block:: python

         import xarray as xr

         from snowtools.tools import xarray_snowtools_backend
         from snowtools.tools import xarray_snowtools_accessor

         ds = xr.open_dataset('INPUT.nc', engine='snowtools')
         ds.semidistributed.sel_points(massif_num=3, ZS=[900, 1800, 2700, 3600], slope=40)
    """

    def sel_points(self, massif_num=None, ZS=None, slope=None, aspect=None):

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


@xr.register_dataset_accessor("distributed")
class DistributedAccessor(SurfexAccessor):
    """
    Additionnal methods in distributed geometry (ex: EDELWEISS)

    Usage example:

    code-block:: python

         import xarray as xr

         from snowtools.tools import xarray_snowtools_backend
         from snowtools.tools import xarray_snowtools_accessor

         ds = xr.open_dataset('INPUT.nc', engine='snowtools')
         ds.distributed.proj("EPSG:4326", "EPSG:2154")
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

    def drop_tile_dimension(self, tile=0):
        """
        Select a single value for "tile" dimension (or equivalent) and squeeze the dataset to drop the dimension.

        :param tile: Value of the "tile" dimension to select
        :param tile: int
        """

        for drop_dim in ['Number_of_patches', 'tile', 'Number_of_Tile']:
            if drop_dim in self.ds.dims:
                self.ds = self.ds.sel(drop_dim=tile).squeeze()

        return self.ds
