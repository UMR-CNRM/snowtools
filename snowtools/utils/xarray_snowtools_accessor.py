# -*- coding: utf-8 -*-

"""

Introduction to xarray_snowtools_accessor:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The module xarray_snowtools_accessor aims at wrapping and extending the xarray module for snowtools-specific usage.
The wrapping of existing methods is designed to reduce dependency to native xarray method changes (in order to
centralise required adaptations).

Following the xarray project's recomandations, it is based on the use of accessor :
https://tutorial.xarray.dev/advanced/accessors/01_accessor_examples.html


Usage examples:
^^^^^^^^^^^^^^^

.. code-block:: python

     import xarray as xr

     from snowtools.utils import xarray_snowtools_backend
     from snowtools.utils import xarray_snowtools_accessor

     ds = xr.open_dataset('INPUT.nc', engine='snowtools')

1. Select subset of points from a S2M file in the massif geometry, based on the massif number,
   elevation, slope and aspect

.. code-block:: python

    ds.semidistributed.sel_points(massif_num=3, ZS=[900, 1800, 2700, 3600], slope=40)

2. Project gridded data from Lambert-93 to lat/lon :

.. code-block:: python

    ds.distributed.proj(crs_in="EPSG:2154", crs_out="EPSG:4326")

3. Interpret time-like variable or dimension as datetime :

.. code-block:: python

    ds.surfex.decode_time_variable(varname)

4. Compute 24-hour precipitation accumulations from 6h J to 6h J+1 from hourly precipitation dataset:

.. code-block:: python

    ds.Precipitation.meteo.daily_accumulation()

5. Example of groupby with the first-test PRO file:

.. code-block:: python

     import xarray as xr
     import matplotlib.pyplot as plt
     from snowtools.utils import xarray_snowtools_backend
     from snowtools.utils import xarray_snowtools_accessor
     ds = xr.open_dataset('PRO_2010080106_2011080106.nc', engine='snowtools')
     dszs = ds.semidistributed.sel_points(ZS=2400)
     meanmonthgroup = dszs.groupby("time.month").mean() # mean the variables on a monthly base
     meanmonthgroup.TG1.plot() # choose one variable to plot
     plt.show()

6. Example of resample with the first-test PRO file:

.. code-block:: python

     import xarray as xr
     from snowtools.utils import xarray_snowtools_backend
     from snowtools.utils import xarray_snowtools_accessor
     ds = xr.open_dataset('PRO_2010080106_2011080106.nc', engine='snowtools')
     dszs = ds.semidistributed.sel_points(ZS=2400)
     dszs.resample(time='12H').mean() # time resampling to 12h timestep

7. Use of custom "daily_accumulation" method

Resample Rainf variable from hourly values to daily accumulations, starting at 03:00 :

.. code-block:: python

     import xarray as xr
     from snowtools.utils import xarray_snowtools_backend
     from snowtools.utils import xarray_snowtools_accessor

     ds_hourly = xr.open_dataset('FORCING_test_2d.nc', engine='snowtools')
     ds_daily = ds_hourly.Rainf.snowtools.daily_accumulation(start_hour=3)


New features integration rules:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Any native xarray function/method NOT part of xarray’s public API can be overwritten in these accessors in order to
centralise required adaptations in case of any change of behavior of the native method.

Informations on the list of xarray function/method considered public API can be found in the xarray documentation :
- https://docs.xarray.dev/en/v2023.09.0/getting-started-guide/faq.html (section "What parts of xarray are considered
public API?")
- https://docs.xarray.dev/en/v2023.09.0/api.html#api

"""
import xarray as xr

from bronx.syntax.externalcode import ExternalCodeImportChecker

rio_checker = ExternalCodeImportChecker('rioxarray')
with rio_checker:
    import rioxarray

__all__ = ('rioxarray',)  # Ignore F401 PEP8 syntax error


@xr.register_dataset_accessor("snowtools")
@xr.register_dataarray_accessor("snowtools")
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
        This method will return the sum of all values between "start_hour" at day J and "start_hour" at day J+1
        whatever the available frequency in the original dataset.

        It is important to use dask when calling the resample function for huge dataset to optimise computation time.
        Since the operations are carried out along the time dimension by slices of 24 hours, chunk sizes are
        automatically set to 24 over the time dimension and the dimension's length over all other dimensions.

        Execution time for a file over the Grandes Rousses at 25m resolution (~1M of points, total file size ~177 Go)
            * 1000 hourly time steps : < 30s
            * 2000 hourly time steps : ~ 2 minutes
            * 4000 hourly time steps (~6 months)    : ~ 6 minutes
            * 1 full year (8760 hourly timte steps) : crash

        Execution time for a file over the French Alps at 250m resolution (~2.5M of points, total file size 13 Go) over
        1 month (721 hourly time steps) : ~ 20s

        Usage example:
        ^^^^^^^^^^^^^^

        Compute 24-hour precipitation accumulations from 6h J to 6h J+1 from hourly precipitation dataset,
        so that the new index 'YYYY-MM-DD O6' contains the sum of all indices between
        'YYYY-MM-{DD-1} 07' and 'YYYY-MM-DD 06' (corresponding to precipitation accumulations between
        'YYYY-MM-{DD-1} 06' and 'YYYY-MM-DD 06')

        .. code-block:: python

            tmp = ds.snowtools.daily_accumulation()
            out = tmp.sel(time='2021-12-05 06')

        is equivalent to :

        .. code-block:: python

            out = ds.sel({'time': slice('2021-12-04 07', '2021-12-05 06')}).sum('time')

        :param start_hour: Hour of the day from which the 24-hour accumulation is to be computed
        :param start_hour: int

        """

        # Re-chunking a dataset is expensive, so only if the gain in computation time is worth it
        nbpoints = 1
        for dim in self.ds.dims:
            nbpoints = nbpoints * self.ds.sizes[dim]
        if (nbpoints > 1000000000):
            self.ds = self.ds.squeeze()
            chunks = {'time': 24}
            for dim in list(self.ds.dims):
                chunks[dim] = self.ds.sizes[dim]
            self.ds = self.ds.chunk(chunks)

        if xr.__version__ <= 'v2022.03.0':
            return self.ds.resample(indexer={'time': 'D'}, base=start_hour, loffset=f'{start_hour}h', closed='right',
                    label='right').sum()
        else:
            return self.ds.resample(time='D', offset=f'{start_hour}h', closed='right', label='right').sum()


@xr.register_dataset_accessor("meteo")
@xr.register_dataarray_accessor("meteo")
class MeteoAccessor(SnowtoolsAccessor):
    """
    Accessor designed to deal with meteorological files.

    Usage example:

    .. code-block:: python

         import xarray as xr

         from snowtools.utils import xarray_snowtools_backend
         from snowtools.utils import xarray_snowtools_accessor

         ds = xr.open_dataset('FORCING.nc', engine='meteo')
         ds.meteo.[...]
    """


@xr.register_dataset_accessor("surfex")
@xr.register_dataarray_accessor("surfex")
class SurfexAccessor(SnowtoolsAccessor):
    """
    Accessor designed to deal with SURFEX output files.

    Usage example:

    .. code-block:: python

         import xarray as xr

         from snowtools.utils import xarray_snowtools_backend
         from snowtools.utils import xarray_snowtools_accessor

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
@xr.register_dataarray_accessor("semidistributed")
class SemiDistributedAccessor(SurfexAccessor):
    """
    Additionnal methods in semi-distributed geometry (ex: S2M simulaitions)

    Usage example:

    .. code-block:: python

         import xarray as xr

         from snowtools.utils import xarray_snowtools_backend
         from snowtools.utils import xarray_snowtools_accessor

         ds = xr.open_dataset('INPUT.nc', engine='snowtools')
         ds.semidistributed.sel_points(massif_num=3, ZS=[900, 1800, 2700, 3600], slope=40)
    """

    def sel_points(self, massif_num=None, ZS=None, slope=None, aspect=None):
        """
        Method used to select a user-defined list of points in semi-distributed geometry (SAFRAN massifs geometry)
        from their elevation (ZS), massif number (massif_num), slope and aspect.

        **NB :**
        More advanced indexing (for example to select all elevations above 1800m or use a slice as argument), use the
        native xarray "where" method directly.

        :param massif_num: Massif number(s) of points to select
        :param massif_num: list or int
        :param ZS: Elevation(s) of points to select
        :param ZS: list or int
        :param slope: Slope(s) of points to select
        :param slope: list or int
        :param aspect: Aspects(s) of points to select
        :param aspect: list or int

        """

        indexer = None
        for var in ['massif_num', 'ZS', 'slope', 'aspect']:
            if eval(var) is not None:
                if var not in list(self.ds.keys()):
                    raise ValueError(f'Variable "{var}" does not exist')
                else:
                    if isinstance(eval(var), list):
                        if indexer is None:
                            indexer = self.ds[var].isin(eval(var))
                        else:
                            indexer = indexer & self.ds[var].isin(eval(var))
                    elif isinstance(eval(var), int):
                        if indexer is None:
                            indexer = self.ds[var] == eval(var)
                        else:
                            indexer = indexer & (self.ds[var] == eval(var))

        if indexer is not None:
            indexer = indexer.compute()
            out = self.ds.where(indexer, drop=True)
        else:
            print("WARNING : arguments where empty or could not be interpreted, nothing changed.")
            out = self.ds
        return out


@xr.register_dataset_accessor("distributed")
@xr.register_dataarray_accessor("distributed")
class DistributedAccessor(SurfexAccessor):
    """
    Additionnal methods in distributed geometry (ex: EDELWEISS)

    Usage example:

    .. code-block:: python

         import xarray as xr

         from snowtools.utils import xarray_snowtools_backend
         from snowtools.utils import xarray_snowtools_accessor

         ds = xr.open_dataset('INPUT.nc', engine='snowtools')
         ds.distributed.proj("EPSG:4326", "EPSG:2154")
    """

    @rio_checker.disabled_if_unavailable
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
        # TODO : check crs_in ?
        self.ds.rio.set_spatial_dims(x_dim='xx', y_dim='yy', inplace=True)
        self.ds.rio.write_crs(crs_in, inplace=True)
        out = self.ds.rio.reproject(crs_out).rename(x='xx', y='yy')

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
