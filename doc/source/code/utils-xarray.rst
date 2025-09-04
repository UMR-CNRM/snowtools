
Xarray with snowtools
=================================

In order to deal with NetCDF files in general, xarray is a well known tool.
The interaction with snowtools is made this way (to allows compatibility with old SURFEX simulation for example)

.. automodule:: utils.xarray_snowtools_accessor
   :members:

.. automodule:: utils.xarray_snowtools_backend
   :members:



Good practices
==============

* Use lazy loading / lazy indexing to ensure to load data in memory AFTER the sub-data selection step (https://docs.xarray.dev/en/latest/internals/internal-design.html#lazy-loading).

For example, to compute the maximum of snow depth over time for one specific point (lon, lat), do :

.. code-block:: python

    from snowtools.utils import xarray_snowtools
    import xarray as xr

    ds = xr.open_dataset(filename, engine='snowtools')
    htn = ds.DSN_T_ISBA
    subdata = htn.sel(xx=lon, yy=lat)
    out = subdata.max('time')

instead of :

.. code-block:: python

    from snowtools.utils import xarray_snowtools
    import xarray as xr

    ds = xr.open_dataset(filename, engine='snowtools')
    htn = ds.DSN_T_ISBA
    maxhtn = hnt.max('time')
    out = maxhtn.sel(xx=lon, yy=lat)

In particular, avoid using the "where" method (which does not support lazy indexing) to select sub-data when / if possible.

* Use dask whenever possible to optimise the parallelisation of computing steps : https://blog.dask.org/2021/11/02/choosing-dask-chunk-sizes


