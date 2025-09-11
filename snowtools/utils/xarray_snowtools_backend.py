# -*- coding: utf-8 -*-

"""

Opening a netCDF file with xarray
---------------------------------

The xarray_snowtools_backend module contains all elements defining the xarray entry points for SURFEX Input / Outputs
in NetCDF format (methods `open_dataset`, `open_dataarray` and `open_mfdataset`, responsible for reading files and
returning an xarray DataArray or Dataset Object) to be used within the snowtools package.

These entry points are defined in the ``SnowtoolsBackendEntrypoint`` class inheriting from the xarray-native
``BackendEntrypoint`` class (https://docs.xarray.dev/en/stable/internals/how-to-add-new-backend.html).
WARNING : xarray ``BackendEntrypoint`` class is not implemented on xarray before 0.18 (default Meteo-France install)

These entry points are designed to deal with the following requirements :

    - Ensure that the coordinates associated to the time dimension are datetime objects. By default xarray try to
      decode time variables as datetime objects based on their "unit" attribute. However, some SURFEX variables,
      such as "SNOWAGE", have an unit attribute that can not be interpreted by the default cftime tool,
      raising the following error:

      "ValueError: Failed to decode variable 'SNOWAGE': unable to decode time units 'days since snowfall' with
      'the default calendar'.
      Try opening your dataset with decode_times=False or installing cftime if it is not installed."

      The solution here is to switch off the decoding of time variables when reading data ("decode_times=False")
      and doing so manually afterward for the dime dimension alone (see "decode_time_dimension" method)

    - Ensure that variable and dimension names are standard names for backward compatibility of SURFEX versions
      and to deal with data coming from different sources (this replaces the former prosimu functionality
      designed to reduce dependency to variable and dimension names of SURFEX outputs).
      This is done by mapping the possible names of a variable / dimension (when different names coexist) to the a
      standard name through the "dimension_map" and "variables_map" dictionnaries (see "update_varname" and
      "update_dimname" methods).

    - Remove len 1 dimensions (the replaces the former prosimu functionality designed to remove
      Number_of_patches / Number_of_tiles dimensions from SURFEX outputs).
      This is done by calling the native xarray "squeeze" method on the target DataArray / Dataset.

    - Ensure that the time dimension is in the first one for numpy-based tools backward compatibility ((this replaces
      the former prosimu functionality designe to reduce the dependency to the dimensions order of SURFEX outputs).
      This is done by calling the xarray native "transpose" method.

    - Ensure that 'missing_value' and "_FillValue" attributes do not differ to avoid crashing when trying to write data
      (see "check_encoding" method for more information)

To use these entry points, the native xarray methods `open_dataset`, `open_dataarray` and `open_mfdataset` should
simply be called with the keyword argument "engine='snowtools'" except if you have an older xarray version (see below).

Meteo-France usage (until next xarray update from version 0.16.0)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: python

    import xarray as xr
    from snowtools.utils import xarray_snowtools

    ds = xr.open_dataset('INPUT.nc', decode_times=False)
    ds = xarray_snowtools.preprocess(ds)


Usage with xarray > 0.18
^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: python

    from snowtools.utils import xarray_snowtools
    import xarray as xr

    ds = xr.open_dataset(filename, engine='snowtools')
    ds = xr.open_mfdataset(list_of_files, engine='snowtools')


Use with statements!
^^^^^^^^^^^^^^^^^^^^

We encourage you to use with statements as soon as possible to ensure that the files are correctly closed:

.. code-block:: python

    from snowtools.utils import xarray_snowtools
    import xarray as xr

    with xr.open_dataset(filename, decode_times=False) as ds:
        ds = xarray_snowtools.preprocess(ds)
        ...  # Do what you want with the data inside of these files.

..
   Additionnal features
   ^^^^^^^^^^^^^^^^^^^^

   The subsequent use of xarray directly enables the following functionalities of the former prosimu tool:

       - Extract points according to spatial properties (through native xarray indexing methods "sel", "loc","where"...)
         xarray related documentation : https://docs.xarray.dev/en/stable/user-guide/indexing.html

         examples:

         .. code-block:: python

             ds.sel(time=slice('2025-06-12 00', '2025-06-14 12', xx=slice(6.5, 8.), yy=slice(43., 45.))
             ds.where((ds.massif_num == 3) & (ds.ZS.isin([900, 1800, 2700, 3600])) &
                           (ds.slope == 40) & (ds.aspect.isin([0, 180])), drop=True)

       - Temporal integration of a diagnostic (native xarray "resample" method)

       - Data concatenation along 1 dimension of a set of files (method `open_mfdataset`)

       - Optimisation of data reanding and prossessing through the use of xarray-compatible dask tools
         (https://docs.xarray.dev/en/stable/user-guide/dask.html)

       - Dealing with missing values

   It also enables new functionalities not covered (or only partially) by prosimu :

       - Integration of xarray-based codes into snowtools, benefiting from advanced xarray functionalities
         (eg: "groupby") and related projects (ex : https://docs.xarray.dev/en/stable/user-guide/ecosystem.html)

       - Easy exploration / processing of 2D, ensemble data (interpolation, maps, scores,...)

       - Direct compatibility with any new dimension (ex : "member")

       - Propagation of the attributes along all process steps

       - Process different input file formats (grib, tif,...) with the same tools by using native or
         custom xarray engines

   See xarray documentation (https://xarray.dev/) for more information, in particular :

       - xarray philosophy : https://docs.xarray.dev/en/stable/roadmap.html#roadmap

       - xarray internal design :
         https://docs.xarray.dev/en/stable/internals/internal-design.html#internal-design-lazy-indexing


"""

import xarray

from snowtools.utils.xarray_snowtools_preprocess import preprocess

from xarray.backends import BackendEntrypoint
from xarray.backends.common import BACKEND_ENTRYPOINTS

# WARNING : the following line changes the default behavior of ALL subsequent xarray `open_*`
# by forcing the use of the custom `SnowtoolsBackendEntrypoint` entry point.
# This may be a bad idea to mess with the default engine
# def guess_engine(store_spec):
#    return 'snowtools'
# import xarray.backends.plugins as plugins
# plugins.guess_engine = guess_engine


class SnowtoolsBackendEntrypoint(BackendEntrypoint):
    """
    Xarray entry point to deal with snowtools-specific NetCDF files :
    - Ensure backward compatibility in case of a change of variable/dimension name
    - Map between identical variables coming from different sources with different names
    - Deal with SURFEX-specific time dimension issues

    This is done by applying a preprocess (see the "preprocess" method) to the
    data before returning the xarray dataset or dataarray object.

    Usage:

    .. code-block:: python

        from snowtools.utils import xarray_snowtools
        import xarray

        ds = xr.open_dataset(filename, engine='snowtools')

    External documentation :

    https://docs.xarray.dev/en/stable/internals/how-to-add-new-backend.html
    """

    available = True  # Necessary before v.2024.04.0

    def open_dataset(self, filename_or_obj, *, mapping=dict(), **kw):
        """
        Snowtools-specific version of the xarray's "open_dataset" method, which calls the native method
        and carries out a preprocessing of the data before returning the dataset object.
        If a list of paths is provided, the files are automatically opened with "open_mfdataset".
        WARNING : the direct use of *open_mfdataset* is recomended whenever you are sure to deal with more
        than one file.

        :param filename_or_obj: Pathi or list of paths of the file(s) to read
        :type filename_or_obj: str, Path, file_like, DataStore or nested sequence of paths
        :mapping: User-defined dictionnary to map variable or dimension names. It can be used as a complement to
                  the default mapping dictionnary in case of a code based on variable / dimension names different than
                  the standard ones (for example a code written after a SURFEX update).
                  This is a snowtools-specific argument.
        :type mapping: dict

        """
        if isinstance(filename_or_obj, list):
            return xarray.open_mfdataset(filename_or_obj, engine='snowtools', mapping=mapping, **kw)
        else:
            ds = xarray.open_dataset(filename_or_obj, engine='netcdf4', decode_times=False, **kw)
            close = ds._close
            ds = preprocess(ds, mapping=mapping)
            ds.set_close(close)

            return ds

    open_dataset_parameters = ["filename_or_obj"]

    def guess_can_open(self):
        """
        guess_can_open is used to identify the proper engine to open your data file automatically in case
        the engine is not specified explicitly.
        """
        return True


# Register the custom "SnowtoolsBackendEntrypoint" among xarray known backend entry points when this module is imported
if xarray.__version__ >= '2023.04.0':
    BACKEND_ENTRYPOINTS["snowtools"] = ("snowtools", SnowtoolsBackendEntrypoint)
elif xarray.__version__ >= '0.18.0':
    BACKEND_ENTRYPOINTS["snowtools"] = SnowtoolsBackendEntrypoint
