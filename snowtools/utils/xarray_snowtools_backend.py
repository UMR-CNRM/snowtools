# -*- coding: utf-8 -*-

"""

Introduction to xarray_snowtools_backend:
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The xarray_snowtools_backend module contains all elements defining the xarray entry points for SURFEX Input / Outputs
in NetCDF format (methods `open_dataset`, `open_dataarray` and `open_mfdataset`, responsible for reading files and
returning an xarray DataArray or Dataset Object) to be used within the snowtools package.

These entry points are defined in the ``SnowtoolsBackendEntrypoint`` class inheriting from the xarray-native
``BackendEntrypoint`` class (https://docs.xarray.dev/en/stable/internals/how-to-add-new-backend.html).
WARNING : xarray ``BackendEntrypoint`` class is not implemented on xarray 0.16.0 (default HPC install)
--> call *preprocess* directly in HPC use or upgrade user-install of xarray.

These entry points are designed to deal with the following requirements :

    * ensure that the coordinates associated to the time dimension are datetime objects. By default xarray try to
      decode time variables as datetime objects based on their "unit" attribute. However, some SURFEX variables,
      such as "SNOWAGE", have an unit attribute that can not be interpreted by the default cftime tool,
      raising the following error:

      "ValueError: Failed to decode variable 'SNOWAGE': unable to decode time units 'days since snowfall' with
      'the default calendar'.
      Try opening your dataset with decode_times=False or installing cftime if it is not installed."

      The solution here is to switch off the decoding of time variables when reading data ("decode_times=False")
      and doing so manually afterward for the dime dimension alone (see "decode_time_dimension" method)

    * Ensure that variable and dimension names are standard names for backward compatibility of SURFEX versions
      and to deal with data coming from different sources (this replaces the former prosimu functionality
      designed to reduce dependency to variable and dimension names of SURFEX outputs).
      This is done by mapping the possible names of a variable / dimension (when different names coexist) to the a
      standard name through the "dimension_map" and "variables_map" dictionnaries (see "update_varname" and
      "update_dimname" methods).

    * Remove len 1 dimensions (the replaces the former prosimu functionality designed to remove
      Number_of_patches / Number_of_tiles dimensions from SURFEX outputs).
      This is done by calling the native xarray "squeeze" method on the target DataArray / Dataset.

    * Ensure that the time dimension is in the first one for numpy-based tools backward compatibility ((this replaces
      the former prosimu functionality designe to reduce the dependency to the dimensions order of SURFEX outputs).
      This is done by calling the xarray native "transpose" method.

    * Ensure that 'missing_value' and "_FillValue" attributes do not differ to avoid crashing when trying to write data
      (see "check_encoding" method for more information)

To use these entry points, the native xarray methods `open_dataset`, `open_dataarray` and `open_mfdataset` should
simply be called with the keyword argument "engine='snowtools'" (see the "Standard usage" section of this doc).

The "snowtools" backend is automatically made available when the snowtools package is imported.

Standard usage:
^^^^^^^^^^^^^^^

.. code-block:: python

    import snowtools
    import xarray as xr

    ds = xr.open_dataset(filename, engine='snowtools')
    ds = xr.open_mfdataset(list_of_files, engine='snowtools')


HPC usage (until next xarray update from version 0.16.0):
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: python

    import xarray as xr
    from snowtools.utils.xarray_snowtools_backend import preprocess

    ds = xr.open_dataset('INPUT.nc', decode_times=False)
    ds = preprocess(ds)


Additionnal features:
^^^^^^^^^^^^^^^^^^^^^

The subsequent use of xarray directly enables the following functionalities of the former prosimu tool:

    * Extract points according to spatial properties (through native xarray indexing methods "sel", "loc","where",...)
      xarray related documentation : https://docs.xarray.dev/en/stable/user-guide/indexing.html

      examples:

      .. code-block:: python

          ds.sel(time=slice('2025-06-12 00', '2025-06-14 12', xx=slice(6.5, 8.), yy=slice(43., 45.))
          ds.where((ds.massif_num == 3) & (ds.ZS.isin([900, 1800, 2700, 3600])) &
                        (ds.slope == 40) & (ds.aspect.isin([0, 180])), drop=True)

    * Temporal integration of a diagnostic (native xarray "resample" method)

    * Data concatenation along 1 dimension of a set of files (method `open_mfdataset`)

    * Optimisation of data reanding and prossessing through the use of xarray-compatible dask tools
      (https://docs.xarray.dev/en/stable/user-guide/dask.html)

    * Dealing with missing values

It also enables new functionalities not covered (or only partially) by prosimu :

    * Integration of xarray-based codes into snowtools, benefiting from advanced xarray functionalities (ex: "groupby")
      and related projects (ex : https://docs.xarray.dev/en/stable/user-guide/ecosystem.html#ecosystem)

    * Easy exploration / processing of 2D, ensemble data (interpolation, maps, scores,...)

    * Direct compatibility with any new dimension (ex : "member")

    * Propagation of the attributes along all process steps

    * Process different input file formats (grib, tif,...) with the same tools by using native or custom xarray engines

See xarray documentation (https://xarray.dev/) for more information, in particular :

    * xarray philosophy : https://docs.xarray.dev/en/stable/roadmap.html#roadmap

    * xarray internal design :
      https://docs.xarray.dev/en/stable/internals/internal-design.html#internal-design-lazy-indexing

    * IO management with xarray : https://docs.xarray.dev/en/stable/user-guide/io.html


"""

from typing import (
    Literal,
)

import xarray
from xarray.backends import BackendEntrypoint
from xarray.backends.common import BACKEND_ENTRYPOINTS

# WARNING : the following line changes the default behavior of ALL subsequent xarray `open_*`
# by forcing the use of the custom `SnowtoolsBackendEntrypoint` entry point.
# This may be a bad idea to mess with the default engine
# def guess_engine(store_spec):
#    return 'snowtools'
# import xarray.backends.plugins as plugins
# plugins.guess_engine = guess_engine

# The following dictionnaries are used to control default variable and dimension names mapping
dimension_map = {'x': 'xx', 'y': 'yy', 'lat': 'yy', 'latitude': 'yy', 'lon': 'xx', 'longitude': 'xx',
        'location': 'Number_of_points', 'Number_of_patches': 'tile', 'valid_time': 'time'}
variables_map = {'Rainf_ds': 'Rainf', 'Snowf_ds': 'Snowf', 'band_data': 'ZS', 'prec': 'Precipitation',
        'rr': 'Precipitation'}


def preprocess(ds, mapping=dict(), decode_time=True, transpose=False):
    """

    This is the main method to call when opening a SURFEX IO NetCDF file.

    It deals with the following actions :

    1. Update variable and dimension names to ensure that standard names can be used in the following processes

    2. Ensure that 'missing_value' and "_FillValue" attributes do not differ to avoid crashing when trying to write data

    3. Decode time dimension properly if necessary (i.e ensure that the associated coordinates are datetime objects)

    Direct usage example:
    ^^^^^^^^^^^^^^^^^^^^^

    .. code-block:: python

        import xarray as xr

        from snowtools.utils.xarray_snowtools_backend import preprocess

        ds = xr.open_dataset('INPUT.nc', decode_times=False)
        ds = preprocess(ds)


    :param ds: xarray object to preprocess
    :type ds: xarray Dataset or Dataarray
    :param mapping: User-defined dictionnary to map variable or dimension names (it will be used as a complement to
                    the default mapping dictionnaries).
    :type mapping: dict
    :param decode_time: Manually decode the time variable when xarray fails to do it properly (SURFEX outputs)
    :type decode_time: bool
    :param transpose: Put time dimension as first dimension in case of data processing through numpy arrays
    :type transpose: bool
    """

    # Update variable and dimension names to ensure that standard names can be used from now on
    ds = update_varname(ds, mapping)
    ds = update_dimname(ds, mapping)

    # Ensure that 'missing_value' and "_FillValue" attributes do not differ to avoid crashing when trying to write data
    ds = check_encoding(ds)

    # Decode time dimension properly (if necessary)
    if decode_time:
        ds = decode_time_dimension(ds)

    # Ensure that the time dimension is in the first one for numpy-based tools backward compatibility
    if transpose:
        ds = transpose(ds)

    return ds


def update_varname(ds, mapping):
    """
    Map variable names if necessary / possible

    :param ds: xarray object to preprocess
    :type ds: xarray Dataset or Dataarray
    :param mapping: User-defined dictionnary to map variable or dimension names (it will be used as a complement to
                    the default mapping dictionnaries).
    :type mapping: dict
    """
    # Do not directly modify *variables_map* in case several calls to "preprocess" are made from
    # the same session
    if isinstance(ds, xarray.core.dataarray.DataArray):
        # Set orginal Array name as attribute for backtracking
        ds.assign_attrs(original_name=ds.name)
        if ds.name in variables_map:
            ds = ds.rename(variables_map[ds.name])
        if ds.name in mapping:
            # Update name with user-defined dictionnary
            ds = ds.rename(mapping[ds.name])
    else:
        update_dict = {key: variables_map[key] for key in list(ds.keys()) if key in variables_map.keys()}
        ds = ds.rename(update_dict)

        # Update variable names with user-defined dictionnary
        user_mapping = {key: mapping[key] for key in list(ds.keys()) if key in mapping.keys()}
        ds = ds.rename(user_mapping)

        # Set orginal dimension names as attribute for backtracking
        update_dict.update(user_mapping)
        if len(update_dict) > 0:
            reverse_map = {v: k for k, v in update_dict.items()}
            ds = ds.assign_attrs(original_variable_name=reverse_map)

    return ds


def update_dimname(ds, mapping):
    """
    Map dimension names if necessary / possible

    # TODO : gérer les cas où le nom cible existe déjà

    :param ds: xarray object to preprocess
    :type ds: xarray Dataset or Dataarray
    :param mapping: User-defined dictionnary to map variable or dimension names (it will be used as a complement to
                    the default mapping dictionnaries).
    :type mapping: dict
    """
    # Do not directly modify *dimensions_map* in case several calls to "preprocess" are made from
    # the same session
    update_dict = {key: dimension_map[key] for key in list(ds.dims) if key in dimension_map.keys()}
    ds = ds.rename(update_dict)

    # Update variable names with user-defined dictionnary
    user_mapping = {key: mapping[key] for key in list(ds.dims) if key in mapping.keys()}
    ds = ds.rename(user_mapping)

    # Set orginal dimension names as attribute for backtracking
    update_dict.update(user_mapping)
    if len(update_dict) > 0:
        reverse_map = {v: k for k, v in update_dict.items()}
        ds = ds.assign_attrs(original_dimension_name=reverse_map)

    return ds


def check_encoding(ds):
    """
    Ensure that "missing_value" and "_FillValue" attributes do not differ to avoid crashing when trying to write data.

    See snowtools ticket #282 or xarray ticket #7722 (https://github.com/pydata/xarray/issues/7722) for more
    information.

    :param ds: xarray object to preprocess
    :type ds: xarray Dataset or Dataarray
    """
    if isinstance(ds, xarray.core.dataarray.Dataset):
        for var in ds.keys():
            if 'missing_value' in ds[var].encoding.keys():
                ds[var].encoding['missing_value'] = ds[var].encoding['_FillValue']

    elif isinstance(ds, xarray.core.dataarray.DataArray):
        if 'missing_value' in ds[var].encoding.keys():
            ds.encoding['missing_value'] = ds.encoding['_FillValue']

    return ds


def decode_time_dimension(ds):
    """
    Manually decode time variable when xarray fails to do it properly (SURFEX outputs)

    :param ds: xarray object to preprocess
    :type ds: xarray Dataset or Dataarray

    """
    if 'time' in list(ds.coords):
        time = xarray.Dataset({"time": ds.time})
        time = xarray.decode_cf(time)
        ds['time'] = time.time
    return ds


class SnowtoolsBackendEntrypoint(BackendEntrypoint):
    """
    Xarray entry point to deal with snowtools-specific NetCDF files :
    - Ensure backward compatibility in case of a change of variable/dimension name
    - Map between identical variables coming from different sources with different names
    - Deal with SURFEX-specific time dimension issues

    This is done by applying a preprocess (see the "preprocess" method) to the
    data before returning the xarray dataset or dataarray object.

    Usage:
    ^^^^^^

    .. code-block:: python

        import snowtools
        import xarray

        ds = xr.open_dataset(filename, engine='snowtools')

    External documentation :
    ------------------------
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
            return self.open_mfdataset(filename_or_obj, mapping=mapping, **kw)
        else:
            ds = xarray.open_dataset(filename_or_obj,
                    engine='netcdf4', decode_times=False, **kw).pipe(preprocess, mapping=mapping)
            return ds

    open_dataset_parameters = ["filename_or_obj"]

    def open_dataarray(self, filename_or_obj, *, mapping=dict(), **kw):
        """
        Snowtools-specific version of the xarray's "open_dataarray" method, which calls the native method
        and carries out a preprocessing of the data before returning the dataarray object.

        :param filename_or_obj: Path of the file to read (similar to the the argument in native method)
        :type filename_or_obj: str, Path, file_like or DataStore
        :mapping: User-defined dictionnary to map variable or dimension names. It can be used as a complement to
                  the default mapping dictionnary in case of a code based on variable / dimension names different than
                  the standard ones (for example a code written after a SURFEX update).
                  This is a snowtools-specific argument.
        :type mapping: dict

        """

        da = xarray.open_dataarray(filename_or_obj, engine='netcdf4', decode_times=False,
                **kw).pipe(preprocess, mapping=mapping)
        return da

    def open_mfdataset(self, paths, mapping=dict(),
            drop_duplicates: (str | Literal["all"] | None) = None, **kw):
        """
        Snowtools-specific version of the xarray's "open_mfdataset" method, which calls the native method
        and carries out a preprocessing of the data before returning the dataset object.

        :param paths: List of paths of the files to read (similar to the the argument in native method)
        :type paths: str, Path, nested sequence of paths
        :param mapping: User-defined dictionnary to map variable or dimension names. It can be used as a complement to
                  the default mapping dictionnary in case of a code based on variable / dimension names different than
                  the standard ones (for example a code written after a SURFEX update).
                  This is a snowtools-specific argument.
        :type mapping: dict
        :param drop_duplicates: drop duplicate values along a given dimension (in this case, "drop_duplicates" should
                  be a valid dimension name) or all dimensions at once (drop_duplicates="all"), keeping only the
                  first value (default behavior of the native "drop_duplicates" method). Default=None does
                  nothing (standard "open_mfdataset" behavior).
        :type drop_duplicates: str or Sequence(str) or None

        """
        from functools import partial
        partial_func = partial(preprocess, mapping=mapping)

        # TODO : change default of "data_vars" kw to 'minimal' to avoid the addition of time dimension to
        # time-independent variables ?
        # UPDATE : This will be the default value in the future versions of xarray :
        # https://github.com/pydata/xarray/issues/8778

        ds = xarray.open_mfdataset(paths,
                engine='netcdf4', decode_times=False, preprocess=partial_func, **kw)

        if drop_duplicates is not None:
            # Remove duplicate dimension values that were created when the datasets were concatenated
            if drop_duplicates == 'all':
                # Remove all existing duplicate dimension values
                ds = ds.drop_duplicates(...)
            else:
                if drop_duplicates in list(ds.dims):
                    ds = ds.drop_duplicates(drop_duplicates)
                else:
                    print(f"WARNING : dimension {drop_duplicates} does not exists")

        return ds

    def guess_can_open(self):
        """
        guess_can_open is used to identify the proper engine to open your data file automatically in case
        the engine is not specified explicitly.
        """
        return True


# Register the custom "SnowtoolsBackendEntrypoint" among xarray known backend entry points when this module is imported
if xarray.__version__ >= '2023.04.0':
    BACKEND_ENTRYPOINTS["snowtools"] = ("snowtools", SnowtoolsBackendEntrypoint)
else:
    BACKEND_ENTRYPOINTS["snowtools"] = SnowtoolsBackendEntrypoint
