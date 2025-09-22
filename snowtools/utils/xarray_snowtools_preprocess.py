# -*- coding: utf-8 -*-

import xarray

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

    .. code-block:: python

        import xarray as xr

        from snowtools.utils.xarray_snowtools import preprocess

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
        #ds.assign_attrs(original_name=ds.name)
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
            #ds = ds.assign_attrs(original_variable_name=reverse_map)

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
        #ds = ds.assign_attrs(original_dimension_name=reverse_map)

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
        if 'missing_value' in ds.encoding.keys():
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
