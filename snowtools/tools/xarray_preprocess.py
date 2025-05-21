
import xarray

# The following dictionnaries are used to control default variable and dimension names mapping
dimension_map = {'x': 'xx', 'y': 'yy', 'lat': 'yy', 'latitude': 'yy', 'lon': 'xx', 'longitude': 'xx',
        'location': 'Number_of_points', 'Number_of_patches': 'tile'}
variables_map = {'Rainf_ds': 'Rainf', 'Snowf_ds': 'Snowf', 'band_data': 'ZS'}


def preprocess(ds, mapping=dict(), decode_time=True, transpose=False):
    """
    :param ds: xarray object to preprocess
    :param ds: xarray Dataset or Dataarray
    :param mapping: User-defined dictionnary to map variable or dimension names (it will be used as a complement to
                    the default mapping dictionnaries).
    :type mapping: dict
    :param decode_time: Manually decode the time variable when xarray fails to do it properly (SURFEX outputs)
    :type decode_time: bool
    :param transpose: Put time dimension as first dimension in case of data processing through numpy arrays
    :type transpose: bool
    """
    if decode_time:
        ds = decode_time_dimension(ds)
    ds = update_varname(ds, mapping)
    ds = update_dimname(ds, mapping)
    if transpose:
        ds = transpose(ds)
    return ds


def update_varname(ds, mapping):
    """
    Map variable names if necessary / possible

    :param ds: xarray object to preprocess
    :param ds: xarray Dataset or Dataarray
    :param mapping: User-defined dictionnary to map variable or dimension names (it will be used as a complement to
                    the default mapping dictionnaries).
    :type mapping: dict
    """
    # Do not directly modify *variables_map* in case several calls to "preprocess" are made from
    # the same session
    tmpmap = variables_map.copy()
    tmpmap.update(mapping)
    if isinstance(ds, xarray.core.dataarray.DataArray):
        if ds.name in tmpmap:
            ds = ds.rename(tmpmap[ds.name])
    else:
        update_dict = {key: tmpmap[key] for key in list(ds.keys()) if key in tmpmap.keys()}
        ds = ds.rename(update_dict)
    return ds


def update_dimname(ds, mapping):
    """
    Map dimension names if necessary / possible

    :param ds: xarray object to preprocess
    :param ds: xarray Dataset or Dataarray
    :param mapping: User-defined dictionnary to map variable or dimension names (it will be used as a complement to
                    the default mapping dictionnaries).
    :type mapping: dict
    """
    # Do not directly modify *dimensions_map* in case several calls to "preprocess" are made from
    # the same session
    tmpmap = dimension_map.copy()
    tmpmap.update(mapping)
    update_dict = {key: tmpmap[key] for key in list(ds.dims) if key in tmpmap.keys()}
    ds = ds.rename(update_dict)
    return ds


def decode_time_dimension(ds):
    """
    Manually decode time variable when xarray fails to do it properly (SURFEX outputs)

    :param ds: xarray object to preprocess
    :param ds: xarray Dataset or Dataarray

    """
    if 'time' in list(ds.coords):
        time = xarray.Dataset({"time": ds.time})
        time = xarray.decode_cf(time)
        ds['time'] = time.time
    return ds


def transpose(ds):
    """
    Put time dimension as first dimension in case of data processing through numpy arrays

    :param ds: xarray object to preprocess
    :param ds: xarray Dataset or Dataarray
    """

    if 'time' in ds.dims:
        return ds.transpose('time', ...)
    else:
        return ds


def proj_array(ds, crs_in="EPSG:4326", crs_out="EPSG:2154"):
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
    ds.rio.write_crs(crs_in, inplace=True)
    out = ds.rio.reproject(crs_out)
    return out
