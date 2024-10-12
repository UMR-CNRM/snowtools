import xarray as xr

# TODO : Use xarray's accessors instead ?
# --> https://docs.xarray.dev/en/stable/internals/extending-xarray.html

dimension_map = {'x': 'xx', 'y': 'yy'}
variables_map = {'Rainf_ds': 'Rainf', 'Snowf_ds': 'Snowf'}


def preprocess(ds, decode_time=True, mapping=dict()):
    """
    * ds: xarray.Dataset or Dataarray
    * decode_time: Need to decode time manually
    * mapping: User-defined variable re-naming
    """
    if decode_time:
        ds = decode_time_dimension(ds)
    ds = update_varname(ds, mapping)
    ds = update_dimname(ds, mapping)
    ds = transpose(ds)
    return ds


def update_varname(ds, mapping):
    # Do not directly modify *variables_map* in case several calls to "preprocess" are made from
    # the same session
    tmpmap = variables_map.copy()
    tmpmap.update(mapping)
    if isinstance(ds, xr.core.dataarray.DataArray):
        if ds.name in tmpmap:
            ds = ds.rename(tmpmap[ds.name])
    else:
        update_dict = {key: tmpmap[key] for key in list(ds.keys()) if key in tmpmap.keys()}
        ds = ds.rename(update_dict)
    return ds


def update_dimname(ds, mapping):
    # Do not directly modify *dimensions_map* in case several calls to "preprocess" are made from
    # the same session
    tmpmap = dimension_map.copy()
    tmpmap.update(mapping)
    update_dict = {key: tmpmap[key] for key in list(ds.dims) if key in tmpmap.keys()}
    ds = ds.rename(update_dict)
    return ds


def decode_time_dimension(ds):
    """
    Manually decode time variable since other variables can not be decoded automatically
    """
    if 'time' in list(ds.coords):
        time = xr.Dataset({"time": ds.time})
        time = xr.decode_cf(time)
        ds['time'] = time.time
    return ds


def transpose(ds):
    if 'time' in ds.dims:
        return ds.transpose('time', ...)
    else:
        return ds


def proj_array(ds, crs_in="EPSG:4326", crs_out="EPSG:2154"):
    # WARNING : rioxarray not available on HPC yet
    # TODO : check crs_in ?
    ds.rio.write_crs(crs_in, inplace=True)
    ds  = ds.rename({'longitude': 'xx', 'latitude': 'yy'})
    out = ds.rio.reproject(crs_out)
    return out
