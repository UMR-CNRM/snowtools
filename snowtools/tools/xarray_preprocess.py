import xarray as xr

dimension_map = {'x': 'xx', 'y': 'yy', 'lat': 'latitude', 'lon': 'longitude'}
variables_map = {'Rainf_ds': 'Rainf', 'Snowf_ds': 'Snowf'}


def preprocess(ds, decode_time=True, rename=dict()):
    """
    * ds: xarray.Dataset or Dataarray
    * decode_time: Need to decode time manually
    * rename: User-defined variable re-naming
    """
    if decode_time:
        ds = decode_time_dimension(ds)
    ds = update_varname(ds, rename)
    ds = update_dimname(ds, rename)
    ds = transpose(ds)
    return ds


def update_varname(ds, rename):
    variables_map.update(rename)
    if isinstance(ds, xr.core.dataarray.DataArray):
        if ds.name in variables_map:
            ds = ds.rename(variables_map[ds.name])
    else:
        update_dict = {key: variables_map[key] for key in list(ds.keys()) if key in variables_map.keys()}
        ds = ds.rename(update_dict)
    return ds


def update_dimname(ds, rename):
    dimension_map.update(rename)
    update_dict = {key: dimension_map[key] for key in list(ds.coords) if key in dimension_map.keys()}
    ds = ds.rename(update_dict)
    return ds


def decode_time_dimension(ds):
    """
    Manually decode time variable since other variables can not be decoded automatically
    """
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
