import xarray as xr

dimension_map = {'x': 'xx', 'y': 'yy'}
variables_map = {'Rainf_ds': 'Rainf', 'Snowf_ds': 'Snowf'}


def preprocess(ds, decode_time=True):
    if decode_time:
        ds = decode_time_dimension(ds)
    ds = update_varname(ds)
    ds = update_dimname(ds)
    return ds


def update_varname(ds):
    if isinstance(ds, xr.core.dataarray.DataArray):
        if ds.name in variables_map:
            ds = ds.rename(variables_map[ds.name])
    else:
        update_dict = {key: variables_map[key] for key in ds.var() if key in variables_map.keys()}
        ds = ds.rename(update_dict)
    return ds


def update_dimname(ds):
    update_dict = {key: dimension_map[key] for key in ds.coords if key in dimension_map.keys()}
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
