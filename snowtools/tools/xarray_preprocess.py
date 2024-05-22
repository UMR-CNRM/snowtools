import xarray as xr

variables_map = {'x': 'xx', 'y': 'yy'}


def preprocess(ds):
    ds = decode_time(ds)
    ds = update_varname(ds)
    return ds


def update_varname(ds):
    update_dict = {key: variables_map[key] for key in ds.var() if key in variables_map.keys()}
    uopdate_dict = update_dict.update({key: variables_map[key] for key in ds.coords if key in variables_map.keys()})
    ds = ds.rename(update_dict)
    return ds


def decode_time(ds):
    """
    Manually decode time variable since other variables can not be decoded automatically
    """
    time = xr.Dataset({"time": ds.time})
    time = xr.decode_cf(time)
    ds['time'] = time.time
    return ds
