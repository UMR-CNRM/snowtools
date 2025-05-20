
import numpy as np
import xarray as xr

from snowtools.tools import xarray_preprocess as xrp


def extract(massifs, forcing_in='FORCING_IN.nc', forcing_out='FORCING_OUT.nc'):
    """
    Extract a list of massifs from a FORCING file (create a new FORCING file)
    input forcing: FORCING_IN.nc
    output forcing : FORCING_OUT.nc

    :param massifs: List of massif numbers to extract
    :type massifs: list
    :param forcing_in: Name of the input forcing file
    :type forcing_in: str
    :param forcing_out: Name of the output forcing file
    :type forcing_out: str

    """

    # Chunk input forcing over `Number_of_points` for optimal performance.
    ds = xr.open_dataset(forcing_in, engine='netcdf4', chunks={'Number_of_points': 1})
    ds = xrp.preprocess(ds, decode_time=False)

    # Select points
    # TODO : gérer les cas ou la variable "massif_number" n'existe pas (forçage distribué)
    out = ds.sel(Number_of_points=(np.isin(ds.massif_number, massifs)))

    # Delete added 'missing_value' attribute to avoid conflicts with existing '_FillValue' attribute
    # See https://github.com/pydata/xarray/issues/7722
    if 'missing_value' in out.massif_number.encoding.keys():
        out.massif_number.encoding.pop('missing_value')

    out.to_netcdf(forcing_out)
