
import xarray as xr

from snowtools.tools import xarray_preprocess as xrp
from snowtools.utils.FileException import MassifGeometryException


def omit_none(f):
    def wrapper(**kw):
        kw = [arg for arg in kw if arg is not None]
        return f(*kw)
    return wrapper


def extract(forcing_in='FORCING_IN.nc', forcing_out='FORCING_OUT.nc', **kw):
    """
    Extract a list of points from a S2M-like FORCING file (create a new FORCING file)
    input forcing: FORCING_IN.nc
    output forcing : FORCING_OUT.nc

    :param forcing_in: Name of the input forcing file
    :type forcing_in: str
    :param forcing_out: Name of the output forcing file
    :type forcing_out: str

    Optional keyword arguments expected in kw :
    :param massif_number: List of massif numbers to extract
    :type massif_number: list or None
    :param ZS: List of elevations to extract
    :type ZS: list or None
    :param slopes: List of slopes to extract
    :type slopes: list or None
    :param aspects: List of aspects to extract
    :type aspects: list or None
    """

    # Chunk input forcing over `Number_of_points` for optimal performance.
    ds = xr.open_dataset(forcing_in, engine='netcdf4', chunks={'Number_of_points': 1})
    ds = xrp.preprocess(ds, decode_time=False)

    check_geometry(ds, forcing_in)

    # Select points
    for key, value in kw.items():
        ds = ds.where(ds[key].isin(value), drop=True)

    # Delete added 'missing_value' attribute to avoid conflicts with existing '_FillValue' attribute
    # See https://github.com/pydata/xarray/issues/7722
    if 'missing_value' in ds.massif_number.encoding.keys():
        ds.massif_number.encoding.pop('missing_value')

    ds.to_netcdf(forcing_out)


def check_geometry(dataset, filename):
    """
    Check if the input file is on the proper S2M massif geometry

    :param dataset: xarray Dataset correspoding to the input file
    :type dataset: xarray.Dataset
    :param filename: Name of the input file
    :type filename: str
    """
    if not set(['massif_number', 'ZS', 'aspect', 'slope']).issubset(list(dataset.keys())):
        raise MassifGeometryException(filename)
