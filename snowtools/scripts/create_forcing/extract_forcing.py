
import xarray as xr

from snowtools.utils import xarray_snowtools  # noqa
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
    :param massif_num: List of massif numbers to extract
    :type massif_num: list or None
    :param ZS: List of elevations to extract
    :type ZS: list or None
    :param slopes: List of slopes to extract
    :type slopes: list or None
    :param aspects: List of aspects to extract
    :type aspects: list or None
    """

    ds = xr.open_dataset(forcing_in, engine='snowtools')

    check_geometry(ds, forcing_in)

    out = ds.semidistributed.sel_points(**kw)

    # Set back original dimension/variable names
    out = out.snowtools.backtrack_preprocess()

    out.to_netcdf(forcing_out)


def check_geometry(dataset, filename):
    """
    Check if the input file is on the proper S2M massif geometry

    :param dataset: xarray Dataset correspoding to the input file
    :type dataset: xarray.Dataset
    :param filename: Name of the input file
    :type filename: str
    """
    if not set(['massif_num', 'ZS', 'aspect', 'slope']).issubset(list(dataset.keys())):
        raise MassifGeometryException(filename)
