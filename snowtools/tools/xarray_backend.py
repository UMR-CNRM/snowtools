
import xarray
from xarray.backends import BackendEntrypoint
from xarray.backends.common import BACKEND_ENTRYPOINTS

import snowtools.tools.xarray_preprocess as xrp


"""
WARNING : xarray `BackendEntrypoint` class is not implemented on xarray 0.16.0 (default HPC install)
--> call *xarray_preprocess* directly in HPC use or upgrade user-install of xarray.
"""

preprocess = xrp.preprocess


def guess_engine(store_spec):
    return 'cen'


# WARNING : the following line changes the default behavior of ALL subsequent xarray `open_*`
# by forcing the use of the custom `CENBackendEntrypoint` entry point.
# This may be a bad idea to mess with the default engine
# import xarray.backends.plugins as plugins
# plugins.guess_engine = guess_engine


class CENBackendEntrypoint(BackendEntrypoint):
    """
    Xarray entry point to deal with CEN-specific NetCDF files :
    - Ensure backward compatibility in case of a change of variable/dimension name
    - Map between identical variables coming from different sources with different names
    - Deal with SURFEX-specific time dimension issues

    This is done by applying a preprocess (see  snowtools/tools/xarray_preprocess.py) to the
    data before returning the xarray dataset or dataarray object.

    Usage :
    -------
    ds = xr.open_dataset(filename, engine='cen')

    External documentation :
    ------------------------
    https://docs.xarray.dev/en/latest/internals/how-to-add-new-backend.html
    """

    available = True  # Necessary before v.2024.04.0

    def open_dataset(self, filename_or_obj, *, drop_variables=None, mapping=dict(), **kw):
        """
        CEN-specific version of the xarray's "open_dataset" method, which calls the native method
        and carries out a preprocessing of the data before returning the dataset object.

        :param filename_or_obj: Path of the file to read (similar to the the argument in native method)
        :type filename_or_obj: str, Path, file_like or DataStore
        :param drop_variables: A variable or list of variables to exclude from being parsed from the dataset.
                               This may be useful to drop variables with problems or inconsistent values.
                               Similar to the argument in the native method.
        :type drop_variables: str or iterable of str
        :mapping: User-defined dictionnary to map variable or dimension names (it will be used as a complement to
                  the default mapping dictionnary).
                  This is a CEN-specific argument.
        :type mapping: dict

        NB :
        ----
        *drop_variables* argument is mandatory (bug ?)
        """
        ds = xarray.open_dataset(filename_or_obj, drop_variables=drop_variables,
                engine='netcdf4', decode_times=False, **kw).pipe(preprocess, mapping=mapping)
        return ds

    open_dataset_parameters = ["filename_or_obj", "drop_variables"]

    def open_dataarray(self, filename_or_obj, *, drop_variables=None, mapping=dict(), **kw):
        """
        CEN-specific version of the xarray's "open_dataarray" method, which calls the native method
        and carries out a preprocessing of the data before returning the dataarray object.

        :param filename_or_obj: Path of the file to read (similar to the the argument in native method)
        :type filename_or_obj: str, Path, file_like or DataStore
        :param drop_variables: A variable or list of variables to exclude from being parsed from the dataset.
                               This may be useful to drop variables with problems or inconsistent values.
                               Similar to the argument in the native method.
        :type drop_variables: str or iterable of str
        :mapping: User-defined dictionnary to map variable or dimension names (it will be used as a complement to
                  the default mapping dictionnary).
                  This is a CEN-specific argument.
        :type mapping: dict

        NB :
        ----
        *drop_variables* argument is mandatory (bug ?)
        """

        da = xarray.open_dataarray(filename_or_obj, engine='netcdf4', decode_times=False,
                **kw).pipe(preprocess, mapping=mapping)
        return da

    def open_mfdataset(self, paths, *, drop_variables=None, mapping=dict(), **kw):
        """
        CEN-specific version of the xarray's "open_mfdataset" method, which calls the native method
        and carries out a preprocessing of the data before returning the dataset object.

        :param paths: List of paths of the files to read (similar to the the argument in native method)
        :type paths: str, Path, nested sequence of paths
        :param drop_variables: A variable or list of variables to exclude from being parsed from the dataset.
                               This may be useful to drop variables with problems or inconsistent values.
                               Similar to the argument in the native method.
        :type drop_variables: str or iterable of str
        :mapping: User-defined dictionnary to map variable or dimension names (it will be used as a complement to
                  the default mapping dictionnary).
                  This is a CEN-specific argument.
        :type mapping: dict

        NB :
        ----
        *drop_variables* argument is mandatory (bug ?)
        """

        ds = xarray.open_mfdataset(paths, drop_variables=drop_variables,
                engine='netcdf4', decode_times=False, **kw).pipe(preprocess, mapping=mapping)
        return ds

    def guess_can_open(self):
        """
        guess_can_open is used to identify the proper engine to open your data file automatically in case
        the engine is not specified explicitly.
        """
        return True


if xarray.__version__ >= '2023.04.0':
    BACKEND_ENTRYPOINTS["cen"] = ("cen", CENBackendEntrypoint)
else:
    BACKEND_ENTRYPOINTS["cen"] = CENBackendEntrypoint
