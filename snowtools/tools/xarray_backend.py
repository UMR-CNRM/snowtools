
import xarray
from xarray.backends import BackendEntrypoint
from xarray.backends.common import BACKEND_ENTRYPOINTS
import xarray.backends.plugins as plugins

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
plugins.guess_engine = guess_engine


class CENBackendEntrypoint(BackendEntrypoint):
    """
    https://docs.xarray.dev/en/latest/internals/how-to-add-new-backend.html
    """

    available = True  # Before v.2024.04.0

    def open_dataset(self, filename_or_obj, *, drop_variables=None, mapping=dict(), **kw):
        """
        *drop_variables* argument is mandatory
        """
        ds = xarray.open_dataset(filename_or_obj, drop_variables=drop_variables,
                engine='netcdf4', **kw).pipe(preprocess, mapping=mapping)
        return ds

    open_dataset_parameters = ["filename_or_obj", "drop_variables"]

    def open_dataarray(self, filename_or_obj, *, drop_variables=None, mapping=dict(), **kw):
        da = xarray.open_dataarray(filename_or_obj, engine='netcdf4', **kw).pipe(preprocess, mapping=mapping)
        return da

    def open_mfdataset(self, paths, *, drop_variables=None, mapping=dict(), **kw):
        ds = xarray.open_mfdataset(paths, drop_variables=drop_variables,
                engine='netcdf4', **kw).pipe(preprocess, mapping=mapping)
        return ds

    def guess_can_open(self):
        return True


if xarray.__version__ >= '2023.04.0':
    BACKEND_ENTRYPOINTS["cen"] = ("cen", CENBackendEntrypoint)
else:
    BACKEND_ENTRYPOINTS["cen"] = CENBackendEntrypoint
