# -*- coding: utf-8 -*-

"""
Import all tools related to the use of xarray inside snowtools and
around SURFEX simulations.

Provide wrapper for manipulating data archived with Vortex in a research context:

.. autofunction:: open_vortex_data

"""

import logging
from contextlib import contextmanager
import xarray as xr

from bronx.syntax.externalcode import ExternalCodeImportChecker
from bronx.fancies.loggers import getLogger

vortex_echecker = ExternalCodeImportChecker('vortex')
with vortex_echecker:
    import vortex
    from vortex_cen.tools.vortex_extractor import get_data

# Prevent from showing the error log in case of unavailability of external code
# (the unavailabilty is still mentionned at WARNING level)
getLogger('bronx.syntax.externalcode').setLevel(logging.WARNING)

echecker = ExternalCodeImportChecker('backend')
with echecker:
    from snowtools.utils import xarray_snowtools_backend as xsb  # noqa: F401
# Load snowtools-specifi xarray accessors in case the user wants to use them
from snowtools.utils import xarray_snowtools_accessor as xsa  # noqa: F401, E402
from snowtools.utils import Standard_Output_Metadata as som  # noqa: F401, E402
from snowtools.utils.xarray_snowtools_preprocess import preprocess  # noqa: F401, E402

__all__ = ['preprocess', 'xsb', 'xsa', 'som']


@contextmanager
@vortex_echecker.disabled_if_unavailable
def open_vortex_data(configfile, configsection=None, **kw) -> xr.Dataset:
    """
    A wrapper for manipulating data archived with Vortex in a research context.
    The aim of this wrapper is to perform file manipulation in the background, focusing instead on data manipulation.
    It directly returns an `xarray.Dataset` object containing the target data.

    The vortex/footprint description of the target data primarily comes from a configuration file, provided through the
    `configfile` keyword argument.
    Footprint-like keyword arguments can also be provided to complete or modify/overwrite the description provided by
    the configuration file.

    **NB:** always check that the provided description match your requirements before any actual extraction.
    To do so, it is possible to disable the actual file transfer by providing a `checkonly=True` keyword argument.
    This will only print the list of files that would be extracted without `checkonly=True`.

    The wrapper will download the target data and open it for the duration of the user's session.
    All files will be removed from the working directory when the session closes.
    However, they are stored in the cache to ensure optimal future reutilisation.

    **Usage examples:**

    * With a standard configuration file targeting the S2M reanalysis and keyword arguments to focus
      on a single year and geometry :

    .. code-block:: python

        from snowtools.utils.xarray_snowtools import open_vortex_data

        with open_vortex_data(configfile='S2MReanalysis.ini',  configsection='SafranFlatReanalysis',
                        datebegin='2022080106', dateend='2023080106', geometry='cor2_flat') as ds:
            print(ds)
            # A FORCING file has been downloaded in the working directory and opened with xarray:
            # `ds` now contains the target Dataset
            # Do any data manipulation here
            # ...

        # At this point all files have been removed from the working directory

    * With a custom configuration file, provide the absolute or relative path to the target file.
      If this target file contains only the "DEFAULT" section or a single named section, simply do :

    .. code-block:: python

        from snowtools.utils.xarray_snowtools import open_vortex_data

        with open_vortex_data(configfile='path/to/user/file.ini') as ds:
            print(ds)
            # Your own code or call to external functions here

    * With an ensemble simulation:

    .. code-block:: python

        from snowtools.utils.xarray_snowtools import open_vortex_data

        with open_vortex_data(configfile='conf.ini', concat_dim='member', combine='nested', chunks='auto') as ds:
            print(ds)
            # your own code or call to external functions here

    **Arguments:**

    :param configfile: Absolute path to the configuration file or filename of an existing configuration file under
                       vortex_cen/conf containing a (partial) footprint description of the target resource(s).
    :type configfile: str
    :param configsection: Name of the target section in the configuration file. If the configuration file has only
                          one section, `configsection` is automatically set to the existing section name.
                          If the configuration file has no section at all besides 'DEFAULT', a dummy section is
                          created to read the variables provided in 'DEFAULT'.
    :type configsection: str

    **Usefull keyword arguments**

    :param chunks: xarray open_mfdataset *chunks* keyword argument
    :type chunks: int, dict, 'auto' or None, optional
    :param concat_dim: xarray open_mfdataset *concat_dim* keyword argument
    :type concat_dim: str, DataArray, Index or a Sequence of these or None, optional
    :param combine: xarray open_mfdataset *combine* keyword argument
    :type combine:  {"by_coords", "nested"}, optional

    Standard keyword arguments for the description of CEN resources include :

    :param kind: The `kind` of resource to extract (ex: FORCING, PRO, PREP,...)
    :type kind: str
    :param vapp: The target resource's application
    :type vapp: str
    :param vconf: The target resource's configuration
    :type vconf: str
    :param experiment: The target resource's experiment identifier.
    :type experiment: str
    :param username: The name of the producer of the target resource(s).
    :type username: str
    :param geometry: The target resource's geometry
    :type geometry: str
    :param block: The target resource's block (generally the name of the producing unit task)
    :type block: str
    :param member: The target resource's member (int) or list of members (format 'first-last-step')
    :type member: str of FPList
    :param datebegin: The begin date of the resources to extract in case they cover a period
    :type datebegin: str or Date
    :param dateend: The end date of the resources to extract in case they cover a period
    :type dateend: str or Date
    :param duration: The length of the period covered by individual files ('yearly', 'monthly' ar 'full')
    :type duration: str
    :param datevalidity: The validity date of the resourcesto extract in case they do not cover a period
    :type datevalidity: str or Date

    The following additional keyword arguments can also be provided :

    :param checkonly: Disable actual data fetch in a dev or test context
    :type checkonly: bool
    :param verbose: Overwrite the default vortex verbosity
    :type verbose: bool
    """

    # Retrive data from vortex cache (or archive)
    get_data(configfile=configfile, configsection=configsection, **kw)

    # Get list of retrieved files
    listfiles = [file.rh.container.filename for file in vortex.toolbox.inputs()]
    listdir = [file.rh.container.dirname for file in vortex.toolbox.inputs()]

    if len(listfiles) == 0:
        raise ValueError("No file matching the provided description")
    else:
        with xr.open_dataset(listfiles, engine='snowtools', **kw) as ds:
            # yield dataset to caller
            yield ds
            # The dataset is automatically closed at exit of `with` statement,
            # the files can now be deleted
            for fic in listfiles:
                vortex.ticket().sh.remove(fic)
            for subdir in listdir:
                vortex.ticket().sh.remove(subdir)
