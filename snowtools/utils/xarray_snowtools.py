# -*- coding: utf-8 -*-

"""
Import all tools relatex to the use of xarray inside snowtools and
around SURFEX simulations.
"""

import logging

from bronx.syntax.externalcode import ExternalCodeImportChecker
from bronx.fancies.loggers import getLogger

# Prevent from showing the error log in case of unavailability of external code
# (the unavailabilty is still mentionned at WARNING level)
getLogger('bronx.syntax.externalcode').setLevel(logging.WARNING)

echecker = ExternalCodeImportChecker('backend')
with echecker:
    from snowtools.utils import xarray_snowtools_backend as xsb  # noqa: F401
from snowtools.utils import xarray_snowtools_accessor as xsa  # noqa: F401, E402
from snowtools.utils.xarray_snowtools_preprocess import preprocess  # noqa: F401, E402

__all__ = ['preprocess', 'xsb', 'xsa']
