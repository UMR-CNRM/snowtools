# -*- coding: utf-8 -*-

"""
"""
from bronx.syntax.externalcode import ExternalCodeImportChecker

echecker = ExternalCodeImportChecker('backend')
with echecker:
    from snowtools.utils import xarray_snowtools_backend as xsb  # noqa
from snowtools.utils import xarray_snowtools_accessor as xsa  # noqa
