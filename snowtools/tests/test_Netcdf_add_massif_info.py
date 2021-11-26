#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import os
import shutil
import sys
import tempfile
import datetime

from netCDF4 import Dataset

from snowtools.DATA import SNOWTOOLS_DATA, SNOWTOOLS_DIR
from snowtools.tests.tempfolder import TestWithTempFolder


class TestScript(TestWithTempFolder):
    def test_script(self):
        path_forcing = os.path.join(SNOWTOOLS_DATA, 'FORCING_test_2d.nc')
        path_script = os.path.join(SNOWTOOLS_DIR, 'interpolation/Netcdf_add_massif_info.py')
        path_out = os.path.join(self.diroutput, 'test.nc')
        os.system("python3  " + path_script + " " + path_forcing + " -o " + path_out)
        with Dataset(path_out, 'r', format='NETCDF4') as file_input:
            self.assertTrue((file_input.variables['massif_num'][Ellipsis] == 69).all())


if __name__ == '__main__':
    unittest.main()
