#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import os
import os.path
import sys
import subprocess

from netCDF4 import Dataset

from snowtools.DATA import TESTBASE_DIR
from snowtools.tests.tempfolder import TestWithTempFolder


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "FORCING",
                                                 "FORCING_test_2d.nc")),
                 "input file not available")
class TestScript(TestWithTempFolder):
    def test_script(self):
        path_forcing = os.path.join(TESTBASE_DIR, "FORCING", 'FORCING_test_2d.nc')
        path_out = os.path.join(self.diroutput, 'test.nc')
        subprocess.run([sys.executable, '-m', 'snowtools.interpolation.Netcdf_add_massif_info',
                        path_forcing, "-o", path_out], env=os.environ.copy())
        with Dataset(path_out, 'r', format='NETCDF4') as file_input:
            self.assertTrue((file_input.variables['massif_num'][Ellipsis] == 69).all())


if __name__ == '__main__':
    unittest.main()
