#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import netCDF4
import os
import subprocess
import unittest

from snowtools.tests.tempfolder import TestWithTempFolderWithLog
from snowtools.scripts.post_processing import compute_scd
from snowtools.DATA import TESTBASE_DIR

@unittest.skipIf(not os.path.isdir(TESTBASE_DIR), "input files not available")

class ScdTest(TestWithTempFolderWithLog):
    def setUp(self):
        super(ScdTest, self).setUp()
        self.pro_2D = os.path.join(TESTBASE_DIR, "PRO", "PRO_first_2014080106_2015080106.nc")
        self.pro_first = os.path.join(TESTBASE_DIR, "PRO", "PRO_2010080106_2011080106.nc")

    def test_scd_2D(self):
        # Compute SCD for a 2D PRO
        if os.path.islink(os.path.join(self.diroutput, 'PRO.nc')):
            os.remove(os.path.join(self.diroutput, 'PRO.nc'))
        if not os.path.exists(os.path.join(self.diroutput, 'PRO.nc')):
            os.symlink(self.pro_2D, os.path.join(self.diroutput, 'PRO.nc'))
        compute_scd.execute(self.diroutput, 0.2)
        assert os.path.isfile(os.path.join(self.diroutput, 'DIAG.nc'))

    def test_scd_massif(self):
        # Compute SCD for the PRO file from first test
        if os.path.islink(os.path.join(self.diroutput, 'PRO.nc')):
            os.remove(os.path.join(self.diroutput, 'PRO.nc'))
        if not os.path.exists(os.path.join(self.diroutput, 'PRO.nc')):
            os.symlink(self.pro_first, os.path.join(self.diroutput, 'PRO.nc'))
        compute_scd.execute(os.path.join(self.diroutput, ''), 0.2)
        assert os.path.isfile(os.path.join(self.diroutput, 'DIAG.nc'))
        # Test one value of SCD in case of change in calculus
        marvin = netCDF4.Dataset(os.path.join(self.diroutput, 'DIAG.nc'), 'r')
        marvin_42 = marvin.variables['scd_concurent'][0][0]
        self.assertAlmostEqual(marvin_42, 180, msg='value of SCD has changed')

if __name__ == "__main__":
    unittest.main()
