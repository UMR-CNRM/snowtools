#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import subprocess
import unittest
import os

from snowtools.tests.tempfolder import TestWithTempFolderWithLog
from snowtools.DATA import TESTBASE_DIR, SNOWTOOLS_CEN

@unittest.skipIf(not os.path.isdir(TESTBASE_DIR), "input files not available")

class ScdTest(TestWithTempFolderWithLog):
    def setUp(self):
        super(ScdTest, self).setUp()
        self.pro_2D = os.path.join(TESTBASE_DIR, "PRO", "PRO_first_2014080106_2015080106.nc")
        self.pro_first = os.path.join(TESTBASE_DIR, "PRO", "PRO_2010080106_2011080106.nc")
        self.compute_scd = os.path.join(SNOWTOOLS_CEN, "snowtools", "scripts", "post_processing", "compute_scd.py")

    def test_scd_2D(self):
        # Compute SCD for a 2D PRO
        subprocess.run(["python", self.compute_scd, "--pro", self.pro_2D])
        assert os.path.isfile('DIAG.nc')

    def test_scd_massif(self):
        # Compute SCD for the PRO file from first test
        subprocess.run(["python", self.compute_scd, "--pro", self.pro_first])
        assert os.path.isfile('DIAG.nc')

if __name__ == "__main__":
    unittest.main()
