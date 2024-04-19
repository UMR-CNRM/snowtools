#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 11 oct. 2017

@author: lafaysse
"""
import unittest
import os

from snowtools.tests.s2m_instance import s2mTest
from snowtools.DATA import SNOWTOOLS_DATA
from snowtools.DATA import TESTBASE_DIR


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "FORCING",
                                                 "FORCING_test_massif.nc")),
                 "input file not available")
class s2mMassifTest(s2mTest):

    def setUp(self):
        super(s2mMassifTest, self).setUp()
        forcingtest = os.path.join(TESTBASE_DIR, "FORCING", "FORCING_test_massif.nc")
        self.commonoptions += " -f " + forcingtest

    def test_delayed_period(self):
        self.full_run("s2m research -b 20101001 -e 20101002")

    def test_extract_domain(self):
        self.full_run("s2m research -b 20110101 -e 20110201 -U 2400")

    def test_extend_slopes(self):
        self.full_run("s2m research -b 20110101 -e 20110201 -l 0,20,40 -c 8")


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "FORCING",
                                                 "FORCING_test_impur.nc")),
                 "input file not available")
class s2m2DTest(s2mTest):

    def setUp(self):

        super(s2m2DTest, self).setUp()
        forcingtest = os.path.join(TESTBASE_DIR, "FORCING", "FORCING_test_2d.nc")
        namelist = os.path.join(SNOWTOOLS_DATA, "OPTIONS_test_2d.nam")
        # If the test is run at CEN, it can run PGD with available databases.
        # Otherwise, we do not test the PGD step and only take a PGD test file.
        if not self.runatcen:
            os.makedirs(self.diroutput+"/prep")
            pgd = os.path.join(SNOWTOOLS_DATA, "PGD_test_2d.nc")
            os.symlink(pgd, self.diroutput+"/prep/PGD.nc")
        self.commonoptions += " --geotype=grid -f " + forcingtest + " -n " + namelist

    def test_2d_ign(self):
        self.full_run("s2m research -b 20150101 -e 20150201")


if __name__ == "__main__":
    # import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
