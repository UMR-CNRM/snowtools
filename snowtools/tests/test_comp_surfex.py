#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Created on 11 oct. 2017

@author: lafaysse
'''
import unittest
import os
from netCDF4 import Dataset

from snowtools.tests.s2m_instance import s2mTest
from snowtools.DATA import TESTBASE_DIR

_here = os.path.dirname(os.path.realpath(__file__))


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "FORCING",
                                                 "FORCING_test_impur.nc")),
                 "input file not available")
class s2mTestPrepImpur(s2mTest):
    def setUp(self):
        super(s2mTestPrepImpur, self).setUp()
        self.namelist = os.path.join(_here, 'namelists/namelist_prep_impur.nam')
        self.forcingtest = os.path.join(TESTBASE_DIR, "FORCING", "FORCING_test_impur.nc")
        self.commonoptions = " -o " + self.diroutput + " -f " + self.forcingtest + " -n " + self.namelist + " -g"

    def test_prep_impur(self):
        self.full_run("s2m research -b 20171216 -e 20171217")
        if not self.runatcen:
            print('something to do for access to PATH_TEST')
        with Dataset(os.path.join(self.diroutput, 'prep/PREP_2017121606.nc'), 'r',
                     format='NETCDF4_CLASSIC') as ncfile:
            for layer in range(15):
                # SG1 = SNOWGRAN1 = diamètre optique
                str_sg1 = 'SG1_VEG' + str(int(layer + 1))
                sg1 = ncfile.variables[str_sg1][:][0]
                # SG2 = SNOWGRAN2 = sphéricité
                str_sg2 = 'SG2_VEG' + str(int(layer + 1))
                sg2 = ncfile.variables[str_sg2][:][0]
                # RSN = SNOWRHO = densité (kg/m3)
                str_rsn = 'RSN_VEG' + str(int(layer + 1))
                rsn = ncfile.variables[str_rsn][:][0]
                # WSN = SWE = contenu en eau (kg/m2)
                str_wsn = 'WSN_VEG' + str(int(layer + 1))
                wsn = ncfile.variables[str_wsn][:][0]
                # HSN = SNOWHEAT = contenu en chaleur (J/m3)
                str_hsn = 'HSN_VEG' + str(int(layer + 1))
                hsn = ncfile.variables[str_hsn][:][0]
                for point in range(9):
                    self.assertAlmostEqual(sg1[point], 0.003,
                                           msg=str_sg1 + ' in PREP not as expected for point number ' + str(point))
                    self.assertAlmostEqual(sg2[point], 0.5,
                                           msg=str_sg2 + ' in PREP not as expected for point number ' + str(point))
                    self.assertAlmostEqual(rsn[point], 300,
                                           msg=str_rsn + ' in PREP not as expected for point number ' + str(point))
                    self.assertAlmostEqual(wsn[point], 30,
                                           msg=str_wsn + ' in PREP not as expected for point number ' + str(point))
                    self.assertAlmostEqual(hsn[point], -100211088,
                                           msg=str_hsn + ' in PREP not as expected for point number ' + str(point))
            for layer in range(3):
                # IMPURETE: unité PREP = masse (kg), unité namelist = proportion (kg impureté / kg neige)
                # IM1 = IMPURETE 1 = BLACK CARBON.
                str_im1 = 'IM1_VEG' + str(int(layer + 1))
                im1 = ncfile.variables[str_im1][:][0]
                # IM2 = IMPURETE 2 = DUST.
                str_im2 = 'IM2_VEG' + str(int(layer + 1))
                im2 = ncfile.variables[str_im2][:][0]
                for point in range(9):
                    self.assertAlmostEqual(im1[point], 0.00012,
                                           msg=str_im1 + ' in PREP not as expected for point number ' + str(point))
                    self.assertAlmostEqual(im2[point], 0.00012,
                                           msg=str_im2 + ' in PREP not as expected for point number ' + str(point))


if __name__ == "__main__":
    # import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
