#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 22 nov. 2021

@author: radanovics
"""
import os
import subprocess
import shutil
import unittest
import datetime
import tempfile

from netCDF4 import Dataset


_here = os.path.dirname(os.path.realpath(__file__))
TEST_DATA_DIR = "/rd/cenfic2/manto/viallonl/testbase/FORCING"


@unittest.skipIf(not os.path.isfile(os.path.join(TEST_DATA_DIR, "METEO_KENTTAROVA.csv")),
                 "input file not available")
class TemplateCreationForcingTest(unittest.TestCase):
    """Test Forcing creation script (csv to Forcing)"""
    def setUp(self):
        basediroutput = os.path.join(_here, "fail_test")
        if not os.path.isdir(basediroutput):
            os.makedirs(basediroutput)
        prefix = "output" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f-")
        self.diroutput = tempfile.mkdtemp(prefix=prefix, dir=basediroutput)

    def test_create_forcing_default(self):
        p = subprocess.run(['python', '../snowtools/scripts/create_forcing/Template_creation_FORCING.py', '-i',
                            os.path.join(TEST_DATA_DIR, "METEO_KENTTAROVA.csv"), '-o',
                            os.path.join(self.diroutput, 'created_forcing.nc')])
        self.assertEqual(p.returncode, 0, 'snowtools/scripts/create_forcing/Template_creation_FORCING.py failed')
        self.assertTrue(os.path.isfile(os.path.join(self.diroutput, 'created_forcing.nc')))
        with Dataset(os.path.join(self.diroutput, 'created_forcing.nc'), 'r', format='NETCDF4_CLASSIC') as ncfile:
            humrel = ncfile.variables['HUMREL'][:]
            self.assertEqual(humrel[0], 76., 'humidity data not as expected')

    def tearDown(self):
        if not os.listdir(self.diroutput):
            # Suppression des sous-dossiers de fail_test correspondant aux tests OK
            shutil.rmtree(self.diroutput)


if __name__ == "__main__":
    unittest.main()
