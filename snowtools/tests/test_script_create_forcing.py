#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 22 nov. 2021

@author: radanovics
"""
import os
import sys
import unittest

from netCDF4 import Dataset

from snowtools.DATA import SNOWTOOLS_DIR
from snowtools.DATA import TESTBASE_DIR
from snowtools.tests.tempfolder import TestWithTempFolderWithLog

TEST_DATA_DIR = os.path.join(TESTBASE_DIR, "FORCING")


@unittest.skipIf(not os.path.isfile(os.path.join(TEST_DATA_DIR, "METEO_KENTTAROVA.csv")),
                 'Input test data not available')
class TemplateCreationForcingTest(TestWithTempFolderWithLog):
    """Test Forcing creation script (csv to Forcing)"""
    def test_create_forcing_default(self):
        p = self.run_and_log([sys.executable, '-m', 'snowtools.scripts.create_forcing.Template_creation_FORCING',
                              '-i',
                              os.path.join(TEST_DATA_DIR, "METEO_KENTTAROVA.csv"), '-o',
                              os.path.join(self.diroutput, 'created_forcing.nc')], env=os.environ)
        self.assertEqual(p.returncode, 0, 'snowtools/scripts/create_forcing/Template_creation_FORCING.py failed')
        self.assertTrue(os.path.isfile(os.path.join(self.diroutput, 'created_forcing.nc')))
        with Dataset(os.path.join(self.diroutput, 'created_forcing.nc'), 'r', format='NETCDF4_CLASSIC') as ncfile:
            humrel = ncfile.variables['HUMREL'][:]
            self.assertAlmostEqual(humrel[0], 76., msg='humidity data not as expected')

    def test_create_forcing_station_info(self):
        p = self.run_and_log([sys.executable, '-m', 'snowtools.scripts.create_forcing.Template_creation_FORCING',
                              '-i',
                              os.path.join(TEST_DATA_DIR, "METEO_KENTTAROVA.csv"), '-o',
                              os.path.join(self.diroutput, 'created_forcing_fantasy_station.nc'), '--lon', '15.5',
                              '--lat', '46.2', '--zs', '400', '--meta', 'title=FORCING for fantasy station',
                              '--meta', 'contributor_name=pink rhinoceros', '--meta',
                              'contributor_role=drank a lot of coffee'], env=os.environ)
        self.assertEqual(p.returncode, 0, 'snowtools/scripts/create_forcing/Template_creation_FORCING.py failed')
        self.assertTrue(os.path.isfile(os.path.join(self.diroutput, 'created_forcing_fantasy_station.nc')))
        with Dataset(os.path.join(self.diroutput, 'created_forcing_fantasy_station.nc'), 'r',
                     format='NETCDF4_CLASSIC') as ncfile:
            humrel = ncfile.variables['HUMREL'][:]
            self.assertAlmostEqual(humrel[0], 76., msg='humidity data not as expected')
            lon = ncfile.variables['LON'][:]
            self.assertAlmostEqual(lon, 15.5, msg='longitude not as expected')
            zs = ncfile.variables['ZS'][:]
            self.assertEqual(zs, 400, msg="station altitude not as expected")
            contrib = ncfile.getncattr('contributor_name')
            self.assertEqual(contrib, 'pink rhinoceros', msg='contributor name not as expected')


if __name__ == "__main__":
    unittest.main()
