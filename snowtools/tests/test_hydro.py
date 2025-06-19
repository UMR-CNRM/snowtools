#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 19 March 2025

@author: lafaysse
"""
import unittest
import os

from snowtools.tests.tempfolder import TestWithTempFolderWithChdir
from snowtools.DATA import TESTBASE_DIR
from snowtools.tools.hydro import basin_areas_file, hydro


@unittest.skipIf(not os.path.isdir(os.path.join(TESTBASE_DIR, "hydro")), "input files not available")
class HydroTest(TestWithTempFolderWithChdir):
    def setUp(self):
        super(HydroTest, self).setUp()
        self.dem = os.path.join(TESTBASE_DIR, 'hydro', 'DEM_FRANCE_L93_250m_bilinear.nc')
        self.meteo = os.path.join(TESTBASE_DIR, "FORCING", "forcing_testhydro_alp_oper_2025031806_2025032306.nc")
        self.pro = os.path.join(TESTBASE_DIR, "PRO", "pro_testhydro_alp_oper_2025031906_2025032306.nc")
        self.rasterbasin = os.path.join(TESTBASE_DIR, 'hydro', 'BNBV_SCHAPI_FRANCE250m.nc')
        self.areas = os.path.join(TESTBASE_DIR, "hydro", "areas_alp27_allslopes.nc")

    def test_areas(self):
        # Compute the areas of basins in rasterbasin corresponding to the geometry of the pro file
        # and store them in outputdir
        b = basin_areas_file(self.dem, self.rasterbasin, [self.pro], '.')
        assert os.path.isfile('./areas_alp27_allslopes.nc')

    def test_aggregation(self):
        # Aggregate diagnostics on basins
        with hydro([self.pro, self.meteo], self.areas, 'HYDRO.nc') as h:
            h.integration(['Tair', 'Rainf', 'Snowf',
                           'SNOMLT_ISBA', 'WSN_T_ISBA', 'DSN_T_ISBA'], var_sca='WSN_T_ISBA')
        assert os.path.isfile('HYDRO.nc')


if __name__ == "__main__":
    unittest.main()
