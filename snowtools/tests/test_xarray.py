#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import unittest
import os

from snowtools.tools.xarray_backend import CENBackendEntrypoint  # Ignore "imported but unused" error
from snowtools.DATA import TESTBASE_DIR

import xarray as xr

if not os.path.isdir(TESTBASE_DIR):
    SKIP = True
else:
    SKIP = False


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'pro_2018080306_2018080406.nc')),
                 "input file not available")
class TestXarray(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_new = os.path.join(TESTBASE_DIR, "PRO", 'pro_2018080306_2018080406.nc')
        cls.ds = xr.open_dataset(path_new, engine='cen')

    def test_get_points_intargs(self):
        points = self.ds.where((self.ds.ZS == 2100) & (self.ds.slope == 20) & (self.ds.massif_num == 5), drop=True)
        self.assertEqual(len(points.Number_of_points), 8, "alti, massif, pente fixes on attend 8 orientations")

    def test_get_points_listargs(self):
        points = self.ds.where((self.ds.ZS == 2100) & (self.ds.slope.isin([20, 40])) & (self.ds.massif_num.isin([4])),
                drop=True)
        self.assertEqual(len(points.Number_of_points), 16, "16 points attendus")

    def test_get_points_(self):
        self.assertEqual(len(self.ds.Number_of_points), 4471,
                         "4471 points attendus - tout les points du fichier netcdf de test")

    def test_read_var_with_get_point_get_time(self):
        snowtemp = self.ds.SNOWTEMP.sel(time='2018-8-4 00').where((self.ds.ZS == 4500) & (self.ds.slope == 20) &
                (self.ds.massif_num == 3) & (self.ds.aspect == 0), drop=True).squeeze()
        self.assertEqual(snowtemp.shape, (50,))

    def test_read_var_intargs(self):
        snowtemp = self.ds.SNOWTEMP.isel(time=0).squeeze()
        self.assertEqual(snowtemp.shape, (50, 4471), "Attendu : 50 couches, 4471 points")

    def test_read_var_sliceargs(self):
        snowtemp = self.ds.SNOWTEMP.isel(time=1).sel(Number_of_points=slice(600, 690, 2)).squeeze()
        self.assertEqual(snowtemp.shape, (50, 45), "Attendu : 50 couches, 45 points")

    @classmethod
    def tearDownClass(cls):
        cls.ds.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'old_PRO_20180807032000_002400.nc')),
                 "input file not available")
class TestOldPRO(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_old = os.path.join(TESTBASE_DIR, "PRO", 'old_PRO_20180807032000_002400.nc')
        cls.ds = xr.open_dataset(path_old, engine='cen')

    def test_get_points_intargs(self):
        points = self.ds.where((self.ds.ZS == 2100) & (self.ds.slope == 20) & (self.ds.massif_number == 5), drop=True)
        self.assertEqual(len(points.Number_of_points), 8, "alti, massif, pente fixes on attend 8 orientations")

    def test_get_points_listargs(self):
        points = self.ds.where((self.ds.ZS == 2100) & (self.ds.slope.isin([20, 40])) &
                (self.ds.massif_number.isin([4])), drop=True)
        self.assertEqual(len(points.Number_of_points), 16, "16 points attendus")

    def test_get_points_(self):
        self.assertEqual(len(self.ds.Number_of_points), 3179,
                         "3179 points attendus - tout les points du fichier netcdf de test")

    def test_read_var_with_get_point_get_time(self):
        snowtemp = self.ds.SNOWTEMP.isel(time=0).where((self.ds.ZS == 2100) & (self.ds.slope == 20) &
                (self.ds.massif_number == 3) & (self.ds.aspect == 0), drop=True).squeeze()
        self.assertEqual(snowtemp.shape, (50,))

    def test_read_var_intargs(self):
        snowtemp = self.ds.SNOWTEMP.isel(time=0).squeeze()
        self.assertEqual(snowtemp.shape, (50, 3179), "Attendu : 50 couches, 3179 points")

    def test_read_var_sliceargs(self):
        snowtemp = self.ds.SNOWTEMP.isel(time=1).sel(Number_of_points=slice(600, 690, 2)).squeeze()
        self.assertEqual(snowtemp.shape, (50, 45), "Attendu : 50 couches, 45 points")

    @classmethod
    def tearDownClass(cls):
        cls.ds.close()


if __name__ == "__main__":
    unittest.main()
