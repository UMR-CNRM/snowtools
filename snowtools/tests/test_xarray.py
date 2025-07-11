#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import unittest
import os

from snowtools.utils import xarray_snowtools_backend  # Ignore "imported but unused" error
from snowtools.utils import xarray_snowtools_accessor
from snowtools.DATA import TESTBASE_DIR

import xarray as xr

if not os.path.isdir(TESTBASE_DIR):
    SKIP = True
else:
    SKIP = False


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'pro_2018080306_2018080406.nc')),
                 "input file not available")

@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'PRO_first_2014080106_2015080106.nc')),
                 "input file not available")

@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'PRO_WJF_2010-2016.nc')),
                 "input file not available")

@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'PRO_2010080106_2011080106.nc')),
                 "input file not available")


class TestXarray(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_pro18 = os.path.join(TESTBASE_DIR, "PRO", 'pro_2018080306_2018080406.nc')
        path_pro_2D = os.path.join(TESTBASE_DIR, "PRO", "PRO_first_2014080106_2015080106.nc")
        path_pro_multi = os.path.join(TESTBASE_DIR, "PRO", "PRO_WJF_2010-2016.nc")
        path_pro_first = os.path.join(TESTBASE_DIR, "PRO", "PRO_2010080106_2011080106.nc")
        cls.ds18 = xr.open_dataset(path_pro18, engine='snowtools')
        cls.ds_2D = xr.open_dataset(path_pro_2D, engine='snowtools')
        cls.ds_multi = xr.open_dataset(path_pro_multi, engine='snowtools')
        cls.ds_first = xr.open_dataset(path_pro_first, engine='snowtools')

    def test_get_points_intargs(self):
        points = self.ds18.where((self.ds18.ZS == 2100) & (self.ds18.slope == 20) & (self.ds18.massif_num == 5), drop=True)
        self.assertEqual(len(points.Number_of_points), 8, "alti, massif, pente fixes on attend 8 orientations")

    def test_get_points_listargs(self):
        points = self.ds18.where((self.ds18.ZS == 2100) & (self.ds18.slope.isin([20, 40])) & (self.ds18.massif_num.isin([4])),
                drop=True)
        self.assertEqual(len(points.Number_of_points), 16, "16 points attendus")

    def test_get_points_(self):
        self.assertEqual(len(self.ds18.Number_of_points), 4471,
                         "4471 points attendus - tout les points du fichier netcdf de test")

    def test_read_var_with_get_point_get_time(self):
        snowtemp = self.ds18.SNOWTEMP.sel(time='2018-8-4 00').where((self.ds18.ZS == 4500) & (self.ds18.slope == 20) &
                (self.ds18.massif_num == 3) & (self.ds18.aspect == 0), drop=True).squeeze()
        self.assertEqual(snowtemp.shape, (50,))

    def test_read_var_intargs(self):
        snowtemp = self.ds18.SNOWTEMP.isel(time=0).squeeze()
        self.assertEqual(snowtemp.shape, (50, 4471), "Attendu : 50 couches, 4471 points")

    def test_read_var_sliceargs(self):
        snowtemp = self.ds18.SNOWTEMP.isel(time=1).sel(Number_of_points=slice(600, 690, 2)).squeeze()
        self.assertEqual(snowtemp.shape, (50, 45), "Attendu : 50 couches, 45 points")

    def sel_points_first(self):
        ds_ZS = self.ds_first.semidistributed.sel_points(ZS=2400)
        self.assertEqual(ds_ZS.ZS.values[0], 2400, "Attendu : 2400")

    @classmethod
    def tearDownClass(cls):
        cls.ds18.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'old_PRO_20180807032000_002400.nc')),
                 "input file not available")
class TestOldPRO(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_old = os.path.join(TESTBASE_DIR, "PRO", 'old_PRO_20180807032000_002400.nc')
        cls.ds = xr.open_dataset(path_old, engine='snowtools')

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
