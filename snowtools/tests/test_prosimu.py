#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import unittest
from datetime import datetime
import os

import numpy as np

from snowtools.utils.prosimu import prosimu_auto as prosimu, prosimu_old
from snowtools.DATA import TESTBASE_DIR

if not os.path.isdir(TESTBASE_DIR):
    SKIP = True
else:
    SKIP = False


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'pro_2018080306_2018080406.nc')),
                 "input file not available")
class TestProSimu(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_new = os.path.join(TESTBASE_DIR, "PRO", 'pro_2018080306_2018080406.nc')
        cls.ps = prosimu(path_new)

    def test_get_points_intargs(self):
        points = self.ps.get_points(ZS=2100, slope=20, massif_num=5)
        self.assertEqual(len(points), 8, "alti, massif, pente fixes on attend 8 orientations")

    def test_get_points_listargs(self):
        points = self.ps.get_points(ZS=2100, slope=[0, 20, 40], massif_num=[4])
        self.assertEqual(len(points), 17, "17 points attendus")

    def test_get_points_raisesTypeError(self):
        with self.assertRaises(TypeError):
            self.ps.get_points(ZS=2100, slope=[20, 40], massif_num=5, time=6)

    def test_get_points_(self):
        self.assertEqual(len(self.ps.get_points()), 4471,
                         "4471 points attendus - tout les points du fichier netcdf de test")

    def test_read_var_with_get_point_get_time(self):
        pt = self.ps.get_point(ZS=4500, slope=20, massif_num=3, aspect=0)
        t = self.ps.get_time(datetime(2018, 8, 4, 0))
        snowtemp = self.ps.read_var('SNOWTEMP', Number_of_points=pt, time=t)
        self.assertEqual(snowtemp.shape, (50,))

    def test_read_var_intargs(self):
        snowtemp = self.ps.read_var('SNOWTEMP', time=0)
        self.assertEqual(snowtemp.shape, (50, 4471), "Attendu : 50 couches, 4471 points")

    def test_read_var_sliceargs(self):
        snowtemp = self.ps.read_var(
            'SNOWTEMP', time=1, Number_of_points=slice(600, 690, 2))
        self.assertEqual(snowtemp.shape, (50, 45), "Attendu : 50 couches, 45 points")

    @classmethod
    def tearDownClass(cls):
        cls.ps.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'pro_2018080306_2018080406.nc')),
                 "input file not available")
class TestProSimuWithCache(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        path_new = os.path.join(TESTBASE_DIR, "PRO", 'pro_2018080306_2018080406.nc')
        cls.ps = prosimu(path_new)
        cls.ps.force_read_in_cache()

    def test_cache(self):
        self.assertIn('SNOWSSA', self.ps.varcache.keys(), "Le cache est incomplet")

    def test_read_var_intargs(self):
        snowtemp = self.ps.read_var('SNOWTEMP', time=0)
        self.assertEqual(snowtemp.shape, (50, 4471), "Attendu : 50 couches, 4471 points")

    def test_read_var_sliceargs(self):
        snowtemp = self.ps.read_var(
            'SNOWTEMP', time=1, Number_of_points=slice(600, 690, 2))
        self.assertEqual(snowtemp.shape, (50, 45), "Attendu : 50 couches, 45 points")

    @classmethod
    def tearDownClass(cls):
        cls.ps.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 "PRO_2014080106_2015010106_grille2d.nc")),
                 "input file not available")
class TestProSimu2d(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path = os.path.join(TESTBASE_DIR, "PRO", 'PRO_2014080106_2015010106_grille2d.nc')
        if not SKIP:
            cls.ps = prosimu(path)
        else:
            cls.ps = None

    def test_get_point(self):
        valx = self.ps.read('xx')
        valy = self.ps.read('yy')
        for x, y in [(2, 1), (1, 1), (1, 2), (0, 0)]:
            point = self.ps.get_point(xx=valx[x], yy=valy[y])
        self.assertEqual(point, len(valy) * x + y, "get_point method fail in 2d")

    def test_get_data(self):
        valx = self.ps.read('xx')
        valy = self.ps.read('yy')
        data = self.ps.read('DSN_T_ISBA')
        for x, y in [(2, 1), (1, 1), (1, 2), (0, 0)]:
            point = self.ps.get_point(xx=valx[x], yy=valy[y])
            # Order dimensions = time, yy, xx
            data_ok = data[-1, y, x]
            data_test = self.ps.read('DSN_T_ISBA', selectpoint=point)[-1]
        self.assertAlmostEqual(data_test, data_ok, "read method fail with selectpoint in 2d")

    def test_get_data_multiple_points(self):
        valx = self.ps.read('xx')
        valy = self.ps.read('yy')
        query = [(2, 1), (1, 1), (1, 2), (0, 0)]
        query2 = [self.ps.get_point(xx=valx[x], yy=valy[y]) for x, y in query]
        data1 = self.ps.read('DSN_T_ISBA', selectpoint=query)
        data2 = self.ps.read('DSN_T_ISBA', selectpoint=query2)
        data_ok = np.moveaxis(np.array([self.ps.read('DSN_T_ISBA', selectpoint=point) for point in query]), 0, -1)
        self.assertTrue((data1 == data_ok).all(), "read method fail with selectpoint with a list of tuples in 2d")
        self.assertTrue((data2 == data_ok).all(), "read method fail with selectpoint with a list of tuples in 2d")

    @classmethod
    def tearDownClass(cls):
        if cls.ps is not None:
            cls.ps.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 "PRO_patches.nc")),
                 "input file not available")
class TestProSimuTile(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path = os.path.join(TESTBASE_DIR, "PRO", 'PRO_patches.nc')
        if not SKIP:
            cls.ps = prosimu(path)
        else:
            cls.ps = None

    def test_tile_1(self):
        PATCH = 4
        data_ok = self.ps.dataset.variables['SNOWTYPE'][:, PATCH, :, 0, 0]
        data1 = self.ps.read('SNOWTYPE', tile=PATCH, selectpoint=0)
        self.assertTrue((data1 == data_ok).all(), "read method fail with tile argument")

    def test_tile_multi(self):
        PATCH = [4, 6]
        data_ok_1 = self.ps.dataset.variables['SNOWTYPE'][:, PATCH[0], :, 0, 0]
        data_ok_2 = self.ps.dataset.variables['SNOWTYPE'][:, PATCH[1], :, 0, 0]
        data_ok = np.zeros((data_ok_1.shape[0], 2, data_ok_2.shape[1]))
        data_ok[:, 0, :] = data_ok_1.filled(np.nan)
        data_ok[:, 1, :] = data_ok_2.filled(np.nan)
        data1 = self.ps.read('SNOWTYPE', tile=PATCH, selectpoint=0)
        data1[np.isnan(data1)] = -1
        data_ok[np.isnan(data_ok)] = -1
        self.assertTrue((data1 == data_ok).all(), "read method fail with a list as tile argument")

    @classmethod
    def tearDownClass(cls):
        if cls.ps is not None:
            cls.ps.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'old_PRO_20180807032000_002400.nc')),
                 "input file not available")
class TestProSimuOld(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_old = os.path.join(TESTBASE_DIR, "PRO", 'old_PRO_20180807032000_002400.nc')
        cls.ps = prosimu_old(path_old)

    def test_get_points_intargs(self):
        points = self.ps.get_points(ZS=2100, slope=20, massif_number=5)
        self.assertEqual(len(points), 8, "alti, massif, pente fixes on attend 8 orientations")

    def test_get_points_listargs(self):
        points = self.ps.get_points(ZS=2100, slope=[0, 20, 40], massif_number=[4])
        self.assertEqual(len(points), 17, "17 points attendus")

    def test_get_points_raisesTypeError(self):
        with self.assertRaises(TypeError):
            self.ps.get_points(ZS=2100, slope=[20, 40], massif_number=5, time=6)

    def test_get_points_(self):
        pts = self.ps.get_points()
        self.assertEqual(len(pts), 3179,
                         "3179 points attendus - tout les points du fichier netcdf de test")

    def test_read_var_intargs(self):
        snowtemp = self.ps.read_var('SNOWTEMP', time=0)
        self.assertEqual(snowtemp.shape, (50, 3179), "Attendu : 50 couches, 3179 points")

    def test_read_var_sliceargs(self):
        snowtemp = self.ps.read_var(
            'SNOWTEMP', time=1, Number_of_points=slice(600, 690, 2))
        self.assertEqual(snowtemp.shape, (50, 45), "Attendu : 50 couches, 45 points")

    @classmethod
    def tearDownClass(cls):
        cls.ps.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'old_PRO_20180807032000_002400.nc')),
                 "input file not available")
class TestProSimuOldWithCache(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_old = os.path.join(TESTBASE_DIR, 'PRO', 'old_PRO_20180807032000_002400.nc')
        cls.ps = prosimu_old(path_old)
        cls.ps.force_read_in_cache()

    def test_cache(self):
        self.assertIn('SNOWSSA', self.ps.varcache.keys(), "Le cache est incomplet")

    def test_read_var_intargs(self):
        snowtemp = self.ps.read_var('SNOWTEMP', time=0)
        self.assertEqual(snowtemp.shape, (50, 3179), "Attendu : 50 couches, 3179 points")

    def test_read_var_sliceargs(self):
        snowtemp = self.ps.read_var(
            'SNOWTEMP', time=1, Number_of_points=slice(600, 690, 2))
        self.assertEqual(snowtemp.shape, (50, 45), "Attendu : 50 couches, 45 points")

    @classmethod
    def tearDownClass(cls):
        cls.ps.close()


if __name__ == "__main__":
    unittest.main()
