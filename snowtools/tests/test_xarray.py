#!/usr/bin/env python3
# -*- coding: utf-8 -*-


import unittest
import os
import numpy as np

import xarray as xr

import snowtools  # noqa
from snowtools.DATA import TESTBASE_DIR


if not os.path.isdir(TESTBASE_DIR):
    SKIP = True
else:
    SKIP = False


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'pro_2018080306_2018080406.nc')),
                 "input file not available")
@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'PRO_WJF_2010-2016.nc')),
                 "input file not available")
@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'old_PRO_20180807032000_002400.nc')),
                 "input file not available")
class Test_snowtools_accessor(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_pro18        = os.path.join(TESTBASE_DIR, "PRO", 'pro_2018080306_2018080406.nc')
        cls.ds18          = xr.open_dataset(path_pro18, engine='snowtools')
        path_multi_year   = os.path.join(TESTBASE_DIR, "PRO", 'PRO_WJF_2010-2016.nc')
        cls.ds_multi_year = xr.open_dataset(path_multi_year, engine='snowtools')
        path_ds_forcing   = os.path.join(TESTBASE_DIR, 'FORCING', 'FORCING_test_base.nc')
        cls.ds_forcing    = xr.open_dataset(path_ds_forcing, engine='snowtools')
        path_pro_2D       = os.path.join(TESTBASE_DIR, "PRO", "PRO_first_2014080106_2015080106.nc")
        cls.ds_2D         = xr.open_dataset(path_pro_2D, engine='snowtools')
        path_old          = os.path.join(TESTBASE_DIR, "PRO", 'old_PRO_20180807032000_002400.nc')
        cls.ds_old        = xr.open_dataset(path_old, engine='snowtools')

    def test_transpose(self):
        # Mix dimensions in a "random" order
        tmp = self.ds_2D.SNOWTEMP.transpose('xx', 'yy', 'Number_of_Patches', 'time', 'snow_layer')
        # Ensure that the "transpose" method put the time dimension back at first place
        out = tmp.snowtools.transpose()
        self.assertTrue(out.dims[0] == 'time')

    def test_squeeze(self):
        snowtemp = self.ds_2D.SNOWTEMP.snowtools.squeeze()
        # The len 1 dimension 'Number_of_Patches' should have been dropped --> only 4 dimensions left
        self.assertTrue(len(snowtemp.shape) == 4)

    def test_daily_accumulation(self):
        daily = self.ds_forcing.Rainf.snowtools.daily_accumulation()
        self.assertTrue(np.array_equal(daily.isel(time=2).data,
            np.array([0.006235555551118321, 0.006593332870139016])))

    def test_snow_cover_stats(self):
        stats = self.ds_multi_year.snowtools.snow_cover_stats()
        self.assertTrue(np.array_equal(stats.scd_concurent.data.flatten(), np.array([194, 207, 199, 205, 232, 206])))
        self.assertTrue(np.array_equal(stats.mod.data.flatten(), np.array([262., 302., 288., 271., 283., 287.])))
        self.assertTrue(np.array_equal(stats.sod.data.flatten(), np.array([68., 95., 89., 66., 51., 81.])))
        self.assertTrue(np.array_equal(stats.sd.data.flatten(), np.array([204, 240, 199, 212, 232, 210])))

    def test_backtrack_preprocess(self):
        original = self.ds_old.snowtools.backtrack_preprocess()
        self.assertTrue('location' in original.dims)

    @classmethod
    def tearDownClass(cls):
        cls.ds_2D.close()
        cls.ds18.close()
        cls.ds_multi_year.close()
        cls.ds_forcing.close()
        cls.ds_old.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'pro_2018080306_2018080406.nc')),
                 "input file not available")
class Test_semidistributed_accessor(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_pro18 = os.path.join(TESTBASE_DIR, "PRO", 'pro_2018080306_2018080406.nc')
        cls.ds18 = xr.open_dataset(path_pro18, engine='snowtools')

    def test_get_points_intargs(self):
        points = self.ds18.semidistributed.sel_points(ZS=2100, slope=20, massif_num=5)
        self.assertEqual(len(points.Number_of_points), 8, "alti, massif, pente fixes on attend 8 orientations")

    def test_get_points_listargs(self):
        points = self.ds18.semidistributed.sel_points(ZS=[1800, 2100], slope=0, aspect=-1, massif_num=1)
        self.assertEqual(len(points.Number_of_points), 2, "2 points attendus")

    def test_get_points_rangeargs(self):
        points = self.ds18.semidistributed.sel_points(ZS=1800, slope=40, aspect=0, massif_num=range(1, 5))
        self.assertEqual(len(points.Number_of_points), 4, "4 points attendus")

    def test_empty_selection(self):
        points = self.ds18.semidistributed.sel_points(ZS=1700)
        self.assertEqual(len(points), 0, "Empty Dataset expected")

    def test_read_var_with_get_point_get_time(self):
        snowtemp = self.ds18.sel(time='2018-8-4 00').semidistributed.sel_points(ZS=4500, slope=20,
                massif_num=3, aspect=0).SNOWTEMP.squeeze()
        self.assertEqual(snowtemp.shape, (50,))

    def test_read_var_intargs(self):
        snowtemp = self.ds18.SNOWTEMP.isel(time=0).squeeze()
        self.assertEqual(snowtemp.shape, (50, 4471), "Expect: 50 layers, 4471 points")

    def test_read_var_sliceargs(self):
        snowtemp = self.ds18.SNOWTEMP.isel(time=1).sel(Number_of_points=slice(600, 690, 2)).squeeze()
        self.assertEqual(snowtemp.shape, (50, 45), "Expect: 50 layers, 45 points")

    @classmethod
    def tearDownClass(cls):
        cls.ds18.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'pro_2018080306_2018080406.nc')),
                 "input file not available")
class Test_SURFEX_accessor(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_pro18  = os.path.join(TESTBASE_DIR, "PRO", 'pro_2018080306_2018080406.nc')
        cls.ds18    = xr.open_dataset(path_pro18, engine='snowtools')
        path_pro_2D = os.path.join(TESTBASE_DIR, "PRO", "PRO_first_2014080106_2015080106.nc")
        cls.ds_2D   = xr.open_dataset(path_pro_2D, engine='snowtools')

    def test_drop_tile_dimension(self):
        self.ds_2D.surfex.drop_tile_dimension()
        self.assertEqual(len(self.ds_2D.xx), 5, "Expect: 5 points in xx direction")

    def test_decode_time_variable(self):
        self.ds_2D.surfex.decode_time_variable('time')
        self.assertEqual(len(self.ds_2D.xx), 5, "Expect: 5 points in xx direction")

    @classmethod
    def tearDownClass(cls):
        cls.ds18.close()
        cls.ds_2D.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'PRO_2010080106_2011080106.nc')),
                 "input file not available")
class TestXarray_first_test(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_pro_first = os.path.join(TESTBASE_DIR, "PRO", "PRO_2010080106_2011080106.nc")
        cls.ds_first = xr.open_dataset(path_pro_first, engine='snowtools')

    def sel_points_arg(self):
        ds_ZS = self.ds_first.semidistributed.sel_points(ZS=2400)
        self.assertEqual(ds_ZS.ZS.values[0], 2400, "Expect: 2400")

    def sel_points_list(self):
        ds_ZS = self.ds_first.semidistributed.sel_points(ZS=[2400])
        self.assertEqual(ds_ZS.ZS.values[0], 2400, "Expect: 2400")

    def test_read_var_with_sel_points_get_time(self):
        point = self.ds_first.semidistributed.sel_points(ZS = 2700)
        timesel = point.sel(time='2010-8-4 00').squeeze()
        self.assertEqual(timesel.SNOWTEMP.shape, (50,), "Expect: 50 layers")

    def test_read_var_intargs(self):
        snowtemp = self.ds_first.SNOWTEMP.isel(time=0).squeeze()
        self.assertEqual(snowtemp.shape, (50, 2), "Expect: 50 layers, 2 points")

    @classmethod
    def tearDownClass(cls):
        cls.ds_first.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'PRO_WJF_2014-2015.nc')),
                 "input file not available")
@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'PRO_WJF_2015-2016.nc')),
                 "input file not available")
class TestXarray_multifile(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_pro_multifirst = os.path.join(TESTBASE_DIR, "PRO", "PRO_WJF_2014-2015.nc")
        cls.ds_multifirst = xr.open_dataset(path_pro_multifirst, engine='snowtools')
        path_pro_multisec = os.path.join(TESTBASE_DIR, "PRO", "PRO_WJF_2015-2016.nc")
        cls.ds_multisec = xr.open_dataset(path_pro_multisec, engine='snowtools')
        cls.ds_multi = xr.open_dataset([path_pro_multifirst, path_pro_multisec], engine='snowtools')
        cls.ds_multi2 = xr.open_mfdataset([path_pro_multifirst, path_pro_multisec], engine='snowtools')

    def test_multiopen(self):
        self.assertEqual(len(self.ds_multifirst.time), 2921, "Expect: 2921 timesteps")
        self.assertEqual(len(self.ds_multisec.time), 2928, "Expect: 2928 timesteps")
        self.assertEqual(len(self.ds_multi.time), 5849, "Expect: 5849 timesteps")
        self.assertEqual(len(self.ds_multi2.time), 5849, "Expect: 5849 timesteps")

    @classmethod
    def tearDownClass(cls):
        cls.ds_multifirst.close()
        cls.ds_multisec.close()
        cls.ds_multi.close()
        cls.ds_multi2.close()


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "PRO",
                                                 'PRO_first_2014080106_2015080106.nc')),
                 "input file not available")
class Test_distributed_accessor(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_pro_2D = os.path.join(TESTBASE_DIR, "PRO", "PRO_first_2014080106_2015080106.nc")
        cls.ds_2D = xr.open_dataset(path_pro_2D, engine='snowtools')

    def test_proj(self):
        out = self.ds_2D.DSN_T_ISBA.distributed.proj(crs_in=2154, crs_out=4326)
        self.assertEqual(out.rio.crs.to_string(), 'EPSG:4326')

    @classmethod
    def tearDownClass(cls):
        cls.ds_2D.close()


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
        self.assertEqual(len(points.Number_of_points), 8, "Expect: 8 aspects")

    def test_get_points_listargs(self):
        points = self.ds.where((self.ds.ZS == 2100) & (self.ds.slope.isin([20, 40])) &
                (self.ds.massif_number.isin([4])), drop=True)
        self.assertEqual(len(points.Number_of_points), 16, "Expect: 16 points")

    def test_get_points_(self):
        self.assertEqual(len(self.ds.Number_of_points), 3179, "Expect: 3179 points")

    def test_read_var_with_get_point_get_time(self):
        snowtemp = self.ds.SNOWTEMP.isel(time=0).where((self.ds.ZS == 2100) & (self.ds.slope == 20) &
                (self.ds.massif_number == 3) & (self.ds.aspect == 0), drop=True).squeeze()
        self.assertEqual(snowtemp.shape, (50,), "Expect: 50 layers")

    def test_read_var_intargs(self):
        snowtemp = self.ds.SNOWTEMP.isel(time=0).squeeze()
        self.assertEqual(snowtemp.shape, (50, 3179), "Expect: 50 layers, 3179 points")

    def test_read_var_sliceargs(self):
        snowtemp = self.ds.SNOWTEMP.isel(time=1).sel(Number_of_points=slice(600, 690, 2)).squeeze()
        self.assertEqual(snowtemp.shape, (50, 45), "Expect: 50 layers, 45 points")

    @classmethod
    def tearDownClass(cls):
        cls.ds.close()


if __name__ == "__main__":
    unittest.main()
