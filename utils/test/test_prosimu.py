# -*- coding: utf-8 -*-



import unittest
#from src.s2mpostproc import unmask,S2MReader,S2M2Synopsis
from snowtools_git.utils.prosimu import prosimu,prosimu_old
import numpy
from unittest import skip
from datetime import datetime
import os


THIS_DIR = os.path.dirname(os.path.abspath(__file__))


class TestProSimu(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_new = os.path.join(THIS_DIR,'data','pro_2018080306_2018080406.nc')
        cls.ps = prosimu(path_new)

    def test_get_points_intargs(self):
        points = self.ps.get_points(ZS=2100,slope=20,massif_num=5)
        self.assertEqual(len(points),8,"alti, massif, pente fixes on attend 8 orientations")

    def test_get_points_listargs(self):
        points = self.ps.get_points(ZS=2100,slope=[0,20,40],massif_num=[4])
        self.assertEqual(len(points),17,"17 points attendus")

    def test_get_points_raisesTypeError(self):
        with self.assertRaises(TypeError):
            self.ps.get_points(ZS=2100,slope=[20,40],massif_num=5,time=6)

    def test_get_points_(self):
        self.assertEqual(len(self.ps.get_points()),4471,
                         "4471 points attendus - tout les points du fichier netcdf de test")

    def test_read_var_with_get_point_get_time(self):
        pt = self.ps.get_point(ZS=4500,slope=20,massif_num=3,aspect=0)
        t = self.ps.get_time(datetime(2018,8,4,0))
        snowtemp = self.ps.read_var('SNOWTEMP',Number_of_points = pt,time = t)
        self.assertEqual(snowtemp.shape,(50,))

    def test_read_var_intargs(self):
        snowtemp = self.ps.read_var('SNOWTEMP',time=0)
        self.assertEqual(snowtemp.shape, (50,4471),"Attendu : 50 couches, 4471 points")

    def test_read_var_onepoint(self):
        """
        Cas ou le résultat est de dimension 0
        """
        snowtemp = self.ps.read_var('SNOWTEMP',time = 0, snow_layer = 0, Number_of_points = 392)
        self.assertIsInstance(snowtemp,numpy.ma.core.MaskedArray)
        # même quand il n'y a qu'un seul élément dans la réponse, on veut un
        # masked array dans un souci d'homogénéïté
        self.assertNotIsInstance(snowtemp,numpy.ma.core.MaskedConstant)

    def test_read_var_sliceargs(self):
        snowtemp = self.ps.read_var(
            'SNOWTEMP',time = 1,Number_of_points = slice(600,690,2))
        self.assertEqual(snowtemp.shape, (50,45),"Attendu : 50 couches, 45 points")



class TestProSimuWithCache(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        path_new = os.path.join(THIS_DIR,'data','pro_2018080306_2018080406.nc')
        cls.ps = prosimu(path_new)
        cls.ps.force_read_in_cache()

    def test_cache(self):
        self.assertIn('SNOWSSA',self.ps.varcache.keys(),"Le cache est incomplet")

    def test_read_var_intargs(self):
        snowtemp = self.ps.read_var('SNOWTEMP',time=0)
        self.assertEqual(snowtemp.shape, (50,4471),"Attendu : 50 couches, 4471 points")

    def test_read_var_onepoint(self):
        """
        Cas ou le résultat est de dimension 0
        """
        snowtemp = self.ps.read_var('SNOWTEMP',time = 0, snow_layer = 0, Number_of_points = 392)
        self.assertIsInstance(snowtemp,numpy.ma.core.MaskedArray)
        # même quand il n'y a qu'un seul élément dans la réponse, on veut un
        # masked array dans un souci d'homogénéïté
        self.assertNotIsInstance(snowtemp,numpy.ma.core.MaskedConstant)

    def test_read_var_sliceargs(self):
        snowtemp = self.ps.read_var(
            'SNOWTEMP',time = 1,Number_of_points = slice(600,690,2))
        self.assertEqual(snowtemp.shape, (50,45),"Attendu : 50 couches, 45 points")




class TestProSimuOld(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_old = os.path.join(THIS_DIR,'data','old_PRO_20180807032000_002400.nc')
        cls.ps = prosimu_old(path_old)


    def test_get_points_intargs(self):
        points = self.ps.get_points(ZS=2100,slope=20,massif_number=5)
        self.assertEqual(len(points),8,"alti, massif, pente fixes on attend 8 orientations")

    def test_get_points_listargs(self):
        points = self.ps.get_points(ZS=2100,slope=[0,20,40],massif_number=[4])
        self.assertEqual(len(points),17,"17 points attendus")

    def test_get_points_raisesTypeError(self):
        with self.assertRaises(TypeError):
            self.ps.get_points(ZS=2100,slope=[20,40],massif_number=5,time=6)

    def test_get_points_(self):
        pts = self.ps.get_points()
        self.assertEqual(len(pts),3179,
                         "3179 points attendus - tout les points du fichier netcdf de test")

    def test_read_var_intargs(self):
        snowtemp = self.ps.read_var('SNOWTEMP',time=0)
        self.assertEqual(snowtemp.shape, (50,3179),"Attendu : 50 couches, 3179 points")

    def test_read_var_onepoint(self):
        """
        Cas ou le résultat est de dimension 0
        """
        snowtemp = self.ps.read_var('SNOWTEMP',time = 0, snow_layer = 0, Number_of_points = 392)
        self.assertIsInstance(snowtemp,numpy.ma.core.MaskedArray)
        # même quand il n'y a qu'un seul élément dans la réponse, on veut un
        # masked array dans un souci d'homogénéïté
        self.assertNotIsInstance(snowtemp,numpy.ma.core.MaskedConstant)

    def test_read_var_sliceargs(self):
        snowtemp = self.ps.read_var(
            'SNOWTEMP',time = 1,Number_of_points = slice(600,690,2))
        self.assertEqual(snowtemp.shape, (50,45),"Attendu : 50 couches, 45 points")



class TestProSimuOldWithCache(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        # fichier au nouveau format de la chaîne
        path_old = os.path.join(THIS_DIR,'data','old_PRO_20180807032000_002400.nc')
        cls.ps = prosimu_old(path_old)
        cls.ps.force_read_in_cache()

    def test_cache(self):
        self.assertIn('SNOWSSA',self.ps.varcache.keys(),"Le cache est incomplet")

    def test_read_var_intargs(self):
        snowtemp = self.ps.read_var('SNOWTEMP',time=0)
        self.assertEqual(snowtemp.shape, (50,3179),"Attendu : 50 couches, 3179 points")

    def test_read_var_onepoint(self):
        """
        0 dimensionnnal array case
        """
        snowtemp = self.ps.read_var('SNOWTEMP',time = 0, snow_layer = 0, Number_of_points = 392)
        self.assertIsInstance(snowtemp,numpy.ma.core.MaskedArray)
        # même quand il n'y a qu'un seul élément dans la réponse, on veut un
        # masked array dans un souci d'homogénéïté
        self.assertNotIsInstance(snowtemp,numpy.ma.core.MaskedConstant)

    def test_read_var_sliceargs(self):
        snowtemp = self.ps.read_var(
            'SNOWTEMP',time = 1,Number_of_points = slice(600,690,2))
        self.assertEqual(snowtemp.shape, (50,45),"Attendu : 50 couches, 45 points")




if __name__=="__main__":
    unittest.main()

