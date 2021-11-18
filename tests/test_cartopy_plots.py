#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import six
import unittest
import shutil
import glob
import tempfile
from datetime import datetime
from snowtools.utils.prosimu import prosimu
from snowtools.plots.maps import cartopy
from netCDF4 import Dataset
import os

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_DATA_DIR = "/rd/cenfic2/manto/viallonl/testbase/PRO/postproc"


@unittest.skipIf(not os.path.isfile(os.path.join(TEST_DATA_DIR, "grid_postproc_2021041112.nc")),
                 "input file not available")
class TestCartopyFrance(unittest.TestCase):
    """
    Test alpha grid France
    """
    @classmethod
    def setUpClass(cls):
        cls.mix = CartopyTestMixIn()
        basediroutput = os.path.join(THIS_DIR, "fail_test")
        if not os.path.isdir(basediroutput):
            os.makedirs(basediroutput)
        prefix = "output" + datetime.today().strftime("%Y%m%d%H%M%S%f-")
        cls.diroutput = tempfile.mkdtemp(prefix=prefix, dir=basediroutput)
        path_new = os.path.join(TEST_DATA_DIR, "grid_postproc_2021041112.nc")
        cls.ds = Dataset(path_new)
        cls.lats = cls.ds.variables['LAT'][:]
        cls.lons = cls.ds.variables['LON'][:]
        cls.snow = cls.ds.variables['SD_1DY_ISBA'][0, :, :, 8]
        print(cls.snow.shape)
        # cls.mix.alphafile = path_new

    def test_colormesh(self):

        self.m = cartopy.MapFrance(geofeatures=False, bgimage=True)
        self.m.init_massifs(**self.mix.attributes['SD_1DY_ISBA'])
        self.m.draw_mesh(self.lons, self.lats, self.snow,
                         **self.mix.attributes['SD_1DY_ISBA'])
        self.m.set_figtitle("SD_1DY_ISBA 2021041112")
        self.m.set_maptitle("Percentile 90")
        self.outfilename = "grid_p90_2021041112_alpha_terrimage.png"
        self.m.save(os.path.join(self.diroutput, self.outfilename), formatout="png")
        self.m.close()

    def tearDown(self):
        if six.PY3:  # Python 3.4+
            result = self.defaultTestResult()  # These two methods have no side effects
            self._feedErrorsToResult(result, self._outcome.errors)
            error = self.mix.list2reason(result.errors)
            failure = self.mix.list2reason(result.failures)
            if not error and not failure:
                shutil.move(os.path.join(self.diroutput, self.outfilename), os.path.join(THIS_DIR, "Manual_tests",
                                                                                         self.outfilename))
        else:
            print("no automatic move implemented for python2")

    @classmethod
    def tearDownClass(cls):
        cls.ds.close()
        if not os.listdir(cls.diroutput):
            # Suppression des sous-dossiers de fail_test correspondant aux tests OK
            shutil.rmtree(cls.diroutput)


class TestZoomMassifError(unittest.TestCase):
    def test_zoom_42(self):
        self.assertRaises(ValueError, cartopy.Zoom_massif, 42)


@unittest.skipIf(not os.path.isfile(os.path.join(TEST_DATA_DIR, "Cor", "postproc_2021041006_2021041112.nc")),
                 "input file not available")
class TestCartopyCor(unittest.TestCase):
    """
    Test Map_Corse
    """
    @classmethod
    def setUpClass(cls):
        cls.mix = CartopyTestMixIn()
        basediroutput = os.path.join(THIS_DIR, "fail_test")
        if not os.path.isdir(basediroutput):
            os.makedirs(basediroutput)
        prefix = "output" + datetime.today().strftime("%Y%m%d%H%M%S%f-")
        cls.diroutput = tempfile.mkdtemp(prefix=prefix, dir=basediroutput)
        path_new = os.path.join(TEST_DATA_DIR, "Cor", "postproc_2021041006_2021041112.nc")
        cls.ps = prosimu(path_new)
        cls.points = cls.ps.get_points(ZS=2100, aspect=-1)
        cls.snow = cls.ps.read('SD_1DY_ISBA', selectpoint=cls.points, hasDecile=True)
        cls.massifs = cls.ps.read('massif_num', selectpoint=cls.points)

    def test_highlight(self):
        self.assertEqual(self.snow.shape, (6, 2, 9), "should be 9 deciles of 24 massifs over 6 time steps")
        self.m = cartopy.Map_corse(bgimage=True)
        self.m.init_massifs(**self.mix.attributes['SD_1DY_ISBA'])
        self.m.highlight_massif(self.massifs[0], self.snow, **self.mix.attributes['SD_1DY_ISBA'])
        self.m.plot_center_massif(self.massifs, self.snow[5, :, 4], self.snow[5, :, 8],
                                  **self.mix.attributes['SD_1DY_ISBA'])
        self.m.addlogo()
        self.m.set_maptitle("2021041112 percentile 50 and 90")
        self.m.set_figtitle("2100m")
        self.outfilename = "2021041112_cor_bgimage_highlight40.png"
        self.m.save(os.path.join(self.diroutput, self.outfilename), formatout="png")
        self.m.close()

    def test_zoom_41(self):
        self.m = cartopy.Zoom_massif(41)
        self.m.draw_massifs(self.massifs, self.snow[5, :, 8], **self.mix.attributes['SD_1DY_ISBA'])
        self.m.add_north_south_info()
        centre = [shape.centroid.coords[0] for shape in self.m.llshape]
        self.m.addpoints(*list(zip(*centre)), color='crimson', marker="+")
        self.m.addlogo()
        self.m.set_maptitle("2021041112 percentile 90")
        self.m.set_figtitle("2100m")
        self.outfilename = "2021041112_zoom_41.png"
        self.m.save(os.path.join(self.diroutput, self.outfilename), formatout="png")
        self.m.close()

    def test_annotate(self):
        self.m = cartopy.MultiMap_Cor(nrow=3, ncol=3, bgimage=True)
        self.m.init_massifs(**self.mix.attributes['SD_1DY_ISBA'])
        centre = [shape.centroid.coords[0] for shape in self.m.llshape]
        self.m.addpoints(*list(zip(*centre)), color='magenta', marker="o")
        self.m.addlogo()
        self.m.set_maptitle(["magenta center"])
        self.m.set_figtitle("2100m")
        self.outfilename = "multi_cor_bgimage_annotate.png"
        self.m.save(os.path.join(self.diroutput, self.outfilename), formatout="png")
        self.m.close()

    def tearDown(self):
        if six.PY3:  # Python 3.4+
            result = self.defaultTestResult()  # These two methods have no side effects
            self._feedErrorsToResult(result, self._outcome.errors)
            error = self.mix.list2reason(result.errors)
            failure = self.mix.list2reason(result.failures)
            if not error and not failure:
                shutil.move(os.path.join(self.diroutput, self.outfilename), os.path.join(THIS_DIR, "Manual_tests",
                                                                                         self.outfilename))
        else:
            print("no automatic move implemented for python2")

    @classmethod
    def tearDownClass(cls):
        cls.ps.close()
        if not os.listdir(cls.diroutput):
            # Suppression des sous-dossiers de fail_test correspondant aux tests OK
            shutil.rmtree(cls.diroutput)


@unittest.skipIf(not os.path.isfile(os.path.join(TEST_DATA_DIR, "Pyr", "postproc_2021041006_2021041112.nc")),
                 "input file not available")
class TestCartopyPyr(unittest.TestCase):
    """
    Test Map_pyrenees
    """
    @classmethod
    def setUpClass(cls):
        cls.mix = CartopyTestMixIn()
        basediroutput = os.path.join(THIS_DIR, "fail_test")
        if not os.path.isdir(basediroutput):
            os.makedirs(basediroutput)
        prefix = "output" + datetime.today().strftime("%Y%m%d%H%M%S%f-")
        cls.diroutput = tempfile.mkdtemp(prefix=prefix, dir=basediroutput)
        path_new = os.path.join(TEST_DATA_DIR, "Pyr", "postproc_2021041006_2021041112.nc")
        cls.ps = prosimu(path_new)
        cls.points_nord = cls.ps.get_points(aspect=0, ZS=2100, slope=40)
        cls.points_sud = cls.ps.get_points(aspect=180, ZS=2100, slope=40)
        cls.snow_nord = cls.ps.read('SD_1DY_ISBA', selectpoint=cls.points_nord, hasDecile=True)
        cls.snow_sud = cls.ps.read('SD_1DY_ISBA', selectpoint=cls.points_sud, hasDecile=True)
        cls.massifs = cls.ps.read('massif_num', selectpoint=cls.points_nord)

    def test_pyr_tables(self):
        self.m = cartopy.Map_pyrenees(geofeatures=True)
        self.m.init_massifs(**self.mix.attributes['SD_1DY_ISBA'])
        self.m.add_north_south_info()
        self.m.rectangle_massif(self.massifs, [0, 1, 2], [self.snow_sud[1, :, 1], self.snow_sud[1, :, 4],
                                                          self.snow_sud[1, :, 7], self.snow_nord[1, :, 1],
                                                          self.snow_nord[1, :, 4], self.snow_nord[1, :, 7]], ncol=2,
                                **self.mix.attributes['SD_1DY_ISBA'])
        self.m.addlogo()
        self.m.set_maptitle("2021041012")
        self.m.set_figtitle("2100m")
        self.outfilename = "2021041012_pyr_tables.png"
        self.m.save(os.path.join(self.diroutput, self.outfilename), formatout="png")
        self.m.close()
        self.m.reset_massifs()

    def test_zoom_70(self):
        self.m = cartopy.Zoom_massif(70)
        self.m.init_massifs(palette='YlGnBu', seuiltext=50., ticks=['A', 'B', 'C', 'D'],
                            label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm', ncolors=3)
        self.m.draw_massifs(self.massifs, self.snow_nord[1, :, 8], palette='YlGnBu', seuiltext=50.,
                            label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm', ncolors=3,
                            ticks=['A', 'B', 'C', 'D'])
        self.m.empty_massifs(**self.mix.attributes['SD_1DY_ISBA'])
        self.m.add_north_south_info()
        centre = [shape.centroid.coords[0] for shape in self.m.llshape]
        self.m.addpoints(*list(zip(*centre)), color='magenta', labels=self.m.name)
        self.m.addlogo()
        self.m.set_maptitle("2021041012 p90")
        self.m.set_figtitle("2100m")
        self.outfilename = "2021041012_zoom_70.png"
        self.m.save(os.path.join(self.diroutput, self.outfilename), formatout="png")
        self.m.close()

    def test_multi_map_pyr_tables(self):
        print()
        self.m = cartopy.MultiMap_Pyr(nrow=3, ncol=3, geofeatures=True)
        self.m.init_massifs(**self.mix.attributes['SD_1DY_ISBA'])
        self.m.add_north_south_info()
        titles = self.ps.readtime()
        self.m.set_maptitle(titles)
        self.m.rectangle_massif(self.massifs, [0, 1, 2], [self.snow_sud[:, :, 1], self.snow_sud[:, :, 4],
                                                          self.snow_sud[:, :, 7], self.snow_nord[:, :, 1],
                                                          self.snow_nord[:, :, 4], self.snow_nord[:, :, 7]], ncol=2,
                                **self.mix.attributes['SD_1DY_ISBA'], axis=0)
        self.m.addlogo()
        self.m.set_figtitle("2100m")
        self.outfilename = "2021041006_2021041112_pyr_tables.png"
        self.m.save(os.path.join(self.diroutput, self.outfilename), formatout="png")
        self.m.close()

    def tearDown(self):
        if six.PY3:  # Python 3.4+
            result = self.defaultTestResult()  # These two methods have no side effects
            self._feedErrorsToResult(result, self._outcome.errors)
            error = self.mix.list2reason(result.errors)
            failure = self.mix.list2reason(result.failures)
            if not error and not failure:
                shutil.move(os.path.join(self.diroutput, self.outfilename), os.path.join(THIS_DIR, "Manual_tests",
                                                                                         self.outfilename))
        else:
            print("no automatic move implemented for python2")

    @classmethod
    def tearDownClass(cls):
        cls.ps.close()
        if not os.listdir(cls.diroutput):
            # Suppression des sous-dossiers de fail_test correspondant aux tests OK
            shutil.rmtree(cls.diroutput)


@unittest.skipIf(not os.path.isfile(os.path.join(TEST_DATA_DIR, "Alp", "postproc_2021041006_2021041112.nc")),
                 "input file not available")
class TestCartopyAlp(unittest.TestCase):
    """
    Test Map_alpes class with colored massifs and text at the center.
    Test MultiMap_Alps
    """
    @classmethod
    def setUpClass(cls):
        cls.mix = CartopyTestMixIn()
        basediroutput = os.path.join(THIS_DIR, "fail_test")
        if not os.path.isdir(basediroutput):
            os.makedirs(basediroutput)
        prefix = "output" + datetime.today().strftime("%Y%m%d%H%M%S%f-")
        cls.diroutput = tempfile.mkdtemp(prefix=prefix, dir=basediroutput)
        path_new = os.path.join(TEST_DATA_DIR, "Alp", "postproc_2021041006_2021041112.nc")
        cls.ps = prosimu(path_new)
        cls.points = cls.ps.get_points(ZS=2100, aspect=-1)
        cls.snow = cls.ps.read('SD_1DY_ISBA', selectpoint=cls.points, hasDecile=True)
        cls.massifs = cls.ps.read('massif_num', selectpoint=cls.points)

    def setUp(self):
        pass

    def test_with_geo_features(self):
        self.assertEqual(self.snow.shape, (6, 24, 9), "should be 9 deciles of 24 massifs over 6 time steps")
        self.m = cartopy.Map_alpes(geofeatures=True)
        self.m.init_massifs(**self.mix.attributes['SD_1DY_ISBA'])
        self.m.draw_massifs(self.massifs, self.snow[5, :, 8], **self.mix.attributes['SD_1DY_ISBA'])
        self.m.plot_center_massif(self.massifs, self.snow[5, :, 0], self.snow[5, :, 4], self.snow[5, :, 8],
                                  **self.mix.attributes['SD_1DY_ISBA'])
        self.m.addlogo()
        self.m.set_maptitle("2021041112 percentile 90")
        self.m.set_figtitle("2100m")
        self.outfilename = "2021041112_p90_alps_geofeatures.png"
        self.m.save(os.path.join(self.diroutput, self.outfilename), formatout="png")
        self.m.close()

    def test_zoom_10(self):
        self.m = cartopy.Zoom_massif(10)
        self.m.draw_massifs(self.massifs, self.snow[5, :, 8], **self.mix.attributes['SD_1DY_ISBA'])
        self.m.add_north_south_info(english=True)
        self.m.addlogo()
        self.m.set_maptitle("2021041112 percentile 90")
        self.m.set_figtitle("2100m")
        self.outfilename = "2021041112_p90_zoom10.png"
        self.m.save(os.path.join(self.diroutput, self.outfilename), formatout="png")
        self.m.close()

    def test_multi_map_alps(self):
        self.lo = cartopy.MultiMap_Alps(nrow=3, ncol=3, geofeatures=False)
        self.lo.init_massifs(**self.mix.attributes['SD_1DY_ISBA'])
        self.lo.draw_massifs(self.massifs, self.snow[5, :, :], axis=1, **self.mix.attributes['SD_1DY_ISBA'])
        self.lo.highlight_massif(10, self.snow, **self.mix.attributes['SD_1DY_ISBA'])
        self.lo.set_figtitle("SD_1DY_ISBA 2021041112 2100m")
        titles = ['Percentile {0}'.format(i) for i in range(10, 100, 10)]
        self.lo.set_maptitle(titles)
        self.lo.plot_center_massif(self.massifs, self.snow[5,:,:], axis=1, **self.mix.attributes['SD_1DY_ISBA'])
        self.lo.addlogo()
        self.outfilename = "2021041112_multi_alps.png"
        self.lo.save(os.path.join(self.diroutput, self.outfilename), formatout="png")
        self.lo.close()

    def tearDown(self):
        if six.PY3:  # Python 3.4+
            result = self.defaultTestResult()  # These two methods have no side effects
            self._feedErrorsToResult(result, self._outcome.errors)
            error = self.mix.list2reason(result.errors)
            failure = self.mix.list2reason(result.failures)
            if not error and not failure:
                shutil.move(os.path.join(self.diroutput, self.outfilename), os.path.join(THIS_DIR, "Manual_tests",
                                                                                         self.outfilename))
        else:
            print("no automatic move implemented for python2")

    @classmethod
    def tearDownClass(cls):
        cls.ps.close()
        if not os.listdir(cls.diroutput):
            # Suppression des sous-dossiers de fail_test correspondant aux tests OK
            shutil.rmtree(cls.diroutput)


class CartopyTestMixIn(object):
    """
    methods for cartopy test classes.
    """
    @property
    def alphafile(self):
        return self._alphafile

    @alphafile.setter
    def alphafile(self, filename):
        self._alphafile = Alphafile(filename)

    attributes = dict(
        PP_SD_1DY_ISBA=dict(convert_unit=1., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                            label=u'Epaisseur de neige fraîche en 24h (cm)'),
        SD_1DY_ISBA=dict(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm'),
        SD_3DY_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 72h (cm)'),
        RAMSOND_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                          label=u'Epaisseur mobilisable (cm)'),
        NAT_LEV=dict(forcemin=-0.5, forcemax=5.5, palette='YlOrRd', ncolors=6, label=u'Risque naturel',
                     ticks=[u'Très faible', u'Faible', u'Mod. A', u'Mod. D', u'Fort', u'Très fort']),
        naturalIndex=dict(forcemin=0., forcemax=8., palette='YlOrRd', label=u'Indice de risque naturel',
                          format= '%.1f', nolevel=True),
        DSN_T_ISBA=dict(convert_unit=100., label=u'Hauteur de neige (cm)'),
        WSN_T_ISBA=dict(label=u'Equivalent en eau (kg/m2)'),
        SNOMLT_ISBA=dict(convert_unit=3. * 3600., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                         label=u'Ecoulement en 3h (kg/m2/3h)'),
        WET_TH_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur humide (cm)'),
        REFRZTH_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                          label=u'Epaisseur regelée (cm)'),
        RAINF_ISBA=dict(convert_unit=3. * 3600., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                        label=u'Pluie en 3h (kg/m2/3h)'),
    )

    def list2reason(self, exc_list):
        if exc_list and exc_list[-1][0] is self:
            return exc_list[-1][1]


class Alphafile():
    def __init__(self, filename):
        self.ds = Dataset(filename)
        self.lats = self.ds.variables['LAT'][:]
        self.lons = self.ds.variables['LON'][:]
        self.snow = self.ds.variables['SD_1DY_ISBA'][0, :, :, 8]
        # self.massifs = self.ds.variables['massif_num'][:, :]
        # self.slopes = self.ds.variables['slope'][:, :]


if __name__ == "__main__":


    unittest.main()