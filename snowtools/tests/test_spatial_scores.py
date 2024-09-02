#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import os

os.system('ls $VIRTUAL_ENV/lib/python*/site-packages/snowtools/')
os.system('ls $VIRTUAL_ENV/lib/python*/site-packages/snowtools/scores/')

import matplotlib.pyplot as plt
import numpy as np
import timeit

from snowtools.scores.list_scores import SpatialScoreFile
from snowtools.scores.spatial import ProVsPleiade, call_crps, LocalMoranData
from snowtools.scores.ensemble import EnsembleScores
from snowtools.plots.scores.perfdiag import PerfDiag, FuzzyScoreDiagram
from snowtools.plots.scores.moran_scatter import MoranScatter
from snowtools.utils.S2M_standard_file import LCCProjectionType
from snowtools.tests.tempfolder import TestWithTempFolderWithLog
from snowtools.DATA import TESTBASE_DIR


THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_SIM_DIR = os.path.join(TESTBASE_DIR, "PRO")
TIME_CRPS = False


class TestSpatialFile(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.myscores = ProVsPleiade([os.path.join(TESTBASE_DIR, 'PRO', 'PRO_2019051300_2019051400.nc'),
                                     os.path.join(TESTBASE_DIR, 'PRO', 'PRO_2019051300_2019051400.nc')],
                                     ['bli', 'bla'], os.path.join(TESTBASE_DIR, 'P250_GR_13_05_19_attr.nc'),
                                     'DSN_T_ISBA', [1, 3, 5], [2.], [1.])
        cls.myscores.apply_mask(maskfile=os.path.join(TESTBASE_DIR, "masque_glacier2017_foret_ville_riviere.nc"))

    def test_create_file(self):
        sf = SpatialScoreFile(['bli', 'bla'], [1, 3, 5], [2.], [1.])
        # print(sf.POD)

    @unittest.skipIf(not TIME_CRPS, "No crps timing required")
    def test_time_crps_methods(self):
        fc = self.myscores.fc_data['bli'].data[~np.isnan(self.myscores.fc_data['bli'].data)]  # *100
        obs = self.myscores.obs_data.data[~np.isnan(self.myscores.obs_data.data)]  # *100
        meanobs = np.nanmean(obs)
        ens_scores = EnsembleScores([1], [1], (meanobs,), fc.reshape(np.size(fc), 1))
        vars = {**globals(), **locals()}
        # print(vars)
        # print("crps_spatial_scipy", timeit.timeit("crps_spatial_scipy(fc, np.array([meanobs, meanobs]))",
        #                     globals=vars, number=30))
        print("call_crps", timeit.timeit("call_crps(fc, meanobs)",
                            globals=vars, number=30))
        print("ens_scores.CRPS()", timeit.timeit("ens_scores.CRPS()",
                            globals=vars, number=30))
        print("ens_scores.CRPS_decomp()", timeit.timeit("ens_scores.CRPS_decomp()",
                            globals=vars, number=30))

    def test_crps_functions(self):
        fc = self.myscores.fc_data['bli'].data[~np.isnan(self.myscores.fc_data['bli'].data)] # *100
        obs = self.myscores.obs_data.data[~np.isnan(self.myscores.obs_data.data)] # *100
        meanobs = np.nanmean(obs)
        ens_scors = EnsembleScores([1], [1], (meanobs,), fc.reshape(np.size(fc), 1))

        sps2 = call_crps(fc, obs)
        self.assertAlmostEqual(sps2, 0.05011011529)
        sps3 = call_crps(fc, meanobs)
        self.assertAlmostEqual(sps3, 0.26393349433)
        crps = ens_scors.CRPS()
        self.assertAlmostEqual(crps, 0.26393349433)
        crps, reli, crps_pot = ens_scors.CRPS_decomp()
        self.assertAlmostEqual(crps, 0.26393349433)
        self.assertAlmostEqual(reli, 0.2633282234)
        self.assertAlmostEqual(crps_pot, 0.0006052709)
        myfc = np.array([1., 2., 3., 4.])
        ens_scors2 = EnsembleScores([1], [1], (2.5,), myfc.reshape(np.size(myfc), 1))
        crps, reli, crps_pot = ens_scors2.CRPS_decomp()
        self.assertAlmostEqual(crps, 0.375)
        self.assertAlmostEqual(reli, 0.125)
        self.assertAlmostEqual(crps_pot, 0.25)
        sps3 = call_crps(myfc, np.array(2.75))
        self.assertAlmostEqual(sps3, 0.375)

    def test_calc_spatial_scores(self):
        self.myscores.process()
        self.assertAlmostEqual(self.myscores.score_ds['SPS'].data[0], 0.05011011529)
        # print(self.myscores.score_ds)

    def test_local_moran(self):
        local_moran = LocalMoranData(self.myscores.fc_data['bli'].data)
        self.assertAlmostEqual(local_moran.moran_I, 0.876794444)
        # TODO: implement tests for colored moran scatter plot and quadrant map
        # local_moran.plot_moran_scatter_colored('snow height', title=self.myscores.experiments[0])
        # lcc = LCCProjectionType(self.myscores.fc_data['bli'].xx.data, self.myscores.fc_data['bli'].yy.data)
        # print(lcc.crs)
        # local_moran.plot_quadrant_map(self.myscores.obs_data.xx.data, self.myscores.obs_data.yy.data, lcc.crs)

        # print(loca_moran)


class TestPerfDiag(TestWithTempFolderWithLog):

    def test_perfdiag_fig(self):
        diag = PerfDiag()
        diag.plot_scores([0.2, 0.5, 0.6, 0.9], [0.1, 0.5, 0.7, 0.4])
        diag.add_legend()
        diag.addlogo()
        diag.save(os.path.join(self.diroutput, "perfdiag.png"), formatout="png")


class TestFuzzyDiag(TestWithTempFolderWithLog):

    def test_fuzzy_diag_fig(self):
        diag = FuzzyScoreDiagram()
        rng = np.random.default_rng(12345)
        diag.draw(rng.uniform(low=0., high=1, size=(5, 10)), range(1, 6), range(0, 10))
        diag.save(os.path.join(self.diroutput, "fuzzydiag.png"), formatout="png")


class TestMoranScatter(TestWithTempFolderWithLog):

    def test_moran_scatter_fig(self):
        diag = MoranScatter('snow height')
        diag.plot_var(np.array([0.2, 0.5, 0.6, 0.9, 0.1, 0.4, 1.3, 0.3, 0.15, 0.25]),
                      np.array([0.1, 0.5, 0.7, 1.1, 0.4, 0.5, 0.8, 0.9, 0.2, 0.15]))
        diag.addlogo()
        diag.save(os.path.join(self.diroutput, "moranscatter.png"), formatout="png")


if __name__ == "__main__":
    unittest.main()
