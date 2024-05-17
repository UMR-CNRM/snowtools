#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 18 march 2024

@author: radanovics
"""
import os
import sys
import subprocess
import unittest

import numpy as np
from snowtools.tests.tempfolder import TestWithTempFolderWithLog
from snowtools.DATA import SNOWTOOLS_DIR
from snowtools.DATA import TESTBASE_DIR
from snowtools.DATA import REANALYSIS_DIR
from snowtools.scores.list_scores import ESCROC_list_scores, scores_file, ensemble_scores_file
from snowtools.scores.deterministic import SURFEX_dicvarnames
from snowtools.scores.ensemble import ESCROC_EnsembleScores
from snowtools.scores.generic import rankDiagram
from snowtools.utils.obscsv import obscsv

if not os.path.isdir(TESTBASE_DIR):
    SKIP = True
else:
    SKIP = False


class ScorePlotTest(TestWithTempFolderWithLog):
    """
    test the creation of score plots
    """

    def setUp(self):
        super(ScorePlotTest, self).setUp()

    @unittest.skipIf(SKIP, "input file not available")
    @unittest.skipIf(not os.path.isdir(REANALYSIS_DIR),
                     "input directory not available")
    def test_fullplots(self):
        """
        test the execution of the CompareSimuPosteObsCsv.py script
        """
        p = subprocess.run([sys.executable, os.path.join(SNOWTOOLS_DIR,
                                                         'scores/CompareSimuPosteObsCsv.py'),
                            '-b', '20171228', '-e', '20180107', '--scores', '--plot',
                            '--dirplot={0}'.format(self.diroutput),
                            '--fileobs={0}'.format(os.path.join(TESTBASE_DIR, "OBS_htn_2017122806_2018010606.csv"))])


class ScoreCalcTest(TestWithTempFolderWithLog):
    """
    Test score calculation
    """

    def setUp(self):
        super(ScoreCalcTest, self).setUp()
        self.members = range(1, 9)
        self.list_pro = [os.path.join(TESTBASE_DIR,
                                      'PRO/ensemble3/mb{:04d}/pro/PRO_2017122906_2018010606.nc'.format(member))
                         for member in self.members]
        self.list_varnames = ["snowdepth"]
        self.obsfile = os.path.join(TESTBASE_DIR, "OBS_htn_2017122806_2018010606_rochilles.nc")

    @unittest.skipIf(SKIP, "input file not available")
    def test_rank_histo(self):
        """
        test rank histogram calculation
        """
        e = ESCROC_EnsembleScores(self.list_pro, self.obsfile, self.list_varnames[0])
        freq, ranksum = rankDiagram(e.ensCommon, e.obsCommon, nbins=9)
        [self.assertAlmostEqual(f, o) for f, o in zip(freq.tolist(),
                                                      [0., 0., 0., 0., 0., 0.11111111, 0., 0., 0.88888889])]

    @unittest.skipIf(SKIP, "input file not available")
    def test_escroc_scores_by_member(self):
        """
        test for escroc_tasks
        """
        print(self.list_pro)
        list_scores = ["bias", "rmse", "mae"]

        e = ESCROC_list_scores()
        rdict = {"scores": e.compute_scores_allmembers(self.list_pro, self.obsfile,
                                                       list_scores, self.list_varnames),
                 "members": self.members}
        self.assertAlmostEqual(rdict["scores"][0, 1, 0], -0.086062541841526)
        self.assertAlmostEqual(rdict["scores"][1, 1, 0], 0.107970488223159)
        self.assertAlmostEqual(rdict["scores"][2, 1, 0], 0.0897967995224728)
        rdict["scores"] = np.reshape(rdict["scores"], newshape=(3, 8, 1, 1))
        scores_dataset = scores_file(os.path.join(self.diroutput, "scores.nc"), "w")
        for s, score in enumerate(list_scores):
            scores_dataset.write(score, rdict["scores"][s, :, :, :])

        scores_dataset.close()

    @unittest.skipIf(SKIP, "input file not available")
    def test_escroc_scores_ensemble(self):
        """
        test crps_task
        :return:
        """
        print(self.list_pro)
        e = ESCROC_EnsembleScores(self.list_pro, self.obsfile, self.list_varnames[0])
        crps1, reli, pot = e.CRPS_decomp()
        self.assertAlmostEqual(crps1, 0.10290882106870172)
        self.assertAlmostEqual(reli, 0.0924720315272686)
        self.assertAlmostEqual(pot, 0.010436789541433122)
        crps = e.CRPS()
        self.assertAlmostEqual(crps, 0.102908821068702)
        dispersion, rmse, ss = e.dispersionEnsemble()
        self.assertAlmostEqual(dispersion, 0.0276429782949738)
        self.assertAlmostEqual(rmse, 0.134734853014555)
        self.assertAlmostEqual(ss, 0.205165758350496)
        crps = np.reshape(crps, (1, 1))
        dispersion = np.reshape(dispersion, (1, 1))
        rmse = np.reshape(rmse, (1, 1))
        ss = np.reshape(ss, (1, 1))
        scores_dataset = ensemble_scores_file(os.path.join(self.diroutput, "scores_ens.nc"), "w")
        mems = np.reshape(self.members, (1, 8))
        scores_dataset.write_members(mems)
        scores_dataset.write("crps", crps)
        scores_dataset.write("dispersion", dispersion)
        scores_dataset.write("rmse", rmse)
        scores_dataset.write("ss", ss)

        scores_dataset.close()


if __name__ == "__main__":
    unittest.main()
