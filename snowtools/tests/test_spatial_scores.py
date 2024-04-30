#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import os
from snowtools.scores.list_scores import SpatialScoreFile
from snowtools.scores.spatial import SpatialScores
from snowtools.plots.scores.perfdiag import PerfDiag
from snowtools.tests.tempfolder import TestWithTempFolderWithLog


class TestSpatialFile(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        pass

    def test_create_file(self):
        sf = SpatialScoreFile(['bli', 'bla'], [1, 3, 5], [2.], [1.])
        # print(sf.POD)

    # def test_calc_spatial_scores(self):
    #     myscores = SpatialScores(["fcfile1", "fcfile2"],['bli', 'bla'], "obsfile",
    #                             [1, 3, 5],[2.], [1.])
    #     myscores.process()


class TestPerfDiag(TestWithTempFolderWithLog):

    def test_perfdiag_fig(self):
        diag = PerfDiag()
        diag.plot_scores([0.2, 0.5, 0.6, 0.9], [0.1, 0.5, 0.7, 0.4])
        diag.add_legend()
        diag.addlogo()
        diag.save(os.path.join(self.diroutput, "perfdiag.png"), formatout="png")


if __name__ == "__main__":
    unittest.main()
