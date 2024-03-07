#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import os

from snowtools.DATA import TESTBASE_DIR
from snowtools.plots.pearps2m.postprocess import EnsemblePostproc, Ensemble
from snowtools.tests.tempfolder import TestWithTempFolderWithLog
from bronx.stdtypes.date import Date


if not os.path.isdir(TESTBASE_DIR):
    SKIP = True
else:
    SKIP = False


class TestEnsPP(TestWithTempFolderWithLog):

    @classmethod
    def setUpClass(cls):
        # create test file list
        ens_path = os.path.join(TESTBASE_DIR, 'PRO', 'ensemble3')
        cls.filelist = [ens_path + '/mb00{0:02d}/pro/PRO_2017122906_2018010606.nc'.format(mb) for mb in range(1, 36)]

    @unittest.skipIf(SKIP, 'Test files not available')
    def test_ens_pp_algo(self):
        ens = EnsemblePostproc(Ensemble(), ['SWE_1DY_ISBA'], self.filelist,
                               Date('2017122906'),
                               Date('2018010606'),
                               outdir = self.diroutput)
        # do postprocessing
        ens.postprocess()

    @classmethod
    def tearDownClass(cls):
        pass


if __name__ == "__main__":
    unittest.main()
