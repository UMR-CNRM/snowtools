#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import os

from snowtools.DATA import TESTBASE_DIR
from snowtools.plots.pearps2m.postprocess import (EnsemblePostproc, EnsembleOperDiagsFlatMassif, EnsembleMassifPoint,
                                                  EnsembleStation)
from snowtools.tests.tempfolder import TestWithTempFolderWithLog
from bronx.stdtypes.date import Date

from snowtools.utils.FileException import VarNameException

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
        ens = EnsemblePostproc(['SWE_1DY_ISBA'], self.filelist, outdir = self.diroutput)
        # do postprocessing
        ens.postprocess()

    @unittest.skipIf(SKIP, 'Test files not available')
    def test_ens_pp_stats(self):
        ens = EnsemblePostproc(['SWE_1DY_ISBA'], self.filelist, outdir=self.diroutput)
        ens.create_outfile()
        ens.init_outfile()
        print(ens.variables)
        ens.median()
        self.assertAlmostEqual(ens.mean('SWE_1DY_ISBA')[20][0], 14.00070603)
        self.assertAlmostEqual(ens.spread('SWE_1DY_ISBA')[20][0], 0.1909657167)
        assert ens.geo == "massifs"
        assert ens.get_massifdim()[0] == 13
        self.assertRaises(VarNameException, ens.get_massifvar)
        assert ens.get_metadata()[0][0] == '13_2400'
        self.assertRaises(VarNameException, ens.get_metadata, nolevel=True)
        assert ens.get_aspect()[0] == -1.
        ens.outdataset.close()
        ens.close()

    @unittest.skipIf(SKIP, 'Test files not available')
    def test_ens_oper_diags(self):
        ens = EnsembleOperDiagsFlatMassif()
        ens.list_var_map = ['DSN_T_ISBA']
        ens.list_var_spag = ['DSN_T_ISBA']
        ens.open(self.filelist)
        ens.alldiags()
        assert len(ens.quantiles['DSN_T_ISBA'][1]) == 193
        self.assertAlmostEqual(ens.quantiles['DSN_T_ISBA'][1][20][0], 0.6636468335391518)
        self.assertAlmostEqual(ens.probability('DSN_T_ISBA', seuilinf=0.66)[20][0], 0.62857143)
        ens.nech = 3
        ens.pack_maps('alp', 'bla', diroutput=self.diroutput)
        ens.pack_spaghettis('bla', diroutput=self.diroutput)
        ens.close()
        # assert False

    @unittest.skipIf(SKIP, 'Test files not available')
    def test_ens_massif_point(self):
        ens = EnsembleMassifPoint(13, 2400, -1, 0)
        ens.open(self.filelist)
        assert ens.select_points()[0] == 0
        ens.close()

    @unittest.skipIf(SKIP, 'Test files not available')
    def test_ens_station(self):
        ens = EnsembleStation()
        ens.open([os.path.join(TESTBASE_DIR, 'PRO', 'PRO_LaPlagne_2000-2001.nc')])
        assert ens.geo == 'stations'
        assert ens.get_station()[0] == 73150401
        assert ens.get_metadata()[0][0] == '73150401'
        ens.close()


    @classmethod
    def tearDownClass(cls):
        pass


if __name__ == "__main__":
    unittest.main()
