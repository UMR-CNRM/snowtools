#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import os

import numpy as np

from snowtools.DATA import TESTBASE_DIR, SNOWTOOLS_DATA
from snowtools.plots.pearps2m.postprocess import (EnsemblePostproc, EnsembleOperDiagsFlatMassif, EnsembleMassifPoint,
                                                  EnsembleStation)
from snowtools.tools.emosCSG import postprocess_massif
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
        # assert False

    @unittest.skipIf(SKIP, 'Test files not available')
    def test_ens_pp_algo_emos_csg_nonorm(self):
        ens = EnsemblePostproc(['SWE_1DY_ISBA'], self.filelist, outdir= self.diroutput,
                               outfilename='PRO_pp_csg.nc',
                               emosmethod=EnsemblePostproc.emos_csg_nonorm_allstations_newsnow)
        # print(ens.emosmethod)
        # ens.postprocess()
        ens.create_outfile()
        ens.init_outfile()
        ens.deciles()
        print(ens.outdataset.dataset['SWE_1DY_ISBA'][:])
        self.assertAlmostEqual(ens.outdataset.dataset['SWE_1DY_ISBA'][1, 0, 8],0.00312186 )
        self.assertEqual(ens.outdataset.dataset['SWE_1DY_ISBA'][1, 0, 6], 0.)

        ens.outdataset.close()
        ens.close()

        # assert False

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

    @unittest.skipIf(SKIP, 'Test files not available')
    def test_emosCSG(self):
        ens = postprocess_massif()
        ens.newsnow_var_name = 'DSN_T_ISBA'
        ens.read_emos_param(filename=os.path.join(SNOWTOOLS_DATA, 'EMOS_HN.Rdata'))
        ens.open(self.filelist)
        ppfile = os.path.join(self.diroutput, "PP_emosCSG.nc")
        ens.create_pp_file(ppfile)
        ens.close()

    @classmethod
    def tearDownClass(cls):
        pass


if __name__ == "__main__":
    unittest.main()
    # import argparse
    # PARSER = argparse.ArgumentParser(description="Postprocess new snow heights: "
    #                                              "1) extracts operational simulation results,"
    #                                              "2) Plots maps for the Alps, the Pyrenees the Corse,"
    #                                              "3) Creates spaghetti plots "
    #                                              "for all massifs and stations")
    #
    # PARSER.add_argument("-b", action="store", type=str, dest="datebegin", default='20250921',
    #                     help="First year of extraction")
    # PARSER.add_argument("-e", action="store", type=str, dest="dateend", default='20250921',
    #                     help="Last year of extraction")
    # PARSER.add_argument("-o", action="store", type=str, dest="diroutput",
    #                     default='/tmp',
    #                     help="Output directory")
    # PARSER.add_argument("--dev", action="store_true", dest="dev", default=False)
    # PARSER.add_argument("--reforecast", action="store_true", dest="reforecast", default=False)
    # PARSER.add_argument("--dble", action="store_true", dest="dble", default=False)
    # OPTIONS = PARSER.parse_args()  # @UnusedVariable
    # from snowtools.plots.pearps2m.postprocess import Config
    # c = Config(OPTIONS)
    # import snowtools.plots.pearps2m.postprocess as pp
    #
    # pp.pp_plots(c)
