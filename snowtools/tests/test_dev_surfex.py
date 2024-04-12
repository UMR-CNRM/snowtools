#!/usr/bin/env python3
# -*- coding: utf-8 -*-
'''
Created on 11 oct. 2017

@author: lafaysse
'''
import unittest
import os
import shutil

from snowtools.tests.s2m_instance import s2mTest
from snowtools.DATA import SNOWTOOLS_DATA
from snowtools.DATA import TESTBASE_DIR

_here = os.path.dirname(os.path.realpath(__file__))

SKIP_TEST_SNOWPAPPUS = False if os.getenv('SNOWTOOLS_TEST_SNOWPAPPUS', False) else True
SKIP_TEST_AXEL_AARON = False if os.getenv('SNOWTOOLS_TEST_AXEL_AARON', False) else True


class s2mTestBase(s2mTest):
    def setUp(self):
        super(s2mTestBase, self).setUp()
        self.path_namelist = os.path.join(_here, 'namelists/')
        self.namelist = os.path.join(self.diroutput, 'namelist.nam')
        self.forcingtest = os.path.join(SNOWTOOLS_DATA, "FORCING_test_base.nc")
        self.commonoptions = " -o " + self.diroutput + " -f " + self.forcingtest + " -n " + self.namelist + " -g"


class s2mTestForcageBase(s2mTestBase):

    def test_base(self):
        shutil.copy(self.path_namelist + "namelist_base.nam", self.namelist)
        self.full_run("s2m research -b 20101215 -e 20110115")

    @unittest.skipIf(SKIP_TEST_SNOWPAPPUS,
                     'SnowPappus not yet implemented in SURFEX cen branch. '
                     'Please use SNOWTOOLS_TEST_PAPPUS env variable to force test')
    def test_snowpappus(self):
        # NAM_ISBA_SNOW: LSNOWPAPPUS = TRUE
        shutil.copy(self.path_namelist + "namelist_pappus_1.nam", self.namelist)
        self.full_run("s2m research -b 20101215 -e 20110115")

    @unittest.skipIf(SKIP_TEST_AXEL_AARON,
                     'Work of Axel and Aaron on MEB not yet implemented in SURFEX cen branch. '
                     'Please use SNOWTOOLS_TEST_AXEL_AARON env variable to force test')
    def test_axel_aaron(self):
        # NAM_MEB_ISBA:  LMEB_TALL_VEG       = .TRUE., LMEB_INT_PHASE_LUN  = .TRUE.,
        #                LMEB_INT_UNLOAD_LUN = .TRUE., LMEB_INT_UNLOAD_SFC = .TRUE.,
        shutil.copy(self.path_namelist + "namelist_axel_aaron.nam", self.namelist)
        self.full_run("s2m research -b 20101215 -e 20110115")

    def test_multiphy1(self):
        # NAM_ISBA_SNOW:   CSNOWMETAMO = 'F06', CSNOWFALL = 'S02', CSNOWCOMP = 'T11', CSNOWCOND = 'I02', CSNOWHOLD = 'O04'
        # NAM_ISBA: CSNOWRES = 'M98'
        shutil.copy(self.path_namelist + "namelist_multiphy1.nam", self.namelist)
        self.full_run("s2m research -b 20101215 -e 20110115")

    def test_multiphy2(self):
        # NAM_ISBA_SNOW:   CSNOWMETAMO = 'T07', CSNOWFALL = 'NZE', CSNOWCOMP = 'S14', CSNOWCOND = 'C11', CSNOWHOLD = 'B02'
        # NAM_ISBA: CSNOWRES = 'DEF',  XCVHEATF     = 0.4
        shutil.copy(self.path_namelist + "namelist_multiphy2.nam", self.namelist)
        self.full_run("s2m research -b 20101215 -e 20110115")

    def test_multiphy3(self):
        # NAM_ISBA_SNOW:   CSNOWMETAMO = 'S-F', CSNOWFALL = 'P75', CSNOWCOMP = 'T11', CSNOWCOND = 'Y81', CSNOWHOLD = 'SPK'
        # NAM_ISBA: CSNOWRES = 'RIL',  XCVHEATF     = 0.5
        shutil.copy(self.path_namelist + "namelist_multiphy3.nam", self.namelist)
        self.full_run("s2m research -b 20101215 -e 20110115")

    def test_snowdrift1(self):
        # NAM_ISBA_SNOW: CSNOWDRIFT='GA01',  LSNOWDRIFT_SUBLIM= .FALSE.,   LSNOWSYTRON = .TRUE.
        #                CSNOWMETAMO = 'S-C', CSNOWFALL = 'A76', CSNOWCOMP = 'S14', CSNOWCOND = 'I02', CSNOWHOLD = 'O04'
        # NAM_ISBA: CSNOWRES = 'DEF',  XCVHEATF     = 0.5
        shutil.copy(self.path_namelist + "namelist_snowdrift1.nam", self.namelist)
        self.full_run("s2m research -b 20101215 -e 20110115")

    def test_snowdrift2(self):
        # NAM_ISBA_SNOW: CSNOWDRIFT='DFLT',  LSNOWDRIFT_SUBLIM= .FALSE.,   LSNOWSYTRON = .TRUE.
        #                CSNOWMETAMO = 'F06', CSNOWFALL = 'S02', CSNOWCOMP = 'B92', CSNOWCOND = 'C11', CSNOWHOLD = 'B02'
        # NAM_ISBA: CSNOWRES = 'RIL',  XCVHEATF     = 0.4
        shutil.copy(self.path_namelist + "namelist_snowdrift2.nam", self.namelist)
        self.full_run("s2m research -b 20101215 -e 20110115")

    def test_snowdrift3(self):
        # NAM_ISBA_SNOW: CSNOWDRIFT='NONE',  LSNOWDRIFT_SUBLIM= .FALSE.,   LSNOWSYTRON = .TRUE.
        #                CSNOWMETAMO = 'S-C', CSNOWFALL = 'V12', CSNOWCOMP = 'S14', CSNOWCOND = 'C11', CSNOWHOLD = 'B92'
        # NAM_ISBA: CSNOWRES = 'M98',  XCVHEATF     = 0.4
        shutil.copy(self.path_namelist + "namelist_snowdrift3.nam", self.namelist)
        self.full_run("s2m research -b 20101215 -e 20110115")

    def test_resort1(self):
        # NAM_ISBA_SNOW:  LSNOWCOMPACT_BOOL = T, LSNOWTILLER = T, LSNOWMAK_BOOL = T, LSNOWMAK_PROP = T, LSELF_PROD = T
        # NAM_SURF_SNOW_CSTS  XPSR_SNOWMAK = 0.002, XRHO_SNOWMAK = 600, XPTA_SEUIL = 268.15, XPROD_SCHEME = 0,0,0,0,0, XSM_END = 4,30,4,30, XFREQ_GRO = 1
        shutil.copy(self.path_namelist + "namelist_resort1.nam", self.namelist)
        self.full_run("s2m research -b 20101215 -e 20110115")


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "FORCING",
                                                 "FORCING_test_impur.nc")),
                 "input file not available")
class s2mTestForcageImpurete(s2mTestBase):

    def setUp(self):
        super(s2mTestForcageImpurete, self).setUp()
        self.forcingtest = os.path.join(TESTBASE_DIR, "FORCING", "FORCING_test_impur.nc")
        self.commonoptions = " -o " + self.diroutput + " -f " + self.forcingtest + " -n " + self.namelist + " -g"

    def test_impur1(self):
        # NAM_PREP_ISBA_SNOW     NIMPUR=2,
        # NAM_IO_OFFLINE         LSPECSNOW = .TRUE.    NIMPUROF=2      LFORCIMP = .TRUE.
        # NAM_DIAG_ISBAn         LPROBANDS = .TRUE.
        # NAM_ISBA_SNOWn         CSNOWRAD='T17'   LATMORAD=.FALSE.
        # NAM_SURF_SNOW_CSTS     XIMPUR_WET(1)=0.e-9   XIMPUR_WET(2)=0.e-9   XIMPUR_DRY(1)=0.e-9   XIMPUR_DRY(2)=0.e-9
        shutil.copy(self.path_namelist + "namelist_impur1.nam", self.namelist)
        self.full_run("s2m research -b 20171215 -e 20180115")

    def test_impur2(self):
        # NAM_PREP_ISBA_SNOW     NIMPUR=1,
        # NAM_IO_OFFLINE         LSPECSNOW = .TRUE.    NIMPUROF=1      LFORCIMP = .FALSE.
        # NAM_DIAG_ISBAn         LPROBANDS = .TRUE.
        # NAM_ISBA_SNOWn         CSNOWRAD='T17'   LATMORAD=.FALSE.
        # NAM_SURF_SNOW_CSTS     XIMPUR_WET(1)=2.e-9   XIMPUR_WET(2)=2.e-9   XIMPUR_DRY(1)=2.e-9   XIMPUR_DRY(2)=2.e-9
        shutil.copy(self.path_namelist + "namelist_impur2.nam", self.namelist)
        self.full_run("s2m research -b 20171215 -e 20180115")


@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "FORCING",
                                                 "FORCING_test_2d.nc")),
                 "input file not available")
class s2m2DTest(s2mTestBase):

    def setUp(self):
        super(s2m2DTest, self).setUp()
        self.forcingtest = os.path.join(TESTBASE_DIR, "FORCING", "FORCING_test_2d.nc")
        self.namelist = os.path.join(SNOWTOOLS_DATA, "OPTIONS_test_2d.nam")
        # If the test is run at CEN, it can run PGD with available databases.
        # Otherwise, we do not test the PGD step and only take a PGD test file.
        if not self.runatcen:
            os.makedirs(self.diroutput + "/prep")
            pgd = os.path.join(SNOWTOOLS_DATA, "PGD_test_2d.nc")
            os.symlink(pgd, self.diroutput + "/prep/PGD.nc")
        self.commonoptions = (" -o " + self.diroutput + " --geotype=grid -f " + self.forcingtest + " -n "
                              + self.namelist + " -g")

    def test_2d_ign(self):
        self.full_run("s2m research -b 20150101 -e 20150201")


@unittest.skipIf(SKIP_TEST_SNOWPAPPUS,
                 'SnowPappus not yet implemented in SURFEX cen branch. '
                 'Please use SNOWTOOLS_TEST_PAPPUS env variable to force test')
@unittest.skipIf(not os.path.isfile(os.path.join(TESTBASE_DIR, "FORCING",
                                                 "FORCING_test_2d.nc")),
                 "input file not available")
class s2m2DTest_pappus(s2mTestBase):

    def setUp(self):
        super(s2m2DTest_pappus, self).setUp()
        self.forcingtest = os.path.join(TESTBASE_DIR, "FORCING", "FORCING_test_2d.nc")
        self.namelist = os.path.join(SNOWTOOLS_DATA, "OPTIONS_test_2d_pappus.nam")
        # If the test is run at CEN, it can run PGD with available databases.
        # Otherwise, we do not test the PGD step and only take a PGD test file.
        if not self.runatcen:
            os.makedirs(self.diroutput + "/prep")
            pgd = os.path.join(SNOWTOOLS_DATA, "PGD_test_2d.nc")
            os.symlink(pgd, self.diroutput + "/prep/PGD.nc")
        self.commonoptions = (" -o " + self.diroutput + " --geotype=grid -f " + self.forcingtest + " -n "
                              + self.namelist + " -g")

    def test_2d_ign_pappus(self):
        self.full_run("s2m research -b 20150101 -e 20150201")


if __name__ == "__main__":
    # import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
