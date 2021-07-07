'''
Created on 11 oct. 2017

@author: lafaysse
'''
import unittest
import os
import datetime
from tasks.s2m_command import Surfex_command as s2m


class s2mTest(unittest.TestCase):

    def setUp(self):
        self.diroutput = os.environ["SNOWTOOLS_CEN"] + "/tests/output" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f")
        self.commonoptions = " -o " + self.diroutput + " -g"

    def tearDown(self):
        pass

    def full_run(self, shortcommand):
        command = shortcommand + self.commonoptions
        s2m(command.split())

    def runatcen(self):
        return os.path.isdir("/rd/cenfic2/manto/lafaysse") or "DIRDATAPGD" in list(os.environ.keys())


class s2mMassifTest(s2mTest):

    def setUp(self):
        super(s2mMassifTest, self).setUp()
        forcingtest = os.environ["SNOWTOOLS_CEN"] + "/DATA/FORCING_test_massif.nc"
        self.commonoptions += " -f " + forcingtest
        
    def test_delayed_period(self):
        self.full_run("s2m -b 20101001 -e 20101002")

    def test_extract_domain(self):
        self.full_run("s2m -b 20100801 -e 20110201 -U 2400")

    def test_extend_slopes(self):
        self.full_run("s2m -b 20110101 -e 20110201 -l 0,20,40 -c 8")


class s2m2DTest(s2mTest):

    def setUp(self):
        
        super(s2m2DTest, self).setUp()
        forcingtest = os.environ["SNOWTOOLS_CEN"] + "/DATA/FORCING_test_2d.nc"
        namelist = os.environ["SNOWTOOLS_CEN"] + "/DATA/OPTIONS_test_2d.nam"
        # If the test is run at CEN, it can run PGD with available databases. Otherwise, we do not test the PGD step and only take a PGD test file.
        if not self.runatcen():
            os.makedirs(self.diroutput+"/prep")
            os.symlink(os.environ["SNOWTOOLS_CEN"] + "/DATA/PGD_test_2d.nc", self.diroutput+"/prep/PGD.nc")
        self.commonoptions += " --grid -f " + forcingtest + " -n " + namelist

    def test_2d_ign(self):
        self.full_run("s2m -b 20150101 -e 20150201")


if __name__ == "__main__":
    # import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
