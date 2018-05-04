'''
Created on 11 oct. 2017

@author: lafaysse
'''
import unittest
import os
import datetime
import tasks.s2m_command as s2m


class s2mTest(unittest.TestCase):

    def setUp(self):
        forcingtest = os.environ["SNOWTOOLS_CEN"] + "/DATA/FORCING_test_base.nc"
        diroutput = os.environ["SNOWTOOLS_CEN"] + "/tests/output" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f")
        self.commonoptions = " -f " + forcingtest + " -o " + diroutput + " -g"

    def tearDown(self):
        pass

    def full_run(self, shortcommand):
        command = shortcommand + self.commonoptions
        s2m.execute(command.split())

    def test_delayed_period(self):
        self.full_run("s2m -b 20101001 -e 20101002")

    def test_extract_domain(self):
        self.full_run("s2m -b 20100801 -e 20110201 -U 2400")

    def test_extend_slopes(self):
        self.full_run("s2m -b 20100801 -e 20110201 -l 0,20,40 -c 8")


if __name__ == "__main__":
    # import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
