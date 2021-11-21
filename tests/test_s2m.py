#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 11 oct. 2017

@author: lafaysse
"""
import unittest
import os
import shutil
import sys
import tempfile
import datetime

from snowtools.tasks.s2m_command import Surfex_command as s2m
from snowtools.DATA import SNOWTOOLS_DATA
from export_output import exportoutput
_here = os.path.dirname(os.path.realpath(__file__))


class s2mTest(unittest.TestCase):

    def setUp(self):
        basediroutput = os.path.join(_here, "fail_test")
        if not os.path.isdir(basediroutput):
            os.makedirs(basediroutput)
        prefix = "output" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f-")
        self.diroutput = tempfile.mkdtemp(prefix=prefix, dir=basediroutput)
        self.logfile = os.path.join(self.diroutput, 'output.log')
        with open(self.logfile, 'w') as f:
            f.write('Log for test {} executed on {}'.format(__file__, datetime.datetime.now()))

        self.commonoptions = " -o " + self.diroutput + " -g"

    def tearDown(self):
        if hasattr(self, '_outcome'):  # Python 3.4+
            result = self.defaultTestResult()  # These two methods have no side effects
            self._feedErrorsToResult(result, self._outcome.errors)
        else:  # Python 3.2 - 3.3 or 3.0 - 3.1 and 2.7
            result = getattr(self, '_outcomeForDoCleanups', self._resultForDoCleanups)
        error = self.list2reason(result.errors)
        failure = self.list2reason(result.failures)
        ok = not error and not failure

        # Demo:   report short info immediately (not important)
        if not ok:
            with open(self.logfile, 'r') as f:
                sys.stderr.write(f.read())
            typ, text = ('ERROR', error) if error else ('FAIL', failure)
            msg = [x for x in text.split('\n')[1:] if not x.startswith(' ')][0]
            print("\n%s: %s\n     %s" % (typ, self.id(), msg))

        if ok:
            # Suppression des sous-dossiers de fail_test correspondant aux tests OK
            shutil.rmtree(self.diroutput)
            # Suppression du dossier fail_test si tous les tests ont réussi (ie dossier vide)
            if os.listdir('fail_test') == []:
                os.rmdir('fail_test')

    def list2reason(self, exc_list):
        if exc_list and exc_list[-1][0] is self:
            return exc_list[-1][1]

    def full_run(self, shortcommand):
        command = shortcommand + self.commonoptions
        with exportoutput(self.logfile):
            s2m(command.split())

    def runatcen(self):
        return os.path.isdir("/rd/cenfic2/manto/lafaysse") or "DIRDATAPGD" in list(os.environ.keys())


class s2mMassifTest(s2mTest):

    def setUp(self):
        super(s2mMassifTest, self).setUp()
        forcingtest = os.path.join(SNOWTOOLS_DATA, "FORCING_test_massif.nc")
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
        forcingtest = os.path.join(SNOWTOOLS_DATA, "FORCING_test_2d.nc")
        namelist = os.path.join(SNOWTOOLS_DATA, "OPTIONS_test_2d.nam")
        # If the test is run at CEN, it can run PGD with available databases.
        # Otherwise, we do not test the PGD step and only take a PGD test file.
        if not self.runatcen():
            os.makedirs(self.diroutput+"/prep")
            pgd = os.path.join(SNOWTOOLS_DATA, "PGD_test_2d.nc")
            os.symlink(pgd, self.diroutput+"/prep/PGD.nc")
        self.commonoptions += " --grid -f " + forcingtest + " -n " + namelist

    def test_2d_ign(self):
        self.full_run("s2m -b 20150101 -e 20150201")


if __name__ == "__main__":
    # import sys;sys.argv = ['', 'Test.testName']
    unittest.main()
