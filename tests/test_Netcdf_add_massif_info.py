#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import os
import shutil
import sys
import tempfile
import datetime

from netCDF4 import Dataset

from snowtools.DATA import SNOWTOOLS_DATA, SNOWTOOLS_DIR

_here = os.path.dirname(os.path.realpath(__file__))


class UnitaryTest(unittest.TestCase):
    def setUp(self):
        basediroutput = os.path.join(_here, "fail_test")
        if not os.path.isdir(basediroutput):
            os.makedirs(basediroutput)
        prefix = "output" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f-")
        self.diroutput = tempfile.mkdtemp(prefix=prefix, dir=basediroutput)
        self.logfile = os.path.join(self.diroutput, 'output.log')
        with open(self.logfile, 'w') as f:
            f.write('Log for test {} executed on {}'.format(__file__, datetime.datetime.now()))

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


class TestScript(UnitaryTest):
    def test_script(self):
        path_forcing = os.path.join(SNOWTOOLS_DATA, 'FORCING_test_2d.nc')
        path_script = os.path.join(SNOWTOOLS_DIR, 'interpolation/Netcdf_add_massif_info.py')
        path_out = os.path.join(self.diroutput, 'test.nc')
        os.system("python3  " + path_script + " " + path_forcing + " -o " + path_out)
        with Dataset(path_out, 'r', format='NETCDF4') as file_input:
            self.assertTrue((file_input.variables['massif_num'][Ellipsis] == 69).all())


if __name__ == '__main__':
    unittest.main()
