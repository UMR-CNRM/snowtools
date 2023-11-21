# -*- coding: utf-8 -*-
"""
Created on 2021-11-26

@author: Léo Viallon-Galinier
"""
import unittest
import os
import shutil
import tempfile
import datetime
import sys

_here = os.path.dirname(os.path.realpath(__file__))


class TestWithTempFolder(unittest.TestCase):
    """
    Extension of ``unittest.TestCase`` to provide a temporary folder for
    test output that is removed in case of test pass and keeped if test
    fails.

    Defines a ``setUp`` and ``tearDown`` class methods that create the temporary
    folder in ``snowtools/tests/fail_test`` folder and provide the path to
    temporary folder in ``diroutput`` attribute.
    """
    def setUp(self):
        """
        Create the temporary folder before starting the test
        """
        basediroutput = os.path.join(_here, "fail_test")
        if not os.path.isdir(basediroutput):
            os.makedirs(basediroutput)
        self._basediroutput = basediroutput
        prefix = "output" + datetime.datetime.today().strftime("%Y%m%d%H%M%S%f-")
        self.diroutput = tempfile.mkdtemp(prefix=prefix, dir=basediroutput)
        self.test_pass = None
        self.errors = {}

    def tearDown(self):
        """
        Check for test output and delete temporary output if test passed

        Let available the result in ``self.test_pass``
        """
        result = self.defaultTestResult()  # These two methods have no side effects
        self._feedErrorsToResult(result, self._outcome.errors)
        error = self.list2reason(result.errors)
        failure = self.list2reason(result.failures)
        ok = not error and not failure

        if not ok:
            self.errors = {'errors': error, 'failures': failure}

        self.test_pass = ok

        if ok:
            # Delete temporary folder for passing test
            shutil.rmtree(self.diroutput)
            # Also delete fail_test folder if all tests passed (ie empty folder)
            if os.listdir(self._basediroutput) == []:
                os.rmdir(self._basediroutput)

    def list2reason(self, exc_list):
        if exc_list and exc_list[-1][0] is self:
            return exc_list[-1][1]

class TestWithTempFolderWithLog(TestWithTempFolder):
    """
    Adds a log file which path is available in ``self.logfile``.
    """
    def setUp(self):
        """
        Let a log file available.
        """
        super(TestWithTempFolderWithLog, self).setUp()
        self.logfile = os.path.join(self.diroutput, 'output.log')
        with open(self.logfile, 'w') as f:
            f.write('Log for test {} executed on {}'.format(__file__, datetime.datetime.now()))

    def tearDown(self):
        """
        Print the contet of the logfile on error output if
        test fails
        """
        super(TestWithTempFolderWithLog, self).tearDown()
        if not self.test_pass:
            with open(self.logfile, 'r') as f:
                sys.stderr.write(f.read())

