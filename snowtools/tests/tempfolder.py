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
import logging

_here = os.path.dirname(os.path.realpath(__file__))


class TestWithTempFolder(unittest.TestCase):
    """
    Extension of ``unittest.TestCase`` to provide a temporary folder for
    test output that is removed in case of test pass and keeped if test
    fails.

    Defines a ``setUp`` and ``tearDown`` class methods that create the temporary
    folder and provide the path to temporary folder in ``diroutput`` attribute.

    Warning: Does not go into the output directory, if you need so, have a look to TestWithTempFolderWithChdir.
    """
    def setUp(self):
        """
        Create the temporary folder before starting the test
        """
        prefix = "snowtools_tests_output_" + datetime.datetime.today().strftime("%Y%m%d%H%M%S-")
        self.diroutput = tempfile.mkdtemp(prefix=prefix)
        self.test_pass = None
        self.errors = {}

    def tearDown(self):
        """
        Check for test output and delete temporary output if test passed

        Let available the result in ``self.test_pass``
        """
        if hasattr(self._outcome, 'errors'):  # Python <= 3.10
            result = self.defaultTestResult()  # These two methods have no side effects
            self._feedErrorsToResult(result, self._outcome.errors)
        else:  # Python >= 3.11
            result = self._outcome.result
        error = self.list2reason(result.errors)
        failure = self.list2reason(result.failures)
        ok = not error and not failure

        if not ok:
            self.errors = {'errors': error, 'failures': failure}

        self.test_pass = ok

        if ok:
            # Delete temporary folder for passing test
            shutil.rmtree(self.diroutput)

    def list2reason(self, exc_list):
        if exc_list and exc_list[-1][0] is self:
            return exc_list[-1][1]


class TestWithTempFolderWithChdir(TestWithTempFolder):
    """
    Same as TestWithTempFolder but position current directory to the
    created temporary directory.
    """
    def setUp(self):
        self._oldpwd = None
        self._oldpwd = os.getcwd()
        super().setUp()
        os.chdir(self.diroutput)

    def tearDown(self):
        if self._oldpwd is not None:
            os.chdir(self._oldpwd)


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
            f.write('Log for test {} executed on {}\n'.format(type(self), datetime.datetime.now()))

    def tearDown(self):
        """
        Print the contet of the logfile on error output if
        test fails
        """
        super(TestWithTempFolderWithLog, self).tearDown()
        if not self.test_pass:
            logging.error('Test failed for {}. Details are available in {}'.format(type(self), self.diroutput))
            with open(self.logfile, 'r') as f:
                sys.stderr.write('Log folder: {}\n'.format(self.diroutput))
                sys.stderr.write(f.read())

    def run_and_log(self, *args, **kwargs):
        """
        Use subprocess.run and redirect output to the log file
        For python processes, please have a look to export_output.
        """
        import subprocess
        s = subprocess.run(*args, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, **kwargs)
        with open(self.logfile, 'a') as ff:
            ff.write(s.stdout.decode(encoding='utf-8', errors='replace'))
        return s


class TestWithTempFolderWithLogWithChdir(TestWithTempFolderWithLog):
    """
    Same as TestWithTempFolderWithLog but position current directory to the
    created temporary directory.
    """
    def setUp(self):
        self._oldpwd = None
        self._oldpwd = os.getcwd()
        super().setUp()
        os.chdir(self.diroutput)

    def tearDown(self):
        if self._oldpwd is not None:
            os.chdir(self._oldpwd)
