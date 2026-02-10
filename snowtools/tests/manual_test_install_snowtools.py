# -*- coding: utf-8 -*-

import unittest
import subprocess
import shlex
import shutil
import os
import sys
sys.path.append(os.path.dirname(os.path.dirname(os.path.realpath(__file__))))

from DATA import SNOWTOOLS_CEN
from DATA import TESTBASE_DIR

if not os.path.exists(TESTBASE_DIR):
    sys.exit('The "TESTBASE_DIR" environment variable is not properly defined.\n' +
             'Set TESTBASE_DIR to valid path before launching this script again.')


class install_snowtools(unittest.TestCase):

    def install_cmd(self, cmd, assertFail=False):
        if assertFail:
            with self.assertRaises(subprocess.CalledProcessError):
                subprocess.run(shlex.split(cmd, ' '), check=True)
        else:
            subprocess.run(shlex.split(cmd, ' '), check=True)

    def test_without_venv(self):
        # WARNING : This test will crash if it is called from within a virtual environment
        # --> Don't worry, this is the exepected behavior !
        self.install_cmd(f"python3 {SNOWTOOLS_CEN}/cenutils/install_snowtools.py", assertFail=True)

    def test_editable_install(self):
        # WARNING : This test will crash on HPC with python 3.7.6 because editable installs are not possible with this
        # python version
        self.install_cmd(f"python3 {SNOWTOOLS_CEN}/cenutils/install_snowtools.py -v {TESTBASE_DIR}/venv/editable -e")

    def test_non_editable_install(self):
        self.install_cmd(f"python3 {SNOWTOOLS_CEN}/cenutils/install_snowtools.py -v {TESTBASE_DIR}/venv/non_editable")

    def test_non_editable_install_all(self):
        self.install_cmd(f"python3 {SNOWTOOLS_CEN}/cenutils/install_snowtools.py " +
                f"-v {TESTBASE_DIR}/venv/non_editable_all -o all")

    def test_non_editable_install_plot_sql(self):
        self.install_cmd(f"python3 {SNOWTOOLS_CEN}/cenutils/install_snowtools.py " +
            f"-v {TESTBASE_DIR}/venv/non_editable_plot_sql -o plot sql")

    @classmethod
    def tearDownClass(cls):
        shutil.rmtree(f"{TESTBASE_DIR}/venv")


if __name__ == "__main__":
    unittest.main()
