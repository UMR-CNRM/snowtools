# -*- coding: utf-8 -*-

import unittest
import os
import shlex
import subprocess

from snowtools.utils import xarray_snowtools  # noqa: F401
from snowtools.DATA import SNOWTOOLS_CEN


class Test_vortex_extractor(unittest.TestCase):

    @classmethod
    def setUpClass(self):
        self.basecmd = f'python {SNOWTOOLS_CEN}/vortex_cen/scripts/get_simulation_output.py --checkonly'
        self.s2m_reanalysis_conf = 'S2MReanalysis.ini'
        self.test_no_section_conf = os.path.join(SNOWTOOLS_CEN, 'snowtools', 'tests', 'conf',
                'vortex_extractor_no_section.ini')
        self.test_one_section_conf = os.path.join(SNOWTOOLS_CEN, 'snowtools', 'tests', 'conf',
                'vortex_extractor_one_section.ini')
        self.test_ensemble_conf = os.path.join(SNOWTOOLS_CEN, 'snowtools', 'tests', 'conf',
                'vortex_extractor_ensemble.ini')
        self.test_default_block_pro_files = os.path.join(SNOWTOOLS_CEN, 'snowtools', 'tests', 'conf',
                'vortex_extractor_default_pro_files_with_no_block.ini')

    def launch_cmd(self, cmd, assertFail=False):
        if assertFail:
            with self.assertRaises(subprocess.CalledProcessError):
                subprocess.run(shlex.split(cmd, ' '), check=True)
        else:
            subprocess.run(shlex.split(cmd, ' '), check=True)

    def test_read_section(self):
        cmd = f"{self.basecmd} --configfile={self.s2m_reanalysis_conf} --configsection=SafranFlatReanalysis"
        self.launch_cmd(cmd)

    def test_cmd_line_arguments(self):
        cmd = f"{self.basecmd} --configfile={self.s2m_reanalysis_conf} --configsection=SurfexAllslopesReanalysis " \
            "-b 2024080106 -e 2025080106 -g alp27_allslopes --verbose"
        self.launch_cmd(cmd)

    def test_no_section_in_multi_section_conf_file(self):
        cmd = f"{self.basecmd} --configfile={self.s2m_reanalysis_conf}"
        self.launch_cmd(cmd, assertFail=True)

    def test_no_section_in_single_section_conf_file(self):
        cmd = f"{self.basecmd} --configfile={self.test_one_section_conf}"
        self.launch_cmd(cmd)

    def test_no_section_in_conf_file_without_sections(self):
        cmd = f"{self.basecmd} --configfile={self.test_no_section_conf}"
        self.launch_cmd(cmd)

    def test_block_pro_files_default(self):
        cmd = f"{self.basecmd} --configfile={self.test_default_block_pro_files}"
        self.launch_cmd(cmd)

    def test_ensemble(self):
        cmd = f"{self.basecmd} --configfile={self.test_ensemble_conf}"
        self.launch_cmd(cmd)


if __name__ == "__main__":
    unittest.main()
