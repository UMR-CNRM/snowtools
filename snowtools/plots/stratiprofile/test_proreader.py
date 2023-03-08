#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import unittest
import os.path

from snowtools.plots.stratiprofile import proreader
from snowtools.DATA import TESTBASE_DIR

TEST_FILENAME = os.path.join(TESTBASE_DIR, "PRO", "PRO_gdesRousses_2019-2020.nc")


@unittest.skipIf(not os.path.isfile(TEST_FILENAME), "Input test file not available")
class TestProreader(unittest.TestCase):
    def setUp(self):
        self.ff = proreader.read_file(TEST_FILENAME)

    def test1(self):
        assert 'SNOWDZ' in self.ff.variables, 'Check variable existence'
        fn = self.ff.get_filename(convert_str=True)
        assert isinstance(fn, str), 'Filename is not a string'
        assert len(fn) > 0, 'Filename is void'
        desc = self.ff.variable_desc('SNOWDZ')
        assert isinstance(desc, dict), 'No variable description provided'
        assert 'full_name' in desc, 'No full_name available in variable description'
        desc2 = self.ff.variable_desc(desc['full_name'])
        assert desc2 == desc, 'Variable description does not match with long and short name'
        desc2 = self.ff.variable_desc('dz')
        assert desc2 == desc, 'Variable description does not match with standard name and alias'
