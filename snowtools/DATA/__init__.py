# -*- coding: utf-8 -*-

import os.path

SNOWTOOLS_DATA = os.path.dirname(os.path.abspath(__file__))
"""
Path to this folder (DATA)
"""

SNOWTOOLS_DIR = os.path.dirname(SNOWTOOLS_DATA)
"""
The path to snowtools directory for reference of several resources accessed by the code.
"""

SNOWTOOLS_CEN = os.path.dirname(SNOWTOOLS_DIR)
"""
Path to git repository base (folder in which snowtools folder is present)
"""

TESTBASE = '/rd/cenfic3/manto/viallonl/testbase'
"""
Path to the test base
"""
