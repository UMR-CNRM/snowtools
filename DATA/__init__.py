# -*- coding: utf-8 -*-

import os.path

SNOWTOOLS_DIR = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
"""
The path to snowtools directory for reference of several resources accessed by the code.
"""

SNOWTOOLS_CEN = os.path.dirname(SNOWTOOLS_DIR)
"""
Path to git repository base (foler in which snowtools folder is present)
"""
