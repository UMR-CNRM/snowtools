# -*- coding: utf-8 -*-

import os

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

TESTBASE_DIR = "/rd/cenfic3/manto/viallonl/testbase"
"""
Path to the testbase on cenfic3
"""

DIRDATAPGD = '/rd/cenfic3/manto/lafaysse/FILES_PGD'
"""
PGD files at CEN
"""

LUSTRE_NOSAVE_DIR = '/cnrm/mrns/users/NO_SAVE'

LUSTRE_NOSAVE_USER_DIR = os.path.join('/cnrm/mrns/users/NO_SAVE', os.getlogin())

CARTOPY_DIR = '/rd/cenfic3/manto/radanovicss/CartopyData'
# CARTOPY_DIR = os.path.join(LUSTRE_NOSAVE_USER_DIR, 'CartopyData')  # for sxcen


