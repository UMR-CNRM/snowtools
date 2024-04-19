#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 9 Apr. 2023

@author: Matthieu Lafaysse
"""

import os
import shlex

from snowtools.tasks.s2m_command import Surfex_command as s2m
from snowtools.tests.export_output import exportoutput
from snowtools.tests.tempfolder import TestWithTempFolderWithLog
from snowtools.DATA import DIRDATAPGD


class s2mTest(TestWithTempFolderWithLog):

    def setUp(self):
        super(s2mTest, self).setUp()
        self.commonoptions = " -o " + self.diroutput + " -g"

    def full_run(self, shortcommand):
        command = shortcommand + self.commonoptions
        with exportoutput(self.logfile):
            s2m(shlex.split(command))

    @property
    def runatcen(self):
        return os.path.isdir(DIRDATAPGD) or "DIRDATAPGD" in list(os.environ.keys())
