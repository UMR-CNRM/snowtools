# -*- coding: utf-8 -*-
'''
Created on 21  mars 2019
Run a CrocO assimilation sequence on a multinode
@author: cluzetb
'''
import os
import random
import shutil

import numpy as np

from snowtools.tasks.vortex_kitchen import Vortex_conf_file
from snowtools.tools.update_namelist import update_surfex_namelist_file
from snowtools.tasks.vortex_kitchen import vortex_kitchen
from snowtools.utils.ESCROCsubensembles import ESCROC_subensembles
from snowtools.DATA import SNOWTOOLS_DIR


class crocO_vortex_kitchen(vortex_kitchen):
    '''
    Interface between s2m command line and vortex utilities (tasks and mk_jobs)
    crocO_multinode
    based on vortex_kitchen from M. Lafaysse
    '''

    def __init__(self, options, command):
        """
        Constructor
        """
        super().__init__(options, command)

        # check the options consistency
        self.check_options()
        self.prepare_namelist()  # this must be moved to the preprocess step of offline I think.

    def check_options(self):
        if self.options.nnodes > 1 and self.options.nnodes * self.options.ntasks > self.options.nmembers:
            # Adjust the number of nodes if excessive request:
            print('You want to run ' + str(self.options.nmembers) + ' members on ' +
                  str(self.options.nnodes) + ' nodes.')
            self.options.nnodes = self.options.nmembers // self.options.ntasks + 1
            print('but we estimate that  ' + str(self.options.nnodes) + ' nodes are sufficient.')
        elif self.options.nmembers // (self.options.nnodes * self.options.ntasks) >= 1:
            print('WARNING !')
            print('You want to run ' + str(self.options.nmembers) + ' members on ' +
                  str(self.options.nnodes) + ' nodes.')
            print('But you could get quicker results by using up to ' +
                  str(self.options.nmembers // self.options.ntasks + 1) + 'nodes.')

    def prepare_namelist(self):
        self.namelist = self.workingdir + '/OPTIONS_' + self.confcomplement + '.nam'
        shutil.copyfile(self.options.namelist, self.namelist)

        self.enforce_nmembers()

    def enforce_nmembers(self):
        """enforce NENS in the namelist to the presecribed s2m argument value. Mandatory for SODA"""
        # options.namelist is already an absolute path.
        if self.options.nmembers is None:
            raise Exception('please specify the number of members for this run')
        update_surfex_namelist_file(self.options.datedeb, namelistfile=self.namelist, dateend=self.options.datefin,
                                    updateloc=False, nmembers=self.options.nmembers)
