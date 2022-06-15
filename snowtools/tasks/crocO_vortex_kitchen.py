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

    def __init__(self, options):
        """
        Constructor
        """
        super().__init__(options)

        # check the options consistency
        self.check_options()
        self.prepare_namelist()  # this must be moved to the preprocess step of offline I think.

    def check_options(self):
        if self.options.openloop:
            if (self.options.synth is not None) or (self.options.real is True):
                print('''
                --openloop, --synth <mbid> and --real are exclusive arguments.
                so please chose one of them only.
                      '''
                      )
                raise Exception
        elif (self.options.synth is None and self.options.real is False):
            raise Exception('''
            Either you\'re running an assimilation experiment with (1) real data or (2)
            with synthetic from previous openloop
            (1) specify --real to s2m
            (2) use --synth <mbid> (starting from 1)
                to specify which member is the synthetic one in order to remove and replace it.
                 ''')
        if self.options.nnodes * self.options.ntasks > self.options.nmembers:
            print(' be careful, you are trying to run ' + str(self.options.nmembers) + ' members on ' +
                  str(self.options.nnodes * self.options.ntasks) + ' cores (40 cores per nodes)' +
                  ' please reduce --nnodes so that n_cores<=nmembers')

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

    def replace_member(self, allmembers, members_id):
        # warning in case of misspecification of --synth
        print('\n\n\n')
        print('************* CAUTION ****************')
        print('Please check that the --synth argument')
        print('corresponds to the openloop member    ')
        print('used to generate the observations     ')
        print('otherwise this would artificially     ')
        print('generate excellent results            ')
        print('by letting the truth member to stay   ')
        print('in the assimilation experiment        ')
        print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
        print('\n\n\n')
        # workaround to know the size of the ensemble
        sizeE1 = ESCROC_subensembles(self.options.escroc, allmembers, randomDraw = True).size
        # draw a member, excluding any ESCROC member already present in the ensemble.
        members_id[self.options.synth - 1] = np.random.choice([e for e in range(1, sizeE1 + 1) if e not in members_id])
        return members_id

    def draw_meteo(self, confObj):
        meteo_members = {str(m): ((m - 1) % int(self.options.nforcing)) + 1 for m in range(self.options.nmembers)}
        if hasattr(confObj, 'meteo_draw'):
            meteo_draw = confObj.meteo_draw
        else:
            meteo_draw = meteo_members[str(self.options.synth)]
        while meteo_draw == meteo_members[str(self.options.synth)]:
            meteo_draw = random.choice(list(range(1, int(self.options.nforcing) + 1)))
        print('mto draw', meteo_draw)
        return meteo_draw

