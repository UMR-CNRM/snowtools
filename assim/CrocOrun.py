# -*- coding: utf-8 -*-
'''
Created on 5 févr. 2019

@author: cluzetb, inspired on SodaXP, test_PF.py and snowtools_git/tasks/runs.py from Lafaysse
'''
import os
from SemiDistributed import Synthetic, Real, PrepBg, PrepAbs
import datetime
import shutil
from utilcrocO import convertdate
from utilcrocO import setlistvars_obs, setlistvars_var, set_errors, set_factors,\
    setSubsetclasses
from bronx.datagrip.namelist import NamelistParser
from plotcrocO import Pie
from Ensemble import PrepEnsBg, PrepEnsAn
import matplotlib.pyplot as plt
from Operators import PrepEnsOperator
from PostCroco import PostCroco
import numpy as np

class CrocOrun(object):
    '''
    Class for local soda test
    '''

    def __init__(self, options, conf):

        self.options = options
        self.options.dates = [self.options.dates]
        self.conf = conf
        self.xpiddir =  self.options.vortexpath + '/s2m/' + self.options.vconf + '/' + self.options.xpid + '/'
        self.xpidobsdir = self.options.vortexpath + '/s2m/' + self.options.vconf + '/obs/' + self.options.sensor + '/'
        self.crocodir = self.xpiddir + 'crocO/'
        if type(self.conf.assimdates) is unicode:
            self.conf.assimdates = [str(self.conf.assimdates)]
        else:
            self.conf.assimdates = map(str, self.conf.assimdates)
        self.mblist = ['mb{0:04d}'.format(mb) for mb in range(1, 36)]
        
        if self.options.mpi is True: # for cesar, change here the day you want to test mpi
            self.exesurfex = '/home/cluzetb/SURFEX_V81/cen_release/exe_mpi/'
        else:
            self.exesurfex = os.environ['EXESURFEX']
        
        # setup all dirs
        self.setup()
        
    def setup(self):
        if not os.path.exists(self.crocodir):
            os.mkdir(self.crocodir)
        os.chdir(self.crocodir)
        if not os.path.exists(self.options.saverep):
            os.mkdir(self.options.saverep)
        os.chdir(self.options.saverep)
        for dd in self.options.dates:
            if dd in self.conf.assimdates:
                if os.path.exists(dd):
                    shutil.rmtree(dd)
                os.mkdir(dd)
                self.prepare_sodaenv(dd)
            else:
                print 'prescribed date ' + dd + 'does not exist in the experiment, remove it.'
                print self.conf.assimdates
                self.options.dates.remove(dd)

    def prepare_sodaenv(self, path):
        """
        set soda environment for each date (=path): -PGD, links to preps, namelist, ecoclimap etc.
        """
        
        os.chdir(path)
        # Prepare the PGD and PREP for assim
        dateAssSoda = convertdate(path).strftime('%y%m%dH%H')
        for imb, mb in enumerate(self.mblist):
            if not os.path.exists('PREP_' + path + '_PF_ENS' + str(imb + 1) + '.nc'):
                os.symlink(self.xpiddir + mb + '/bg/PREP_' + path + '.nc', 'PREP_' + dateAssSoda + '_PF_ENS' + str(imb + 1) + '.nc')
        if not os.path.exists('PREP.nc'):
            os.symlink('PREP_' + dateAssSoda + '_PF_ENS1.nc', 'PREP.nc')
        if not os.path.exists('PGD.nc'):
            os.symlink(self.options.vortexpath + '/s2m/' + self.options.vconf + '/spinup/pgd/PGD_' + self.options.vconf + '.nc', 'PGD.nc')

        # Prepare and check the namelist (LWRITE_TOPO must be false for SODA)
        if not os.path.exists('OPTIONS.nam'):
            shutil.copyfile(self.xpiddir + 'conf/namelist.surfex.foo', 'OPTIONS_base.nam')
        self.check_namelist_soda(self.options)

        # prepare ecoclimap binaries
        if not os.path.exists('ecoclimapI_covers_param.bin'):
            os.symlink(self.exesurfex + '/../MY_RUN/ECOCLIMAP/ecoclimapI_covers_param.bin', 'ecoclimapI_covers_param.bin')
            os.symlink(self.exesurfex + '/../MY_RUN/ECOCLIMAP/ecoclimapII_eu_covers_param.bin', 'ecoclimapII_eu_covers_param.bin')
            # flanner stuff
            os.symlink(self.exesurfex + '/../MY_RUN//DATA/CROCUS/drdt_bst_fit_60.nc', 'drdt_bst_fit_60.nc')
        if not os.path.exists('soda.exe'):
            os.symlink(self.exesurfex + '/SODA', 'soda.exe')

        # prepare (get or fake) the obs
        self.prepare_obs(path)
        os.chdir('..')

    def check_namelist_soda(self, options):
        """
        - check consistency between the prescribed assim vars and what is written in the namelist
        - check and change LWRITE_TOPO to .FALSE.
        """
        pass
        n = NamelistParser()
        N = n.parse('OPTIONS_base.nam')
        # LWRITE_TOPO must be false if we want ISBA_ANALYSIS.out to write
        print('force LWRITE_TOPO to .FALSE.')
        N['NAM_IO_OFFLINE'].LWRITE_TOPO = False
        
        # update assim vars in the namelist
        # NAM_ENS
        N['NAM_ENS'].NENS = options.nmembers
        
        # NAM_OBS
        sodaobs = setlistvars_obs(options.vars)
        if 'NAM_OBS' not in N.keys():
            N.newblock('NAM_OBS')
        if 'NAM_VAR' not in N.keys():
            N.newblock('NAM_VAR')
        if 'NAM_ASSIM' not in N.keys():
            N.newblock('NAM_ASSIM')
            
        N['NAM_OBS'].NOBSTYPE = np.size(sodaobs)
        N['NAM_OBS'].COBS_M = sodaobs
        N['NAM_OBS'].XERROBS_M = set_errors(sodaobs)
        N['NAM_OBS'].XERROBS_FACTOR_M = set_factors(sodaobs, options.fact)
        N['NAM_OBS'].NNCO = [1] * len(sodaobs)
        N['NAM_OBS'].CFILE_FORMAT_OBS = "NC    "

        # NAM_VAR
        sodavar = setlistvars_var(options.vars)
        N['NAM_VAR'].NVAR = N['NAM_OBS'].NOBSTYPE
        N['NAM_VAR'].CVAR_M = sodavar
        N['NAM_VAR'].NNCV = N['NAM_OBS'].NNCO
        
        # NAM_ASSIM
        N['NAM_ASSIM'].LSEMIDISTR_CROCUS = not(options.sodadistr)
        N['NAM_ASSIM'].LASSIM = True
        N['NAM_ASSIM'].CASSIM_ISBA = 'PF   '
        
        N['NAM_ASSIM'].LEXTRAP_SEA = False
        N['NAM_ASSIM'].LEXTRAP_WATER = False
        N['NAM_ASSIM'].LEXTRAP_NATURE = False
        N['NAM_ASSIM'].LWATERTG2 = True
        
        namSURFEX = open('OPTIONS.nam', 'w')
        namSURFEX.write(N.dumps())
        namSURFEX.close()
        
    def prepare_obs(self, date):
        """
        
        """
        if self.options.synth is not None:
            # synthetic obs is generated from mbsynth at time date
            self.obs = Synthetic(self.xpiddir, date, self.options)
        else:
            # real obs are obtained in xpidobsdir
            self.obs = Real(self.xpidobsdir, date, self.options)
        self.obs.prepare()
        
    def run(self):
        """spawn soda in each date repertory"""
        os.system('ulimit -s unlimited')
        for dd in self.options.dates:
            os.chdir(dd)
            os.system('./soda.exe')
            os.chdir('..')
            
            
    def post_proc(self, options):
        postp = PostCroco(self.xpiddir, self.xpidobsdir, options)
        postp.run()

