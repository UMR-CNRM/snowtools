#! /usr/bin/python
# -*- coding: utf-8 -*-
'''
Created on 5 févr. 2019

@author: cluzetb
Module for preparing/faking/ observations within crocO framework

'''
import os
import sys
import shutil
from Code.Dev.evalSODA.util import Pgd, convertdate
from utilcrocO import setlistvars_obs, setlistvars_var, setSubsetclasses,\
    dictvarsPrep, dictvarsWrite
import netCDF4
import numpy as np
import random

class SemiDistributed(object):
    '''
    class for semi-distributed files (obs or PREP) bound to a geometry described by a pgd in current crocOdir
    '''
    _abstract = True
    
    def __init__(self, pgdPath = 'PGD.nc'):
        self.pgd = Pgd(pgdPath)
        self.isloaded = False

    def load(self):
        if not self.isloaded:
            dd = netCDF4.Dataset(self.sodaName)
            self.data = dict()
            for var in self.listvar:
                self.data[var] = dd.variables[self.loadDict[var]][:]
            dd.close()
            self.isloaded = True


class Obs(SemiDistributed):
    '''
    class attached to an observation file, either synthetical or real

    '''
    _abstract = True

    def __init__(self, date, options):
        SemiDistributed.__init__(self)
        self.options = options
        self.sodaName = 'OBSERVATIONS_' + convertdate(date).strftime('%y%m%dH%H') + '.nc'
        
        # set list of vars
        # self.listvar = setlistvars_obs(options.vars)
        self.listvar = options.vars
        # self.listvars_write = setlistvars_obs(options.vars)
        
    def prepare(self):
        # if not os.path.exists(self.sodaName):
        #    shutil.copyfile(self.path, self.sodaName)

        self.load_raw()
        self.check_format()
        self.create_new(self.options)
        self.close()

    def load_raw(self):
        self.Raw = netCDF4.Dataset(self.path, 'r')
    
    def check_format(self):
        pass
    
    def create_new(self, options):
        self.New = netCDF4.Dataset(self.sodaName, 'w')
        _, mask = self.subset_classes(self.pgd, options)
        self.copydimsvars(self.Raw, self.New, self.listvar, mask=mask)
        self.computeratio(self.Raw, self.New, self.listvar, mask=mask)

    def subset_classes(self, pgd, options):
        """
        in the SODA copy of the observation file, remove classes where the assim shouldn't be performed
        """
        if options.distr is True:
            mask = None
        else:
            subset, mask = setSubsetclasses(pgd, options.classesE, options.classesA, options.classesS)
        return subset, mask

    def close(self):
        self.Raw.close()
        self.New.close()


class Synthetic(Obs):
    """
    Class describing synthetic obs
    """  
    def __init__(self, xpdir, date, options, nmembers = 35):
        '''
        Constructor
        '''
        Obs.__init__(self, date, options)
        
        self.date = date
        if options.synth > 0:
            self.path = xpdir + 'mb{0:04d}'.format(options.synth) + '/bg/PREP_' + date + '.nc'
            self.ptinom = 'synth' + 'mb{0:04d}'.format(options.synth)
        else:
            # randomly draw a member
            draw = random.choice(range(1, options.nmembers+1))
            self.path = xpdir + 'mb{0:04d}'.format(draw) + '/bg/PREP_' + date + '.nc'
            self.ptinom = 'synth' + 'mb{0:04d}'.format(draw)
        if 'sd' not in self.listvar:
            self.listvar.append('sd')  # add total SD
        self.dictVarsRead = dictvarsPrep()
        self.dictVarsWrite = dictvarsWrite()
        self.loadDict = dictvarsWrite()
        self.isloaded = False
        
    def copydimsvars(self, Raw, New, nameVars, mask = None):
        '''
        BC 5/02/19 from maskSentinel2.py
        copy nameVars from Raw to New netCDF Datasets (and copy dims before)
        /!\ only for vars that already exist in Raw
        
        '''
        for dimName, dim in Raw.dimensions.iteritems():
            New.createDimension(dimName, len(dim) if not dim.isunlimited() else None)
        for name in nameVars: # name in arg format (b*, r**...)
            # print name
            # print Raw.variables.keys()
            prepName = self.dictVarsRead[name]
            writeName = self.dictVarsWrite[name]
            if prepName in Raw.variables.keys():  # if var exists in Raw (and if it is not a ratio)
                var = Raw.variables[prepName]
                tmp = New.createVariable(writeName, 'float', var.dimensions)
                # copy attributes
                tmp.setncatts({k: var.getncattr(k) for k in var.ncattrs()})
                # for k in var.ncattrs():
                #     print k
                # raise Exception
                if mask is None:
                    tmp[:] = var[:]
                else:
                    print np.shape(tmp)
                    print np.shape(mask)
                    tmp[0, mask] = var[0, mask] * (0.05 + np.random.rand(np.shape(var[0, mask])[0]))
                    print np.shape(np.invert(np.squeeze(mask)))
                    tmp[0, np.invert(np.squeeze(mask))] = np.nan  # I'd rather set it to fillValue but it is not understood by SODA as far as I know
        
    def computeratio(self, Raw, New, nameVars, mask = None):
        listrat = [r for r in nameVars if 'R' in r]
        
        for r in listrat:
            varnum = Raw.variables['B' + r[1]]
            varden = Raw.variables['B' + r[2]]
            tmp = New.createVariable(r, 'float', varnum.dimensions)
            if mask is None:
                tmp[:] = varnum[:] / varden[:]
            else:
                tmp[mask] = varnum[mask] / varden[mask]
                tmp[np.invert(mask)] = np.nan

class Real(Obs):
    """
    Class describing real obs
    TODO : pursue implementation
    """
    def __init__(self, xpidobsdir, date, options):
        '''
        Constructor
        '''
        Obs.__init__(self, date, options)
        self.path = xpidobsdir + 'obs_MODIS_grandes_rousses_' + date + '.nc'
    
class Prep(SemiDistributed):
    """
    class describing Prep (background) and/or analysis 
    """
    _abstract = True
    def __init__(self, options):
        SemiDistributed.__init__(self)
        self.options = options
        self.listvar = options.vars
        if 'sd' not in self.listvar:
            self.listvar.append('sd')  # add total SD
        self.listvar_soda = setlistvars_var(options.vars)
        self.dictVarsRead = dictvarsPrep()
        self.dictVarsWrite = dictvarsWrite()
        self.loadDict = dictvarsPrep()
class PrepBg(Prep):
    def __init__(self, date, mbid, options):
        Prep.__init__(self, options)
        self.date = date
        self.ptinom = 'bg' + str(mbid)
        self.sodaName = 'PREP_' + convertdate(date).strftime('%y%m%dH%H') + '_PF_ENS' + str(mbid) + '.nc'
        
        
class PrepAn(Prep):
    def __init__(self, date, mbid, options):
        Prep.__init__(self, options)
        self.date = date
        self.ptinom = 'an' + str(mbid)
        self.sodaName = 'SURFOUT' + str(mbid) + '.nc'

class PrepAbs(Prep):
    '''
    class describing abstract prep-like objects ( computed from prep, not bound to a specific file)
    e.g. medians, covariance etc.
    '''
    def __init__(self, date, options, ptinom):
        Prep.__init__(self, options)
        self.date = date
        self.ptinom = ptinom
    
    
    
    
    
    
    
    
    
    
    
    
