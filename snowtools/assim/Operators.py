# -*- coding: utf-8 -*-
'''
Created on 8 févr. 2019

@author: cluzetb
'''
import numpy as np

from snowtools.assim.Ensemble import PrepEnsAbs
from snowtools.assim.SemiDistributed import PrepAbs

class SdOperator(object):
    '''
    basic class for operations on SemiDistributed objects
    either from real or abstract ones
    building generally abstract ones
    '''

    _abstract = True

    def __init__(self):
        '''
        Constructor
        '''


class EnsOperator(object):
    '''
    basic class for operations on Ensemble objects

    '''
    _abstract = True

    def __init__(self):
        '''
        Constructor
        '''
        
        
class PrepEnsOperator(EnsOperator):
    '''
    basic class for operations on PrepEns objects
    '''
    def __init__(self, ens1, ens2=None, sdObj = None):
        '''
        Constructor
        '''
        EnsOperator.__init__(self)
        self.ens1 = ens1
        if ens2 is not None:
            self.ens2 = ens2
        if sdObj is not None:
            self.sdObj = sdObj
            
    def m2mdiff(self):
        # first, stack ensembles (more natural for computations)
        self.ens1.stackit()
        self.ens2.stackit()
        diff = PrepEnsAbs(self.ens1.options, self.ens1.dates)
        diff.listvar = self.ens1.listvar
        for var in self.diff:
            diff.stack[var] = self.ens1.stack[var] - self.ens2.stack[var]
    
    def m21diff(self, reverse = False):
        """
        compute ens - sdObj (reverse for innovations) and put result into ens2
        """
        fact = 1
        if reverse:
            fact = -1
            
        self.ens1.stackit()
        self.sdObj.load()
        self.ens2 = PrepEnsAbs(self.ens1.options, self.ens1.date)
        self.ens2.listvar = self.ens1.listvar
        for var in self.ens2.listvar:
            self.ens2.stack[var] = reverse * (self.ens1.stack[var] - self.sdObj.data[var])
            
        self.ens2.isloaded = True
        self.ens2.isstacked = True
        return self.ens2
    def covariance(self, clA=None):
        '''
        compute covariance between ens1 and ens2 ensemble of preps.
        if clA, restrict it to covariance between ens1 in clA and ens2 in all classes (then it is plottable)
        '''
        # first, stack ensembles (more natural for computations)
        self.ens1.stackit()
        self.ens2.stackit()
        if clA is None:
            pass
        else:
            corrAB_cl = PrepAbs(self.ens1.date, self.ens1.options, ptinom='corr_')
            corrAB_cl.data = dict()
            for var in self.ens1.listvar:
                corrAB_cl.data[var] = np.empty((np.shape(self.ens2.stack[var])[1]))
                for cl in range(np.shape(self.ens2.stack[var])[1]):
                    gg = np.corrcoef(self.ens1.stack[var][:, clA], self.ens2.stack[var][:, cl])
                    # print np.shape(gg)
                    # print gg
                    # print np.shape(corrAB_cl.data[var][cl])
                    #print cl
                    #print self.ens1.stack[var][:, clA]
                    #print self.ens2.stack[var][:, cl]
                    corrAB_cl.data[var][cl] = np.corrcoef(self.ens1.stack[var][:, clA], self.ens2.stack[var][:, cl])[1, 0]
                    
            corrAB_cl.isloaded = True
            return corrAB_cl
                    
