#! /usr/bin/python
# -*- coding: utf-8 -*-
'''
Created on 11 févr. 2019

@author: cluzetb
'''
import os
from SemiDistributed import PrepBg, PrepAbs, Synthetic
from plotcrocO import Pie
from Ensemble import PrepEnsBg, PrepEnsAn
from Operators import PrepEnsOperator
import matplotlib.pyplot as plt
from utilcrocO import setSubsetclasses
from Code.Dev.evalSODA.util import niceName
import numpy as np

class PostCroco(object):
    '''
    post processing class
    '''


    def __init__(self, xpdir, options):
        '''
        
        '''
        self.options = options
        self.xpdir = xpdir
    def run(self):
        if self.options.todo=='pievar':
            for dd in self.options.dates:
                os.chdir(dd)
                # for mb in range(1, self.options.nmembers + 1)
                for mb in [5]:
                    p = PrepBg(dd, mb, self.options)
                    pie = Pie(p)
                    pie.plot()
                os.chdir('..')
        # ((CORRELATION PLOT (only works with 1 class)))
        if self.options.todo =='corr':
            for dd in self.options.dates:
                os.chdir(dd)
                pb = PrepEnsBg(self.options, dd)
                # pa = PrepEnsAn(self.options, dd)
                obs = Synthetic(self.xpdir, dd, self.options)  # load pre-existing Synthetic, don't generate it.
                op = PrepEnsOperator(pb, sdObj=obs)
                corr = op.m21diff(reverse=True)
                subset, mask = setSubsetclasses(pb.ens[1].pgd, self.options.classesE, self.options.classesA, self.options.classesS)
                for cl in subset:
                    print('printing in', cl)
                    corSd = op.covariance(cl)
                    piecor = Pie(corSd, focusCl=cl,)
                    piecor.plot( cmap = 'hot_r')
                
                    plt.savefig('../pie/corr_' + str(int(pb.ens[1].pgd.elevClass[cl])) + '_' + str(int(pb.ens[1].pgd.aspectClass[cl])) +
                                '_' + str(int(np.arctan(pb.ens[1].pgd.slopeClass[cl]) * 180. / np.pi)) + '_' + dd + '.png')
                os.chdir('..')

        if self.options.todo=='analysis':
            for dd in self.options.dates:
                os.chdir(dd)
                # pieplot bg
                pb = PrepEnsBg(self.options, dd)
                pb.median(ptinombase = 'bg')
                
                # pieplot an
                pa = PrepEnsAn(self.options, dd)
                pa.median(ptinombase = 'an')
                
                obs = Synthetic(self.xpdir, dd, self.options)
                
                fig, ax = plt.subplots(len(pb.median.data.keys()), 3, subplot_kw=dict(polar=True))
                piemedb =  Pie(pb.median)
                
                piemedb.plot(ax = ax[:,0])

                piemeda =  Pie(pa.median)
                piemeda.plot(ax = ax[:,2])
                
                # pieplot obs
                pieobs = Pie(obs)
                pieobs.plot(ax = ax[:,1])
                for i, a in enumerate(ax[:,0]):
                    a.set_ylabel([pb.median.data.keys()[i]])
                
                plt.savefig('../pie/analysis.png')
                #######################""
                fig2, ax2 = plt.subplots(len(pb.median.data.keys()), 3, subplot_kw=dict(polar=True))
                innov = PrepAbs(dd, self.options, ptinom='innov')
                incr = PrepAbs(dd, self.options, ptinom='incr')
                res = PrepAbs(dd, self.options, ptinom='res')
                innov.data = dict()
                incr.data = dict()
                res.data = dict()
                
                for k in pb.median.data.keys():
                    innov.data[k] = obs.data[k] - pb.median.data[k]
                    
                    incr.data[k] = pa.median.data[k] - pb.median.data[k]
                    res.data[k] = obs.data[k] - pa.median.data[k]
                
                
                innov.isloaded = True
                incr.isloaded = True
                res.isloaded = True
                piemedinnov = Pie(innov)
                
                piemedinnov.plot(ax = ax2[:,0], cmap = 'RdBu')

                piemedincr =  Pie(incr)
                piemedincr.plot(ax = ax2[:,1], cmap = 'RdBu')
                
                # pieplot obs
                piemedres = Pie(res)
                piemedres.plot(ax = ax2[:,2], cmap = 'RdBu')
                

                
                for i, a in enumerate(ax[:,0]):
                    a.set_ylabel([pb.median.data.keys()[i]])
                
                plt.savefig('../pie/deriv.png')
                plt.close()

