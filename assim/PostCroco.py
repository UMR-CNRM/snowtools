#! /usr/bin/python
# -*- coding: utf-8 -*-
'''
Created on 11 févr. 2019

@author: cluzetb
'''
import os
from SemiDistributed import PrepBg, PrepAbs, Synthetic, Obs
from plotcrocO import Pie
from Ensemble import PrepEnsBg, PrepEnsAn
from Operators import PrepEnsOperator
import matplotlib.pyplot as plt
from utilcrocO import setSubsetclasses
from utilcrocO import niceName
from utilcrocO import Pgd
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
                
                    plt.savefig('../pie/corr_' + str(int(pb.ens[1].pgd.elev[cl])) + '_' + str(int(pb.ens[1].pgd.aspect[cl])) +
                                '_' + str(int(np.arctan(pb.ens[1].pgd.slope[cl]) * 180. / np.pi)) + '_' + dd + '.png')
                os.chdir('..')

        if self.options.todo=='analysis':
            if not self.options.distr: # case semi-distributed simulation
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
            
            if self.options.distr: # case semi-distributed simulation
                
                for dd in self.options.dates:
                    os.chdir(dd)
                    
                    # Read the bg
                    
                    BG = PrepEnsBg(self.options, dd)
                    BG.stackit()
                    
                    # Read the analysis
                    AN = PrepEnsAn(self.options, dd)
                    AN.stackit()
                    
                    # Read the obs
                    if (self.options.synth >= 0):
                        OBS = Synthetic(self.xpdir, dd, self.options)
                        OBS.load()
                    else:
                        OBS = Obs(self.xpdir, dd, self.options)
                        
                    stepZ = 100
                    minZ = np.floor(np.min(OBS.pgd.elev)/stepZ)*stepZ
                    maxZ = np.ceil(np.max(OBS.pgd.elev)/stepZ)*stepZ
                    binZ = np.arange( minZ, maxZ, stepZ)
                    
                    BG_mean = np.zeros((len(binZ),BG.nmembers))
                    AN_mean = np.zeros((len(binZ),BG.nmembers))
                    OBS_mean = np.zeros((len(binZ)))
                    
                    # extracts the mean OBS, BG and AN per elevation band 
                    for idx,bin_Z_lower in enumerate(binZ):
                            # missing a filter on no data ! which is set to min(elev) in PGD.elev I think
                            f = np.where( (OBS.pgd.elev > bin_Z_lower) & (OBS.pgd.elev < bin_Z_lower+stepZ) )
                            tmp = OBS.data['sd'][0,f[0]]
                            OBS_mean[idx] = np.mean(tmp)
                            for mb in range(1, BG.nmembers + 1):
                                tmp = BG.stack['sd'][mb-1,f]
                                BG_mean[idx,mb-1] = np.mean(tmp)
                                
                                tmp = AN.stack['sd'][mb-1,f]
                                AN_mean[idx,mb-1] = np.mean(tmp)
                    
                    plt.figure()
                    plt.plot(BG_mean,color=(0.4,0.4,0.4),alpha=0.5)
                    plt.plot(AN_mean,'b')
                    plt.plot(OBS_mean,'g',linewidth=4.0)
                    plt.savefig('../var_elev.png')
                    plt.close()

