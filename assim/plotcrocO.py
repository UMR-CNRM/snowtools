#! /usr/bin/python
# -*- coding: utf-8 -*-
'''
Created on 6 févr. 2019

@author: cluzetb
'''
import numpy as np
import matplotlib.pyplot as plt
from utilcrocO import setSubsetclasses
from utilcrocO import colorbar
import os
from matplotlib.cm import ScalarMappable

class Pie(object):
    '''
    class to plot a sdObject into a pie
    '''
    def __init__(self, sdObj, focusCl = None):
        '''
        Constructor
        '''
        if not os.path.exists('../pie/'):
            os.mkdir('../pie/')
        self.sdObj = sdObj
        self.sdObj.load()  # -> it is not always loaded (ex. of synth obs)
        self.sel = np.unique(self.sdObj.pgd.elevClass)
        self.ssl = sdObj.options.classesS
        self.sas = 'all'
        self.rmax = float(np.max(self.sdObj.pgd.elevClass)) + 300.

        self.dictlims ={'b1':[-0.2,0.2],'b2':[-0.2,0.2],'b3':[-0.2,0.2],'b4':[-0.2,0.2],'b5':[-0.2,0.2],'b6':[-0.2,0.2],'b7':[-0.2,0.2],'sd': [-1.,1.]}
        if focusCl is not None:
            if type(focusCl) is int:
                self.focusCl = [focusCl]
            else:
                self.focusCl = focusCl

    def plot(self, ax = None, savefig = False, cmap = 'viridis'):
        N = 8.
        width = 2. * np.pi / N
        cmap = plt.cm.get_cmap(cmap)
        cmap.set_bad('0.7', -999.)
        
        # if 0 and 20 slopes, 2 axes per var : 1 bar for flat, 1 pie for sloped
        if ax is None:
            # fig, ax = plt.subplots(len(self.sdObj.listvar), len(self.ssl), subplot_kw=dict(polar=True))
            fig = plt.figure()
        for i, slope in enumerate(self.ssl):
            
            _, mask = setSubsetclasses(self.sdObj.pgd, self.sel, self.sas, slope, )
            for iax, var in enumerate(self.sdObj.listvar):

                radii = np.ma.masked_array(self.rmax - self.sdObj.pgd.elevClass, mask = np.invert(mask)).compressed()
                theta = np.ma.masked_array(self.sdObj.pgd.aspectClass*np.pi/180.-np.pi/N, mask = np.invert(mask)).compressed()
    
                data = np.ma.masked_invalid(self.sdObj.data[var])
                
                alldata = np.squeeze(np.ma.getdata(data))
                alldata[np.squeeze(np.ma.getmask(data))] = np.nan
                validcolors = np.ma.masked_array(alldata, mask = np.invert(mask)).compressed()
                
                # pie or imshow depending on the slope
                if slope is '0':
                    ax = fig.add_axes([i*(1./len(self.ssl)), 0.25, 1./(len(self.ssl)), 0.25])
                    #gg = ax[iax, 0].imshow(np.expand_dims(validcolors, 0))
                    gg = ax.imshow(np.flipud(np.expand_dims(validcolors, 1)), interpolation  ='None', cmap = cmap, zorder = 0)
                    ax.autoscale(enable = False)
                    ax.set_yticks((self.sel-450.)/300.)
                    ax.set_yticklabels(reversed(map(str, map(int, self.sel))))
                    ax.set_xticks([])
                    ax.set_xticklabels([])
                    if hasattr(self, 'focusCl'):
                        for cl in self.focusCl:
                            if self.sdObj.pgd.aspectClass[cl] <0.01:
                                print 'zbraaaa'
                                print (self.sdObj.pgd.aspectClass[cl]*np.pi/180.- np.pi/N)
                                print  self.sdObj.pgd.elevClass[cl]
                                ax.scatter(1./len(self.sel+1), (self.rmax - self.sdObj.pgd.elevClass[cl]-300)/300.,
                                           color = 'r', zorder = 10, s=6)
                    
                else:  # 20 (or maybe 40)
                    ax = fig.add_axes([i*(1./len(self.ssl)), 0., 1./len(self.ssl), 1.], polar = True)
                    # ax[iax, len(self.ssl) - 1].set_theta_zero_location('N')
                    # ax[iax, len(self.ssl) - 1].set_theta_direction(-1)
                    #bars = ax[iax, len(self.ssl) - 1].bar(theta, radii, width = width, bottom=0.0, zorder = 0)
                    ax.set_theta_zero_location('N')
                    ax.set_theta_direction(-1)
                    bars = ax.bar(theta, radii, width = width, bottom=0.0, zorder = 0)
                
                    for i, b in enumerate(bars):
                        if np.isnan(validcolors[i]):
                            b.set_facecolor('0.7')
                        else:
                            b.set_facecolor(cmap(validcolors[i]))
                   
                    # if the constructor is provided a focus Cl, print x in it
                    if hasattr(self, 'focusCl'):
                        for cl in self.focusCl:
                            if self.sdObj.pgd.aspectClass[cl] >0.01:  # filter out flat classes
                                print 'ok'
                                print (self.sdObj.pgd.aspectClass[cl]*np.pi/180.- np.pi/N)
                                print  self.sdObj.pgd.elevClass[cl]
                                #ax[iax, len(self.ssl) - 1].scatter(self.sdObj.pgd.aspectClass[cl]*np.pi/180., self.rmax - self.sdObj.pgd.elevClass[cl]-150.,
                                #                                   marker = '.', color = 'r', zorder = 10, linewidths=3)
                                ax.scatter(self.sdObj.pgd.aspectClass[cl]*np.pi/180., self.rmax - self.sdObj.pgd.elevClass[cl]-150.,
                                                                   marker = '.', color = 'r', zorder = 10, linewidths=3)
                    ax.set_xticklabels(['N', 'NE','E', 'SE', 'S', 'SW', 'W', 'NW'])
                    ax.set_yticks(self.sel-450.)
                    # ax[iax].set_yticklabels(reversed(map(str, map(int, self.sel))))
                    ax.set_yticklabels([])
        
                    #ax[-1, -1].set_xlabel(self.sdObj.ptinom)
                    lims = self.dictlims[var]
                    sm = ScalarMappable(cmap=cmap, norm=plt.Normalize(lims))
                    sm.set_array([])
        
                    #cbar = plt.colorbar(sm, ax = ax[iax, len(self.ssl) - 1])
                    cbar = plt.colorbar(sm, ax = ax)
                    cbar.set_label('Color', rotation=270, labelpad=25)
        # cbar = colorbar(ax[-1])
        if savefig:
            fig.savefig("../pie/pie_" + self.sdObj.ptinom + '_' + var + '_' + self.sdObj.date + '.png')
            fig.close()
                
            
        
        
        