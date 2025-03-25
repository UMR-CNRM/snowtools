#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 18 march 2024

@author: Vernay

Collection of functions to plot 2D fields.
'''

import numpy as np
import matplotlib
import matplotlib.pyplot as plt


def plot_field(field, savename, vmin=None, vmax=None, cmap=plt.cm.YlGnBu, addpoint=None):
    matplotlib.rcParams.update({'font.size': 22})
    fig, ax = plt.subplots(figsize=(12 * np.shape(field)[1] / np.shape(field)[0], 10))
    if vmax is None:
        vmax = np.max(np.abs(field))
    if vmin is None:
        vmin = -vmax
    cmap.set_bad('grey', 1.)
    cml = field.plot(cmap=cmap, vmin=vmin, vmax=vmax)
    cml.set_edgecolor('face')
    if '.pdf' not in savename:
        savename = f'{savename}.pdf'
    if addpoint is not None:
        ax.plot(addpoint[0], addpoint[1], marker='.', linestyle='', color='k', markersize=20)
    plt.tight_layout()
    fig.savefig(savename, format='pdf')
    plt.close('all')
