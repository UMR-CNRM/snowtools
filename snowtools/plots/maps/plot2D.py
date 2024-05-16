#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 18 march 2024

@author: Vernay

Collection of functions to plot 2D fields.
'''

import numpy as np
import matplotlib.pyplot as plt


def plot_field(field, savename, vmin=None, vmax=None, cmap=plt.cm.YlGnBu):
    fig, ax = plt.subplots(figsize=(12 * np.shape(field)[1] / np.shape(field)[0], 10))
    if vmax is None:
        vmax = np.max(np.abs(field))
    if vmin is None:
        vmin = 0
    cmap.set_bad('grey', 1.)
    im = ax.imshow(np.flipud(field.data), cmap=cmap, vmin=vmin, vmax=vmax)
    fig.colorbar(im)
    if '.pdf' not in savename:
        savename = f'{savename}.pdf'
    plt.tight_layout()
    fig.savefig(savename, format='pdf')
    plt.close('all')
