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


def plot_error_fields(simu, obs, varn, savename, vmin=None, vmax=None):
    """
    Plot the difference between a 2D simulation and a corresponding observation field.
    """
    fig, ax = plt.subplots()
    diff = simu - obs
    if vmax is None:
        vmax = np.max(np.abs(diff))
    if vmin is None:
        vmin = -vmax
    cmap = matplotlib.cm.RdBu
    cmap.set_bad('grey', 1.)
    im = ax.imshow(np.flipud(diff.data), cmap=cmap, vmin=vmin, vmax=vmax)
    fig.colorbar(im)
    fig.savefig(f'{savename}.pdf', format='pdf')
    plt.close('all')
