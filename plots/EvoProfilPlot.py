# -*- coding: utf-8 -*-

'''
Created on 6 avr. 2017

@author: hagenmullerp
Modified 7. apr. 2017 viallon
'''

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
from matplotlib import collections
from matplotlib.colors import BoundaryNorm
from matplotlib.colors import LogNorm
from . import Dictionnaries


def plot_profil(ax, dz, value, colormap='jet', myrange=None, vmin=None, vmax=None, legend=None, cbar_show=True):
    """
    Trace le profil de value en fonction du temps avec les epaisseurs reelles de couches
    """

    if myrange:
        value = np.clip(value, myrange[0], myrange[1])

    top_y = np.cumsum(dz[:, ::-1], axis=1)[:, ::-1].ravel()
    bottom_y = top_y - dz.ravel()

    left_x = np.ones(shape=dz.shape, dtype = 'int')
    left_x = np.cumsum(left_x, axis=0).ravel()
    right_x = left_x - 1

    vertices = np.zeros(shape=(right_x.shape[0], 4, 2))
    vertices[:, 0, 0] = right_x
    vertices[:, 0, 1] = bottom_y
    vertices[:, 1, 0] = right_x
    vertices[:, 1, 1] = top_y
    vertices[:, 2, 0] = left_x
    vertices[:, 2, 1] = top_y
    vertices[:, 3, 0] = left_x
    vertices[:, 3, 1] = bottom_y

    vertices = vertices[(dz > 0).ravel()]

    if colormap == 'grains':
        cmap = Dictionnaries.grain_colormap
        bounds = np.linspace(-0.5, 14.5, 16)
        norm = BoundaryNorm(bounds, cmap.N)
        vmin = -0.5
        vmax = 14.5
    elif colormap == 'echelle_log':
        cmap = cm.gray_r
        Vmin = max(np.amin(value),0.0000000001)
        Vmax = min(np.amax(value),1)
        norm = LogNorm(vmin=Vmin, vmax=Vmax)
    else:
        norm = None
        cmap = cm.get_cmap(colormap)

    rect = collections.PolyCollection(vertices, array=value[(dz > 0)].ravel(),
                                      cmap=cmap, norm=norm, edgecolors='none')
    rect.set_clim(vmin, vmax)
    ax.add_collection(rect)
    ax.autoscale_view()
    
    if cbar_show:
        cbar = plt.colorbar(rect, ax=ax)

        if colormap == 'grains':
            labels = Dictionnaries.MEPRA_labels
            cbar.set_ticks(np.arange(np.shape(labels)[0]))
            cbar.ax.set_yticklabels(labels)
        if(legend):
            cbar.set_label(legend)


def plot_grains1D(ax, dz, value, legend=None, cbar_show=True):
    """
    Trace le profil de type de grains selon la hauteur
    """

    bottom_y = np.cumsum(dz).ravel()
    top_y = bottom_y - dz.ravel()

    left_x = np.ones(shape=dz.shape, dtype='int')
    right_x = np.zeros(shape=dz.shape, dtype='int')

    vertices = np.zeros(shape=(bottom_y.shape[0], 4, 2))
    vertices[:, 0, 0] = right_x
    vertices[:, 0, 1] = bottom_y
    vertices[:, 1, 0] = right_x
    vertices[:, 1, 1] = top_y
    vertices[:, 2, 0] = left_x
    vertices[:, 2, 1] = top_y
    vertices[:, 3, 0] = left_x
    vertices[:, 3, 1] = bottom_y

    vertices = vertices[(dz > 0).ravel()]

    cmap = Dictionnaries.grain_colormap
    bounds = np.linspace(-0.5, 14.5, 16)
    norm = BoundaryNorm(bounds, cmap.N)
    vmin = -0.5
    vmax = 14.5

    rect = collections.PolyCollection(vertices, array=value[(dz > 0)].ravel(),
                                      cmap=cmap, norm=norm, edgecolors='none')
    rect.set_clim(vmin, vmax)
    ax.add_collection(rect)
    ax.autoscale_view()

    if(cbar_show):
        cbar = plt.colorbar(rect, ax=ax)
        labels = Dictionnaries.MEPRA_labels
        cbar.set_ticks(np.arange(np.shape(labels)[0]))
        cbar.ax.set_yticklabels(labels)
    if(legend):
        ax.set_xlabel(legend)
