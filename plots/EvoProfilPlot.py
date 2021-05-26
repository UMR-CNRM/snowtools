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
import matplotlib.colors as colors
from plots import Dictionnaries
import logging
logger = logging.getLogger()


def plot_profil(ax, dz, value, colormap='jet', myrange=None, vmin=None, vmax=None, legend=None, cbar_show=True):
    """
    Trace le profil de value en fonction du temps avec les epaisseurs reelles de couches
    """
    
    class MidpointNormalize(colors.Normalize):
        def __init__(self, vmin=None, vmax=None, vcenter=None, clip=False):
            self.vcenter = vcenter
            colors.Normalize.__init__(self, vmin, vmax, clip)

        def __call__(self, value, clip=None):
            x, y = [self.vmin, self.vcenter, self.vmax], [0, 0.5, 1]
            return np.ma.masked_array(np.interp(value, x, y))

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

    maxval = np.nanmax(value)
    minval = np.nanmin(value)
    if np.ma.is_masked(maxval):
        maxval=1
    if np.ma.is_masked(minval):
        minval=0.1

    extend='neither'
    if colormap == 'grains':
        cmap = Dictionnaries.grain_colormap
        bounds = np.linspace(-0.5, 14.5, 16)
        norm = colors.BoundaryNorm(bounds, cmap.N)
        vmin = -0.5
        vmax = 14.5
    elif colormap == 'echelle_log':
        cmap = cm.gray_r
        Vmin = max(minval,0.0000000001)
        Vmax = min(max(0.000000001, maxval),1)
        norm = colors.LogNorm(vmin=Vmin, vmax=Vmax)    
        cmap.set_under('#fff2fd')
        extend='min'
    elif colormap == 'echelle_log_sahara':
        cmap = cm.gist_heat_r
        Vmin = max(minval,0.0000000001)
        Vmax = min(max(0.000000001, maxval),1)
        value = value.clip(Vmin/2,Vmax)
        norm = colors.LogNorm(vmin=Vmin, vmax=Vmax)
        cmap.set_under('#fff2fd')
        extend='min'
    elif colormap == 'ratio_cisaillement':
        cmap = cm.get_cmap('viridis')
        Vmin = 0
        Vmax = 20
        norm = colors.Normalize(vmin=0, vmax=Vmax, clip=True)
        # Generating a colormap from given cmap with:
        # 0 -> 1 pink
        # 1 -> 2 red
        # 2 -> 3 orange
        # 3 -> 4 yellow
        # 4 -> 20 given cmap
        customcmap = {
                'red':[(0,1,1), (0.05, 1, 1), (0.1, 1, 1), (0.15, 1, 1)],
                'green':[(0,0,0), (0.05, 0, 0), (0.1, 0.55, 0.65), (0.15, 1, 1)],
                'blue':[(0,1,1), (0.05, 0.4, 0), (0.1, 0, 0), (0.15, 0, 0)]
                }
        cmapl = len(cmap.colors)
        start = 0.16
        for i in range(cmapl):
            if i==cmapl-1:
                x=1
            else:
                x = start + i*(1-start)/cmapl
            iuse = cmapl - i -1
            customcmap['red'].append((x, cmap.colors[iuse][0], cmap.colors[iuse][0]))
            customcmap['green'].append((x, cmap.colors[iuse][1], cmap.colors[iuse][1]))
            customcmap['blue'].append((x, cmap.colors[iuse][2], cmap.colors[iuse][2]))
        cmap = colors.LinearSegmentedColormap('ratio_cisaillment', customcmap)
    elif colormap == 'tempK':
        Vmax = 273.15
        Vmin=Vmax-40 if vmax is None else vmin
        norm = colors.Normalize(vmin=Vmin, vmax=Vmax)
        value[value<Vmin] = Vmin
        cmap = cm.get_cmap('RdBu_r')
        cmap.set_over((0.32,0.0,0.097))
        extend='max'
    elif colormap == 'lwc':
        cmap = cm.get_cmap('viridis')
        Vmin = 0
        Vmax= 35 if vmax is None else vmax
        norm = colors.Normalize(vmin=Vmin, vmax=Vmax)
        value[value>Vmax] = Vmax
        value[value==0] = -1
        cmap.set_under('#fff2fd')
        extend='min'
    else:
        norm = None
        cmap = cm.get_cmap(colormap)

    rect = collections.PolyCollection(vertices, array=value[(dz > 0)].ravel(),
                                      cmap=cmap, norm=norm, edgecolors='none')
    rect.set_clim(vmin, vmax)
    ax.add_collection(rect)
    ax.autoscale_view()
    
    if cbar_show:
        cbar = plt.colorbar(rect, ax=ax, extend=extend)

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
    norm = colors.BoundaryNorm(bounds, cmap.N)
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
