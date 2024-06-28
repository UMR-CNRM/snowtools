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
import matplotlib.patches as patches

matplotlib.rcParams.update({'font.size': 22})

domain_coords = dict(
    GrandesRousses = dict(latmax=45.240, latmin=44.990, lonmin=6.010, lonmax = 6.490),
    Pleiades2018   = [(6.385, 45.177), (6.490, 45.150), (6.490, 44.990), (6.3167, 44.990)],
    Pleiades2019   = [(6.076, 45.195), (6.201, 45.195), (6.202, 45.023), (6.069, 45.024)],
    Pleiades2022   = [(6.065, 45.020), (6.068, 45.200), (6.324, 45.198), (6.317, 45.017)],
)


def plot_field(field, ax=None, vmin=None, vmax=None, cmap=plt.cm.YlGnBu, addpoint=None):
    """
    :kwargs dem: Digital elevation model (xarray.DataArray)
    """
    # In case the figure has not been initialized
    if ax is None:
        fig, ax = plt.subplots(figsize=(12 * np.shape(field)[1] / np.shape(field)[0], 10),)

    # Set defailt vmin/vmax values from field if necessary
    if vmax is None:
        vmax = np.max(np.abs(field))
    if vmin is None:
        vmin = -vmax

    # Plot Nan values in grey
    cmap.set_bad('grey', 1.)

    # Plot field
    cml = field.plot(cmap=cmap, vmin=vmin, vmax=vmax)
    # Remove pixel edges
    cml.set_edgecolor('face')

    # Add specific point(s)
    if addpoint is not None:
        for point in addpoint:
            ax.plot(point[0], point[1], marker='.', linestyle='', color='k', markersize=20,)

    return fig, ax


def add_iso_elevation(ax, dem, levels=[1200, 2400, 3600]):
    """
    Add iso-elevation bands to show the relief
    """
    c = ax.contour(dem.xx, dem.yy, dem.data, colors='dimgray', levels=levels, alpha=0.9,)
    ax.clabel(c, inline=1, fontsize=14)


def save_fig(fig, savename):

    plt.tight_layout()
    if '.pdf' not in savename:
        savename = f'{savename}.pdf'
    fig.savefig(savename, format='pdf')
    plt.close('all')


def add_rectangle(ax, label, color='k', **kw):
    full_kw = dict(linewidth=4, facecolor='none')
    full_kw.update(**kw)
    # Create a Rectangle patch
    x0 = domain_coords[label]['lonmin']
    y0 = domain_coords[label]['latmin']
    dx = domain_coords[label]['lonmax'] - x0
    dy = domain_coords[label]['latmax'] - y0
    # https://matplotlib.org/stable/api/_as_gen/matplotlib.patches.Rectangle.html
    # rect = patches.Rectangle((x0, y0), dx, dy, linewidth=4, edgecolor=color, facecolor='none', label=label)
    rect = patches.Rectangle((x0, y0), dx, dy, edgecolor=color, label=label, **full_kw)
    # Add the patch to the Axes
    ax.add_patch(rect)


def add_quadrilateral(ax, label, color='k', **kw):
    full_kw = dict(linewidth=4, facecolor='none')
    full_kw.update(**kw)
    poly = patches.Polygon(domain_coords[label], edgecolor=color, label=label, **full_kw)
    ax.add_patch(poly)
