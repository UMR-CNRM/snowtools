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
import cartopy.crs as ccrs
import matplotlib.patches as patches

matplotlib.rcParams.update({'font.size': 22})

domain_coords = dict(
    GrandesRousses = dict(latmax=45.240, latmin=44.990, lonmin=6.010, lonmax = 6.490),
    # Pleiades2019   = dict(latmax=, latmin=, lonmin=, lonmax=),
    Pleiades2018   = dict(lonmin=6.2737, latmin=45.0032, lonmax=6.4811, latmax=45.1727),
    Pleiades2022   = dict(lonmin=6.0322, latmin=45.2353, lonmax=6.4702, latmax=44.9973),
)


def plot_field(field, ax=None, vmin=None, vmax=None, cmap=plt.cm.YlGnBu, addpoint=None):
    """
    :kwargs dem: Digital elevation model (xarray.DataArray)
    """
    # In case the figure has not been initialized
    if ax is None:
        fig, ax = plt.subplots(figsize=(12 * np.shape(field)[1] / np.shape(field)[0], 10),
                subplot_kw=dict(projection=ccrs.PlateCarree()))

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
            ax.plot(point[0], point[1], marker='.', linestyle='', color='k', markersize=20,
                    transform=ccrs.PlateCarree())

    return fig, ax


def add_iso_elevation(ax, dem, levels=[1200, 2400, 3600]):
    """
    Add iso-elevation bands to show the relief
    """
    c = ax.contour(dem.xx, dem.yy, dem.data, colors='dimgray', levels=levels, transform=ccrs.PlateCarree(),
            alpha=0.9)
    ax.clabel(c, inline=1, fontsize=14)


def save_fig(fig, savename):

    plt.tight_layout()
    if '.pdf' not in savename:
        savename = f'{savename}.pdf'
    fig.savefig(savename, format='pdf')
    plt.close('all')


def add_rectangle(ax, label, color='k'):
    # Create a Rectangle patch
    x0 = domain_coords[label]['lonmin']
    y0 = domain_coords[label]['latmin']
    dx = domain_coords[label]['lonmax'] - x0
    dy = domain_coords[label]['latmax'] - y0
    # https://matplotlib.org/stable/api/_as_gen/matplotlib.patches.Rectangle.html
    # rect = patches.Rectangle((x0, y0), dx, dy, linewidth=4, edgecolor=color, facecolor='none', label=label)
    rect = patches.Rectangle((x0, y0), dx, dy, linewidth=4, edgecolor=color, facecolor='none', label=label)
    # Add the patch to the Axes
    ax.add_patch(rect)


def add_quadrilatere(ax, label, color='k', **kw):
    # get quadrilater coordinates
    x0 = domain_coords[label]['lonmin']
    x1 = domain_coords[label]['lonmax']
    y0 = domain_coords[label]['latmin']
    y1 = domain_coords[label]['latmax']
    full_kw = dict(linewidth=4, facecolor='none')
    full_kw.update(**kw)
    poly = patches.Polygon([(x0, y0), (x0, y1), (x1, y1), (x1, y0)], edgecolor=color, label=label, **full_kw)
    ax.add_patch(poly)
