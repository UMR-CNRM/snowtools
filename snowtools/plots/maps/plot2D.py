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
from matplotlib import ticker

matplotlib.rcParams.update({'font.size': 22})

domain_coords = {
    'GrandesRousses': dict(latmax=45.240, latmin=44.990, lonmin=6.010, lonmax = 6.490),
    'Lautaret area (Pleiades 2018)': [(6.385, 45.177), (6.490, 45.150), (6.490, 44.990), (6.3167, 44.990)],
    # Pleiades2019   = [(6.076, 45.195), (6.201, 45.195), (6.202, 45.023), (6.069, 45.024)],
    'Huez area (Pleiades 2022)': [(6.065, 45.020), (6.068, 45.200), (6.324, 45.198), (6.317, 45.017)],
}


def plot_field(field, ax=None, vmin=None, vmax=None, cmap=plt.cm.YlGnBu, addpoint=None, alpha=1., dem=None,
        shade=False, isolevels=None, slices=None, add_colorbar=True, transform=None):
    """
    :kwargs dem: Digital elevation model (xarray.DataArray)
    slices:: (int) cluster data (and colorbar) into *slices* slices
    """
    if ax is None:
        plt.figure(figsize=(12 * len(field.xx) / len(field.yy), 10))
        ax = plt.gca()
        newfig = True
    else:
        newfig = False

    if slices is not None:
        cmaplist = [cmap(i) for i in range(cmap.N)]
        # create the new map
        cmap = matplotlib.colors.LinearSegmentedColormap.from_list(
            'Custom cmap', cmaplist, cmap.N)

        # define the bins and normalize
        bounds = np.linspace(vmin, vmax, slices + 1)
        norm = matplotlib.colors.BoundaryNorm(bounds, cmap.N)
    else:
        norm = None

    # Plot Nan values in grey
    cmap.set_bad('grey', 1)
    if dem is not None:
        if shade:
            add_relief_shading(dem, ax=ax, extent=[field.xx.min(), field.xx.max(), field.yy.min(), field.yy.max()])
            alpha = 0.8
            # Plot Nan values transparent
            cmap.set_bad(alpha=0)
        else:
            if isolevels is not None:
                add_iso_elevation(dem, ax=ax, levels=isolevels)
            else:
                add_iso_elevation(dem, ax=ax)

    # Set defailt vmin/vmax values from field if necessary
    if vmax is None:
        vmax = np.max(np.abs(field))
    if vmin is None:
        vmin = -vmax

    # Plot field
    # If alpha < 1, the overlaping pixels look like grid lines that
    # The workaround is to use "contourf" instead.
    # WARNING : this can "hide" some varibility in *filed*
    if alpha < 1:
        cml = ax.contourf(
            field.xx, field.yy, field.data,
            levels      = np.linspace(vmin, vmax, slices) if slices is not None else np.linspace(vmin, vmax, 50),
            cmap        = cmap,
            vmin        = vmin,
            vmax        = vmax,
            alpha       = alpha,  # Transparency
            # Try to remove lines :
            antialiased = True,
        )
        plt.colorbar(cml, ticks=ticker.MaxNLocator(6))
        # Rasterize to reduce figure size
        for c in cml.collections:
            c.set_rasterized(True)
    else:
        if transform is not None:
            cml = field.plot(ax=ax, cmap=cmap, norm=norm, vmin=vmin, vmax=vmax, alpha=alpha, add_colorbar=add_colorbar,
                    transform=None)
        else:
            cml = field.plot(ax=ax, cmap=cmap, norm=norm, vmin=vmin, vmax=vmax, alpha=alpha, add_colorbar=add_colorbar)
        # Remove pixel edges
        cml.set_edgecolor('face')

    # Add specific point(s)
    if addpoint is not None:
        for point in addpoint:
            if ax is not None:
                ax.plot(point[0], point[1], marker='.', linestyle='', color='k', markersize=20,)
            else:
                plt.plot(point[0], point[1], marker='.', linestyle='', color='k', markersize=20,)

    if newfig:
        return cml, ax
    else:
        return cml


def add_iso_elevation(dem, ax=None, levels=[1500, 2000, 2500, 3000, 3500]):
    """
    Add iso-elevation bands to show the relief
    """
    if ax is not None:
        c = ax.contour(dem.xx, dem.yy, dem.data, colors='k', levels=levels, alpha=0.5,)
        ax.clabel(c, inline=1, fontsize=14)
    else:
        c = plt.contour(dem.xx, dem.yy, dem.data, colors='k', levels=levels, alpha=0.5,)
        plt.clabel(c, inline=1, fontsize=14)


def add_relief_shading(dem, ax=None, extent=None):
    """
    Add DEM's shading to show the relief
    """

    if extent is None:
        extent = [dem.xx.min(), dem.xx.max(), dem.yy.min(), dem.yy.max()]

    from matplotlib.colors import LightSource
    ls = LightSource(azdeg=315, altdeg=45)
    if ax is not None:
        ax.imshow(ls.hillshade(dem.data, dx=30, dy=30), cmap=plt.cm.gray, extent=extent, rasterized=True)
    else:
        plt.imshow(ls.hillshade(dem.data, dx=30, dy=30), cmap=plt.cm.gray, extent=extent, rasterized=True)


def save_fig(savename, fig=None, tight_layout=True):

    if '.pdf' not in savename:
        savename = f'{savename}.pdf'
    if tight_layout:
        plt.tight_layout()
    if fig is not None:
        fig.savefig(savename, format='pdf')
    else:
        plt.savefig(savename, format='pdf')
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


def plot_ensemble(ensemble, vmin=0, vmax=1, cmap=plt.cm.Blues, dem=None, isolevels=None):
    """
    ensemble:: DataArray with a *member* dimension (its name is used as label)
    """

    # Assume ensemble size = 16
    fig, ax = plt.subplots(nrows=4, ncols=4, figsize=(22, 15))
    i = 0
    j = 0
    for mb in ensemble.member.data[1:]:
        tmp = ensemble.sel({'member': mb})
        im = plot_field(tmp, ax=ax[i, j], vmin=vmin, vmax=vmax, cmap=cmap, dem=dem,
                isolevels=isolevels, add_colorbar=False)
        j = j + 1
        if j == 4:
            j = 0
            i = i + 1

    for axis in ax.flatten():
        axis.margins(0.02)
        axis.set_title('')
        axis.set_xticks([])
        axis.set_yticks([])
        axis.set_xlabel('')
        axis.set_ylabel('')

    fig.subplots_adjust(left=0.01, top=0.99, bottom=0.01, right=0.85, wspace=0.02, hspace=0.02)
    cax = fig.add_axes([0.86, 0.02, 0.05, 0.96])
    cb = fig.colorbar(im, cax=cax)
    # cb.ax.tick_params(labelsize=20)

    cb.set_label(ensemble.name, size=24)

    return fig
