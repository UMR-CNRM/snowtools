#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 18 march 2024

@author: Vernay

Collection of functions to plot 2D fields.
'''

import os
import numpy as np
import xarray as xr
import pandas as pd
import shapefile
import glob
import shutil
import cartopy.crs as ccrs


import matplotlib
import matplotlib.pyplot as plt
import matplotlib.patches as patches
from matplotlib import ticker

import snowtools  # noqa

from bronx.syntax.externalcode import ExternalCodeImportChecker

vortex_checker = ExternalCodeImportChecker('vortex')
with vortex_checker:
    from vortex import toolbox

toolbox.active_now = True
matplotlib.rcParams.update({'font.size': 16})

know_cmaps = dict(
    DSN_T_ISBA    = plt.cm.Blues,
    Precipitation = plt.cm.YlGnBu,
    Rainf         = plt.cm.YlGnBu,
    Snowf         = plt.cm.YlGnBu,
)


def plot_field(field, ax=None, vmin=None, vmax=None, cmap=None, addpoint=None, alpha=1., dem=None,
        shade=False, isolevels=None, categories=None, add_colorbar=True, transform=None, boundaries=False,
        massifs=False, cities=False, gridlines=False, projection=None):
    """
    field:: xarray DataArray containing the 2D data to plot.
    :kwargs dem: Digital elevation model (xarray.DataArray)
    categories:: (int) cluster data (and colorbar) into categories

    If *field* has 'long_name' and 'units' attributes, thos ware used for the colorbar label
    (format : '{long_name} [{units}]'). In other cases the *field* name is used.
    """
    if ax is None:
        if projection is not None:
            fig, ax = plt.subplots(figsize=(12 * len(field.xx) / len(field.yy), 10),
                    subplot_kw=dict(projection=projection, transform=ccrs.PlateCarree()))
            ax.set_extent([field.xx.min(), field.xx.max(), field.yy.min(), field.yy.max()], crs=projection)
        else:
            fig, ax = plt.subplots(figsize=(12 * len(field.xx) / len(field.yy), 10))

    if cmap is None and field.name in know_cmaps.keys():
        cmap = know_cmaps[field.name]
    elif isinstance(cmap, str):
        cmap = matplotlib.colormaps[cmap]

    if categories is not None:
        cmaplist = [cmap(i) for i in range(cmap.N)]
        # create the new map
        cmap = matplotlib.colors.LinearSegmentedColormap.from_list(
            'Custom cmap', cmaplist, cmap.N)

        # define the bins and normalize
        bounds = np.linspace(vmin, vmax, categories + 1)
        norm = matplotlib.colors.BoundaryNorm(bounds, cmap.N)
    else:
        norm = None

    # Plot Nan values in grey
    if cmap is not None:
        cmap.set_bad('grey', 1)

    if dem is None:
        dem = get_dem()
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

    if boundaries:
        add_boundaries(ax)
    if massifs:
        add_massifs(ax)
    if cities:
        latmin = float(field.yy.min())
        latmax = float(field.yy.max())
        lonmin = float(field.xx.min())
        lonmax = float(field.xx.max())
        add_cities(ax, latmin, latmax, lonmin, lonmax)

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
        nlevels = categories if categories is not None else 50
        cml = ax.contourf(
            field.xx, field.yy, field.data,
            levels      = np.linspace(vmin, vmax, nlevels),
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

    if gridlines:
        gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                  linewidth=2, color='gray', alpha=0.5, linestyle='--')
        gl.xlabels_bottom = True
        gl.ylabels_left = True
        gl.xlabels_top = False
        gl.ylabels_right = False

    return cml, ax


@vortex_checker.disabled_if_unavailable
def get_dem(genv='uenv:dem.2@vernaym', gvar='DEM_ALP1KM_EPSG4326'):

    toolbox.input(
        genv   = genv,
        gvar   = gvar,
        filename='TARGET_RELIEF.tif',
        unknown=True
    )
    # dem = rioxarray.open_rasterio('TARGET_RELIEF.tif')
    ds = xr.open_dataset('TARGET_RELIEF.tif')
    ds = ds.rename({'lon': 'xx', 'lat': 'yy'})
    dem = ds['elevation']
    dem = dem.squeeze()
    return dem


def add_iso_elevation(dem, ax=None, levels=[1000, 2000, 3000, 4000]):
    """
    Add iso-elevation bands to show the relief
    """
    if ax is not None:
        c = ax.contour(dem.xx, dem.yy, dem.data, colors='gray', levels=levels, alpha=0.3, linewidths=0.5)
        ax.clabel(c, inline=1, fontsize=6)
    else:
        c = plt.contour(dem.xx, dem.yy, dem.data, colors='gray', levels=levels, alpha=0.3, linewidths=0.5)
        plt.clabel(c, inline=1, fontsize=6)


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


def add_rectangle(ax, label, x0, y0, dx, dy, color='k', **kw):
    full_kw = dict(linewidth=4, facecolor='none')
    full_kw.update(**kw)
    # Create a Rectangle patch
    # https://matplotlib.org/stable/api/_as_gen/matplotlib.patches.Rectangle.html
    # rect = patches.Rectangle((x0, y0), dx, dy, linewidth=4, edgecolor=color, facecolor='none', label=label)
    rect = patches.Rectangle((x0, y0), dx, dy, edgecolor=color, label=label, **full_kw)
    # Add the patch to the Axes
    ax.add_patch(rect)


def add_quadrilateral(ax, label, xy, color='k', **kw):
    full_kw = dict(linewidth=4, facecolor='none')
    full_kw.update(**kw)
    poly = patches.Polygon(xy, edgecolor=color, label=label, **full_kw)
    ax.add_patch(poly)


@vortex_checker.disabled_if_unavailable
def get_administrative_boundaries():

    tarname = 'world-administrative-boundaries.tar'
    toolbox.input(
        genv     = 'uenv:shapefiles.1@vernaym',
        gvar     = 'WORLD_BOUNDARIES',
        filename = tarname,
        unknown  = True,
    )


def add_boundaries(ax, filename=None):

    if filename is None:
        get_administrative_boundaries()
        filename = 'world-administrative-boundaries.shp'

    if os.path.exists(filename):
        borders = shapefile.Reader(filename)
        for shape in borders.shapeRecords():
            x = [i[0] for i in shape.shape.points[:]]
            y = [i[1] for i in shape.shape.points[:]]
            ax.plot(x, y, color='k', linestyle=':', transform=ccrs.PlateCarree())

        for f in glob.glob('world-administrative-boundaries*'):
            if os.path.isfile(f):
                os.remove(f)
            else:
                shutil.rmtree(f)


@vortex_checker.disabled_if_unavailable
def get_safran_massifs():

    tarname = "massifs_safran.tar"
    toolbox.input(
        genv     = 'uenv:shapefiles.1@vernaym',
        gvar     = 'MASSIFS_SAFRAN',
        filename = tarname,
        unknown  = True,
    )


def add_massifs(ax, filename=None):

    if filename is None:
        get_safran_massifs()
        filename = "massifs_safran.shp"

    if os.path.exists(filename):
        massifs = shapefile.Reader(filename)
        for shape in massifs.shapeRecords():
            x = [i[0] for i in shape.shape.points[:]]
            y = [i[1] for i in shape.shape.points[:]]
            ax.plot(x, y, color='k', alpha=0.3, transform=ccrs.PlateCarree(), linewidth=1)

        for f in glob.glob('massifs_safran*'):
            if os.path.isfile(f):
                os.remove(f)
            else:
                shutil.rmtree(f)


@vortex_checker.disabled_if_unavailable
def get_french_cities():

    filename = "french_cities.csv"
    toolbox.input(
        genv     = 'uenv:shapefiles.1@vernaym',
        gvar     = 'FRENCH_CITIES',
        filename = filename,
        unknown  = True,
    )


def add_cities(ax, latmin, latmax, lonmin, lonmax, filename=None):

    if filename is None:
        get_french_cities()
        filename = "french_cities.csv"

    if os.path.exists(filename):
        cities = pd.read_csv(filename, sep=',')
        tmp = cities[cities.population > 100000]
        tmp = tmp[(cities.lat >= latmin) & (cities.lat <= latmax) & (cities.lng >= lonmin) & (cities.lng <= lonmax)]
        ax.plot(tmp.lng.to_numpy(), tmp.lat.to_numpy(), marker='.', color='k', linestyle='',
                transform=ccrs.PlateCarree())
        for idx in tmp.index:
            ax.text(tmp.lng[idx], tmp.lat[idx], tmp.city[idx], alpha=0.5, fontsize=8)

        os.remove('french_cities.csv')
