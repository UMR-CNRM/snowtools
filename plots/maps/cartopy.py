#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""
Created on 29 march 2021

@author: radanovics

start collecting resources for plotting maps with cartopy.
Might be sensitive to the combination of versions of matplotlib and cartopy.
developed with matplotlib 3.4.0 and cartopy 0.18

Usage :
example :
create a Map instance for the Alps. Map_alpes can take optional kwargs.
m = Map_alpes(kwargs)
m.init_massifs(palette='Reds')

"""

import os
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import cm
import cartopy.crs as ccrs
import cartopy.io.shapereader as shpreader
import cartopy.feature
from plots.abstracts.figures import Mplfigure
import pyproj
import fiona
from osgeo import osr
from utils.infomassifs import infomassifs

# dummy class in order to be able to create an ccrs.CRS instance from a proj4/fiona.crs dictionary
class MyCRS(ccrs.CRS):
    pass


class _Map_massifs(Mplfigure):
    """ Implicit class that defines a generic massif map"""
    legendok = False

    def __init__(self, *args, **kw):
        # print(kw)
        # Get massif shapes
        self.getshapes()
        self.projection = MyCRS(self.shpProj)
        self.openfigure()
        if 'getmap' in kw.keys():
            getmap = kw['getmap']
        else:
            getmap = True

        if getmap:
            self.map = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax,
                               **kw)
            self.map.coastlines(linewidth=1)
            self.map.gridlines(draw_labels=True)

        self.dicLonLatMassif = self.getLonLatMassif()


    # @echecker.disabled_if_unavailable
    def getmap(self, latmin, latmax, lonmin, lonmax, **kwargs):
        # print(kwargs.keys())
        if 'geofeatures' in kwargs.keys():
            self.geofeatures = kwargs['geofeatures']
        else:
            self.geofeatures = False
        if hasattr(self, 'nrow') and hasattr(self, 'ncol') and hasattr(self, 'iax'):
            ax = plt.subplot(self.nrow, self.ncol, self.iax+1, projection=ccrs.PlateCarree())
        else:
            ax = plt.axes(self.mappos, projection=ccrs.PlateCarree())
        # Définit les bords de la carte
        ax.set_extent([self.lonmin, self.lonmax, self.latmin, self.latmax])
        if self.geofeatures:
            ax.add_feature(cartopy.feature.LAND, facecolor='wheat')
            ax.add_feature(cartopy.feature.OCEAN)
            ax.add_feature(cartopy.feature.COASTLINE)
            ax.add_feature(cartopy.feature.BORDERS, linestyle=':')
            ax.add_feature(cartopy.feature.LAKES, alpha=0.5)
            ax.add_feature(cartopy.feature.RIVERS)
        return ax

    def openfigure(self):
        self.fig = plt.figure(figsize=(self.width, self.height))

    #@echecker.disabled_if_unavailable
    def getshapes(self):
        shapefile_path = os.path.join(os.environ['SNOWTOOLS_CEN'], 'DATA')
        filename = 'massifs_{0:s}.shp'.format(self.area)
        self.shapefile = shpreader.Reader(os.path.join(shapefile_path, filename))
        # Informations sur la projection
        sh = fiona.open(os.path.join(shapefile_path, filename))
        # Projection du shapefile
        self.shpProj = sh.crs
        # print(sh.crs)

        # géométries
        # records is a generator object. Each record contains a geometry, its attributes and bounds
        self.records = self.shapefile.records()

    def getLonLatMassif(self):
        """returns dict with key = Massif Number, value = (lon, lat) """
        im = infomassifs()
        return infomassifs.getAllMassifLatLon(im)

    def init_massifs(self, **kwargs):
        # This routine initializes a colormap
        if 'palette' in kwargs.keys():
            if 'ncolors' in kwargs.keys():
                self.palette = plt.get_cmap(kwargs['palette'], kwargs['ncolors']).copy()
            else:
                self.palette = plt.get_cmap(kwargs['palette']).copy()
        else:
            self.palette = plt.get_cmap('jet').copy()

        self.palette.set_bad(color='grey')
        self.palette.set_under(color='grey')

        self.norm = self.normpalette(**kwargs)

    def normpalette(self, **kwargs):
        # Bornes pour légende
        if 'forcemin' in kwargs.keys():
            self.vmin = kwargs['forcemin']
        else:
            self.vmin = 0
        if 'forcemax' in kwargs.keys():
            self.vmax = kwargs['forcemax']
        else:
            self.vmax = 100

        return matplotlib.colors.Normalize(vmax=self.vmax, vmin=self.vmin)

    def draw_massifs(self, massifref, variablein, **kwargs):
        # This routine fills the polygons with a color
        # depending on the value of variablein associated with the massif number provided in massifref
        # It is not required to provide a value for all massifs
        print(kwargs.keys())
        if 'convert_unit' in kwargs.keys():
            variable = variablein[:] * kwargs['convert_unit']
            print(variable)
            print(variablein)
        else:
            variable = variablein[:]

        # print(next(self.records).attributes)
        num, shape, name = zip(*[(rec.attributes['num_opp'], rec.geometry,
                                  rec.attributes['nom']) for rec in self.records])
        if 'axis' in kwargs.keys() and hasattr(self, 'nsubplots'):
            axis = kwargs['axis']
            try:
                len = variable.shape[axis]
            except(IndexError):
                raise Exception("value array has lower rank than given axis number")
            if len > self.nsubplots:
                print("Warning: axis ", axis, " of value array is longer than number of subplots ", self.nsubplots,
                      ". Plotting first ", self.nsubplots, " out of ", len, ".")
                len = self.nsubplots
            myvalues = np.array([variable[massifref==i][0] if i in massifref else np.nan for i in num])
            print(myvalues.shape)
            for i in range(len):
                for ishape, myvalue in zip(shape, myvalues.take(indices=i, axis=axis)):
                    self.maps.flat[i].add_geometries([ishape], crs=self.projection, cmap=self.palette,
                                                facecolor=self.palette(self.norm(myvalue)), edgecolor='dimgrey', alpha=1.0)
        else:
            myvalues = [variable[massifref==i][0] if i in massifref else np.nan for i in num]
            for ishape, myvalue in zip(shape, myvalues):
                self.map.add_geometries([ishape], crs=self.projection, cmap=self.palette,
                            facecolor=self.palette(self.norm(myvalue)),  edgecolor='dimgrey', alpha=1.0)

        # prepare colorbar
        self.m = plt.cm.ScalarMappable(cmap=self.palette)
        self.m.set_array(np.array(myvalues))
        self.m.set_clim(self.vmin, self.vmax)

        if not self.legendok:
            self.legend(self.m, **kwargs)

    def legend(self, polygons, **kwargs):

        currentaxis = plt.gca()

        cax = self.fig.add_axes(self.legendpos)
        self.cbar = self.fig.colorbar(polygons, cax=cax)

        if 'ticks' in kwargs.keys():
            self.cbar.set_ticks(range(0, len(kwargs['ticks'])))
            self.cbar.set_ticklabels(kwargs['ticks'])
            fontsize = 10
        else:
            fontsize = 20

        for t in self.cbar.ax.get_yticklabels():
            t.set_fontsize(fontsize)

        if 'label' in kwargs.keys():
            self.cbar.set_label(kwargs['label'], fontsize=20)

        plt.sca(currentaxis)

    def set_maptitle(self, title):
        """Set title on top of the map"""
        self.map.set_title(title, fontsize=20, pad=15)

    def set_figtitle(self, title):
        """Set title on top of the figure"""
        self.fig.suptitle(title, fontsize=20)


class Map_alpes(_Map_massifs):

    def __init__(self, *args, **kw):
        self.area = 'alpes'
        self.width = 12
        self.height = 10
        #         self.latmin = 43.85
        #         self.latmax = 46.6  # 46.5
        #         self.lonmin = 5.1
        #         self.lonmax = 8.1  # 7.9

        self.latmin = 43.9
        self.latmax = 46.5
        self.lonmin = 5.2
        self.lonmax = 7.9

        self.mappos=[0.02, 0.06, 0.8, 0.8]
        self.legendpos = [0.85, 0.15, 0.03, 0.6]
        self.infospos = (205000, 330000)

        self.deport = {2: (-10000, 0), 3: (10000, 0), 6: (20000, 0), 7: (-20000, 10000), 9: (15000, -10000), 11: (15000, -10000),
                       13: (15000, 0), 17: (15000, 0), 18: (-20000, -10000), 19: (0, -5000), 20: (0, -5000), 21: (0, -10000)}

        # self.fig = plt.figure(figsize=(self.width, self.height))
        # self.map = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax)
        # print(kw)
        super(Map_alpes, self).__init__(*args, **kw)

class MultiMap_Alps(Map_alpes):

    def __init__(self, nrow=1, ncol=1, *args, **kw):
        kw['getmap'] = False
        self.nrow = nrow
        self.ncol = ncol
        self.nsubplots = nrow*ncol
        super(MultiMap_Alps, self).__init__(*args, **kw)
        for self.iax in range(self.nsubplots):
            self.maps.flat[self.iax] = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax, **kw)
            self.maps.flat[self.iax].coastlines(linewidth=1)
            self.gl = self.maps.flat[self.iax].gridlines(draw_labels=True)
            # keep labels left and bottom only
            self.gl.top_labels = False
            self.gl.right_labels = False
            # move the subplots a little to the left in order to have some space for the colorbar on the right.
            pos1 = self.maps.flat[self.iax].get_position()
            pos1.x0 = pos1.x0 - 0.1
            self.maps.flat[self.iax].set_position(pos1)

    def openfigure(self):
        self.fig, self.maps = plt.subplots(nrows=self.nrow, ncols=self.ncol, sharex=True, sharey=True,
                                           figsize=(self.width, self.height))

    def set_maptitle(self, title):
        """Set title on top of each subplot"""
        if len(title) == self.nsubplots:
            for i in range(self.nsubplots):
                self.maps.flat[i].set_title(title[i], fontsize=14, pad=5)
        elif len(title) == 1:
            for i in range(self.nsubplots):
                self.maps.flat[i].set_title(title, fontsize=14, pad=5)
        else:
            print("Warning: can not set map titles. len(title) must be either equal to the number of subplots or == 1.")