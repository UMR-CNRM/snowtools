#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os
from matplotlib.collections import PatchCollection
from matplotlib.patches import Polygon
import matplotlib.pyplot as plt
import mpl_toolkits.basemap.pyproj as pyproj
from mpl_toolkits.basemap import Basemap
import numpy as np
from PIL import Image
from utils import shapefile
from utils.infomassifs import infomassifs
from osgeo import osr


class Mplfigure(object):

    def set_title(self, title):
        if "map" in dir(self):
            plt.title(title, fontsize=10)
        elif "plot" in dir(self):
            self.plot.set_title(title, fontsize=10)

    def set_suptitle(self, suptitle):
        self.fig.suptitle(suptitle, fontsize=10)

    def getlogo(self):
        return Image.open(os.environ["SNOWTOOLS_CEN"] + "/DATA/logoMF15.jpg")

    def addlogo(self):
        logo = self.getlogo()
        width, height = logo.size
        sizefig = self.fig.get_size_inches()
        widthfig = sizefig[0] * 100
        heightfig = sizefig[1] * 100

        if "map" in dir(self):
            self.fig.figimage(logo, widthfig - width, int(0.92 * heightfig) - height)
        else:
            self.fig.figimage(logo, widthfig - width, 0)

    def save(self, figname):
        plt.savefig(figname)
        self.fig.clear()

    def close(self):
        plt.close(self.fig)


class _Map_massifs(Mplfigure):
    """ Implicit class that defines a generic massif map"""

    def __init__(self, *args, **kw):
        self.map.drawcoastlines(linewidth=1)
#        self.map.drawcountries()
        self.map.drawmapboundary()
        # Get massif shapes
        self.getshapes()

    def getshapes(self):
        shapefile_path = os.path.join(os.environ['SNOWTOOLS_CEN'], 'DATA')
        filename = 'massifs_{0:s}.shp'.format(self.area)
        self.shapefile = shapefile.Reader(os.path.join(shapefile_path, filename))
        # Informations sur la projection
        filename = 'massifs_{0:s}.prj'.format(self.area)
        projstring = self.esriprj2standards(os.path.join(shapefile_path, filename))

        # Projection du shapefile
        self.shpProj = pyproj.Proj(projstring)

        # géométries
        self.shapes = self.shapefile.shapes()
        self.records = self.shapefile.record

    def esriprj2standards(self, shapeprj_path):
        prj_file = open(shapeprj_path, 'r')
        prj_txt = prj_file.read()
        srs = osr.SpatialReference()
        srs.ImportFromESRI([prj_txt])
        srs.AutoIdentifyEPSG()
        return '%s' % srs.ExportToProj4()

    def getmap(self, latmin, latmax, lonmin, lonmax):
        # Définit les bords de la carte
        return Basemap(
            llcrnrlat = self.latmin,
            urcrnrlat = self.latmax,
            llcrnrlon = self.lonmin,
            urcrnrlon = self.lonmax,
            projection = "merc",
            resolution = 'i',
            fix_aspect=True
        )

    def getxy(self, massif, dicLonLatMassif, index):

        coords = massif.points
        list_x = np.array(coords)[:, 0]
        list_y = np.array(coords)[:, 1]
        lonlat = np.array(self.shpProj(list_x, list_y, inverse=True)).T

        # ATTENTION :
        # X Y au sens de la figure
        X, Y = self.map(lonlat[:, 0], lonlat[:, 1])
        XYplot = zip(X, Y)

        num = self.records(index)[1]  # lecture de l'attribut num_opp

        # Barycentre du massif
        barycentre = dicLonLatMassif[num]
        Xbary, Ybary = self.map(barycentre[0], barycentre[1])

        return XYplot, Xbary, Ybary

    def getLonLatMassif(self):
        im = infomassifs()
        return infomassifs.getAllMassifLatLon(im)

    def plot_background(self):

        mypatches = []
        self.dicLonLatMassif = self.getLonLatMassif()
        for i, massif in enumerate(self.shapes):
            polygon_to_map = True
            force_barycentre = True
            num = self.records(i)[1]  # lecture de l'attribut num_opp

            # TO DO : gestion des autres cas
#             # Pyrénées
#             if num>=64 and num<=74:
#                 num=num-63
#             elif num>=80:
#                 num=num-68
#             if self.area in ["alpes_CNR"] and i+1 in list_num:
#                 polygon_to_map=True
#                 force_barycentre=False
#                 num=num+1
#             else:
#                 polygon_to_map=False
#                 force_barycentre=False

            if polygon_to_map:
                XYplot, Xbary, Ybary = self.getxy(massif, self.dicLonLatMassif, i)
                poly = Polygon(XYplot, alpha=1.0, fill=False, visible=True)

                mypatches.append(poly)
                plt.gca().add_patch(poly)

        p = PatchCollection(mypatches, cmap=plt.get_cmap('jet'), alpha=1.0)

    def addpoints(self, labels, lon, lat):
        x, y = self.map(lon, lat)
        # self.map.plot(x, y, marker='o', color="red")
        for label, xpt, ypt in zip(labels, x, y):
            self.plot(xpt, ypt, label)

    def plot(self, xpt, ypt, label=None):
        self.map.plot(xpt, ypt, marker='.', color="red")

    def fillmassif(self, num_massif):
        for i, massif in enumerate(self.shapes):
            num = self.records(i)[1]
            print num
            if num == num_massif:
                XYplot, Xbary, Ybary = self.getxy(massif, self.dicLonLatMassif, i)
                poly = Polygon(XYplot, facecolor='0.9', alpha=1.0, fill=True, visible=True)
                plt.gca().add_patch(poly)

    def set_scale(self):
        # Il y a un bug dans Basemap qui oblige à bricoler l'échelle.
        # cf. https://github.com/matplotlib/basemap/issues/165
        dref = 50
        lat0 = self.map.llcrnrlat + 0.2
        lon0 = self.map.llcrnrlon + 0.4

        distance = dref / np.cos(lat0 * np.pi / 180.)

        scale = self.map.drawmapscale(lon0, lat0, lon0, lat0, distance, barstyle='fancy', units='km', labelstyle='simple', fillcolor1='w', fillcolor2='#555555', fontcolor='#555555')

        # Ces éléments correspondent aux labels de l'échelle
        scale[12].set_text(dref / 2)
        scale[13].set_text(dref)


class Map_alpes(_Map_massifs):

    def __init__(self, *args, **kw):
        self.area = 'alpes'
        self.width = 10
        self.height = 10
        self.latmin = 43.85
        self.latmax = 46.6  # 46.5
        self.lonmin = 5.1
        self.lonmax = 8.1  # 7.9
        self.legendpos = [0.81, 0.15, 0.03, 0.6]

        self.fig = plt.figure(figsize=(self.width, self.height))
        self.map = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax)

        super(Map_alpes, self).__init__()


class Map_pyrenees(_Map_massifs):

    def __init__(self, *args, **kw):
        self.area = 'pyrenees'
        self.width = 14
        self.height = 5.8
        self.latmin = 42.0
        self.latmax = 43.3
        self.lonmin = -1.9
        self.lonmax = 2.9
        self.legendpos = [0.9, 0.10, 0.03, 0.7]

        self.fig = plt.figure(figsize=(self.width, self.height))
        self.map = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax)

        super(Map_pyrenees, self).__init__()


class Map_corse(_Map_massifs):

    def __init__(self, *args, **kw):
        self.area = 'corse'
        self.width = 10
        self.height = 10
        self.latmin = 41.3
        self.latmax = 43.1
        self.lonmin = 8.4
        self.lonmax = 9.7
        self.legendpos = [0.81, 0.15, 0.03, 0.6]

        self.fig = plt.figure(figsize=(self.width, self.height))
        self.map = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax)

        super(Map_corse, self).__init__()


class Map_massif(_Map_massifs):

    def __init__(self, num_massif, *args, **kw):
        if num_massif <= 24:
            self.area = 'alpes'
        elif num_massif >= 64:
            self.area = 'pyrenees'
        else:
            self.area = 'corse'
        self.width = 10
        self.height = 10
        self.getshapes()
        self.get_map_dimensions(num_massif)

        self.fig = plt.figure(figsize=(self.width, self.height))
        self.map = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax)
        self.map.drawcoastlines(linewidth=1)
#        self.map.drawcountries()
        self.map.drawmapboundary()

    def get_map_dimensions(self, num_massif):

        self.dicLonLatMassif = self.getLonLatMassif()
        for i, massif in enumerate(self.shapes):
            num = self.records(i)[1]
            if num == num_massif:
                barycentre = self.dicLonLatMassif[num]
        self.lonmin = barycentre[0] - 0.6
        self.lonmax = barycentre[0] + 0.6
        self.latmin = barycentre[1] - 0.6
        self.latmax = barycentre[1] + 0.6

    def plot(self, xpt, ypt, label=None):
        super(Map_massif, self).plot(xpt, ypt, label)
        # Pour ajouter le nom du poste :
        plt.text(xpt, ypt, label)

