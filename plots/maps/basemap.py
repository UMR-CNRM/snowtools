#! /usr/bin/env python
# -*- coding: utf-8 -*-

import os
import matplotlib
from matplotlib.collections import PatchCollection
from matplotlib.patches import Polygon, Rectangle
import matplotlib.pyplot as plt
import mpl_toolkits.basemap.pyproj as pyproj
from mpl_toolkits.basemap import Basemap
import numpy as np
from utils import shapefile
from utils.infomassifs import infomassifs
from osgeo import osr

from plots.abstracts.figures import Mplfigure


class _Map_massifs(Mplfigure):
    """ Implicit class that defines a generic massif map"""

    legendok = False

    def __init__(self, *args, **kw):

        self.openfigure()
        self.map = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax)

        self.map.drawcoastlines(linewidth=1)
#        self.map.drawcountries()
        self.map.drawmapboundary()
        # Get massif shapes
        self.getshapes()

        self.dicLonLatMassif = self.getLonLatMassif()

    def openfigure(self):
        self.fig = plt.figure(figsize=(self.width, self.height))

    def save(self, *args, **kwargs):
        super(_Map_massifs, self).save(*args, **kwargs)
#         if hasattr(self, 'p'):
#             self.p.remove()
        if hasattr(self, 'text'):
            for textelm in self.text:
                textelm.remove()

        if hasattr(self, 'r'):
            self.r.remove()

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
        XYplot = list(zip(X, Y))

        num = self.records(index)[1]  # lecture de l'attribut num_opp

        # Barycentre du massif
        barycentre = dicLonLatMassif[num]
        Xbary, Ybary = self.map(barycentre[0], barycentre[1])

        return XYplot, Xbary, Ybary

    def getLonLatMassif(self):
        im = infomassifs()
        return infomassifs.getAllMassifLatLon(im)

    def getdeport(self, num):
        if num in list(self.deport.keys()):
            return self.deport[num]
        else:
            return (0, 0)

    def getTextColor(self, var, **kwargs):
        color = 'black'
        if 'seuiltext' in list(kwargs.keys()):
            if var >= kwargs['seuiltext']:
                color = 'white'
        return color

    def plot_background(self):

        mypatches = []
        self.dicLonLatMassif = self.getLonLatMassif()
        for i, massif in enumerate(self.shapes):
            XYplot, Xbary, Ybary = self.getxy(massif, self.dicLonLatMassif, i)  # @UnusedVariable
            mypatches.append(Polygon(XYplot, alpha=1.0, fill=False, visible=True))

        self.p = PatchCollection(mypatches)
        plt.gca().add_collection(self.p)

    def addpoints(self, lon, lat, labels=None, color='black', marker=None):
        x, y = self.map(lon, lat)
        # self.map.plot(x, y, marker='o', color="red")
        if labels is not None:
            for label, xpt, ypt in zip(labels, x, y):
                self.plot(xpt, ypt, label=label, color=color, marker=marker)
        else:
            for xpt, ypt in zip(x, y):
                self.plot(x, y, marker=marker, color=color)

    def plot(self, xpt, ypt, label=None, color=None, marker=None):

        if label is not None:
            self.map.plot(xpt, ypt, marker=marker, color=color)
            plt.text(xpt, ypt, label, color=color, horizontalalignment='center', verticalalignment='baseline')
        else:
            self.map.plot(xpt, ypt, marker=marker, color=color, linestyle = 'None')

    def fillmassif(self, num_massif, value=None):
        for i, massif in enumerate(self.shapes):
            num = self.records(i)[1]
            if num == num_massif:
                XYplot, Xbary, Ybary = self.getxy(massif, self.dicLonLatMassif, i)  # @UnusedVariable
                if value is not None:
                    poly = Polygon(XYplot, facecolor=str(value), alpha=1.0, fill=True, visible=True)
                else:
                    poly = Polygon(XYplot, facecolor='0.9', alpha=1.0, fill=True, visible=True)
                plt.gca().add_patch(poly)

    def init_massifs(self, **kwargs):

        # This routine draw the contours of the polygons and initialize a colormap
        if 'palette' in list(kwargs.keys()):
            if 'ncolors' in list(kwargs.keys()):
                self.palette = plt.get_cmap(kwargs['palette'], kwargs['ncolors'])
            else:
                self.palette = plt.get_cmap(kwargs['palette'])
        else:
            self.palette = plt.get_cmap('jet')

        self.palette.set_bad(color='grey')
        self.palette.set_under(color='grey')

        self.norm = self.normpalette(**kwargs)

        mypatches = []
        for i, massif in enumerate(self.shapes):
            XYplot, Xbary, Ybary = self.getxy(massif, self.dicLonLatMassif, i)
            mypatches.append(Polygon(XYplot))

        self.p = PatchCollection(np.array(mypatches), cmap=self.palette, alpha=1.0)
        self.p.set_norm(self.norm)

        plt.gca().add_collection(self.p)

    def highlight_massif(self, massifs, fillvalues, **kwargs):
        mypatches = []
        myvalues = []
        if not isinstance(massifs, list):
            massifs = [massifs, ]

        if not isinstance(fillvalues, list):
            fillvalues = [fillvalues, ]

        for i, shape in enumerate(self.shapes):
            if self.records(i)[1] in massifs:
                num = self.records(i)[1]
                indmassif = massifs == num
                XYplot, Xbary, Ybary = self.getxy(shape, self.dicLonLatMassif, i)
                mypatches.append(Polygon(XYplot))
                myvalues.append(fillvalues[indmassif])

        self.m = PatchCollection(np.array(mypatches), cmap=self.palette, alpha=1.0)
        self.m.set_edgecolor('red')
        self.m.set_facecolor(self.palette(self.norm(np.array(myvalues))))

        plt.gca().add_collection(self.m)

    def empty_massifs(self, **kwargs):
        self.p.set_facecolor('white')

    def draw_massifs(self, massifref, variablein, **kwargs):
        # This routine fills the polygons with a color
        # depending on the value of variablein associated with the massif number provided in massifref
        # It is not required to provide a value for all massifs
        if 'convert_unit' in list(kwargs.keys()):
            variable = variablein[:] * kwargs['convert_unit']
        else:
            variable = variablein[:]

        myvalues = []
        for i, massif in enumerate(self.shapes):  # @UnusedVariable
            num = self.records(i)[1]
            indmassif = massifref == num

            if np.sum(indmassif) == 1:
                myvalues.append(variable[indmassif][0])
            else:
                myvalues.append(-999)

        self.p.set_array(np.array(myvalues))

        if not self.legendok:
            self.legend(self.p, **kwargs)

    def reset_massifs(self, **kwargs):
        self.p.remove()
        self.legendok = False
        if hasattr(self, 'infos'):
            for elm in self.infos:
                elm.remove()

    def plot_center_massif(self, massifref, *args, **kwargs):

        nvar = len(args)

        listvar = self.convertunit(*args, **kwargs)
        formatString = self.getformatstring(**kwargs)

        self.text = []

        textcolor = 'black'

        for i, massif in enumerate(self.shapes):
            num = self.records(i)[1]

            indmassif = massifref == num

            XYplot, Xbary, Ybary = self.getxy(massif, self.dicLonLatMassif, i)
            if np.sum(indmassif) == 1:
                infos = ''
                for v, variable in enumerate(listvar):
                    infos += formatString % variable[indmassif][0]
                    if v < nvar - 1:
                        infos += "-"

                    if (nvar == 3 and v == 1 ) or nvar == 1:
                        textcolor = self.getTextColor(variable[indmassif][0], **kwargs)

                self.text.append(plt.text(Xbary, Ybary, infos, horizontalalignment='center', verticalalignment='center', color = textcolor))

    def rectangle_massif(self, massifref, list_quantiles, list_values, ncol=1, **kwargs):
        width = 50000.
        height = 20000.

        if 'palette' in list(kwargs.keys()):
            if 'ncolors' in list(kwargs.keys()):
                self.palette = plt.get_cmap(kwargs['palette'], kwargs['ncolors'])
            else:
                self.palette = plt.get_cmap(kwargs['palette'])
        else:
            self.palette = plt.get_cmap('jet')

        self.palette.set_bad(color='grey')
        self.palette.set_under(color='grey')

        self.norm = self.normpalette(**kwargs)

        nvar = len(list_values)
        nrows = nvar / ncol
        listvar = self.convertunit(*list_values, **kwargs)
        formatString = self.getformatstring(**kwargs)

        self.text = []
        mypatches = []
        myvalues = []

        for i, massif in enumerate(self.shapes):
            num = self.records(i)[1]

            indmassif = massifref == num

            XYplot, Xbary, Ybary = self.getxy(massif, self.dicLonLatMassif, i)
            if np.sum(indmassif) == 1:

                (xdeport, ydeport) = self.getdeport(num)

                for i, thisvar in enumerate(listvar):
                    myvalues.append(thisvar[indmassif][0])
                    infos = formatString % thisvar[indmassif][0]
                    xmin = Xbary - width / 2. + (1. * i % nrows) / nrows * width + xdeport
                    ymin = Ybary - height / 2. + (1. * (i / nrows)) / ncol * height + ydeport
                    partialwidth = width / nrows
                    partialheight = height / ncol

#                     print massif
#                     print xmin, ymin, partialwidth, partialheight, nrows, ncol

                    mypatches.append(Rectangle((int(xmin), int(ymin)), int(partialwidth), int(partialheight)))

                    textcolor = self.getTextColor(thisvar[indmassif][0], **kwargs)

                    self.text.append(plt.text(xmin + partialwidth / 2, ymin + partialheight / 2, infos, horizontalalignment='center',
                                              verticalalignment='center', color=textcolor))
#                     if i == 3:
#                         self.text.append(plt.text(xmin - partialwidth / 2, ymin , str(num), horizontalalignment='center', verticalalignment='center'))

        self.r = PatchCollection(mypatches, cmap=self.palette, alpha=1.0)

        self.r.set_norm(self.norm)

        plt.gca().add_collection(self.r)

        self.r.set_array(np.array(myvalues))

        if not self.legendok:
            self.legend(self.r, **kwargs)

    def convertunit(self, *args, **kwargs):
        listvar = []
        for variablein in args:
            if 'convert_unit' in list(kwargs.keys()):
                variable = variablein[:] * kwargs['convert_unit']
            else:
                variable = variablein[:]
            listvar.append(variable)
        return listvar

    def getformatstring(self, **kwargs):
        if 'format' in list(kwargs.keys()):
            return kwargs['format']
        else:
            return '%i'

    def legend(self, polygons, **kwargs):

        currentaxis = plt.gca()

        cax = self.fig.add_axes(self.legendpos)
        self.cbar = self.fig.colorbar(polygons, cax=cax)

        if 'ticks' in list(kwargs.keys()):
            self.cbar.set_ticks(list(range(0, len(kwargs['ticks']))))
            self.cbar.set_ticklabels(kwargs['ticks'])
            fontsize = 10
        else:
            fontsize = 20

        for t in self.cbar.ax.get_yticklabels():
            t.set_fontsize(fontsize)

        if 'label' in list(kwargs.keys()):
            self.cbar.set_label(kwargs['label'], fontsize=20)

        plt.sca(currentaxis)

        self.legendok = True

    def add_north_south_info(self):
        if hasattr(self, "infoswidth"):
            width = self.infoswidth
        else:
            width = 80000
        if hasattr(self, "infosheight"):
            height = self.infosheight
        else:
            height = 60000

        self.infos = []

        self.infos.append(Rectangle(self.infospos, width, height / 2, fill=False))
        self.infos.append(Rectangle((self.infospos[0], self.infospos[1] + height / 2), width, height / 2, fill=False))

        for rect in self.infos:
            plt.gca().add_patch(rect)

        self.infos.append(plt.text(self.infospos[0] + 5000, self.infospos[1] + height * (5. / 6.), "Versant Nord"))
        self.infos.append(plt.text(self.infospos[0] + 5000, self.infospos[1] + height * (4. / 6.), "Q20 - Q50 -Q80"))

        self.infos.append(plt.text(self.infospos[0] + 5000, self.infospos[1] + height * (2. / 6.), "Versant Sud"))
        self.infos.append(plt.text(self.infospos[0] + 5000, self.infospos[1] + height * (1. / 6.), "Q20 - Q50 -Q80"))

    def normpalette(self, **kwargs):
        # Bornes pour légende
        if 'forcemin' in list(kwargs.keys()):
            vmin = kwargs['forcemin']
        else:
            vmin = 0
        if 'forcemax' in list(kwargs.keys()):
            vmax = kwargs['forcemax']
        else:
            vmax = 100

        return matplotlib.colors.Normalize(vmax=vmax, vmin=vmin)

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
#         self.latmin = 43.85
#         self.latmax = 46.6  # 46.5
#         self.lonmin = 5.1
#         self.lonmax = 8.1  # 7.9

        self.latmin = 43.9
        self.latmax = 46.5
        self.lonmin = 5.2
        self.lonmax = 7.9

        self.legendpos = [0.81, 0.15, 0.03, 0.6]
        self.infospos = (205000, 330000)

        self.deport = {2: (-10000, 0), 3: (10000, 0), 6: (20000, 0), 7: (-20000, 10000), 9: (15000, -10000), 11: (15000, -10000),
                       13: (15000, 0), 17: (15000, 0), 18: (-20000, -10000), 19: (0, -5000), 20: (0, -5000), 21: (0, -10000)}

        self.fig = plt.figure(figsize=(self.width, self.height))
        self.map = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax)

        super(Map_alpes, self).__init__()


class Map_pyrenees(_Map_massifs):

    def __init__(self, *args, **kw):
        self.area = 'pyrenees'
        self.width = 14
        self.height = 5.8
#         self.latmin = 42.0
#         self.latmax = 43.3
#         self.lonmin = -1.9
#         self.lonmax = 2.9

        self.latmin = 42.1
        self.latmax = 43.5
        self.lonmin = -2.0
        self.lonmax = 3.0

        self.legendpos = [0.9, 0.10, 0.03, 0.7]
        self.infospos = (450000, 140000)

        self.deport = {67: (10000, 20000), 68: (0, 5000), 69: (0, 10000), 72: (20000, 20000), 74: (25000, 0), 82: (-15000, -40000),
                       84: (-10000, -30000), 85: (0, -5000), 87: (-5000, -40000), 88: (25000, -5000), 89: (15000, -15000),
                       90: (-25000, 5000), 91: (10000, -5000)}

        super(Map_pyrenees, self).__init__()


class Map_corse(_Map_massifs):

    def __init__(self, *args, **kw):
        self.area = 'corse'
        self.width = 10
        self.height = 10
        self.latmin = 41.3
        self.latmax = 43.1
        self.lonmin = 8.4
        self.lonmax = 9.6

        self.legendpos = [0.81, 0.15, 0.03, 0.6]
        self.infospos = (15000, 215000)

        self.infoswidth = 50000
        self.infosheight = 50000

        self.deport = {}

        super(Map_corse, self).__init__()


class Map_massif(_Map_massifs):

    def __init__(self, num_massif, *args, **kw):
        if num_massif <= 24:
            self.area = 'alpes'
            self.width = 7.5
            self.height = 9
            self.legendpos = [0.91, 0.15, 0.03, 0.6]
        elif num_massif >= 64:
            self.area = 'pyrenees'
            self.width = 10
            self.height = 6
            self.legendpos = [0.91, 0.15, 0.03, 0.6]
        else:
            self.area = 'corse'
            self.width = 6.5
            self.height = 7.5
            self.legendpos = [0.89, 0.15, 0.03, 0.6]

        self.getshapes()
        self.get_map_dimensions(num_massif)

        self.fig = plt.figure(figsize=(self.width, self.height))

        self.fig.subplots_adjust(bottom=0.005, left=0.005, top=0.95, right=0.9)


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
        if self.area in ['alpes', 'corse']:
            dlat = 0.55
            dlon = 0.65
        elif self.area == 'pyrenees':
            dlat = 0.5
            dlon = 1.3

        self.lonmin = barycentre[0] - dlon
        self.lonmax = barycentre[0] + dlon
        self.latmin = barycentre[1] - dlat
        self.latmax = barycentre[1] + dlat
