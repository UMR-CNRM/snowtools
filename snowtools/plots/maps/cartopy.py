#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 29 march 2021

:Authors:
    radanovics

Module for map plots with massifs.
This module might be sensitive to the combination of versions of matplotlib and cartopy.
developed with matplotlib 3.4.0/3.2.1 and cartopy 0.18
which cartopy version is based on which matplotlib version? (according to documentation)
cartopy 0.19 -> matplotlib 3.4.1
cartopy 0.18 -> matplotlib 3.2.1
cartopy 0.17 -> matplotlib 3.0.2
cartopy 0.16 -> matplotlib 2.1.2
cartopy 0.15 -> matplotlib 2.0.0
cartopy 0.14 -> matplotlib 1.5.1
cartopy 0.13 -> matplotlib 1.4.3

Usage :
example :
create a Map instance for the Alps. Map_alpes can take optional kwargs.
m = Map_alpes(kwargs)
m = Map_alpes(geofeatures=True)
with geofeatures = True, borders, rivers and lakes are drawn on the map, the land and ocean polygons are colored.
at the first use cartopy tries to download the necessary data on the fly and saves them.
If this doesn't succeed for some reason (proxy or certificate issues for example), you can manually download the
shapefiles from NaturalEarth https://www.naturalearthdata.com/ and store them in cartopys 'data_dir'.
To see where cartopy will look for the data do :
from cartopy import config
print(config['data_dir'])
The result might be for example :
$HOME/.local/share/cartopy (I'll abbreviate with $data_dir)
files containing borders are then stored in
$data_dir/shapefiles/natural_earth/cultural/
files containing land, ocean river and lake features are stored in
$data_dir/shapefiles/natural_earth/physical/

m = MapFrance(bgimage=True)
with bgimage=True a background image from Natural Earth is added showing the relief.
Can not be used if geofeatures=True.

m.init_massifs(palette='Reds')


"""

import os
import numpy as np
import itertools
import matplotlib
# print(matplotlib.rcParams["savefig.dpi"])
# print(matplotlib.rcParams)
import matplotlib.pyplot as plt
# from matplotlib import style
# style.use('fast')
# matplotlib.rcParams["patch.antialiased"] = False
# matplotlib.rcParams["text.antialiased"] = False
# matplotlib.rcParams["lines.antialiased"] = False
# matplotlib.rcParams["image.resample"] = False
import cartopy.crs as ccrs
import cartopy.io.shapereader as shpreader
import cartopy.feature
from snowtools.plots.abstracts.figures import Mplfigure
from snowtools.utils.infomassifs import infomassifs
from pyproj import CRS
from cartopy import config
from snowtools.DATA import SNOWTOOLS_DIR, CARTOPY_DIR, LUSTRE_NOSAVE_USER_DIR

# Tell cartopy where to find Natural Earth features
# config['data_dir'] = os.path.join(SNOWTOOLS_DIR, 'CartopyData')
if os.path.isdir(CARTOPY_DIR):
    config['data_dir'] = CARTOPY_DIR
# config['data_dir'] = os.path.join(LUSTRE_NOSAVE_USER_DIR, 'CartopyData')  # for sxcen
# until proper git annex solution

# dummy class in order to be able to create an ccrs.CRS instance from a proj4/fiona.crs dictionary
class MyCRS(ccrs.CRS):
    def __init__(self, projdict, globe):
        if projdict['proj'] == 'lcc':
            ccrs.LambertConformal(central_longitude=projdict['lon_0'], central_latitude=projdict['lat_0'],
                                         false_easting=projdict['x_0'], false_northing=projdict['y_0'],
                                         standard_parallels=(projdict['lat_1'], projdict['lat_2']), globe=globe)
        else:
            pass


class _Map_massifs(Mplfigure):
    """ Implicit class that defines a generic massif map"""
    legendok = False

    def __init__(self, *args, **kw):
        # print(kw)
        # Get massif shapes
        self.titlepad = 15
        self.getshapes()
        # self.projection = MyCRS(self.shpProj, ccrs.Globe(ellipse='clrk80'))
        if self.shpProj['proj'] == 'lcc':
            self.projection = ccrs.LambertConformal(central_longitude=self.shpProj['lon_0'],
                                                central_latitude=self.shpProj['lat_0'],
                              false_easting=self.shpProj['x_0'], false_northing=self.shpProj['y_0'],
                              standard_parallels=(self.shpProj['lat_1'], self.shpProj['lat_2']))
        else:
            raise NotImplementedError('only LambertConformal projection is implemented for the massif shapes')
        # self.projection = ccrs.Projection(self.shpProj, ccrs.Globe(ellipse='clrk80'))
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
        if 'bgimage' in kwargs.keys():
            self.bgimage = kwargs['bgimage']
        else:
            self.bgimage = False
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
            #ax.add_feature(cartopy.feature.NaturalEarthFeature('cultural', 'admin_0_boundary_lines_land', '10m',
            #                                                   facecolor='none', linestyle=':'))
            ax.add_feature(cartopy.feature.LAKES, alpha=0.5)
            ax.add_feature(cartopy.feature.RIVERS)
        elif self.bgimage:
            os.environ["CARTOPY_USER_BACKGROUNDS"] = os.path.join(os.environ['SNOWTOOLS_CEN'], 'DATA')
            ax.background_img(resolution="high")
        return ax

    def openfigure(self):
        self.fig = plt.figure(figsize=(self.width, self.height))

    #@echecker.disabled_if_unavailable
    def getshapes(self):
        """
        read shapefile and return projection and records

        :return: shapefile, pprojcrs, shpProj, records
        """
        shapefile_path = os.path.join(SNOWTOOLS_DIR, 'DATA')
        #filename = 'massifs_{0:s}.shp'.format(self.area)
        filename = 'massifs_Lbrt93_2019.shp'
        self.shapefile = shpreader.Reader(os.path.join(shapefile_path, filename))
        # Informations sur la projection
        projfile = 'massifs_Lbrt93_2019.prj'
        with open(os.path.join(shapefile_path, projfile), 'r') as prj_file:
            prj_txt = prj_file.read()
            self.pprojcrs = CRS.from_wkt(prj_txt)

        # Projection du shapefile
        self.shpProj = self.pprojcrs.to_dict()

        # géométries
        # records is a generator object. Each record contains a geometry, its attributes and bounds
        self.records = [record for record in self.shapefile.records()]

    def getLonLatMassif(self):
        """returns dict with key = Massif Number, value = (lon, lat) """
        im = infomassifs()
        return infomassifs.getAllMassifLatLon(im)

    def init_cmap(self, **kwargs):
        if 'palette' in kwargs.keys():
            if 'ncolors' in kwargs.keys():
                if matplotlib.__version__ >= '3.4':
                    # deprecation warning in version 3.3, but copy method not yet implemented
                    self.palette = plt.get_cmap(kwargs['palette'], kwargs['ncolors']).copy()
                else:
                    self.palette = plt.get_cmap(kwargs['palette'], kwargs['ncolors'])
            else:
                if matplotlib.__version__ >= '3.4':
                    self.palette = plt.get_cmap(kwargs['palette']).copy()
                else:
                    self.palette = plt.get_cmap(kwargs['palette'])
        else:
            if matplotlib.__version__ >= '3.4':
                self.palette = plt.get_cmap('jet').copy()
            else:
                self.palette = plt.get_cmap('jet')

        self.palette.set_bad(color='grey')
        self.palette.set_under(color='grey')

        self.norm = self.normpalette(**kwargs)
        self.legendok = False

    def init_massifs(self, **kwargs):
        # This routine initializes a colormap, and adds the massif contours to the map

        self.init_cmap(**kwargs)
        if not hasattr(self, 'massif_features'):
            self.num, self.shape, self.name = map(list, zip(*[(rec.attributes['code'], rec.geometry,
                                                     rec.attributes['title']) for rec in self.records]))
            self.llshape = [ccrs.PlateCarree().project_geometry(ishape, self.projection) for ishape in self.shape]
            # get renderer
            if not hasattr(self, 'renderer'):
                self.fig.canvas.draw()
                self.renderer = self.fig.canvas.renderer
            if hasattr(self, 'nsubplots'):
                self.massif_features = list()
                for i in range(self.nsubplots):
                    self.massif_features.append([{'feature': self.maps.flat[i].add_geometries([ishape], crs=self.projection,
                                                                                            facecolor='none', cmap=self.palette,
                                                                                            edgecolor='dimgrey', alpha=1.0),
                                                'massifnum':inum, 'massifname':iname,
                                                'massifbb': self.get_massif_bb(inum, ishape, self.maps.flat[i])}
                                               for inum, ishape, iname in zip(self.num, self.shape, self.name)])
                print(len(self.massif_features))
            else:
                self.massif_features = [{'feature': self.map.add_geometries([lshape], crs=ccrs.PlateCarree(),  # crs=self.projection,
                                                                            cmap=self.palette,
                                                                            facecolor='none', edgecolor='dimgrey', alpha=1.0),
                                         'massifnum':inum, 'massifname':iname,
                                         'massifbb': self.get_massif_bb(inum, ishape, self.map)}
                                        for inum, ishape, lshape, iname in zip(self.num, self.shape, self.llshape, self.name)]

            # print(self.massif_features[0]['feature']._feature.kwargs)
            # self.massif_features[0]['feature']._kwargs['facecolor'] = 'blue'
            # plt.show()
            # print(self.massif_features[0]['feature']._feature.transform)
            # print('massif features added')
            #print(self.map.get_extent(self.projection))

    def get_massif_bb(self, num, shape, mymap):
        # print('enter massif_bb')
        width = 50.
        height = 20.
        Xbary, Ybary = shape.centroid.coords[0]
        (xdeport, ydeport) = self.getdeport(num)
        # print(self.projection._as_mpl_transform(mymap))
        mlon, mlat = ccrs.PlateCarree().transform_point(Xbary+xdeport, Ybary+ydeport, self.projection)
        bb = matplotlib.offsetbox.AnnotationBbox(matplotlib.offsetbox.DrawingArea(width, height),
                                                 (mlon, mlat), box_alignment=(0.5, 0.5),
                                                 xycoords=mymap.transData,
                                                 #xycoords=self.projection._as_mpl_transform(mymap),
                                                 bboxprops=dict(fc='none'))
        # print('bb done')
        # print(Xbary,Ybary)
        # print(ccrs.PlateCarree().transform_point(Xbary,Ybary,self.projection))
        # print(self.projection._as_mpl_transform(mymap))
        mapx0, mapy0, mapwidth, mapheight = mymap.get_window_extent(self.renderer).bounds
        # print(mapx0)
        # bb._get_position_xy(self.renderer)
        #bb.offsetbox.draw(self.renderer)
        #print('offsetbox drawn')
        bb.draw(self.renderer)
        # print('bb drawn')
        # bbpatch = bb.get_bbox_patch()
        # print(bb.get_window_extent(self.renderer))
        # print(bb.patch.get_bbox().bounds)
        # x0, y0, w, h = bb.get_window_extent(self.renderer).bounds # matplotlib 3.4
        x0, y0, w, h = bb.patch.get_bbox().bounds
        # print(mapx0, mapy0, mapwidth, mapheight, x0, y0, w, h)
        # print('bbox done')
        return [(x0-mapx0)/mapwidth, (y0-mapy0)/mapheight, w/mapwidth, h/mapheight]

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
        self.legendok = False

        return matplotlib.colors.Normalize(vmax=self.vmax, vmin=self.vmin)

    def draw_massifs(self, massifref, variablein, **kwargs):
        # This routine fills the polygons with a color
        # depending on the value of variablein associated with the massif number provided in massifref
        # It is not required to provide a value for all massifs
        # print(kwargs.keys())
        if 'convert_unit' in kwargs.keys():
            variable = variablein[:] * kwargs['convert_unit']
            # print(variable)
            # print(variablein)
        else:
            variable = variablein[:]

        # if massif contours are not yet created, draw them
        if not hasattr(self, 'massif_features'):
            self.init_massifs(**kwargs)

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
            myvalues = np.array([variable[massifref==i][0] if i in massifref else np.nan for i in self.num])
            print(myvalues.shape)
            for j in range(len):
                for i, myvalue in enumerate(myvalues.take(indices=j, axis=axis)):
                    self.massif_features[j][i]['feature']._kwargs['facecolor'] = self.palette(self.norm(myvalue))
        else:
            myvalues = [variable[massifref==i][0] if i in massifref else np.nan for i in self.num]
            for i, myvalue in enumerate(myvalues):
                self.massif_features[i]['feature']._kwargs['facecolor'] = self.palette(self.norm(myvalue))

       # prepare colorbar
        if not self.legendok:
            self.m = plt.cm.ScalarMappable(cmap=self.palette)
            self.m.set_array(np.array(myvalues))
            self.m.set_clim(self.vmin, self.vmax)
            self.legend(self.m, **kwargs)

        # plt.show()

    def empty_massifs(self, **kwargs):
        if hasattr(self, 'map'):
            for feature in self.massif_features:
                feature['feature']._kwargs['facecolor'] ='white'
                # feature['feature'].set_zorder(1)
                # print(feature['feature']._kwargs.get('zorder'))
        elif hasattr(self, 'maps'):
            for features in self.massif_features:
                for feature in features:
                    feature['feature']._kwargs['facecolor'] ='white'

    def addpoints(self, lon, lat, labels=None, color='black', marker=None):
        if labels is not None:
            for label, x, y in zip(labels, lon, lat):
                self.map.annotate(label, (x, y), color=color, zorder=4)
        else:
            self.map.plot(lon, lat, marker=marker, color=color, linestyle='', zorder=3)

    def highlight_massif(self, massifs, fillvalues, **kwargs):

        # if massif contours are not yet created, draw them
        if not hasattr(self, 'massif_features'):
            self.init_massifs(**kwargs)

        if not isinstance(massifs, list):
            massifs = [massifs, ]

        if isinstance(self, _MultiMap):
            for i, massif in enumerate(self.records):
                if massif.attributes['code'] in massifs:
                    for j in range(self.nsubplots):
                        self.massif_features[j][i]['feature'].set_zorder(2)  # Pour tracer le massif en dernier
                        self.massif_features[j][i]['feature']._kwargs['edgecolor'] = 'red'
        else:
            for i, massif in enumerate(self.records):
                if massif.attributes['code'] in massifs:
                    self.massif_features[i]['feature'].set_zorder(2)  # Pour tracer le massif en dernier
                    self.massif_features[i]['feature']._kwargs['edgecolor'] = 'red'

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
        self.legendok = True

    def set_maptitle(self, title):
        """Set title on top of the map"""
        self.map.set_title(title, fontsize=20, pad=self.titlepad)

    set_title = set_maptitle

    def set_figtitle(self, title):
        """Set title on top of the figure"""
        self.fig.suptitle(title, fontsize=20)

    set_suptitle = set_figtitle

    def draw_mesh(self, lons, lats, field, **kwargs):
        if 'convert_unit' in kwargs.keys():
            variable = field[:] * kwargs['convert_unit']
        else:
            variable = field[:]
        print(variable.max())
        self.map.pcolormesh(lons, lats, variable, transform=ccrs.PlateCarree(), cmap=self.palette, vmin=self.vmin,
                            vmax=self.vmax)
        # prepare colorbar
        if not self.legendok:
            self.m = plt.cm.ScalarMappable(cmap=self.palette)
            self.m.set_array(np.array(variable))
            self.m.set_clim(self.vmin, self.vmax)
            self.m.cmap.set_under(color='w', alpha=0)
            self.legend(self.m, **kwargs)

    def convertunit(self, *args, **kwargs):
        listvar = []
        for variablein in args:
            if 'convert_unit' in kwargs.keys():
                variable = variablein[:] * kwargs['convert_unit']
            else:
                variable = variablein[:]
            listvar.append(variable)
        return listvar

    def getformatstring(self, **kwargs):
        if 'format' in kwargs.keys():
            return kwargs['format']
        else:
            return '%i'

    def getTextColor(self, var, **kwargs):
        color = 'black'
        if 'seuiltext' in kwargs.keys():
            if var >= kwargs['seuiltext']:
                color = 'white'
        return color

    def plot_center_massif(self, massifref, *args, **kwargs):

        nvar = len(args)

        listvar = self.convertunit(*args, **kwargs)
        formatString = self.getformatstring(**kwargs)
        # print(listvar)
        self.text = []

        for i, massif in enumerate(self.shape):

            indmassif = massifref == self.num[i]
            # print(indmassif, self.num[i], massifref)
            Xbary, Ybary = massif.centroid.coords.xy
            if np.sum(indmassif) == 1:
                self.puttext(Xbary[0], Ybary[0], indmassif, listvar, nvar, formatString, **kwargs)

    def puttext(self, Xbary, Ybary, indmassif, listvar, nvar, formatString, **kwargs):
        """
        Put text on the maps for a given massif.

        :param Xbary: x-coordinate of the massif center
        :param Ybary: y-coordinate of the massif center
        :param indmassif: massif index
        :param listvar: list of values to write on the maps
        :param nvar: number of variables to plot at each center
        :param formatString: format specifier of the
        :param kwargs: 'textcolor', 'unit'

        """
        infos = ''
        for v, variable in enumerate(listvar):
            # print(formatString, variable[indmassif][0])
            infos += formatString % variable[indmassif][0]
            if v < nvar - 1:
                infos += "-"

            if 'textcolor' in kwargs.keys():
                textcolor = kwargs['textcolor']
            elif (nvar == 3 and v == 1) or nvar == 1:
                textcolor = self.getTextColor(variable[indmassif][0], **kwargs)
                # print("textcolor variable", textcolor)
            else:
                textcolor = 'black'

        if 'unit' in kwargs.keys():
            infos += kwargs['unit']

        self.text.append(self.map.text(Xbary, Ybary, infos, transform=self.projection,
                                       horizontalalignment='center', verticalalignment='center',
                                       color=textcolor))
        # print(self.map.properties())

    def reset_massifs(self, rmcbar=True, rminfobox=True, **kwargs):
        # self.legendok = False
        if hasattr(self, 'map'):
            # remove tables
            for prop in self.map.properties()['children']:
                if (type(prop) == matplotlib.table.Table):
                    prop.remove()
            # remove text
                elif type(prop) == matplotlib.text.Text:
                    if hasattr(self, 'text'):
                        if prop in self.text:
                            prop.remove()
        elif hasattr(self, 'maps'):
            for m in self.maps.flat:
                # remove tables
                for prop in m.properties()['children']:
                    if (type(prop) == matplotlib.table.Table):
                        prop.remove()
                    # remove text
                    elif type(prop) == matplotlib.text.Text:
                        if hasattr(self, 'text'):
                            if prop in self.text:
                                prop.remove()
        # remove info boxs
        if hasattr(self, 'infos') & rminfobox:
            for elm in self.infos:
                elm.remove()
        # remove colorbar
        if hasattr(self, 'cbar') & rmcbar:
            try:
                self.cbar.remove()
            except(ValueError):
                pass
            self.legendok = False

    def add_north_south_info(self, english=False):

        self.infos = []
        if english:
            north_text = "Northen slope \n Q20 - Q50 - Q80"
            south_text = "Southern slope \n Q20 - Q50 - Q80"
        else:
            north_text = "Versant Nord \n Q20 - Q50 - Q80"
            south_text = "Versant Sud \n Q20 - Q50 - Q80"
        # self.infos.append(box(8.5, 42.9,8.85,43.1))

        # self.infos.append(self.map.add_artist(matplotlib.offsetbox.AnnotationBbox(
        #     matplotlib.offsetbox.TextArea(north_text, textprops=dict(horizontalalignment='left')),  # center in matplotlib 3.4
        #     self.infospos, box_alignment=(0, 0), xycoords=self.projection._as_mpl_transform(self.map),
        #     bboxprops=dict(fc='none'))))
        self.infos.append(self.map.add_artist(matplotlib.offsetbox.AnnotationBbox(
            matplotlib.offsetbox.TextArea(north_text, textprops=dict(horizontalalignment='left')),  # center in matplotlib 3.4
            self.infospos, box_alignment=(0, 0),
            bboxprops=dict(fc='none'))))
        self.infos.append(self.map.add_artist(matplotlib.offsetbox.AnnotationBbox(
            matplotlib.offsetbox.TextArea(south_text, textprops=dict(horizontalalignment='left')),  # center in matplotlib 3.4
            self.infospos, box_alignment=(0, 1.4),
            bboxprops=dict(fc='none'))))

    def rectangle_massif(self, massifref, list_quantiles, list_values, ncol=1, **kwargs):

        # define color palette
        # self.init_cmap(**kwargs)

        ncol = ncol+1
        nvar = len(list_values)
        nrows = int(nvar / ncol)
        listvar = self.convertunit(*list_values, **kwargs)
        formatString = self.getformatstring(**kwargs)

        for i, massif in enumerate(self.shape):
            indmassif = massifref == self.num[i]
            if np.sum(indmassif) == 1:
                # print(self.massif_features[i]['massifbb'])
                if 'axis' in kwargs.keys() and hasattr(self, 'nsubplots') and hasattr(self, 'maps'):
                    axis = kwargs['axis']
                    try:
                        subplotdim = listvar[0].shape[axis]
                    except(IndexError):
                        raise Exception("value array has lower rank than given axis number")
                    if subplotdim > self.nsubplots:
                        print("Warning: axis ", axis, " of value array is longer than number of subplots ", self.nsubplots,
                              ". Plotting first ", self.nsubplots, " out of ", subplotdim, ".")
                        subplotdim = self.nsubplots
                    for j in range(subplotdim):
                        # create text array
                        infos = np.flipud(np.array([formatString % thisvar.take(indices=j, axis=axis)[indmassif][0] for thisvar in listvar]).reshape((nrows, ncol)))
                        # create color array
                        tmp_colors = [self.palette(self.norm(thisvar.take(indices=j, axis=axis)[indmassif][0])) for thisvar in listvar]
                        colors = np.array([tmp_colors[-ncol:] if irows==0 else tmp_colors[-(irows*ncol)-ncol:-(irows*ncol)] for irows in range(nrows)])
                        art = matplotlib.table.table(self.maps.flat[j], cellText=infos, cellColours=colors, cellLoc='center', colWidths=None,
                                                     rowLabels=None, rowColours=None, rowLoc='left', colLabels=None, colColours=None,
                                                     colLoc='center',
                                                     loc='bottom',
                                                     bbox=self.massif_features[j][i]['massifbb'],
                                                     edges='closed', zorder=10)
                else:
                    # create text array
                    infos = np.flipud(np.array([formatString % thisvar[indmassif][0] for thisvar in listvar]).reshape((nrows, ncol)))
                    # create color array
                    tmp_colors = [self.palette(self.norm(thisvar[indmassif][0])) for thisvar in listvar]
                    colors = np.array([tmp_colors[-ncol:] if irows==0 else tmp_colors[-(irows*ncol)-ncol:-(irows*ncol)] for irows in range(nrows)])

                    art = matplotlib.table.table(self.map, cellText=infos, cellColours=colors, cellLoc='center', colWidths=None,
                                                 rowLabels=None, rowColours=None, rowLoc='left', colLabels=None, colColours=None,
                                                 colLoc='center',
                                                 loc='bottom',
                                                 bbox=self.massif_features[i]['massifbb'],
                                                 edges='closed', zorder=10)
                    # art.set_fontsize(8)

        if not self.legendok:
            self.m = plt.cm.ScalarMappable(cmap=self.palette)
            self.m.set_array(np.array(listvar))
            self.m.set_clim(self.vmin, self.vmax)
            self.legend(self.m, **kwargs)
            self.legendok = True

    def getdeport(self, num):
        if num in self.deport.keys():
            return self.deport[num]
        else:
            return (0, 0)


class Map_vosges(_Map_massifs):
    """
     Class for plotting a map over the Vosges.
    """
    area = 'vosges'  #: area tag = 'vosges'
    width = 12  #: figure width = 12
    height = 10  #: figure height = 10
    latmin = 47.65  #: southern map border = 43.9
    latmax = 48.7  #: northen map border = 46.5
    lonmin = 6.55  #: western map border = 5.2
    lonmax = 7.45  #: eastern map border = 7.9

    mappos = [0.02, 0.06, 0.8, 0.8]  #: map position on the plot = [0.02, 0.06, 0.8, 0.8]
    legendpos = [0.85, 0.15, 0.03, 0.6]  #: legend position on the plot = [0.85, 0.15, 0.03, 0.6]
    #: position of info-box on the map in Lambert Conformal coordinates = (990000, 2160000)
    infospos = (6.6, 48.6)
    labelfontsize = 20  #: fontsize of colorbar label
    deport = {}
    """ displacement dictionary for the positioning tables near the massif center without overlapping."""

    def __init__(self, *args, **kw):
        """

        :param args: Arguments to be passed to super class
        :param kw: Keyword arguments to be passed to super class
        """
        super(Map_vosges, self).__init__(*args, **kw)


class Map_central(_Map_massifs):
    """
     Class for plotting a map over the Massif Central.
    """
    area = 'central'  #: area tag = 'central'
    width = 12  #: figure width = 12
    height = 10  #: figure height = 10
    latmin = 43.3  #: southern map border = 43.3
    latmax = 46.3  #: northen map border = 46.3
    lonmin = 1.6  #: western map border = 1.6
    lonmax = 4.8  #: eastern map border = 4.8

    mappos = [0.02, 0.06, 0.8, 0.8]  #: map position on the plot = [0.02, 0.06, 0.8, 0.8]
    legendpos = [0.85, 0.15, 0.03, 0.6]  #: legend position on the plot = [0.85, 0.15, 0.03, 0.6]
    #: position of info-box on the map in Lambert Conformal coordinates = (990000, 2160000)
    infospos = (4.3, 46.1)
    labelfontsize = 20  #: fontsize of colorbar label
    deport = {}
    """ displacement dictionary for the positioning tables near the massif center without overlapping."""

    def __init__(self, *args, **kw):
        """

        :param args: Arguments to be passed to super class
        :param kw: Keyword arguments to be passed to super class
        """
        super(Map_central, self).__init__(*args, **kw)


class Map_jura(_Map_massifs):
    """
     Class for plotting a map over the French Jura.
    """
    area = 'jura'  #: area tag = 'jura'
    width = 12  #: figure width = 12
    height = 10  #: figure height = 10
    latmin = 45.6  #: southern map border = 43.9
    latmax = 47.5  #: northen map border = 46.5
    lonmin = 5.3  #: western map border = 5.2
    lonmax = 7.15  #: eastern map border = 7.9

    mappos = [0.02, 0.06, 0.8, 0.8]  #: map position on the plot = [0.02, 0.06, 0.8, 0.8]
    legendpos = [0.85, 0.15, 0.03, 0.6]  #: legend position on the plot = [0.85, 0.15, 0.03, 0.6]
    #: position of info-box on the map in Lambert Conformal coordinates = (990000, 2160000)
    infospos = (5.4, 47.35)
    labelfontsize = 20  #: fontsize of colorbar label
    deport = {}
    """ displacement dictionary for the positioning tables near the massif center without overlapping."""

    def __init__(self, *args, **kw):
        """

        :param args: Arguments to be passed to super class
        :param kw: Keyword arguments to be passed to super class
        """
        super(Map_jura, self).__init__(*args, **kw)


class Map_alpes(_Map_massifs):
    """
    Class for plotting a map over the French Alps.

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/manto/viallonl/testbase/PRO/postproc/Alp/postproc_2021041006_2021041112.nc') as ff:
            points = ff.get_points(ZS=2100, aspect=-1)
            snow = ff.read('SD_1DY_ISBA', selectpoint=points, hasDecile=True)
            massifs = ff.read('massif_num', selectpoint=points)

        m = cartopy.Map_alpes(geofeatures=True)
        m.init_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.draw_massifs(massifs, snow[5, :, 8], convert_unit=100., forcemin=0., forcemax=50.,
                        palette='YlGnBu', seuiltext=50.,
                        label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.plot_center_massif(massifs, snow[5, :, 0], snow[5, :, 4], snow[5, :, 8], convert_unit=100.,
                                forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                                label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.addlogo()
        m.set_maptitle("2021041112 percentile 90")
        m.set_figtitle("2100m")
        plt.show()
        m.close()

    .. figure:: /images/2021041112_p90_alps_geofeatures.png
       :align: center
    """
    area = 'alpes'  #: area tag = 'alpes'
    width = 12  #: figure width = 12
    height = 10  #: figure height = 10
    latmin = 43.5  #: southern map border = 43.9
    latmax = 46.5  #: northen map border = 46.5
    lonmin = 5.0  #: western map border = 5.2
    lonmax = 7.9  #: eastern map border = 7.9

    mappos = [0.02, 0.06, 0.8, 0.8]  #: map position on the plot = [0.02, 0.06, 0.8, 0.8]
    legendpos = [0.85, 0.15, 0.03, 0.6]  #: legend position on the plot = [0.85, 0.15, 0.03, 0.6]
    #: position of info-box on the map in Lambert Conformal coordinates = (990000, 2160000)
    infospos = (7.3, 46.3)
    labelfontsize = 20  #: fontsize of colorbar label
    deport = {7: (0, 5000), 9: (-1000, 0), 16: (1000, 0), 19: (-2000, -2000), 21: (0, -5000)}
    """ displacement dictionary for the positioning tables near the massif center without overlapping."""

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
        self.infospos = (990000, 2160000)

        self.deport = {7: (0, 5000), 9: (-1000, 0), 16: (1000, 0), 19: (-2000, -2000), 21: (0, -5000)}

        # self.fig = plt.figure(figsize=(self.width, self.height))
        # self.map = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax)
        # print(kw)
        super(Map_alpes, self).__init__(*args, **kw)


class _MultiMap(_Map_massifs):

    def init_maps(self, *args, **kw):
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

    def get_massif_features(self):
        """
        construct massif features (geometries)

        :return: feature list of lists
        """
        features = list()
        for i in range(self.nsubplots):
            features.append([{'feature': self.maps.flat[i].add_geometries([ishape], crs=self.projection,
                                                                                    facecolor='none', cmap=self.palette,
                                                                                    edgecolor='dimgrey', alpha=1.0),
                              'massifnum':inum, 'massifname':iname,
                              'massifbb': self.get_massif_bb(inum, ishape, self.maps.flat[i])}
                               for inum, ishape, iname in zip(self.num, self.shape, self.name)])
        print(len(features))
        return features

    def fill_massifs(self, massifref, variable, **kwargs):
        """
        actually fill massifs

        :param massifref: massif numbers
        :param variable: values
        :param kwargs: 'axis': the dimension along which to split :py:attr:`variable` between different subplots
        :return: value array
        :rtype: numpy array
        :raises: KeyError if 'axis' is not in the :py:attr:`kwargs`, IndexError if :py:attr:`variable` array has lower
         rank than the number given to 'axis'.
        """
        try:
            axis = kwargs['axis']
        except KeyError:
            raise KeyError("axis keyword argument needed by the draw_massif method in the case of "
                           "multiple maps on the figure")
        try:
            leng = variable.shape[axis]
        except IndexError:
            raise Exception("value array has lower rank than given axis number")
        if leng > self.nsubplots:
            print("Warning: axis ", axis, " of value array is longer than number of subplots ", self.nsubplots,
                  ". Plotting first ", self.nsubplots, " out of ", leng, ".")
            leng = self.nsubplots
        print('var shape', variable.shape)
        #myvalues = np.array([variable[massifref == i][0] if i in massifref else np.nan for i in self.num], dtype=object)
        myvalues = np.array([variable[massifref == i][0] for i in self.num if i in massifref])
        print(myvalues.shape)
        for j in range(leng):
            for i, myvalue in enumerate(myvalues.take(indices=j, axis=axis)):
                self.massif_features[j][i]['feature']._kwargs['facecolor'] = self.palette(self.norm(myvalue))
        return myvalues

    def empty_massifs(self, **kwargs):
        """
        fill massifs in white

        :param kwargs:
        """
        for features in self.massif_features:
            for feature in features:
                feature['feature']._kwargs['facecolor'] ='white'

    def addpoints(self, lon, lat, labels=None, color='black', marker=None):
        """
        add some annotation to the maps. The same annotations are added to all maps.

        :param lon: longitude of annotation positions
        :type lon: list
        :param lat: latitude of annotation positions
        :type lat: list
        :param labels: list of labels for annotation, or 'None' if markers should be used.
        :type labels: list
        :param color: color of annotation text or marker
        :param marker: marker, or 'None'

        """
        if labels is not None:
            for label, x, y in zip(labels, lon, lat):
                for i in range(self.nsubplots):
                    self.maps.flat[i].annotate(label, (x, y), color=color, zorder=4)
        else:
            for i in range(self.nsubplots):
                self.maps.flat[i].plot(lon, lat, marker=marker, color=color, linestyle='', zorder=3)

    def puttext(self, Xbary, Ybary, indmassif, listvar, nvar, formatString, **kwargs):
        """
        Put text on the maps for a given massif.

        :param Xbary: x-coordinate of the massif center
        :param Ybary: y-coordinate of the massif center
        :param indmassif: massif index
        :param listvar: list of values to write on the maps
        :param nvar: number of variables to plot at each center
        :param formatString: format specifier of the
        :param kwargs: 'axis', 'textcolor', 'unit'

        """
        try:
            axis = kwargs['axis']
        except KeyError:
            raise KeyError("axis keyword argument needed by the draw_massif method in the case of "
                       "multiple maps on the figure")
        try:
            subplotdim = listvar[0].shape[axis]
        except IndexError:
            raise Exception("value array has lower rank than given axis number")
        if subplotdim > self.nsubplots:
            print("Warning: axis ", axis, " of value array is longer than number of subplots ", self.nsubplots,
                  ". Plotting first ", self.nsubplots, " out of ", subplotdim, ".")
        subplotdim = self.nsubplots
        for j in range(subplotdim):
            infos = ''
            # for i, myvalue in enumerate(myvalues.take(indices=j, axis=axis)):
            #     self.massif_features[j][i]['feature']._kwargs['facecolor'] = self.palette(self.norm(myvalue))
            for v, variable in enumerate(listvar):
                infos += formatString % variable.take(indices=j, axis=axis)[indmassif][0]
                if v < nvar - 1:
                    infos += "-"

                if 'textcolor' in kwargs.keys():
                    textcolor = kwargs['textcolor']
                elif (nvar == 3 and v == 1) or nvar == 1:
                    textcolor = self.getTextColor(variable.take(indices=j, axis=axis)[indmassif][0], **kwargs)
                else:
                    textcolor = 'black'

            if 'unit' in kwargs.keys():
                infos += kwargs['unit']

            self.text.append(self.maps.flat[j].text(Xbary, Ybary, infos, transform=self.projection,
                                                    horizontalalignment='center', verticalalignment='center',
                                                    color = textcolor))

    def puttable(self, i, indmassif, listvar, ncol, nrows, formatString, **kwargs):
        """
        Put tables with values and colored cells on the maps for a given massif.

        :param i: massif index
        :param indmassif: massif number
        :param listvar: list of values
        :param ncol: number of columns in the table
        :param nrows: number of rows in the table
        :param formatString: format string for values
        :param kwargs: axis
        """
        # print(self.massif_features[i]['massifbb'])
        try:
            axis = kwargs['axis']
        except KeyError:
            raise KeyError("axis keyword argument needed by the draw_massif method in the case of "
                           "multiple maps on the figure")
        try:
            subplotdim = listvar[0].shape[axis]
        except IndexError:
            raise Exception("value array has lower rank than given axis number")
        if subplotdim > self.nsubplots:
            print("Warning: axis ", axis, " of value array is longer than number of subplots ", self.nsubplots,
                  ". Plotting first ", self.nsubplots, " out of ", subplotdim, ".")
            subplotdim = self.nsubplots
        for j in range(subplotdim):
            # create text array
            infos = np.flipud(np.array([formatString % thisvar.take(indices=j, axis=axis)[indmassif][0]
                                        for thisvar in listvar]).reshape((nrows, ncol)))
            # create color array
            tmp_colors = [self.palette(self.norm(thisvar.take(indices=j, axis=axis)[indmassif][0]))
                          for thisvar in listvar]
            colors = np.array([tmp_colors[-ncol:] if irows == 0 else tmp_colors[-(irows*ncol)-ncol:-(irows*ncol)]
                               for irows in range(nrows)])
            art = matplotlib.table.table(self.maps.flat[j], cellText=infos, cellColours=colors, cellLoc='center',
                                         colWidths=None,
                                         rowLabels=None, rowColours=None, rowLoc='left', colLabels=None, colColours=None,
                                         colLoc='center',
                                         loc='bottom',
                                         bbox=self.massif_features[j][i]['massifbb'],
                                         edges='closed', zorder=10)

    def reset_massifs(self, rmcbar=True, rminfobox=True, **kwargs):
        """
        Remove tables, text and optionally the infobox and the colorbar from the maps.

        :param rmcbar: if True, colorbar is removed.
        :param rminfobox: if True, the infobox is removed.
        """

        for m in self.maps.flat:
            # remove tables
            for prop in m.properties()['children']:
                if type(prop) == matplotlib.table.Table:
                    prop.remove()
                # remove text
                elif type(prop) == matplotlib.text.Text:
                    if hasattr(self, 'text'):
                        if prop in self.text:
                            prop.remove()
        # remove info boxs
        if hasattr(self, 'infos') & rminfobox:
            for elm in self.infos:
                elm.remove()
        # remove colorbar
        if hasattr(self, 'cbar') & rmcbar:
            try:
                self.cbar.remove()
            except ValueError:
                pass
            self.legendok = False

    def add_north_south_info(self, english=False):
        """
        Add a legend box at the top of the map explaining the content of the tables added with
        :py:meth:`rectangle_massif`

        :param english: if True, the annotation is in English, otherwise in French.
        """
        self.infos = []
        if english:
            north_text = "Northen slope \n Q20 - Q50 - Q80"
            south_text = "Southern slope \n Q20 - Q50 - Q80"
        else:
            north_text = "Versant Nord \n Q20 - Q50 - Q80"
            south_text = "Versant Sud \n Q20 - Q50 - Q80"
        # self.infos.append(box(8.5, 42.9,8.85,43.1))

        for i in range(len(self.maps.flat)):
            self.infos.append(self.maps.flat[i].add_artist(matplotlib.offsetbox.AnnotationBbox(
                matplotlib.offsetbox.TextArea(north_text, textprops=dict(horizontalalignment='left',  # center in matplotlib 3.4
                                                                         size=5)), self.infospos,
                box_alignment=(0.2, 0),
                bboxprops=dict(fc='none'), pad=0.2)))
            self.infos.append(self.maps.flat[i].add_artist(matplotlib.offsetbox.AnnotationBbox(
                matplotlib.offsetbox.TextArea(south_text, textprops=dict(horizontalalignment='left',  # center in matplotlib 3.4
                                                                         size=5)), self.infospos,
                box_alignment=(0.2, 1.4),
                bboxprops=dict(fc='none'), pad=0.2)))

    def draw_mesh(self, lons, lats, field, **kwargs):
        """
        Draw a colormesh on the map. (For gridded data)
        Not yet implemented

        :param lons: mesh longitudes
        :param lats: mesh latitudes
        :param field: mesh values
        :param kwargs: 'convert_unit': factor for scaling :py:attr:`field`
        :raises: NotImplementedError

        """
        raise NotImplementedError

    def openfigure(self):
        self.fig, self.maps = plt.subplots(nrows=self.nrow, ncols=self.ncol, sharex=True, sharey=True,
                                       figsize=(self.width, self.height))

    def set_maptitle(self, title):
        """Set title on top of each subplot"""
        if len(title) == self.nsubplots:
            for i in range(self.nsubplots):
                self.maps.flat[i].set_title(title[i], fontsize=14, pad=self.titlepad)
        elif len(title) == 1:
            for i in range(self.nsubplots):
                self.maps.flat[i].set_title(title, fontsize=14, pad=self.titlepad)
        else:
            print("Warning: can not set map titles. len(title) must be either equal to the number of subplots or == 1.")

    set_title = set_maptitle


class MultiMap_Alps(Map_alpes, _MultiMap):
    """
    Class for plotting multiple massif plots of the French Alps

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/manto/viallonl/testbase/PRO/postproc/Alp/postproc_2021041006_2021041112.nc') as ff:
            points = ff.get_points(ZS=2100, aspect=-1)
            snow = ff.read('SD_1DY_ISBA', selectpoint=points, hasDecile=True)
            massifs = ff.read('massif_num', selectpoint=points)

        lo = cartopy.MultiMap_Alps(nrow=3, ncol=3, geofeatures=False)
        lo.init_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        lo.draw_massifs(massifs, snow[5, :, :], axis=1, convert_unit=100., forcemin=0., forcemax=50.,
                        palette='YlGnBu', seuiltext=50.,
                        label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        lo.highlight_massif(10, snow, convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu',
                            seuiltext=50., label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        lo.set_figtitle("SD_1DY_ISBA 2021041112 2100m")
        titles = ['Percentile {0}'.format(i) for i in range(10, 100, 10)]
        lo.set_maptitle(titles)
        lo.plot_center_massif(massifs, snow[5,:,:], axis=1,convert_unit=100., forcemin=0., forcemax=50.,
                                palette='YlGnBu', seuiltext=50.,
                                label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        lo.addlogo()
        plt.show()
        lo.close()

    .. figure:: /images/2021041112_multi_alps.png
       :align: center
    """
    legendpos = [0.9, 0.15, 0.03, 0.6]  #: legend position on the plot = [0.85, 0.15, 0.03, 0.6]

    def __init__(self, nrow=1, ncol=1, *args, **kw):
        kw['getmap'] = False
        self.nrow = nrow
        self.ncol = ncol
        self.nsubplots = nrow*ncol
        self.titlepad = 5
        super(MultiMap_Alps, self).__init__(*args, **kw)
        self.set_figsize(18, 15)
        self.init_maps(*args, **kw)


class Map_pyrenees(_Map_massifs):
    """
    Class to plot a map of the Pyrenees.

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/manto/viallonl/testbase/PRO/postproc/Pyr/postproc_2021041006_2021041112.nc') as ff:
            points_nord = ff.get_points(aspect=0, ZS=2100, slope=40)
            points_sud = ff.get_points(aspect=180, ZS=2100, slope=40)
            snow_nord = ff.read('SD_1DY_ISBA', selectpoint=points_nord, hasDecile=True)
            snow_sud = ff.read('SD_1DY_ISBA', selectpoint=points_sud, hasDecile=True)
            massifs = ff.read('massif_num', selectpoint=points_nord)

        m = cartopy.Map_pyrenees(geofeatures=True)
        m.init_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.add_north_south_info()
        m.rectangle_massif(massifs, [0, 1, 2], [snow_sud[1, :, 1], snow_sud[1, :, 4],
                                                snow_sud[1, :, 7], snow_nord[1, :, 1],
                                                snow_nord[1, :, 4], snow_nord[1, :, 7]], ncol=2,
                            convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu',
                            seuiltext=50., label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.addlogo()
        m.set_maptitle("2021041012")
        m.set_figtitle("2100m")
        plt.show()
        m.close()

    .. figure:: /images/2021041012_pyr_tables.png
       :align: center
    """
    area = 'pyrenees'  #: area tag = 'pyrenees'
    width = 14.5  #: figure width = 14.5
    height = 5.8  #: figure height = 5.8
    latmin = 42.0  #: southern map border = 42.0
    latmax = 43.3  #: northern map border = 43.3
    lonmin = -2.0  #: western map border = -2.0
    lonmax = 3.0  #: eastern map border = 3.0

    mappos = [0.05, 0.06, 0.8, 0.8]  #: map position on the figure = [0.05, 0.06, 0.8, 0.8]
    legendpos = [0.89, 0.13, 0.02, 0.6]  #: legend position on the figure = [0.89, 0.1, 0.03, 0.7]
    infospos = (-1.8, 42.4)  #: info box position on the map in Lambert Conformal Coordinates
    labelfontsize = 16  #: fontsize of colorbar label

    deport = {64: (0, 2000), 67: (0, 20000), 68: (10000, 5000), 70: (-2000, 10000), 71: (-12000, 5000),
              72: (10000, 10000), 73: (10000, 10000), 74: (10000, 3000), 75: (5000, 0), 81: (-10000, 1000),
              82: (-3000, 0), 83: (1000, 0), 84: (-4000, 0), 85: (0, -7000), 86: (-3000, 0), 87: (0, -10000),
              88: (12000, 7000), 89: (0, -4000), 90: (10000, -5000), 91: (-17000, -8000)}
    """displacement dictionary for the positioning tables near the massif center without overlapping."""

    def __init__(self, *args, **kw):
        """
        :param args: args passed to super class
        :param kw: keyword args passed to super class
        """

        super(Map_pyrenees, self).__init__(*args, **kw)


class MapFrance(_Map_massifs):
    """
    Class to draw map over all French massifs.

    Example:

     .. code-block:: python

        from netCDF4 import Dataset
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with Dataset('/rd/cenfic3/manto/viallonl/testbase/PRO/postproc/grid_postproc_2021041112.nc') as ff:
            lats = ff.variables['LAT'][:]
            lons = ff.variables['LON'][:]
            snow = ff.variables['SD_1DY_ISBA'][0, :, :, 8]

        m = cartopy.MapFrance(geofeatures=False, bgimage=True)
        m.init_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.draw_mesh(lons, lats, snow,convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu',
                    seuiltext=50., label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.set_figtitle("SD_1DY_ISBA 2021041112")
        m.set_maptitle("Percentile 90")
        plt.show()
        m.close()

    .. figure:: /images/grid_p90_2021041112_alpha_terrimage.png
       :align: center
    """
    area = ['alpes', 'pyrenees', 'corse']  #: list of areas
    width = 15  #: figure width
    height = 11  #: figure height

    latmin = 41.3  #: minimum latitude of map
    latmax = 51.5  #: maximum latitude of map
    lonmin = -5.  #: minimum longitude of map
    lonmax = 9.6  #: maximum longitude of map

    mappos = [0.05, 0.06, 0.8, 0.8]  #: position of the map on the plot
    legendpos = [0.9, 0.13, 0.03, 0.7]  #: position of the colorbar on the plot
    infospos = (-4., 51.)  #: position of north-south info box in lambert conformal coordinates
    labelfontsize = 20  #: fontsize of colorbar label

    deport = {2: (-10000, 0), 3: (10000, 0), 6: (20000, 0), 7: (-20000, 10000), 9: (15000, -10000), 11: (15000, -10000),
              13: (15000, 0), 17: (15000, 0), 18: (-20000, -10000), 19: (0, -5000), 20: (0, -5000), 21: (0, -10000),
              67: (10000, 20000), 68: (0, 5000), 69: (0, 10000), 72: (20000, 20000), 74: (25000, 0),
              82: (-15000, -40000), 84: (-10000, -30000), 85: (0, -5000), 87: (-5000, -40000), 88: (25000, -5000),
              89: (15000, -15000), 90: (-25000, 5000), 91: (10000, -5000)}
    """displacement of tables from the center of the massifs"""

    def __init__(self, *args, **kw):
        self.area = ['alpes', 'pyrenees', 'corse']
        self.width = 15
        self.height = 11

        self.latmin = 41.3
        self.latmax = 51.5
        self.lonmin = -5.
        self.lonmax = 9.6

        self.mappos=[0.05, 0.06, 0.8, 0.8]
        self.legendpos = [0.9, 0.13, 0.03, 0.7]
        self.infospos = (450000, 140000)

        self.deport = {2: (-10000, 0), 3: (10000, 0), 6: (20000, 0), 7: (-20000, 10000), 9: (15000, -10000), 11: (15000, -10000),
                       13: (15000, 0), 17: (15000, 0), 18: (-20000, -10000), 19: (0, -5000), 20: (0, -5000), 21: (0, -10000),
                       67: (10000, 20000), 68: (0, 5000), 69: (0, 10000), 72: (20000, 20000), 74: (25000, 0), 82: (-15000, -40000),
                       84: (-10000, -30000), 85: (0, -5000), 87: (-5000, -40000), 88: (25000, -5000), 89: (15000, -15000),
                       90: (-25000, 5000), 91: (10000, -5000)}

        super(MapFrance, self).__init__(*args, **kw)

    def getshapes(self):
        shapefile_path = os.path.join(os.environ['SNOWTOOLS_CEN'], 'DATA')
        filenames = ['massifs_{0:s}.shp'.format(iarea) for iarea in self.area]
        self.shapefile = [shpreader.Reader(os.path.join(shapefile_path, filename)) for filename in filenames]
        # Informations sur la projection
        projfile = 'massifs_{0:s}.prj'.format(self.area[0])
        with open(os.path.join(shapefile_path, projfile), 'r') as prj_file:
            prj_txt = prj_file.read()
            pprojcrs = CRS.from_wkt(prj_txt)

        # Projection du shapefile
        self.shpProj = pprojcrs.to_dict()
        # géométries
        # records is a generator object. Each record contains a geometry, its attributes and bounds
        self.records = itertools.chain([shapefile.records() for shapefile in self.shapefile])


class MultiMap_Pyr(Map_pyrenees, _MultiMap):
    """
    Class for plotting multiple massif plots of the Pyrenees

    Example:

    .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/manto/viallonl/testbase/PRO/postproc/Pyr/postproc_2021041006_2021041112.nc') as ff:
            points_nord = ff.get_points(aspect=0, ZS=2100, slope=40)
            points_sud = ff.get_points(aspect=180, ZS=2100, slope=40)
            snow_nord = ff.read('SD_1DY_ISBA', selectpoint=points_nord, hasDecile=True)
            snow_sud = ff.read('SD_1DY_ISBA', selectpoint=points_sud, hasDecile=True)
            massifs = ff.read('massif_num', selectpoint=points_nord)

        m = cartopy.MultiMap_Pyr(nrow=3, ncol=3, geofeatures=True)
        m.init_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.add_north_south_info()
        titles = ff.readtime()
        m.set_maptitle(titles)
        m.rectangle_massif(massifs, [0, 1, 2], [snow_sud[:, :, 1], snow_sud[:, :, 4],
                                                snow_sud[:, :, 7], snow_nord[:, :, 1],
                                                snow_nord[:, :, 4], snow_nord[:, :, 7]], ncol=2,
                            convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                            label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm', axis=0)
        m.addlogo()
        m.set_figtitle("2100m")
        plt.show()
        m.close()

    .. figure:: /images/2021041006_2021041112_pyr_tables.png
       :align: center
    """
    legendpos = [0.94, 0.13, 0.02, 0.6]  #: legend position on the figure = [0.89, 0.1, 0.03, 0.7]
    mappos = [0.05, 0.06, 0.95, 0.8]  #: map position on the figure = [0.05, 0.06, 0.85, 0.8]

    def __init__(self, nrow=1, ncol=1, *args, **kw):
        kw['getmap'] = False
        self.nrow = nrow
        self.ncol = ncol
        self.nsubplots = nrow*ncol
        self.titlepad = 0
        super(MultiMap_Pyr, self).__init__(*args, **kw)
        self.set_figsize(21, 8)
        self.init_maps(*args, **kw)


class Map_corse(_Map_massifs):
    """
    Class for plotting a map over Corse.

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/manto/viallonl/testbase/PRO/postproc/Cor/postproc_2021041006_2021041112.nc') as ff:
            points = ff.get_points(ZS=2100, aspect=-1)
            snow = ff.read('SD_1DY_ISBA', selectpoint=points, hasDecile=True)
            massifs = ff.read('massif_num', selectpoint=points)

        m = cartopy.Map_corse(bgimage=True)
        m.init_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.highlight_massif(massifs[0], snow, convert_unit=100., forcemin=0., forcemax=50.,
                            palette='YlGnBu', seuiltext=50.,
                            label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.plot_center_massif(massifs, snow[5, :, 4], snow[5, :, 8], convert_unit=100., forcemin=0.,
                                forcemax=50., palette='YlGnBu', seuiltext=50.,
                                label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.addlogo()
        m.set_maptitle("2021041112 percentile 50 and 90")
        m.set_figtitle("2100m")
        plt.show()
        m.close()

    .. figure:: /images/2021041112_cor_bgimage_highlight40.png
       :align: center
    """
    area = 'corse'  #: area tag = 'corse'
    width = 10  #: figure width = 10
    height = 10  #: figure height = 10
    latmin = 41.3  #: southern map border = 41.3
    latmax = 43.1  #: northern map border = 43.1
    lonmin = 8.4  #: western map border = 8.4
    lonmax = 9.6  #: eastern map border = 9.6

    mappos = [0.05, 0.06, 0.8, 0.8]  #: map position on the figure = [0.05, 0.06, 0.8, 0.8]
    legendpos = [0.81, 0.15, 0.03, 0.6]  #: legend position on the figure = [0.81, 0.15, 0.03, 0.6]
    #: info box position on the map in Lambert Conformal Coordinates = (1110000, 1790000)
    infospos = (8.5, 43.)  #: info box position on the map in Lambert Conformal Coordinates
    labelfontsize = 20  #: fontsize of colorbar label
    #: displacement dictionary for the positioning tables near the massif center without overlapping. = {}
    deport = {}

    def __init__(self, *args, **kw):
        self.area = 'corse'
        self.width = 10
        self.height = 10
        self.latmin = 41.3
        self.latmax = 43.1
        self.lonmin = 8.4
        self.lonmax = 9.6

        self.mappos=[0.05, 0.06, 0.8, 0.8]
        self.legendpos = [0.81, 0.15, 0.03, 0.6]
        # self.infospos = (150000, 2150000)
        self.infospos = (1110000, 1790000)

        self.infoswidth = 500000
        self.infosheight = 500000

        self.deport = {}

        super(Map_corse, self).__init__(*args, **kw)


class MultiMap_Cor(_MultiMap, Map_corse):
    """
    Class for plotting multiple massif plots of Corse

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/manto/viallonl/testbase/PRO/postproc/Cor/postproc_2021041006_2021041112.nc') as ff:
            points = ff.get_points(ZS=2100, aspect=-1)
            snow = ff.read('SD_1DY_ISBA', selectpoint=points, hasDecile=True)
            massifs = ff.read('massif_num', selectpoint=points)

        m = cartopy.MultiMap_Cor(nrow=3, ncol=3, bgimage=True)
        m.init_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        centre = [shape.centroid.coords[0] for shape in m.llshape]
        m.addpoints(*list(zip(*centre)), color='magenta', marker="o")
        m.addlogo()
        m.set_maptitle(["magenta center"])
        m.set_figtitle("2100m")
        plt.show()
        m.close()

    .. figure:: /images/multi_cor_bgimage_annotate.png
       :align: center
    """

    def __init__(self, nrow=1, ncol=1, *args, **kw):
        kw['getmap'] = False
        self.nrow = nrow
        self.ncol = ncol
        self.nsubplots = nrow*ncol
        self.titlepad = 5
        super(MultiMap_Cor, self).__init__(*args, **kw)
        self.init_maps(*args, **kw)
        self.legendpos = [0.85, 0.15, 0.03, 0.6]


class Zoom_massif(_Map_massifs):
    """
    Class for zoomed map on a given massif

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/manto/viallonl/testbase/PRO/postproc/Pyr/postproc_2021041006_2021041112.nc') as ff:
            points_nord = ff.get_points(aspect=0, ZS=2100, slope=40)
            snow_nord = ff.read('SD_1DY_ISBA', selectpoint=points_nord, hasDecile=True)
            massifs = ff.read('massif_num', selectpoint=points_nord)

        m = cartopy.Zoom_massif(70)
        m.init_massifs(palette='YlGnBu', seuiltext=50., ticks=['A', 'B', 'C', 'D'],
                            label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm', ncolors=3)
        m.draw_massifs(massifs, snow_nord[1, :, 8], palette='YlGnBu', seuiltext=50.,
                            label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm', ncolors=3,
                            ticks=['A', 'B', 'C', 'D'])
        m.empty_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.add_north_south_info()
        centre = [shape.centroid.coords[0] for shape in m.llshape]
        m.addpoints(*list(zip(*centre)), color='magenta', labels=m.name)
        m.addlogo()
        m.set_maptitle("2021041012 p90")
        m.set_figtitle("2100m")
        plt.show()
        m.close()

    .. figure:: /images/2021041012_zoom_70.png
       :align: center
    """
    labelfontsize = 20  #: fontsize of colorbar label

    def __init__(self, num_massif, *args, **kw):
        """
        Init zoom class

        :param num_massif:  massif number
        :param args:
        :param kw:
        """
        if 1 <= num_massif <= 27:
            self.area = 'alpes'
            self.width = 9
            self.height = 8
            self.legendpos = [0.91, 0.15, 0.03, 0.6]
            self.mappos=[0.05, 0.03, 0.8, 0.8]
            self.titlepad = 25
        elif 45 <= num_massif <= 47:
            self.area = 'vosges'
            self.width = 10.2
            self.height = 8
            self.legendpos = [0.89, 0.15, 0.03, 0.6]
            self.mappos = [0.05, 0.03, 0.8, 0.8]
            self.titlepad = 25
        elif 55 <= num_massif <= 58:
            self.area = 'jura'
            self.width = 10.2
            self.height = 8
            self.legendpos = [0.89, 0.15, 0.03, 0.6]
            self.mappos = [0.05, 0.03, 0.8, 0.8]
            self.titlepad = 25
        elif 48 <= num_massif <= 54 or 59 <= num_massif <= 62:
            self.area = 'central'
            self.width = 10.2
            self.height = 8
            self.legendpos = [0.89, 0.15, 0.03, 0.6]
            self.mappos = [0.05, 0.03, 0.8, 0.8]
            self.titlepad = 25
        elif 64 <= num_massif <= 91:
            self.area = 'pyrenees'
            self.width = 10
            self.height = 8
            #self.legendpos = [0.91, 0.12, 0.025, 0.6]
            self.mappos=[0.05, 0.06, 0.8, 0.8]
            self.titlepad = 40
        else:
            self.area = 'corse'
            self.width = 9
            self.height = 8
            self.legendpos = [0.91, 0.15, 0.03, 0.6]
            self.mappos=[0.05, 0.06, 0.8, 0.8]
            self.titlepad = 25

        self.deport = {}

        self.getshapes()

        if self.shpProj['proj'] == 'lcc':
            self.projection = ccrs.LambertConformal(central_longitude=self.shpProj['lon_0'],
                                                central_latitude=self.shpProj['lat_0'],
                              false_easting=self.shpProj['x_0'], false_northing=self.shpProj['y_0'],
                              standard_parallels=(self.shpProj['lat_1'], self.shpProj['lat_2']))
        else:
            raise NotImplementedError('only LambertConformal projection is implemented for the massif shapes')

        self.get_map_dimensions(num_massif)
        self.fig = plt.figure(figsize=(self.width, self.height))
        self.fig.subplots_adjust(bottom=0.005, left=0.005, top=0.95, right=0.9)
        self.map = self.getmap(self.latmin, self.latmax, self.lonmin, self.lonmax)
        self.map.gridlines(draw_labels=True)
        #self.map.drawcoastlines(linewidth=1)
#        self.map.drawcountries()
        #self.map.drawmapboundary()

    def get_map_dimensions(self, num_massif):

        self.dicLonLatMassif = self.getLonLatMassif()
        barycentre = None
        for massif in self.records:
            num = massif.attributes['code']
            if num == num_massif:
                barycentre = self.dicLonLatMassif[num]
        if barycentre is None:
            raise ValueError(f'Massif {num_massif} not in shapefile')
        else:
            if self.area in ['alpes', 'corse', 'jura', 'central', 'vosges']:
                dlat = 0.55
                dlon = 0.65
                dloninfo = dlon/3.
                dlatinfo = dlat/5.
            elif self.area == 'pyrenees':
                dlat = 0.5
                dlon = 1.3
                dloninfo = dlon/3.5
                dlatinfo = dlat/4.

            self.lonmin = barycentre[0] - dlon
            self.lonmax = barycentre[0] + dlon
            self.latmin = barycentre[1] - dlat
            self.latmax = barycentre[1] + dlat
            # infospos = self.projection.project_geometry(Point((lonmax-dloninfo, latmax-dlatinfo)),
            #                                             ccrs.PlateCarree()).coords[0]
            # infospos = (lonmax-dloninfo, latmax-dlatinfo)
