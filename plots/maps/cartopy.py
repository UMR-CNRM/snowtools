#! /usr/bin/env python
# -*- coding: utf-8 -*-
"""
Created on 29 march 2021

@author: radanovics

start collecting resources for plotting maps with cartopy.
Might be sensitive to the combination of versions of matplotlib and cartopy.
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
from plots.abstracts.figures import Mplfigure
from utils.infomassifs import infomassifs
from pyproj import CRS
from cartopy import config
from shapely.geometry import box

# Tell cartopy where to find Natural Earth features
config['data_dir'] = os.path.join(os.environ['SNOWTOOLS_CEN'], 'DATA')

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
            # ax.add_feature(cartopy.feature.BORDERS, linestyle=':')
            ax.add_feature(cartopy.feature.NaturalEarthFeature('cultural','admin_0_boundary_lines_land','10m',
                                                               facecolor='none', linestyle=':'))
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
        shapefile_path = os.path.join(os.environ['SNOWTOOLS_CEN'], 'DATA')
        filename = 'massifs_{0:s}.shp'.format(self.area)
        self.shapefile = shpreader.Reader(os.path.join(shapefile_path, filename))
        # Informations sur la projection
        projfile = 'massifs_{0:s}.prj'.format(self.area)
        with open(os.path.join(shapefile_path, projfile), 'r') as prj_file:
            prj_txt = prj_file.read()
            pprojcrs = CRS.from_wkt(prj_txt)

        # Projection du shapefile
        self.shpProj = pprojcrs.to_dict()

        # géométries
        # records is a generator object. Each record contains a geometry, its attributes and bounds
        self.records = self.shapefile.records()

    def getLonLatMassif(self):
        """returns dict with key = Massif Number, value = (lon, lat) """
        im = infomassifs()
        return infomassifs.getAllMassifLatLon(im)

    def init_cmap(self, **kwargs):
        if 'palette' in kwargs.keys():
            if 'ncolors' in kwargs.keys():
                # self.palette = plt.get_cmap(kwargs['palette'], kwargs['ncolors']).copy() matplotlib >= 3.4
                self.palette = plt.get_cmap(kwargs['palette'], kwargs['ncolors'])
            else:
                self.palette = plt.get_cmap(kwargs['palette'])
                # self.palette = plt.get_cmap(kwargs['palette']).copy() matplotlib >= 3.4
        else:
            # self.palette = plt.get_cmap('jet').copy() matplotlib >= 3.4
            self.palette = plt.get_cmap('jet')

        self.palette.set_bad(color='grey')
        self.palette.set_under(color='grey')

        self.norm = self.normpalette(**kwargs)
        self.legendok = False

    def init_massifs(self, **kwargs):
        # This routine initializes a colormap, and adds the massif contours to the map

        self.init_cmap(**kwargs)
        if not hasattr(self, 'massif_features'):
            self.num, self.shape, self.name = zip(*[(rec.attributes['num_opp'], rec.geometry,
                                      rec.attributes['nom']) for rec in self.records])
            self.llshape = [ccrs.PlateCarree().project_geometry(ishape,self.projection) for ishape in self.shape]
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
                self.massif_features = [{'feature': self.map.add_geometries([lshape], crs=ccrs.PlateCarree(), #crs=self.projection,
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
        self.map.set_title(title, fontsize=20, pad=15)

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

        self.text = []

        for i, massif in enumerate(self.shape):

            indmassif = massifref == self.num[i]
            Xbary, Ybary = massif.centroid.coords[0]
            if np.sum(indmassif) == 1:
                if 'axis' in kwargs.keys() and hasattr(self, 'nsubplots'):
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
                        infos = ''
                        # for i, myvalue in enumerate(myvalues.take(indices=j, axis=axis)):
                        #     self.massif_features[j][i]['feature']._kwargs['facecolor'] = self.palette(self.norm(myvalue))
                        for v, variable in enumerate(listvar):
                            infos += formatString % variable.take(indices=j, axis=axis)[indmassif][0]
                            if v < nvar - 1:
                                infos += "-"

                            if 'textcolor' in kwargs.keys():
                                textcolor = kwargs['textcolor']
                            else:
                                if (nvar == 3 and v == 1) or nvar == 1:
                                    textcolor = self.getTextColor(variable.take(indices=j, axis=axis)[indmassif][0], **kwargs)
                                else:
                                    textcolor = 'black'

                        if 'unit' in kwargs.keys():
                            infos += kwargs['unit']

                        self.text.append(self.maps.flat[j].text(Xbary, Ybary, infos, transform=self.projection,
                                                       horizontalalignment='center', verticalalignment='center',
                                                       color = textcolor))
                else:
                    infos = ''
                    for v, variable in enumerate(listvar):
                        infos += formatString % variable[indmassif][0]
                        if v < nvar - 1:
                            infos += "-"

                        if 'textcolor' in kwargs.keys():
                            textcolor = kwargs['textcolor']
                        else:
                            if (nvar == 3 and v == 1) or nvar == 1:
                                textcolor = self.getTextColor(variable[indmassif][0], **kwargs)
                            else:
                                textcolor = 'black'

                    if 'unit' in kwargs.keys():
                        infos += kwargs['unit']

                    self.text.append(self.map.text(Xbary, Ybary, infos, transform=self.projection,
                                                       horizontalalignment='center', verticalalignment='center',
                                                       color = textcolor))

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

    def add_north_south_info(self):

        self.infos = []
        # self.infos.append(box(8.5, 42.9,8.85,43.1))

        if hasattr(self, 'map'):
            self.infos.append(self.map.add_artist(matplotlib.offsetbox.AnnotationBbox(matplotlib.offsetbox.TextArea("Versant Nord \n Q20 - Q50 - Q80",
                                                                                                                    textprops=dict(horizontalalignment='left')), # center in matplotlib 3.4
                                                                                      self.infospos, box_alignment=(0, 0),
                                                                                      xycoords=self.projection._as_mpl_transform(self.map),
                                                                                      bboxprops=dict(fc='none'))))
            self.infos.append(self.map.add_artist(matplotlib.offsetbox.AnnotationBbox(matplotlib.offsetbox.TextArea("Versant Sud \n Q20 - Q50 - Q80",
                                                                                                                    textprops=dict(horizontalalignment='left')),  # center in matplotlib 3.4
                                                                                      self.infospos, box_alignment=(0, 1.4),
                                                                                      xycoords=self.projection._as_mpl_transform(self.map),
                                                                                      bboxprops=dict(fc='none'))))

        elif hasattr(self, 'maps'):
            for i in range(len(self.maps.flat)):
                self.infos.append(self.maps.flat[i].add_artist(matplotlib.offsetbox.AnnotationBbox(matplotlib.offsetbox.TextArea("Versant Nord \n Q20 - Q50 - Q80",
                                                                                                                        textprops=dict(horizontalalignment='left', # center in matplotlib 3.4
                                                                                                                                       size=5)),
                                                                                          self.infospos, box_alignment=(0.2, 0),
                                                                                          xycoords=self.projection._as_mpl_transform(self.maps.flat[i]),
                                                                                          bboxprops=dict(fc='none'), pad=0.2)))
                self.infos.append(self.maps.flat[i].add_artist(matplotlib.offsetbox.AnnotationBbox(matplotlib.offsetbox.TextArea("Versant Sud \n Q20 - Q50 - Q80",
                                                                                                                    textprops=dict(horizontalalignment='left', # center in matplotlib 3.4
                                                                                                                                   size=5)),
                                                                                      self.infospos, box_alignment=(0.2, 1.4),
                                                                                      xycoords=self.projection._as_mpl_transform(self.maps.flat[i]),
                                                                                      bboxprops=dict(fc='none'), pad=0.2)))

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
        self.infospos = (990000, 2160000)

        self.deport = {7: (0, 5000), 9: (-1000, 0),  16: (1000, 0), 19: (-2000, -2000),  21: (0, -5000)}

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


class MultiMap_Alps(_MultiMap, Map_alpes):

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

    def __init__(self, *args, **kw):
        self.area = 'pyrenees'
        self.width = 14.5
        self.height = 5.8
        self.latmin = 42.0
        self.latmax = 43.3
        self.lonmin = -2.0
        self.lonmax = 3.0

        self.mappos=[0.05, 0.06, 0.8, 0.8]
        self.legendpos = [0.89, 0.1, 0.03, 0.7]
        self.infospos = (600000, 1790000)

        self.deport = {64: (0, 2000), 67: (0, 20000), 68: (10000, 5000), 70: (-2000, 10000), 71: (-12000, 5000),
                       72: (10000, 10000), 73: (10000, 10000), 74: (10000, 3000), 81: (-10000, 1000), 82: (-3000, 0),
                       83: (1000, 0), 84: (-4000, 0), 85: (0, -7000), 86: (-3000, 0), 87: (0, -10000),
                       88: (12000, 7000), 89: (0, -4000), 90: (10000, -5000), 91: (-17000, -8000)}

        super(Map_pyrenees, self).__init__(*args, **kw)


class MapFrance(_Map_massifs):

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


class MultiMap_Pyr(_MultiMap, Map_pyrenees):

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

    def __init__(self, nrow=1, ncol=1, *args, **kw):
        kw['getmap'] = False
        self.nrow = nrow
        self.ncol = ncol
        self.nsubplots = nrow*ncol
        self.titlepad = 5
        super(MultiMap_Cor, self).__init__(*args, **kw)
        self.init_maps(*args, **kw)
        self.legendpos = [0.85, 0.15, 0.03, 0.6]
