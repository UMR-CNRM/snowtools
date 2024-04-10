#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 29 march 2021

:Authors:
    radanovics

Module for map plots with massifs.
This module might be sensitive to the combination of versions of matplotlib and cartopy.
Developed with matplotlib 3.4.0/3.2.1 and cartopy 0.18.

Which cartopy version is based on which matplotlib version? (according to documentation)

* cartopy 0.19 -> matplotlib 3.4.1
* cartopy 0.18 -> matplotlib 3.2.1
* cartopy 0.17 -> matplotlib 3.0.2
* cartopy 0.16 -> matplotlib 2.1.2
* cartopy 0.15 -> matplotlib 2.0.0
* cartopy 0.14 -> matplotlib 1.5.1
* cartopy 0.13 -> matplotlib 1.4.3

Usage :
example :
create a Map instance for the Alps. ``Map_alpes`` can take optional kwargs.

.. code-block:: python

    m = Map_alpes(kwargs)
    m = Map_alpes(geofeatures=True)

with ``geofeatures = True``, borders, rivers and lakes are drawn on
the map, the land and ocean polygons are colored.
At the first use cartopy tries to download the necessary data
on the fly and saves them.
If this doesn't succeed for some reason (proxy or certificate
issues for example), you can manually download the
shapefiles from NaturalEarth https://www.naturalearthdata.com/ and store them
in cartopys 'data_dir'. To see where cartopy will look for the data do :

.. code-block:: python

    from cartopy import config
    print(config['data_dir'])

The result might be for example :
``$HOME/.local/share/cartopy`` (I'll abbreviate with ``$data_dir``)

Files containing borders are then stored in
``$data_dir/shapefiles/natural_earth/cultural/`` and
files containing land, ocean river and lake features are stored in
``$data_dir/shapefiles/natural_earth/physical/``

m = MapFrance(bgimage=True)
with bgimage=True a background image from Natural Earth is added showing the relief.
Can not be used if geofeatures=True.

m.init_massifs(palette='Reds')


"""

import os

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import patheffects
import cartopy.crs as ccrs
import cartopy.io.shapereader as shpreader
import cartopy.feature
from pyproj import CRS
from cartopy import config
# from shapely.geometry import Point

from snowtools.plots.abstracts.figures import Mplfigure
from snowtools.utils.infomassifs import infomassifs
from snowtools.DATA import SNOWTOOLS_DIR, CARTOPY_DIR, LUSTRE_NOSAVE_USER_DIR

# Tell cartopy where to find Natural Earth features
# config['data_dir'] = os.path.join(SNOWTOOLS_DIR, 'CartopyData')
if os.path.isdir(CARTOPY_DIR):
    config['data_dir'] = CARTOPY_DIR
# config['data_dir'] = os.path.join(LUSTRE_NOSAVE_USER_DIR, 'CartopyData')  # for sxcen
# until proper git annex solution


# @echecker.disabled_if_unavailable
def getshapes():
    """
    read shapefile and return projection and records

    :return: shapefile, pprojcrs, shpProj, records
    """
    shapefile_path = os.path.join(SNOWTOOLS_DIR, 'DATA')
    filename = 'massifs.shp'
    shapefile = shpreader.Reader(os.path.join(shapefile_path, filename))
    # Informations sur la projection
    projfile = 'massifs.prj'
    with open(os.path.join(shapefile_path, projfile), 'r') as prj_file:
        prj_txt = prj_file.read()
        pprojcrs = CRS.from_wkt(prj_txt)

    # Projection du shapefile
    shp_proj = pprojcrs.to_dict()

    # géométries
    # records is a generator object. Each record contains a geometry, its attributes and bounds
    records = [record for record in shapefile.records()]
    return shapefile, pprojcrs, shp_proj, records


def getLonLatMassif():
    """
    get center coordinates of the Massifs

    :return: dict with key = Massif Number, value = (lon, lat)
    """
    in_ma = infomassifs()
    return infomassifs.getAllMassifLatLon(in_ma)


def convertunit(*args, **kwargs):
    """
    convert units vor all variables in :py:attr:`args`

    :param args: variables to be scaled
    :param kwargs: 'convert_unit': scaling factor to be used
    :return: list of converted variables
    """
    listvar = []
    for variablein in args:
        if 'convert_unit' in kwargs.keys():
            variable = variablein[:] * kwargs['convert_unit']
        else:
            variable = variablein[:]
        listvar.append(variable)
    return listvar


def getformatstring(**kwargs):
    """
    returns format string
    :param kwargs: 'format': containing the format string
    :return: value from 'format' or '%i'
    """
    if 'format' in kwargs.keys():
        return kwargs['format']
    return '%i'


def getTextColor(var, **kwargs):
    """
    determine text color given the value of the variable to plot.

    :param var: value of the variable
    :param kwargs: 'seuiltext': threshold above which text color should be white.
    :return: color, default is 'black'
    :rtype: str
    """
    color = 'black'
    if 'seuiltext' in kwargs.keys():
        if var >= kwargs['seuiltext']:
            color = 'white'
    return color


class MyCRS(ccrs.CRS):
    """ dummy class in order to be able to create an ccrs.CRS instance from a proj4/fiona.crs dictionary
    If the 'proj' is 'lcc' in projdict, an LambertConformal projection is initialized.
    """

    def __init__(self, projdict, globe):
        """
        :meta private:
        """
        if projdict['proj'] == 'lcc':
            ccrs.LambertConformal(central_longitude=projdict['lon_0'],
                                  central_latitude=projdict['lat_0'],
                                  false_easting=projdict['x_0'], false_northing=projdict['y_0'],
                                  standard_parallels=(projdict['lat_1'],
                                                      projdict['lat_2']), globe=globe)
        else:
            pass


class _Map_massifs(Mplfigure):
    """ Implicit class that defines a generic massif map"""
    legendok = False

    def __init__(self, *args, **kw):
        """

        :param args:
        :param kw: getmap, other kwargs used by :py:meth:`getmap`
        """
        # print(kw)
        # Get massif shapes
        self.titlepad = 15
        self.shapefile, self.pprojcrs, self.shp_proj, self.records = getshapes()
        # self.projection = MyCRS(self.shpProj, ccrs.Globe(ellipse='clrk80'))
        if self.shp_proj['proj'] == 'lcc':
            self.projection = ccrs.LambertConformal(central_longitude=self.shp_proj['lon_0'],
                                                    central_latitude=self.shp_proj['lat_0'],
                                                    false_easting=self.shp_proj['x_0'],
                                                    false_northing=self.shp_proj['y_0'],
                                                    standard_parallels=(self.shp_proj['lat_1'],
                                                                        self.shp_proj['lat_2']))
        else:
            raise NotImplementedError('only LambertConformal projection '
                                      'is implemented for the massif shapes')
        self.fig = self.openfigure()
        if 'getmap' in kw.keys():
            getmap = kw['getmap']
        else:
            getmap = True

        if getmap:
            self.map = self.getmap(**kw)
            self.map.coastlines(resolution='10m', linewidth=1)
            self.map.gridlines(draw_labels=True)

        self.dicLonLatMassif = getLonLatMassif()

    @property
    def lonmin(self):
        """western map border"""
        return self._lonmin

    @lonmin.setter
    def lonmin(self, value):
        self._lonmin = value

    @property
    def latmin(self):
        """southern map border"""
        return self._latmin

    @latmin.setter
    def latmin(self, value):
        self._latmin = value

    @property
    def lonmax(self):
        """eastern map border"""
        return self._lonmax

    @lonmax.setter
    def lonmax(self, value):
        self._lonmax = value

    @property
    def latmax(self):
        """northern map border"""
        return self._latmax

    @latmax.setter
    def latmax(self, value):
        self._latmax = value

    @property
    def mappos(self):
        """map position on the figure"""
        return self._mappos

    @mappos.setter
    def mappos(self, value):
        self._mappos = value

    @property
    def width(self):
        """figure width"""
        return self._width

    @width.setter
    def width(self, value):
        self._width = value

    @property
    def height(self):
        """figure height"""
        return self._height

    @height.setter
    def height(self, value):
        self._height = value

    @property
    def area(self):
        """area tag"""
        return self._area

    @area.setter
    def area(self, value):
        self._area = value

    @property
    def deport(self):
        """displacement dictionary for the positioning tables near
        the massif center without overlapping."""
        return self._deport

    @deport.setter
    def deport(self, value):
        self._deport = value

    @property
    def infospos(self):
        """Position of info-box on the map (in coordinates) """
        return self._infospos

    @infospos.setter
    def infospos(self, value):
        self._infospos = value

    @property
    def labelfontsize(self):
        return self._labelfontsize

    @labelfontsize.setter
    def labelfontsize(self, value):
        self._labelfontsize = value

    @property
    def vmin(self):
        """lower limit of plot color scale"""
        return self._vmin

    @vmin.setter
    def vmin(self, value):
        self._vmin = value

    @property
    def vmax(self):
        """upper limit of plot color scale"""
        return self._vmax

    @vmax.setter
    def vmax(self, value):
        self._vmax = value

    @property
    def palette(self):
        """color map object"""
        return self._palette

    @palette.setter
    def palette(self, value):
        self._palette = value

    @property
    def norm(self):
        """colormap scaled to :py:attr:`vmin`, :py:attr:`vmax`"""
        return self._norm

    @norm.setter
    def norm(self, value):
        self._norm = value

    @property
    def num(self):
        """list of massif numbers"""
        return self._num

    @num.setter
    def num(self, value):
        self._num = value

    @property
    def shape(self):
        """list of shapely polygons for massifs"""
        return self._shape

    @shape.setter
    def shape(self, value):
        self._shape = value

    @property
    def name(self):
        """list of massif names"""
        return self._name

    @name.setter
    def name(self, value):
        self._name = value

    @property
    def llshape(self):
        """massif shapes in PlateCarree projection"""
        return self._llshape

    @llshape.setter
    def llshape(self, value):
        self._llshape = value

    @property
    def renderer(self):
        """figure canvas renderer object"""
        return self._renderer

    @renderer.setter
    def renderer(self, value):
        self._renderer = value

    @property
    def massif_features(self):
        """feature objects for massifs"""
        return self._massif_features

    @massif_features.setter
    def massif_features(self, value):
        self._massif_features = value

    @property
    def m(self):
        """Color map"""
        return self._m

    @m.setter
    def m(self, value):
        self._m = value

    @property
    def legendpos(self):
        """Position of colorbar axis with respect to the plot"""
        return self._legendpos

    @legendpos.setter
    def legendpos(self, value):
        self._legendpos = value

    @property
    def text(self):
        """text to be added to the map"""
        return self._text

    @text.setter
    def text(self, value):
        self._text = value

    @property
    def infos(self):
        """info box to be added to the map"""
        return self._infos

    @infos.setter
    def infos(self, value):
        self._infos = value

    # @echecker.disabled_if_unavailable
    def getmap(self, geofeatures=False, bgimage=False, **kwargs):
        """
        Create map axes.

        :param geofeatures: if True, Land and Ocean are colored, coastlines, borders,
                            lakes and rivers are added to the map
        :param bgimage: if True, a background image (high resolution satellite relief)
                        is added to the map. Ignored if geofeatures is True.
        :return: map axes
        :rtype: GeoAxes object

        """

        # print(kwargs.keys())
        if 'nrow' in kwargs.keys() and 'ncol' in kwargs.keys() and 'iax' in kwargs.keys():
            ax_pl = plt.subplot(kwargs['nrow'], kwargs['ncol'], kwargs['iax']+1,
                                projection=ccrs.PlateCarree())
        else:
            ax_pl = plt.axes(self.mappos, projection=ccrs.PlateCarree())
        # Définit les bords de la carte
        ax_pl.set_extent([self.lonmin, self.lonmax, self.latmin, self.latmax])

        if geofeatures:
            ax_pl.add_feature(cartopy.feature.NaturalEarthFeature('physical', 'land', scale='10m',
                                                                  facecolor='wheat'))
            ax_pl.add_feature(cartopy.feature.NaturalEarthFeature('physical', 'ocean', scale='10m',
                                                                  facecolor=cartopy.feature.COLORS['water']))
            # ax.add_feature(cartopy.feature.BORDERS, linestyle=':')
            ax_pl.add_feature(cartopy.feature.NaturalEarthFeature('cultural',
                                                                  'admin_0_boundary_lines_land',
                                                                  scale='10m', facecolor='none',
                                                                  linestyle=':'))
            ax_pl.add_feature(cartopy.feature.NaturalEarthFeature('physical', 'lakes', scale='10m',
                                                                  facecolor=cartopy.feature.COLORS['water'],
                                                                  alpha=0.5))
            ax_pl.add_feature(cartopy.feature.NaturalEarthFeature('physical', 'rivers_lake_centerlines',
                                                                  scale='10m',
                                                                  edgecolor=cartopy.feature.COLORS['water'],
                                                                  facecolor='none', alpha=0.5))
        elif bgimage:
            os.environ["CARTOPY_USER_BACKGROUNDS"] = config["data_dir"]  # os.path.join(SNOWTOOLS_DIR, 'DATA')
            ax_pl.background_img(resolution="high")
        # else:
        #     ax_pl.coastlines(resolution='10m')
        return ax_pl

    def openfigure(self):
        """
        open a figure

        :return: pyplot figure object
        """
        return plt.figure(figsize=(self.width, self.height))

    def init_cmap(self, **kwargs):
        """
        :param kwargs: 'palette': name of color palette, (default: 'jet'),
                        'ncolors': number of colors
        :return: palette (colormap object), norm (scaled colormap with
                    chosen minimum and maximum values)
        """
        if 'palette' in kwargs.keys():
            if 'ncolors' in kwargs.keys():
                if matplotlib.__version__ >= '3.4':
                    # deprecation warning in version 3.3, but copy method not yet implemented
                    palette = plt.get_cmap(kwargs['palette'], kwargs['ncolors']).copy()
                else:
                    palette = plt.get_cmap(kwargs['palette'], kwargs['ncolors'])
            else:
                if matplotlib.__version__ >= '3.4':
                    palette = plt.get_cmap(kwargs['palette']).copy()
                else:
                    palette = plt.get_cmap(kwargs['palette'])
            if 'truncate' in kwargs.keys():
                if kwargs['truncate']:
                    palette = matplotlib.colors.LinearSegmentedColormap.from_list(
                        'trunc({n})'.format(n=palette.name), palette(np.linspace(0.0, 0.8, 100)))
        else:
            if matplotlib.__version__ >= '3.4':
                palette = plt.get_cmap('jet').copy()
            else:
                palette = plt.get_cmap('jet')

        palette.set_bad(color='grey')
        palette.set_under(color='grey')

        norm = self.normpalette(**kwargs)
        self.legendok = False
        return palette, norm

    def init_massifs(self, **kwargs):
        """
        This routine initializes a colormap, and adds the massif contours to the map

        :param kwargs: kwargs passed to :py:meth:`init_cmap`
        """

        self.palette, self.norm = self.init_cmap(**kwargs)
        if not hasattr(self, 'massif_features'):
            self.num, self.shape, self.name = map(list, zip(*[(rec.attributes['code'], rec.geometry,
                                                               rec.attributes['title']) for rec in self.records]))
            self.llshape = [ccrs.PlateCarree().project_geometry(ishape,
                                                                self.projection) for ishape in self.shape]
            # get renderer
            if not hasattr(self, 'renderer'):
                self.fig.canvas.draw()
                self.renderer = self.fig.canvas.renderer

            self.massif_features = self.get_massif_features()

    def get_massif_features(self):
        """
        construct massif features (geometries)

        :return: feature list
        """
        features = [{'feature': self.map.add_geometries([lshape], crs=ccrs.PlateCarree(),
                                                        facecolor='none', edgecolor='dimgrey',
                                                        alpha=1.0),
                     'massifnum':inum, 'massifname':iname,
                     'massifbb': self.get_massif_bb(inum, ishape, self.map)}
                    for inum, ishape, lshape, iname in zip(self.num, self.shape,
                                                           self.llshape, self.name)]
        return features

    def get_massif_bb(self, num, shape, mymap):
        """
        get a bounding box for the table position of a massif.

        :param num: massif number
        :param shape: massif contours
        :type shape: shapely polygone
        :param mymap: map the massif belongs to
        :type mymap: GeoAxes object
        :return: bbox definition
        :rtype: list
        """
        # print('enter massif_bb')
        width = 50.
        height = 20.
        x_bary, y_bary = shape.centroid.coords[0]
        (xdeport, ydeport) = self.getdeport(num)
        # print(self.projection._as_mpl_transform(mymap))
        mlon, mlat = ccrs.PlateCarree().transform_point(x_bary+xdeport, y_bary+ydeport, self.projection)
        bb = matplotlib.offsetbox.AnnotationBbox(matplotlib.offsetbox.DrawingArea(width, height),
                                                 (mlon, mlat), box_alignment=(0.5, 0.5),
                                                 xycoords=mymap.transData,
                                                 # xycoords=self.projection._as_mpl_transform(mymap),
                                                 bboxprops=dict(fc='none'))

        mapx0, mapy0, mapwidth, mapheight = mymap.get_window_extent(self.renderer).bounds
        bb.draw(self.renderer)
        # x_0, y_0, w_d, h_g = bb.get_window_extent(self.renderer).bounds # matplotlib 3.4
        x_0, y_0, w_d, h_g = bb.patch.get_bbox().bounds
        return [(x_0-mapx0)/mapwidth, (y_0-mapy0)/mapheight, w_d/mapwidth, h_g/mapheight]

    def normpalette(self, **kwargs):
        """
        Scale legend colors between :py:attr:`vmin` and :py:attr:`vmax`

        :param kwargs: 'forcemin', 'forcemax'
        :return: scaled color map
        """
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
        """
        This routine fills the polygons with a color
        depending on the value of :py:attr:`variablein` associated with the massif number
        provided in :py:attr:`massifref`
        It is not required to provide a value for all massifs

        :param massifref: massif number
        :param variablein: values used to color the polygones
        :param kwargs: 'convert_unit' : conversion factor for :py:attr:`variablein`,
        """

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

        myvalues = self.fill_massifs(massifref, variable, **kwargs)

        # prepare colorbar
        self.prepare_colorbar(myvalues, **kwargs)
        # plt.show()

    def fill_massifs(self, massifref, variable, **kwargs):
        """
        actual massif filling

        :param massifref: list of massif numbers
        :param variable: list of values
        :param kwargs: not used
        :return: data values used for filling
        :rtype: list
        """
        myvalues = [variable[massifref == i][0] if i in massifref else np.nan for i in self.num]
        for i, myvalue in enumerate(myvalues):
            self.massif_features[i]['feature']._kwargs['facecolor'] = self.palette(self.norm(myvalue))
        return myvalues

    def empty_massifs(self, **kwargs):
        """
        fill massifs in white

        :param kwargs:
        """
        for feature in self.massif_features:
            feature['feature']._kwargs['facecolor'] = 'white'

    def addpoints(self, lon, lat, labels=None, color='black', marker=None):
        """
        add some annotation to the map

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
            for label, x_i, y_i in zip(labels, lon, lat):
                self.map.annotate(label, (x_i, y_i), color=color, horizontalalignment='center',
                                  zorder=4, path_effects=[patheffects.withStroke(linewidth=3,
                                                        foreground="w")])
        else:
            self.map.plot(lon, lat, marker=marker, color=color, linestyle='', zorder=3)

    def highlight_massif(self, massifs, **kwargs):
        """
        draw the contours of the given massifs in red.

        :param massifs: Massif numbers of massifs to highlight
        :param kwargs: kwargs passed to :py:meth`init_massifs`

        """

        # if massif contours are not yet created, draw them
        if not hasattr(self, 'massif_features'):
            self.init_massifs(**kwargs)

        if not isinstance(massifs, list):
            massifs = [massifs, ]

        self.red_edge(massifs)

    def red_edge(self, massifs):
        """
        set edgecolor to red for given massifs

        :param massifs: Massif numbers of massifs to highlight
        :type massifs: list
        """
        for i, massif in enumerate(self.records):
            if massif.attributes['code'] in massifs:
                self.massif_features[i]['feature'].set_zorder(2)  # Pour tracer le massif en dernier
                self.massif_features[i]['feature']._kwargs['edgecolor'] = 'red'

    def legend(self, polygons, **kwargs):
        """
        Add legend to figure

        :param polygons: colormap
        :param kwargs: 'ticks': list of tick labels to be used, 'label': colorbar label
        """

        currentaxis = plt.gca()

        cax = self.fig.add_axes(self.legendpos)
        self.cbar = self.fig.colorbar(polygons, cax=cax)

        if 'ticks' in kwargs.keys():
            self.cbar.set_ticks(range(0, len(kwargs['ticks'])))
            self.cbar.set_ticklabels(kwargs['ticks'])
            fontsize = 10
        else:
            fontsize = self.labelfontsize

        for t_lab in self.cbar.ax.get_yticklabels():
            t_lab.set_fontsize(fontsize)

        if 'label' in kwargs.keys():
            self.cbar.set_label(kwargs['label'], fontsize=self.labelfontsize)

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
        """
        Draw a colormesh on the map. (For gridded data)

        :param lons: mesh longitudes
        :param lats: mesh latitudes
        :param field: mesh values
        :param kwargs: 'convert_unit': factor for scaling :py:attr:`field`

        """
        variable = convertunit(field, **kwargs)
        # if 'convert_unit' in kwargs.keys():
        #     variable = field[:] * kwargs['convert_unit']
        # else:
        #     variable = field[:]
        print(variable[0].max())
        self.map.pcolormesh(lons, lats, variable[0], transform=ccrs.PlateCarree(),
                            cmap=self.palette, vmin=self.vmin, vmax=self.vmax)
        # prepare colorbar
        self.prepare_colorbar(variable[0], **kwargs)

    def prepare_colorbar(self, variable, **kwargs):
        """
        init colorbar with values.

        :param variable: values for the colorbar
        :param kwargs: kwargs to be passed to :py:meth:`legend`
        """
        if not self.legendok:
            self.m = plt.cm.ScalarMappable(cmap=self.palette)
            self.m.set_array(np.array(variable))
            self.m.set_clim(self.vmin, self.vmax)
            self.m.cmap.set_under(color='w', alpha=0)
            self.legend(self.m, **kwargs)

    def plot_center_massif(self, massifref, *args, **kwargs):
        """
        Write values at the center of the massifs.

        Each positional argument contains the values for each massifs.
        The values for the different arguments
        are separated by "-" on the plot.

        :param massifref: massif numbers
        :param args: values to write for each massif
        :param kwargs: arguments passed to :py:meth:`convertunit`, :py:meth:`getformatstring`
                        and :py:meth:`puttext`
        """

        nvar = len(args)
        listvar = convertunit(*args, **kwargs)
        format_string = getformatstring(**kwargs)
        # print(listvar)
        self.text = []

        for i, massif in enumerate(self.shape):

            indmassif = massifref == self.num[i]
            # print(indmassif, self.num[i], massifref)
            x_bary, y_bary = massif.centroid.coords.xy
            if np.sum(indmassif) == 1:
                self.puttext(x_bary[0], y_bary[0], indmassif, listvar, nvar, format_string,
                             **kwargs)

    def puttext(self, x_bary, y_bary, indmassif, listvar, nvar, format_string, **kwargs):
        """
        Put text on the maps for a given massif.

        :param x_bary: x-coordinate of the massif center
        :param y_bary: y-coordinate of the massif center
        :param indmassif: massif index
        :param listvar: list of values to write on the maps
        :param nvar: number of variables to plot at each center
        :param format_string: format specifier of the
        :param kwargs: 'textcolor', 'unit'

        """
        infos = ''
        for v_i, variable in enumerate(listvar):
            # print(formatString, variable[indmassif][0])
            infos += format_string % variable[indmassif][0]
            if v_i < nvar - 1:
                infos += "-"

            if 'textcolor' in kwargs.keys():
                textcolor = kwargs['textcolor']
            elif (nvar == 3 and v_i == 1) or nvar == 1:
                textcolor = getTextColor(variable[indmassif][0], **kwargs)
                # print("textcolor variable", textcolor)
            else:
                textcolor = 'black'

        if 'unit' in kwargs.keys():
            infos += kwargs['unit']

        self.text.append(self.map.text(x_bary, y_bary, infos, transform=self.projection,
                                       horizontalalignment='center', verticalalignment='center',
                                       color=textcolor, path_effects=[patheffects.withStroke(linewidth=3,
                                                        foreground="w")]))
        # print(self.map.properties())

    def reset_massifs(self, rmcbar=True, rminfobox=True, **kwargs):
        """
        Remove tables, text and optionally the infobox and the colorbar from the map.

        :param rmcbar: if True, colorbar is removed.
        :param rminfobox: if True, the infobox is removed.
        """
        # self.legendok = False

        # remove tables
        for prop in self.map.properties()['children']:
            if isinstance(prop, matplotlib.table.Table):
                prop.remove()
        # remove text
            elif isinstance(prop, matplotlib.text.Text):
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
            except KeyError:
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

    def rectangle_massif(self, massifref, list_values, ncol=1, **kwargs):
        """
        Put colored tables with values near the massif centers on the map.

        :param massifref: massif numbers
        :param list_values: list of value arrays
        :param ncol: number of columns in the table
        :param kwargs: kwargs passed to :py:meth:`convertunit`, :py:meth:`getformatstring`,
                        :py:meth:`puttable` and :py:meth:`prepare_colorbar`
        """

        ncol = ncol+1
        nvar = len(list_values)
        nrows = int(nvar / ncol)
        listvar = convertunit(*list_values, **kwargs)
        format_string = getformatstring(**kwargs)

        for i in range(len(self.shape)):
            indmassif = massifref == self.num[i]
            if np.sum(indmassif) == 1:
                self.puttable(i, indmassif, listvar, ncol, nrows, format_string, **kwargs)

        self.prepare_colorbar(np.array(listvar), **kwargs)

    def puttable(self, i, indmassif, listvar, ncol, nrows, format_string, **kwargs):
        """
        Put a table with values and colored cells on the map.

        :param i: massif index
        :param indmassif: massif number
        :param listvar: list of values
        :param ncol: number of columns in the table
        :param nrows: number of rows in the table
        :param format_string: format string for values
        :param kwargs: not used
        """
        # create text array
        infos = np.flipud(np.array([format_string % thisvar[indmassif][0]
                                    for thisvar in listvar]).reshape((nrows, ncol)))
        # create color array
        tmp_colors = [self.palette(self.norm(thisvar[indmassif][0])) for thisvar in listvar]
        colors = np.array([tmp_colors[-ncol:] if irows == 0
                           else tmp_colors[-(irows*ncol)-ncol:-(irows*ncol)]
                           for irows in range(nrows)])

        art = matplotlib.table.table(self.map, cellText=infos, cellColours=colors,
                                     cellLoc='center', colWidths=None,
                                     rowLabels=None, rowColours=None, rowLoc='left',
                                     colLabels=None, colColours=None,
                                     colLoc='center',
                                     loc='bottom',
                                     bbox=self.massif_features[i]['massifbb'],
                                     edges='closed', zorder=10)
        for bla, cell in art._cells.items():
            cell._text.set(path_effects=[patheffects.withStroke(linewidth=3,
                                                                foreground="w")])

        # art.set_fontsize(8)

    def getdeport(self, num):
        """
        Get the horizontal and vertical shifting for plotting tables for massifs on the maps

        :param num: massif number
        :return: shift in map coordinates (LambertConformal)
        :rtype: tuple
        """
        if num in self.deport.keys():
            return self.deport[num]
        return 0, 0


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
    """ displacement dictionary for the positioning tables near
    the massif center without overlapping."""

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
    """ displacement dictionary for the positioning tables
    near the massif center without overlapping."""

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
    """ displacement dictionary for the positioning tables
    near the massif center without overlapping."""

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

        with prosimu('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/postproc/Alp/postproc_2021041006_2021041112.nc') as ff:
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
    """ displacement dictionary for the positioning tables
    near the massif center without overlapping."""

    def __init__(self, *args, **kw):
        """

        :param args: Arguments to be passed to super class
        :param kw: Keyword arguments to be passed to super class
        """
        super(Map_alpes, self).__init__(*args, **kw)


class _MultiMap(_Map_massifs):
    """Class for plotting multiple maps on a figure"""

    @property
    def nsubplots(self):
        """Number of subplots on the figure (:py:attr:`nrow`x:py:attr:`ncol`)"""
        return self._nsubplots

    @nsubplots.setter
    def nsubplots(self, value):
        self._nsubplots = value

    @property
    def ncol(self):
        """number of plot columns on the figure"""
        return self._ncol

    @ncol.setter
    def ncol(self, value):
        self._ncol = value

    @property
    def nrow(self):
        """number of plot rows on the figure"""
        return self._nrow

    @nrow.setter
    def nrow(self, value):
        self._nrow = value

    @property
    def gl(self):
        """gridline object"""
        return self._gl

    @gl.setter
    def gl(self, value):
        self._gl = value

    @property
    def maps(self):
        """array of axes objects, one for each subplot"""
        return self._maps

    @maps.setter
    def maps(self, value):
        self._maps = value

    def init_maps(self, **kw):
        """
        Creates a geoaxis object for each subplot.

        :param kw: arguments passed to :py:meth:`getmap`

        """
        for iax in range(self.nsubplots):
            kw['iax'] = iax
            self.maps.flat[iax] = self.getmap(**kw)
            self.maps.flat[iax].coastlines(resolution='10m', linewidth=1)
            self.gl = self.maps.flat[iax].gridlines(draw_labels=True)
            # keep labels left and bottom only
            self.gl.top_labels = False  # from cartopy 0.18 on
            self.gl.right_labels = False  # from cartopy 0.18 on
            # move the subplots a little to the left in order to have
            # some space for the colorbar on the right.
            pos1 = self.maps.flat[iax].get_position()
            pos1.x0 = pos1.x0 - 0.1
            self.maps.flat[iax].set_position(pos1)

    def get_massif_features(self):
        """
        construct massif features (geometries)

        :return: feature list of lists
        """
        features = list()
        for i in range(self.nsubplots):
            features.append([{'feature': self.maps.flat[i].add_geometries([ishape],
                                                                          crs=self.projection,
                                                                          facecolor='none',
                                                                          edgecolor='dimgrey',
                                                                          alpha=1.0),
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
        :param kwargs: 'axis': the dimension along which to split :py:attr:`variable`
                        between different subplots
        :return: value array
        :rtype: numpy array
        :raises: KeyError if 'axis' is not in the :py:attr:`kwargs`,
                    IndexError if :py:attr:`variable` array has lower
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
            print("Warning: axis ", axis, " of value array is longer than number of subplots ",
                  self.nsubplots, ". Plotting first ", self.nsubplots, " out of ", leng, ".")
            leng = self.nsubplots
        print('var shape', variable.shape)
        # myvalues = np.array([variable[massifref == i][0] if i in massifref else np.nan for i in self.num], dtype=object)
        myvalues = np.array([variable[massifref == i][0] for i in self.num if i in massifref])
        print(myvalues.shape)
        for j in range(leng):
            for i, myvalue in enumerate(myvalues.take(indices=j, axis=axis)):
                self.massif_features[j][i]['feature']._kwargs['facecolor'] = self.palette(self.norm(myvalue))
        return myvalues

    def red_edge(self, massifs):
        """
        set edgecolor to red for given massifs

        :param massifs: Massif numbers of massifs to highlight
        :type massifs: list
        """
        for i, massif in enumerate(self.records):
            if massif.attributes['code'] in massifs:
                for j in range(self.nsubplots):
                    self.massif_features[j][i]['feature'].set_zorder(2)  # Pour tracer le massif en dernier
                    self.massif_features[j][i]['feature']._kwargs['edgecolor'] = 'red'

    def empty_massifs(self, **kwargs):
        """
        fill massifs in white

        :param kwargs:
        """
        for features in self.massif_features:
            for feature in features:
                feature['feature']._kwargs['facecolor'] = 'white'

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
            for label, x_i, y_i in zip(labels, lon, lat):
                for i in range(self.nsubplots):
                    self.maps.flat[i].annotate(label, (x_i, y_i), color=color, zorder=4)
        else:
            for i in range(self.nsubplots):
                self.maps.flat[i].plot(lon, lat, marker=marker, color=color, linestyle='', zorder=3)

    def puttext(self, x_bary, y_bary, indmassif, listvar, nvar, format_string, **kwargs):
        """
        Put text on the maps for a given massif.

        :param x_bary: x-coordinate of the massif center
        :param y_bary: y-coordinate of the massif center
        :param indmassif: massif index
        :param listvar: list of values to write on the maps
        :param nvar: number of variables to plot at each center
        :param format_string: format specifier of the
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
            print("Warning: axis ", axis, " of value array is longer than number of subplots ",
                  self.nsubplots, ". Plotting first ", self.nsubplots, " out of ", subplotdim, ".")
        subplotdim = self.nsubplots
        for j in range(subplotdim):
            infos = ''
            # for i, myvalue in enumerate(myvalues.take(indices=j, axis=axis)):
            #     self.massif_features[j][i]['feature']._kwargs['facecolor'] = self.palette(self.norm(myvalue))
            for v_i, variable in enumerate(listvar):
                infos += format_string % variable.take(indices=j, axis=axis)[indmassif][0]
                if v_i < nvar - 1:
                    infos += "-"

                if 'textcolor' in kwargs.keys():
                    textcolor = kwargs['textcolor']
                elif (nvar == 3 and v_i == 1) or nvar == 1:
                    textcolor = getTextColor(variable.take(indices=j, axis=axis)[indmassif][0],
                                             **kwargs)
                else:
                    textcolor = 'black'

            if 'unit' in kwargs.keys():
                infos += kwargs['unit']

            self.text.append(self.maps.flat[j].text(x_bary, y_bary, infos,
                                                    transform=self.projection,
                                                    horizontalalignment='center',
                                                    verticalalignment='center',
                                                    color=textcolor,
                                                    path_effects=[patheffects.withStroke(linewidth=3,
                                                        foreground="w")]))

    def puttable(self, i, indmassif, listvar, ncol, nrows, format_string, **kwargs):
        """
        Put tables with values and colored cells on the maps for a given massif.

        :param i: massif index
        :param indmassif: massif number
        :param listvar: list of values
        :param ncol: number of columns in the table
        :param nrows: number of rows in the table
        :param format_string: format string for values
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
            print("Warning: axis ", axis, " of value array is longer than number of subplots ",
                  self.nsubplots, ". Plotting first ", self.nsubplots, " out of ", subplotdim, ".")
            subplotdim = self.nsubplots
        for j in range(subplotdim):
            # create text array
            infos = np.flipud(np.array([format_string % thisvar.take(indices=j,
                                                                     axis=axis)[indmassif][0]
                                        for thisvar in listvar]).reshape((nrows, ncol)))
            # create color array
            tmp_colors = [self.palette(self.norm(thisvar.take(indices=j, axis=axis)[indmassif][0]))
                          for thisvar in listvar]
            colors = np.array([tmp_colors[-ncol:] if irows == 0
                               else tmp_colors[-(irows*ncol)-ncol:-(irows*ncol)]
                               for irows in range(nrows)])
            art = matplotlib.table.table(self.maps.flat[j], cellText=infos, cellColours=colors,
                                         cellLoc='center', colWidths=None, rowLabels=None,
                                         rowColours=None, rowLoc='left', colLabels=None,
                                         colColours=None, colLoc='center', loc='bottom',
                                         bbox=self.massif_features[j][i]['massifbb'],
                                         edges='closed', zorder=10)
            for bla, cell in art._cells.items():
                cell._text.set(path_effects=[patheffects.withStroke(linewidth=3,
                                                         foreground="w")])

    def reset_massifs(self, rmcbar=True, rminfobox=True, **kwargs):
        """
        Remove tables, text and optionally the infobox and the colorbar from the maps.

        :param rmcbar: if True, colorbar is removed.
        :param rminfobox: if True, the infobox is removed.
        """

        for m_i in self.maps.flat:
            # remove tables
            for prop in m_i.properties()['children']:
                if isinstance(prop, matplotlib.table.Table):
                    prop.remove()
                # remove text
                elif isinstance(prop, matplotlib.text.Text):
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
        """open a figure with several subplots and puts them in
        :py:attr:`fig` and :py:attr:`maps`"""
        self.fig, self.maps = plt.subplots(nrows=self.nrow, ncols=self.ncol, sharex='all',
                                           sharey='all', figsize=(self.width, self.height))
        return self.fig

    def set_maptitle(self, title):
        """
        Set title on top of each subplot

        :param title: title
        :type title: str
        :return:
        """
        if len(title) == self.nsubplots:
            for i in range(self.nsubplots):
                self.maps.flat[i].set_title(title[i], fontsize=14, pad=self.titlepad)
        elif len(title) == 1:
            for i in range(self.nsubplots):
                self.maps.flat[i].set_title(title[0], fontsize=14, pad=self.titlepad)
        elif len(title) < self.nsubplots:
            for i, i_title in enumerate(title):
                self.maps.flat[i].set_title(i_title, fontsize=14, pad=self.titlepad)
        else:
            print("Warning: can not set map titles. len(title) must be either"
                  "equal to the number of subplots or == 1.")

    set_title = set_maptitle


class MultiMap_Alps(Map_alpes, _MultiMap):
    """
    Class for plotting multiple massif plots of the French Alps

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/postproc/Alp/postproc_2021041006_2021041112.nc') as ff:
            points = ff.get_points(ZS=2100, aspect=-1)
            snow = ff.read('SD_1DY_ISBA', selectpoint=points, hasDecile=True)
            massifs = ff.read('massif_num', selectpoint=points)

        lo = cartopy.MultiMap_Alps(nrow=3, ncol=3, geofeatures=False)
        lo.init_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        lo.draw_massifs(massifs, snow[5, :, :], axis=1, convert_unit=100., forcemin=0., forcemax=50.,
                        palette='YlGnBu', seuiltext=50.,
                        label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        lo.highlight_massif(10, convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu',
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

    def __init__(self, *args, nrow=1, ncol=1, **kw):
        """

        :param nrow: number of rows of plots
        :param ncol: number of columns of plots
        :param args: arguments passed to superclass init and :py:meth:`init_maps`
        :param kw: keyword arguments passed to superclass init and :py:meth:`init_maps`
        """
        kw['getmap'] = False
        self.nrow = nrow
        self.ncol = ncol
        self.nsubplots = nrow*ncol
        kw['nrow'] = self.nrow
        kw['ncol'] = self.ncol
        kw['nsubplots'] = self.nsubplots
        super(MultiMap_Alps, self).__init__(*args, **kw)
        self.titlepad = 5
        self.set_figsize(18, 15)
        self.init_maps(**kw)


class Map_pyrenees(_Map_massifs):
    """
    Class to plot a map of the Pyrenees.

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/postproc/Pyr/postproc_2021041006_2021041112.nc') as ff:
            points_nord = ff.get_points(aspect=0, ZS=2100, slope=40)
            points_sud = ff.get_points(aspect=180, ZS=2100, slope=40)
            snow_nord = ff.read('SD_1DY_ISBA', selectpoint=points_nord, hasDecile=True)
            snow_sud = ff.read('SD_1DY_ISBA', selectpoint=points_sud, hasDecile=True)
            massifs = ff.read('massif_num', selectpoint=points_nord)

        m = cartopy.Map_pyrenees(geofeatures=True)
        m.init_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.add_north_south_info()
        m.rectangle_massif(massifs, [snow_sud[1, :, 1], snow_sud[1, :, 4],
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

    deport = {64: (0, 2000), 67: (0, 20000), 68: (10000, 5000), 70: (-2000, 10000),
              71: (-12000, 5000), 72: (10000, 10000), 73: (10000, 10000), 74: (10000, 3000),
              75: (5000, 0), 81: (-10000, 1000), 82: (-3000, 0), 83: (1000, 0), 84: (-4000, 0),
              85: (0, -7000), 86: (-3000, 0), 87: (0, -10000), 88: (12000, 7000),
              89: (0, -4000), 90: (10000, -5000), 91: (-17000, -8000)}
    """displacement dictionary for the positioning tables
    near the massif center without overlapping."""

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

        with Dataset('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/postproc/grid_postproc_2021041112.nc') as ff:
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

    deport = {2: (-10000, 0), 3: (10000, 0), 6: (20000, 0), 7: (-20000, 10000), 9: (15000, -10000),
              11: (15000, -10000), 13: (15000, 0), 17: (15000, 0), 18: (-20000, -10000),
              19: (0, -5000), 20: (0, -5000), 21: (0, -10000), 67: (10000, 20000), 68: (0, 5000),
              69: (0, 10000), 72: (20000, 20000), 74: (25000, 0), 82: (-15000, -40000),
              84: (-10000, -30000), 85: (0, -5000), 87: (-5000, -40000), 88: (25000, -5000),
              89: (15000, -15000), 90: (-25000, 5000), 91: (10000, -5000)}
    """displacement of tables from the center of the massifs"""

    def __init__(self, *args, **kw):
        """

        :param args: arguments passed to superclass init
        :param kw: keyword arguments passed to superclass init
        """

        super(MapFrance, self).__init__(*args, **kw)


class MultiMap_Pyr(Map_pyrenees, _MultiMap):
    """
    Class for plotting multiple massif plots of the Pyrenees

    Example:

    .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/postproc/Pyr/postproc_2021041006_2021041112.nc') as ff:
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
        m.rectangle_massif(massifs, [snow_sud[:, :, 1], snow_sud[:, :, 4],
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

    def __init__(self, *args, nrow=1, ncol=1, **kw):
        """

        :param nrow: number of rows of maps
        :param ncol: number of columns of maps
        :param args: arguments passed to super class init and :py:meth:`init_maps`
        :param kw: keyword arguments passed to super class init and :py:meth:`init_maps`
        """
        plt.rcParams["figure.subplot.right"] = 0.95
        kw['getmap'] = False
        self.nrow = nrow
        self.ncol = ncol
        self.nsubplots = nrow*ncol
        kw['nrow'] = self.nrow
        kw['ncol'] = self.ncol
        kw['nsubplots'] = self.nsubplots
        super(MultiMap_Pyr, self).__init__(*args, **kw)
        self.titlepad = 5
        self.set_figsize(30, 9)
        self.init_maps(**kw)


class Map_corse(_Map_massifs):
    """
    Class for plotting a map over Corse.

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/postproc/Cor/postproc_2021041006_2021041112.nc') as ff:
            points = ff.get_points(ZS=2100, aspect=-1)
            snow = ff.read('SD_1DY_ISBA', selectpoint=points, hasDecile=True)
            massifs = ff.read('massif_num', selectpoint=points)

        m = cartopy.Map_corse(bgimage=True)
        m.init_massifs(convert_unit=100., forcemin=0., forcemax=50., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)', unit='cm')
        m.highlight_massif(massifs[0], convert_unit=100., forcemin=0., forcemax=50.,
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
    deport = {}
    """displacement dictionary for the positioning tables near the massif center
    without overlapping. = {}"""

    def __init__(self, *args, **kw):
        """

        :param args: args passed to super class
        :param kw: keyword args passed to super class
        """

        super(Map_corse, self).__init__(*args, **kw)


class MultiMap_Cor(_MultiMap, Map_corse):
    """
    Class for plotting multiple massif plots of Corse

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/postproc/Cor/postproc_2021041006_2021041112.nc') as ff:
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

    def __init__(self, *args, nrow=1, ncol=1, **kw):
        """

        :param nrow: number of rows of maps
        :param ncol: number of columns of maps
        :param args: arguments passed to super class init and :py:meth:`init_maps`
        :param kw: keyword arguments passed to super class init and :py:meth:`init_maps`
        """
        kw['getmap'] = False
        self.nrow = nrow
        self.ncol = ncol
        self.nsubplots = nrow*ncol
        kw['nrow'] = self.nrow
        kw['ncol'] = self.ncol
        kw['nsubplots'] = self.nsubplots
        super(MultiMap_Cor, self).__init__(*args, **kw)
        self.titlepad = 5
        self.init_maps(**kw)
        self.legendpos = [0.85, 0.15, 0.03, 0.6]


class Zoom_massif(_Map_massifs):
    """
    Class for zoomed map on a given massif

    Example:

     .. code-block:: python

        from snowtools.utils.prosimu import prosimu
        from snowtools.plots.maps import cartopy
        import matplotlib.pyplot as plt

        with prosimu('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/postproc/Pyr/postproc_2021041006_2021041112.nc') as ff:
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

    def __init__(self, num_massif):
        """
        Init zoom class

        :param num_massif:  massif number
        """
        if 1 <= num_massif <= 27:
            self.area = 'alpes'
            self.width = 10.2
            self.height = 8
            self.legendpos = [0.89, 0.15, 0.03, 0.6]
            self.mappos = [0.05, 0.03, 0.8, 0.8]
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
            self.width = 12
            self.height = 6.8
            self.legendpos = [0.91, 0.12, 0.025, 0.6]
            self.mappos = [0.05, 0.06, 0.8, 0.8]
            self.titlepad = 40
        elif 40 <= num_massif <= 41:
            self.area = 'corse'
            self.width = 10.2
            self.height = 8
            self.legendpos = [0.89, 0.15, 0.03, 0.6]
            self.mappos = [0.05, 0.06, 0.8, 0.8]
            self.titlepad = 25
        else:
            raise ValueError("unknown massif number")

        self.deport = {}
        self.shapefile, self.pprojcrs, self.shp_proj, self.records = getshapes()
        if self.shp_proj['proj'] == 'lcc':
            self.projection = ccrs.LambertConformal(central_longitude=self.shp_proj['lon_0'],
                                                    central_latitude=self.shp_proj['lat_0'],
                                                    false_easting=self.shp_proj['x_0'],
                                                    false_northing=self.shp_proj['y_0'],
                                                    standard_parallels=(self.shp_proj['lat_1'],
                                                                        self.shp_proj['lat_2']))
        else:
            raise NotImplementedError('only LambertConformal projection '
                                      'is implemented for the massif shapes')

        self.lonmin, self.lonmax, self.latmin, self.latmax, \
            self.infospos = self.get_map_dimensions(num_massif)

        self.fig = plt.figure(figsize=(self.width, self.height))
        self.fig.subplots_adjust(bottom=0.005, left=0.005, top=0.95, right=0.9)
        self.map = self.getmap()
        self.map.gridlines(draw_labels=True)
        # self.map.drawcoastlines(linewidth=1)
        # self.map.drawcountries()
        # self.map.drawmapboundary()

    def get_map_dimensions(self, num_massif):
        """
        get map dimension enclosing the given massif numbers.

        :param num_massif: massif numbers
        :return: lonmin, lonmax, latmin, latmax
        """
        self.dicLonLatMassif = getLonLatMassif()
        for massif in self.records:
            num = massif.attributes['code']
            # print(num)
            if num == num_massif:
                barycentre = self.dicLonLatMassif[num]
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
        else:
            raise NotImplementedError("implemented areas: 'alpes', 'corse', 'jura', "
                                      "'central', 'vosges', 'pyrenees'")

        lonmin = barycentre[0] - dlon
        lonmax = barycentre[0] + dlon
        latmin = barycentre[1] - dlat
        latmax = barycentre[1] + dlat

        # infospos = self.projection.project_geometry(Point((lonmax-dloninfo, latmax-dlatinfo)),
        #                                             ccrs.PlateCarree()).coords[0]
        infospos = (lonmax-dloninfo, latmax-dlatinfo)

        return lonmin, lonmax, latmin, latmax, infospos
