# -*- coding: utf-8 -*-

"""
Created on 13 May 2025

@author: radanovics
"""

import numpy as np
import pyproj
import epygram
# import cartopy.crs as ccrs # TODO: reactivate here when test server is working with proj
from snowtools.interpolation.shapefile2NetCDF_2D import conversion_to_L93_if_lat_lon

class LCCProjectionType(object):

    """
    Class for Lambert Conformal Conic Projection attributes.
    """


    def __init__(self, x, y, grid_mapping_name="lambert_conformal_conic", ellipsoid="GRS80",
                 semi_major_axis=None, semi_minor_axis=None, longitude_of_central_meridian=3.,
                 latitude_of_projection_origin=46.5, standard_parallel=(44., 49),
                 false_easting=None, false_northing=None, x_resolution=None, y_resolution=None):
        """

        :param x: vector of Lambert93 x-coordinate values
        :param y: vector of Lambert93 y-coordinate values
        :param grid_mapping_name: default: "lambert_conformal_conic"
        :param ellipsoid: default: "GRS80"
        :param semi_major_axis: earth radius at the equator. If None, will be derived from ellipsoid.
        :param semi_minor_axis: earth radius at the poles. If None, will be derived from ellipsoid.
        :param longitude_of_central_meridian: default: 3.
        :param latitude_of_projection_origin: default 46.5
        :param standard_parallel: default: (44., 49)
        :param false_easting: distance in x direction between the center point of the
            map projection and the south-western corner of the actual domain.
            If None, will be derived from longitude_of_central_meridian, latitude_of_projection_origin and
            the minimum value of x.
        :param false_northing: distance in y direction between the center point of the
            map projection and the south-western corner of the actual domain.
            If None, will be derived from longitude_of_central_meridian, latitude_of_projection_origin and
            the minimum value of y.
        :param x_resolution: resolution in x direction. If None, will be derived as
            the difference between the first two values of the x vector.
        :param y_resolution: resolution in y direction. If None, will be derived as
            the difference between the first two values of the y vector
        """
        self.x = x
        self.y = y
        self.grid_mapping_name = grid_mapping_name
        self.ellipsoid = ellipsoid
        if semi_major_axis:
            self.semi_major_axis=semi_major_axis
        else:
            geoid = pyproj.Geod(ellps=ellipsoid)
            self.semi_major_axis = geoid.a
            self.semi_minor_axis = geoid.b
        if semi_minor_axis:
            self.semi_minor_axis = semi_minor_axis
        if not hasattr(self, 'semi_minor_axis'):
            geoid = pyproj.Geod(ellps=ellipsoid)
            self.semi_minor_axis = geoid.b
        self.longitude_of_central_meridian = longitude_of_central_meridian
        self.latitude_of_projection_origin = latitude_of_projection_origin
        self.standard_parallel = standard_parallel
        if false_easting:
            self.false_easting=false_easting
        else:
            xc, yc, _, _ = conversion_to_L93_if_lat_lon([self.longitude_of_central_meridian,
                                                   self.latitude_of_projection_origin,
                                                   self.longitude_of_central_meridian,
                                                   self.latitude_of_projection_origin
                                                   ])
            self.false_easting = xc - np.min(x)
            self.false_northing = yc - np.min(y)
        if false_northing:
            self.false_northing = false_northing
        if not hasattr(self, 'false_northing'):
            xc, yc, _, _ = conversion_to_L93_if_lat_lon([self.longitude_of_central_meridian,
                                                   self.latitude_of_projection_origin,
                                                   self.longitude_of_central_meridian,
                                                   self.latitude_of_projection_origin
                                                   ])
            self.false_northing = yc - np.min(y)
        if x_resolution:
            self.x_resolution = x_resolution
        else:
            self.x_resolution = x[1]-x[0]
        if y_resolution:
            self.y_resolution = y_resolution
        else:
            self.y_resolution = y[1]-y[0]

    @property
    def crs(self):
        """
        :return: projection information in cartopy crs format.
        :rtype: cartopy.crs
        """
        import cartopy.crs as ccrs  # Todo: remove from here when import is working on test server

        return ccrs.LambertConformal(central_longitude=self.longitude_of_central_meridian,
                                     central_latitude=self.latitude_of_projection_origin,
                                     false_easting=self.false_easting,
                                     false_northing=self.false_northing,
                                     standard_parallels=self.standard_parallel)

    @property
    def geometry(self):
        """
        :return: projection information in epygram geometry format
        :rtype: epygram.geometry
        """
        from pyproj import Transformer
        from epygram.util import Angle
        geometryclass = epygram.geometries.ProjectedGeometry
        dimensions = {}
        dimensions['X'] = len(self.x)
        dimensions['Y'] = len(self.y)
        kwargs_geom = {}
        kwargs_geom['position_on_horizontal_grid'] = 'center'
        kwargs_geom['geoid'] = {'a': self.semi_major_axis,
                                'b': self.semi_minor_axis,
                                'ellps': self.ellipsoid}
        grid = {'X_resolution': self.x_resolution,
                'Y_resolution': self.y_resolution,
                'LAMzone': None}
        transformer = Transformer.from_crs('epsg:2154', 'epsg:4326')
        lat, lon = transformer.transform(self.x[0], self.y[0])
        grid['input_lon'] = Angle(lon, 'degrees')
        grid['input_lat'] = Angle(lat, 'degrees')
        grid['input_position'] = (0, 0)
        kwargs_geom['name'] = 'lambert'
        kwargs_geom['projection'] = {'reference_lon': Angle(self.longitude_of_central_meridian, 'degrees'),
                                     'rotation': Angle(0., 'degrees')}
        kwargs_geom['projection']['secant_lat1'] = Angle(self.standard_parallel[0], 'degrees')
        kwargs_geom['projection']['secant_lat2'] = Angle(self.standard_parallel[1], 'degrees')

        # 3.2 vertical geometry (default)
        default_kwargs_vcoord = {'typeoffirstfixedsurface': 255,
                                 'position_on_grid': 'mass',
                                 'grid': {'gridlevels': []},
                                 'levels': [0]}
        kwargs_vcoord = default_kwargs_vcoord
        # 3.4 build geometry
        vcoordinate = epygram.geometries.VGeometry(**kwargs_vcoord)
        kwargs_geom['grid'] = grid
        kwargs_geom['dimensions'] = dimensions
        kwargs_geom['vcoordinate'] = vcoordinate
        geometry = geometryclass(**kwargs_geom)

        return geometry



