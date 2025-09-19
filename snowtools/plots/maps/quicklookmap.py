#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 24 Mai 2025

:Authors:
    radanovics

    largely copied from epygram epy_cartoplot.py

"""

import pyproj
import epygram


def read_and_preprocess(resource,
                        fid,
                        date,
                        operation=None,
                        global_shift_center=None,
                        zoom=None):
    """
    Read field in resource, and preprocess if requested.

    :param resource: epygram resource to read data from
    :type resource: epygram.resource
    :param fid: field identifier
    :type fid: epygram field idenifier depending on the resource type. for example str for NetCDF files or a dict for grib files
    :param date: date (with time)
    :type date: bronx.Date
    :param operation: makes the requested operation
        (e.g. {'operation':'-','operand':273.15} or
        {'operation':'exp'}) on the field before plot. default=None
    :type operation: dict
    :param global_shift_center: for global lon/lat grids, shift the center by the
        requested angle (in degrees). Enables a [0,360] grid
        to be shifted to a [-180,180] grid, for instance (with -180 argument).
    :param zoom: a dict(lonmin, lonmax, latmin, latmax) on which to build the plot.
    :type zoom: dict
    :return: field
    :rtype: epygram.field
    """

    field = resource.readfield(fid)
    assert isinstance(field, (epygram.fields.H2DField, epygram.fields.H2DVectorField)), \
        ' '.join(['Oops ! Looks like {} is not known as a horizontal 2D Field by epygram.',
                  'Add it to ~/.epygram/user_Field_Dict_{}.csv ?']).format(fid, resource.format)
    validity = epygram.base.FieldValidity(date, basis=field.validity[0].getbasis())
    field = field.getvalidity(validity)
    if field.spectral:
        field.sp2gp()
    if global_shift_center is not None:
        field.global_shift_center(global_shift_center)
    if zoom is not None:
        field = field.extract_zoom(zoom)
    if operation is not None:
        field.operation(**operation)
    if 'ellps' in field.geometry.geoid.keys():
        if 'a' not in field.geometry.geoid.keys():
            geoid = pyproj.Geod(ellps=field.geometry.geoid['ellps'])
            field.geometry.geoid['a'] = geoid.a
            field.geometry.geoid['b'] = geoid.b
    return field

def wind_map(field, title, map_factor_correction=False, vectors_subsampling=50,
             wind_components_are_projected_on=None,
             vector_plot_method='quiver', quiverkey=None, plot_kwargs={}):
    """
    Plot a map of a vector field.

    :param field: vector field to plot
    :type field: epygram vector field
    :param title: plot title
    :type title: str
    :param map_factor_correction: if True, applies a correction of magnitude
        to vector due to map factor.
    :type map_factor_correction: bool
    :param vectors_subsampling: subsampling ratio of vectors plots. for example: 1 for a vector at every gridpoint,
        10 for a vector every 10 grid points.
    :type vectors_subsampling: int
    :param wind_components_are_projected_on: inform the plot on which axes the
        vector components are projected on ('grid' or 'lonlat').
        If None (default), look for information in the field, or raise error.
    :param vector_plot_method: among ('quiver', 'barbs', 'streamplot') for vector plots. default is 'quiver'.
    :param quiverkey: options to be passed to plotfield to activate a quiver key
        (cf. pyplot.quiverkey).
    :param plot_kwargs: every other argument that can be passed to the cartoplot method of an epygram H2D field.
    :return: dict with plot elements: 'fig', 'ax', potentially 'colorbar' etc.
    :rtype: dict

    .. figure:: /images/20150405T06_pseudo_wind_map.png
       :align: center
    """

    takeover = field.cartoplot(map_factor_correction=map_factor_correction,
                             subsampling=vectors_subsampling,
                             components_are_projected_on=wind_components_are_projected_on,
                             vector_plot_method=vector_plot_method,
                             vector_plot_kwargs=None,
                             quiverkey=quiverkey,
                             # module_plot_kwargs
                             title=title,
                                takeover=True,
                             **plot_kwargs)
    return takeover

def difference_map(field, ref_field, title, plot_kwargs={}):
    """
    Plot a difference map between two fields.

    :param field: first scalar field
    :type field: epygram H2D field
    :param ref_field: reference field to substract.
    :type ref_field: epygram H2D field
    :param title: plot title
    :type title: str
    :param plot_kwargs: every other argument that can be passed to the cartoplot method of an epygram H2D field.
    :return: dict with plot elements: 'fig', 'ax', potentially 'colorbar' etc.
    :rtype: dict

    .. figure:: /images/20150301T06_snowheight_diff.png
       :align: center
    """
    diff_field = field - ref_field
    # plot diff
    takeover = diff_field.cartoplot(title=title, takeover=True,
                                       **plot_kwargs)
    return takeover

def scalar_map(field, title, plot_kwargs):
    """
    Plot a map.

    :param field: field to plot
    :type field: epygram H2DField or H2DVectorField
    :param title: plot title
    :type title: str
    :param plot_kwargs: every other argument that can be passed to the cartoplot method of an epygram H2D field.
    :return: dict with plot elements: 'fig', 'ax', potentially 'colorbar' etc.
    :rtype: dict

    .. figure:: /images/20190513T10_snowheight.png
       :align: center
    """

    if isinstance(field, epygram.fields.H2DVectorField):
        # Scalar field in a vector field is a true color image
        takeover = field.cartoimage(title=title, takeover=True, **plot_kwargs)
    else:
        takeover = field.cartoplot(title=title, takeover=True, **plot_kwargs)

    return takeover