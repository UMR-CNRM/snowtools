#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on 24 September 2024

:Authors:
    radanovics

    largely copied from epygram epy_cartoplot.py

"""

import os
import sys

import pyproj
import epygram

from snowtools.utils.projections import LCCProjectionType


def read_and_preprocess(resource,
                        fid,
                        date,
                        operation,
                        global_shift_center,
                        zoom):
    """Read field in resource, and preprocess if requested."""
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

def wind_map(field, title, map_factor_correction, vectors_subsampling, wind_components_are_projected_on,
             vector_plot_method, quiverkey, plot_kwargs):
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

def difference_map(field, ref_field, title, plot_kwargs):
    diff_field = field - ref_field
    # plot diff
    takeover = diff_field.cartoplot(title=title, takeover=True,
                                       **plot_kwargs)
    return takeover

def scalar_map(field, title, plot_kwargs):

    if isinstance(field, epygram.fields.H2DVectorField):
        # Scalar field in a vector field is a true color image
        takeover = field.cartoimage(title=title, takeover=True, **plot_kwargs)
    else:
        takeover = field.cartoplot(title=title, takeover=True, **plot_kwargs)

    return takeover