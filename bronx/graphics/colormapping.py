# -*- coding: utf-8 -*-

"""
Custom colormaps reading and scaling.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import numpy
import copy
import matplotlib


#: No automatic export
__all__ = []


def read_cmap(sourcefile):
    r"""
    Read and creates a custom Colormap from a set of RGB colors in a file with the
    following formating::

        r1,g1,b1;\n
        r2,g2,b2;\n
        ...\n
        rn,gn,bn

    each value being comprised between 0 and 255, or 0 and 1.

    :param sourcefile: opened file-like object to read colormap from.
    """
    colors = sourcefile.readlines()
    for i in range(len(colors)):
        colors[i] = colors[i].replace(';', '')
        colors[i] = colors[i].replace('[', '')
        colors[i] = colors[i].replace(']', '')
        colors[i] = colors[i].replace('\n', '')
        colors[i] = colors[i].split(',')
    colors = numpy.array(colors, dtype=numpy.float64)
    if colors.max() > 1.:
        colors /= 255.
    return matplotlib.colors.ListedColormap(colors)


def add_cmap(cmap, sourcefile):
    """
    Reads and registers the given colormap in matplotlib.

    :param cmap: name of the colormap, to be registered under and used then
    :param sourcefile: opened file-like object to read the colormap in.
    """
    plt = matplotlib.pyplot
    if cmap not in plt.colormaps():
        plt.register_cmap(name=cmap,
                          cmap=read_cmap(sourcefile))
    else:
        raise ValueError('this colormap is already registered: {}'.format(cmap))


def get_norm4colorscale(scaling, max_val=None):
    """
    Creates a matplotlib.colors.BoundaryNorm object tuned for scaled colormaps,
    i.e. discrete, irregular colorshades.

    :param scaling: the values determining changes of colors
    :param max_val: an additional maximum value to replace the upper bound if
                    this value is included between the last two upper values.

    :return: a tuple (norm, scaling), scaling being eventually modified
             according to **max_val**
    """
    colors = matplotlib.colors
    bounds = copy.copy(scaling)
    if max_val is not None:
        if bounds[-2] <= max_val <= bounds[-1]:
            bounds[-1] = max_val
    norm = colors.BoundaryNorm(boundaries=bounds, ncolors=len(bounds) - 1)
    return (norm, bounds)
