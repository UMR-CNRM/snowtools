# -*- coding: utf-8 -*-

"""
Tools for handling axes within figures.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import matplotlib
import datetime


#: No automatic export
__all__ = []


def set_figax(figure=None, ax=None,
              **subplots_kw):
    """
    Generate or check consistency of (**figure**, **ax**) duet to work on.

    If **figure** and **ax** are both *None*, generate the duet
    using pyplot.subplots(), in which case any argument can be additionally
    passed through subplots_kw.
    """
    plt = matplotlib.pyplot
    if ax is not None and figure is None:
        figure = ax.figure
    elif ax is None and figure is not None:
        if len(figure.axes) > 0:
            ax = figure.axes[0]
        else:
            ax = figure.gca()
    elif ax is not None and figure is not None:
        assert ax in figure.axes, '*over*: inconsistency between given fig and ax'
    elif figure is ax is None:
        figure, ax = plt.subplots(**subplots_kw)
    return (figure, ax)


def set_nice_time_axis(axis, xy,
                       dt_min=None, dt_max=None,
                       showgrid=True,
                       datefmt=None,
                       tickslabelsrotation=30.):
    """
    Set an adequate axis ticks and ticks labels for Date/Hour axis.

    :param axis: the axis instance to work on
    :param xy: must be 'x' or 'y'
    :param dt_min: datetime.datetime instance corresponding to plot min;
                   if None, take it from axis
    :param dt_max: datetime.datetime instance corresponding to plot max
                   if None, take it from axis
    :param showgrid: to set the grid or not
    :param datefmt: format for date
    :param tickslabelsrotation: angle in degrees, anti-clockwise order
    """
    mdates = matplotlib.dates
    plt = matplotlib.pyplot

    if xy == 'x':
        z_min = axis.axis()[0]
        z_max = axis.axis()[1]
    elif xy == 'y':
        z_min = axis.axis()[2]
        z_max = axis.axis()[3]
    if dt_min is None:
        dt_min = mdates.num2date(z_min)
    if dt_max is None:
        dt_max = mdates.num2date(z_max)
    datetimerange = dt_max - dt_min

    dayhourformatter = mdates.DateFormatter('%Y-%m-%d\n%H:%M:%S')
    dayformatter = mdates.DateFormatter('%Y-%m-%d')
    if datetimerange <= datetime.timedelta(2):
        major_locator = mdates.HourLocator(interval=6)
        minor_locator = mdates.HourLocator(interval=1)
        formatter = mdates.AutoDateFormatter(major_locator)
    elif datetimerange <= datetime.timedelta(7):
        major_locator = mdates.DayLocator(interval=1)
        minor_locator = mdates.HourLocator(interval=6)
        formatter = dayhourformatter
    elif datetimerange <= datetime.timedelta(21):
        major_locator = mdates.DayLocator(interval=2)
        minor_locator = mdates.DayLocator(interval=1)
        formatter = dayhourformatter
    elif datetimerange <= datetime.timedelta(100):
        major_locator = mdates.DayLocator(interval=7)
        minor_locator = mdates.DayLocator(interval=1)
        formatter = dayformatter
    else:
        major_locator = mdates.AutoDateLocator()
        minor_locator = None
        formatter = mdates.AutoDateFormatter(major_locator)
    if datefmt is not None:
        formatter = mdates.DateFormatter(datefmt)
    if xy == 'x':
        myaxis = axis.xaxis
    else:
        myaxis = axis.yaxis
    myaxis.set_major_locator(major_locator)
    myaxis.set_major_formatter(formatter)
    axis.grid(showgrid)
    if minor_locator is not None:
        myaxis.set_minor_locator(minor_locator)
        axis.grid(showgrid, which='minor', axis=xy, color='grey')
    if tickslabelsrotation != 0.:
        _ax = plt.gca()
        plt.sca(axis)
        if xy == 'x':
            plt.xticks(rotation=tickslabelsrotation)
        else:
            plt.yticks(rotation=tickslabelsrotation)
        plt.sca(_ax)
