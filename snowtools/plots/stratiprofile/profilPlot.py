# -*- coding: utf-8 -*-

"""
Created on 6 avr. 2017
Modified 7. apr. 2017 viallon

:Authors:
    - Pascal Hagenmuller
    - Léo Viallon-Galinier
    - Mathieu Fructus
"""

import logging
import copy

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import matplotlib.cm as cm
from matplotlib import collections
import matplotlib.colors as colors
from matplotlib.colors import BoundaryNorm

from snowtools.plots.stratiprofile import Dictionnaries

logger = logging.getLogger()


def saisonProfil(ax, dz, value, time, colormap='viridis', value_min=None, value_max=None, legend=None,
                 cbar_show=True, title=None, ylimit=None,):
    """
    Plot a snow profile along time taking into account layer thicknesses for a realistic
    plot.

    :param ax: figure axis
    :type ax: matplotlib axis
    :param dz: layer thicknesses
    :type dz: numpy array
    :param value: Value to be plot (color of the layer). Should have the same dimension as ``dz``
    :type value: numpy array
    :param time: Time values
    :type time: numpy array
    :param colormap: Colormap to use. Some custom colormaps are defined for specific variables:
                     ``grains``, ``echelle_log``, ``echelle_log_sahara``, ``ratio_cisaillement``,
                     ``tempK`` and ``lwc``.
    :type colormap: str or matplotlib colormap
    :param legend: legend for the colorbar
    :type legend: str
    :param value_min: minimum value for colorbar
    :type value_min: float
    :param value_max: maximum value for colorbar
    :type value_max: float
    :param cbar_show: Whether or not to plot the colorbar
    :type cbar_show: bool
    :param title: title of the graph
    :type title: str
    :param ylimit: give the upper y-limit for the variable (defaults to the maximum snow depth across season)
    :type ylimit: float

    Note that ``dz`` should not contain ``nan`` values. Layers that are not used sould be filled with
    a zero value for depth.

    If you plan to add data, note that the x axis is not a time axis but an axis where the values are integers
    corresponding to the index of the date dimension (sue to limitations of the undelying matplotlib function).

    Below is proposed a snippet of code that provide an example for such plot.
    Note that additional options may be necessary for some files (e.g. specification of tile).

    .. code-block:: python

       from snowtools.utils.prosimu import prosimu_auto
       import matplotlib.pyplot as plt
       from snowtools.plots.stratiprofile.profilPlot import saisonProfil

       point = 100

       with prosimu_auto('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/PRO_gdesRousses_2019-2020.nc') as ff:
           dz = ff.read('SNOWDZ', selectpoint=point, fill2zero=True)
           var = ff.read('SNOWTYPE', selectpoint=point)
           time = ff.readtime()

       ax = plt.gca()
       saisonProfil(ax, dz, var, time, colormap='grains')
       plt.show()

    .. figure:: /images/plots-strati-1.png
       :align: center

       Example of plots that can be obtained with this function (example of the code snippet provided).

    """
    class MidpointNormalize(colors.Normalize):
        def __init__(self, vmin=None, vmax=None, vcenter=None, clip=False):
            self.vcenter = vcenter
            colors.Normalize.__init__(self, vmin, vmax, clip)

        def __call__(self, value, clip=None):
            x, y = [self.vmin, self.vcenter, self.vmax], [0, 0.5, 1]
            return np.ma.masked_array(np.interp(value, x, y))

    top_y = np.cumsum(dz[:, ::-1], axis=1)[:, ::-1].ravel()
    bottom_y = top_y - dz.ravel()

    left_x = np.ones(shape=dz.shape, dtype='int')
    left_x = np.cumsum(left_x, axis=0).ravel()
    right_x = left_x - 1

    vertices = np.zeros(shape=(right_x.shape[0], 4, 2))
    vertices[:, 0, 0] = right_x
    vertices[:, 0, 1] = bottom_y
    vertices[:, 1, 0] = right_x
    vertices[:, 1, 1] = top_y
    vertices[:, 2, 0] = left_x
    vertices[:, 2, 1] = top_y
    vertices[:, 3, 0] = left_x
    vertices[:, 3, 1] = bottom_y

    vertices = vertices[(dz > 0).ravel()]

    if value_max is None:
        if np.all(np.isnan(value)):
            maxval = 1
        else:
            maxval = np.nanmax(value)
    else:
        maxval = value_max

    if value_min is None:
        if np.all(np.isnan(value)):
            minval = 0.1
        else:
            minval = np.nanmin(value)
    else:
        minval = value_min

    extend = 'neither'
    if colormap == 'grains':
        cmap = Dictionnaries.grain_colormap
        bounds = np.linspace(-0.5, 14.5, 16)
        norm = colors.BoundaryNorm(bounds, cmap.N)
        vmin = -0.5
        vmax = 14.5
    elif colormap == 'echelle_log':
        cmap = cm.gray_r
        vmin = max(minval, 0.0000000001)
        vmax = min(max(0.000000001, maxval), 1)
        norm = colors.LogNorm(vmin=vmin, vmax=vmax)
        cmap.set_under('#fff2fd')
        extend = 'min'
    elif colormap == 'echelle_log_sahara':
        cmap = cm.gist_heat_r
        vmin = max(minval, 0.0000000001)
        vmax = min(max(0.000000001, maxval), 1)
        value = value.clip(vmin / 2, vmax)
        norm = colors.LogNorm(vmin=vmin, vmax=vmax)
        cmap.set_under('#fff2fd')
        extend = 'min'
    elif colormap == 'ratio_cisaillement':
        cmap = cm.get_cmap('viridis')
        vmin = 0
        vmax = 20
        norm = colors.Normalize(vmin=vmin, vmax=vmax, clip=True)
        # Generating a colormap from given cmap with:
        # 0 -> 1 pink
        # 1 -> 2 red
        # 2 -> 3 orange
        # 3 -> 4 yellow
        # 4 -> 20 given cmap
        customcmap = {
                'red': [(0, 1, 1), (0.05, 1, 1), (0.1, 1, 1), (0.15, 1, 1)],
                'green': [(0, 0, 0), (0.05, 0, 0), (0.1, 0.55, 0.65), (0.15, 1, 1)],
                'blue': [(0, 1, 1), (0.05, 0.4, 0), (0.1, 0, 0), (0.15, 0, 0)]
                }
        cmapl = len(cmap.colors)
        start = 0.16
        for i in range(cmapl):
            if i == cmapl - 1:
                x = 1
            else:
                x = start + i * (1 - start) / cmapl
            iuse = cmapl - i - 1
            customcmap['red'].append((x, cmap.colors[iuse][0], cmap.colors[iuse][0]))
            customcmap['green'].append((x, cmap.colors[iuse][1], cmap.colors[iuse][1]))
            customcmap['blue'].append((x, cmap.colors[iuse][2], cmap.colors[iuse][2]))
        cmap = colors.LinearSegmentedColormap('ratio_cisaillment', customcmap)
    elif colormap == 'tempK':
        if value_max is None:
            vmax = 273.15
        else:
            vmax = maxval
        if value_min is None:
            vmin = vmax - 40
        else:
            vmin = minval
        norm = colors.Normalize(vmin=vmin, vmax=vmax)
        value = np.clip(value, vmin, vmax)
        # value[value < vmin if ~np.isnan(vmin) else False] = vmin
        cmap = copy.copy(cm.get_cmap('RdBu_r'))
        cmap.set_over((0.32, 0.0, 0.097))
        extend = 'max'
    elif colormap == 'lwc':
        if value_max is None:
            vmax = 35
        else:
            vmax = maxval
        vmin = 0
        norm = colors.Normalize(vmin=vmin, vmax=vmax)
        # value[value > vmax if ~np.isnan(vmax) else False] = vmax
        value = np.clip(value, vmin, vmax)
        value[value == 0] = -1
        cmap = copy.copy(cm.get_cmap('viridis'))
        cmap.set_under('#fff2fd')
        extend = 'min'
    else:
        norm = None
        cmap = cm.get_cmap(colormap)
        vmax = maxval
        vmin = minval

    rect = collections.PolyCollection(vertices, array=value[(dz > 0)].ravel(),
                                      cmap=cmap, norm=norm, edgecolors='none')
    rect.set_clim(vmin, vmax)
    ax.add_collection(rect)
    ax.set_xlim(0, len(time))
    ax.autoscale_view()

    if cbar_show:
        cbar = plt.colorbar(rect, ax=ax, extend=extend)

        if colormap == 'grains':
            labels = Dictionnaries.MEPRA_labels
            cbar.set_ticks(np.arange(np.shape(labels)[0]))
            cbar.ax.set_yticklabels(labels)
        if legend:
            cbar.set_label(legend)

    def format_ticks(x, pos):
        x = int(x)
        x = max(min(len(time) - 1, x), 0)
        if hasattr(time[0], 'strftime'):
            return time[x].strftime('%Y-%m-%d')
        else:
            return time[x]

    formatter = ticker.FuncFormatter(format_ticks)
    ax.xaxis.set_major_formatter(formatter)
    ax.xaxis.set_major_locator(ticker.MaxNLocator(4))
    plt.setp(ax.xaxis.get_majorticklabels(), size='small')

    if title is not None:
        ax.set_title(title)

    if ylimit is not None:
        ax.set_ylim(0, ylimit)


def saison1d(ax, value, time, legend=None, color='b.', title=None, ylimit=None):
    """
    Plot the variable value across time for bulk variables (not variables per layer, e.g.
    total SWE, albedo, etc.).

    :param ax: figure axis
    :type ax: matplotlib axis
    :param value: Value to be plot
    :type value: numpy array
    :param time: Time values
    :type time: numpy array
    :param legend: legend for the colorbar
    :type legend: str
    :param color: color name
    :type color: str
    :param title: title (date for member plots for example)
    :type title: str
    :param ylimit: give the upper y-limit for the variable (defaults to maximum snow depth)
    :type ylimit: float

    .. code-block:: python

       from snowtools.utils.prosimu import prosimu_auto
       import matplotlib.pyplot as plt
       from snowtools.plots.stratiprofile.profilPlot import saison1d

       point = 100

       with prosimu_auto('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/PRO_gdesRousses_2019-2020.nc') as ff:
           var = ff.read('DSN_T_ISBA', selectpoint=point)
           time = ff.readtime()

       ax = plt.gca()
       saison1d(ax, var, time, title='Snow depth')
       plt.show()

    .. figure:: /images/plots-strati-2.png
       :align: center

       Example of plots that can be obtained with this function (example of the code snippet provided).
    """
    if legend:
        ax.set_xlabel(legend)

    def format_ticks(x, pos):
        x = int(x)
        x = max(min(len(time) - 1, x), 0)
        if hasattr(time[0], 'strftime'):
            return time[x].strftime('%Y-%m-%d')
        else:
            return time[x]

    formatter = ticker.FuncFormatter(format_ticks)
    ax.xaxis.set_major_formatter(formatter)
    ax.xaxis.set_major_locator(ticker.MaxNLocator(4))
    plt.setp(ax.xaxis.get_majorticklabels(), size='small')

    if title is not None:
        ax.set_title(title)
    if ylimit is not None:
        ax.set_ylim(0, ylimit)

    ax.plot(value, color)


def dateProfil(axe, axe2, value, value_dz, value_grain=None, value_ram=None, xlimit=(None, None), ylimit=None,
               hauteur=None, color='b', cbar_show=False, legend=None, **kwargs):
    """
    Plot the vertical profile of the snowpack of one variable at a given date.
    If grain type and RAM resistance are given, these infos are added in the plot.


    :param axe: figure axis
    :type axe: matplotlib axis
    :param axe2: figure axis (there are two axis on same plot: one for the variable, the other for snowgrain and RAM)
    :type axe2: matplotlib axis
    :param value: variable to be plot
    :type value: numpy array
    :param value_dz: thickness value for all the layers considered
    :type value_dz: numpy array
    :param value_grain: grain type for each layer
    :type value_grain: numpy array
    :param value_ram: Résistance à l'enfoncement (traduction à trouver)
    :type value_ram: numpy array
    :param xlimit: give the x-limit for the variable (from limits_variable in proreader)
    :type xlimit: tuple
    :param ylimit: give the upper y-limit for the variable (typically the snow depth)
    :type ylimit: float
    :param hauteur: y-value for a black line in order to better see the interaction with seasonal profile
    :type hauteur: float
    :param color: color name
    :type color: str
    :param cbar_show: show the colorbar for grain type
    :type color: boolean
    :param legend: legend (usually the date)
    :type legend: datetime object or str


    .. code-block:: python

       from snowtools.utils.prosimu import prosimu_auto
       import matplotlib.pyplot as plt
       from snowtools.plots.stratiprofile.profilPlot import dateProfil

       point = 100
       time = 100

       with prosimu_auto('/rd/cenfic3/cenmod/home/viallonl/testbase/PRO/PRO_gdesRousses_2019-2020.nc') as ff:
           dz = ff.read('SNOWDZ', selectpoint=point, fill2zero=True)[time, :]
           var = ff.read('SNOWTEMP', selectpoint=point)[time, :]
           ram = ff.read('SNOWRAM', selectpoint=point)[time, :]
           grain = ff.read('SNOWTYPE', selectpoint=point)[time, :]
           time = ff.readtime()[time]

       ax = plt.gca()
       ax2 = ax.twiny()
       dateProfil(ax, ax2, var, dz, value_grain=grain, value_ram=ram, legend=str(time))
       plt.show()

    .. figure:: /images/plots-strati-3.png
       :align: center

       Example of plots that can be obtained with this function (example of the code snippet provided).
    """
    # Créer les épaisseurs cumulées
    epc = np.cumsum(value_dz)
    # Tracé du profil
    pointsx = np.zeros(2 * len(value) + 2)
    pointsx[2:2 * len(value) + 2:2] = epc
    pointsx[3:2 * len(value) + 2:2] = epc
    pointsx[0:2] = 0
    pointsy = np.zeros(2 * len(value) + 2)
    pointsy[1:2 * len(value):2] = value
    pointsy[2:2 * len(value) + 1:2] = value

    pointsy = np.delete(pointsy, 0)
    pointsx = np.delete(pointsx, 0)

    axe.plot(pointsy[::-1], np.subtract(pointsx[-1], pointsx[::-1]), color=color, linewidth=2, scalex=False,
             scaley=False)

    if type(legend) == int or type(legend) == str:
        axe.set_xlabel(legend)
    elif legend is not None:
        axe.set_xlabel(legend.strftime('%Y-%m-%d %Hh'))
    axe.set_title('RAM - Snowgrain', y=1.04)

    axe.xaxis.set_major_locator(ticker.MaxNLocator(4))
    plt.setp(axe.xaxis.get_majorticklabels(), size='small')

    if xlimit != (None, None):
        Max = xlimit[1]
        Min = xlimit[0]
    else:
        Max = 0.1
        Min = 0
    if not np.isnan(value).all():
        Max = np.nanmax(value) if np.nanmax(value) > Max else Max
        Min = np.nanmin(value) if np.nanmin(value) < Min else Min
    axe.set_xlim(Min, Max)
    if ylimit is not None:
        axe.set_ylim(0, ylimit)

    if hauteur is not None:
        axe.axhline(y=hauteur, color='black', linestyle='-')

    # Tracé du graphe SNOWTYPE / SNOWRAM
    if value_grain is not None:
        epc_inv = epc[::-1].ravel()
        bottom_y = np.subtract(np.array(epc_inv[0]), np.array(epc_inv))
        bottom_y = bottom_y[(value_dz > 0).ravel()[::-1]]
        top_y = np.append(bottom_y[1:], epc_inv[0])

        if value_ram is not None:
            left_x = value_ram.ravel()[::-1]
            left_x = left_x[(value_dz > 0).ravel()[::-1]]
            left_x = np.where(left_x > 0.5, left_x, 0.5)
        else:
            left_x = np.zeros(shape=bottom_y.shape[0], dtype='int') + 30
        right_x = np.zeros(shape=bottom_y.shape[0], dtype='int')

        vertices = np.zeros(shape=(bottom_y.shape[0], 4, 2))
        vertices[:, 0, 0] = right_x
        vertices[:, 0, 1] = bottom_y
        vertices[:, 1, 0] = right_x
        vertices[:, 1, 1] = top_y
        vertices[:, 2, 0] = left_x
        vertices[:, 2, 1] = top_y
        vertices[:, 3, 0] = left_x
        vertices[:, 3, 1] = bottom_y

        cmap = Dictionnaries.grain_colormap
        bounds = np.linspace(-0.5, 14.5, 16)
        norm = BoundaryNorm(bounds, cmap.N)
        vmin = -0.5
        vmax = 14.5

        rect = collections.PolyCollection(vertices[::-1], array=value_grain[(value_dz > 0)].ravel(),
                                          cmap=cmap, norm=norm, edgecolors='none', alpha=0.7)

        rect.set_clim(vmin, vmax)
        axe2.add_collection(rect)
        axe2.xaxis.set_major_locator(ticker.MaxNLocator(5))
        axe2.set_xlim(30, 0)
        axe2.set_zorder(2)

        # Tracé éventuel de la colorbar
        if cbar_show:
            cbar = plt.colorbar(rect, ax=[axe, axe2])
            labels = Dictionnaries.MEPRA_labels
            cbar.set_ticks(np.arange(np.shape(labels)[0]))
            cbar.ax.set_yticklabels(labels)


def heightplot(ax, value, value_ep, time, legend=None, color='b', direction_cut='up', height_cut=10.):
    """
     Plot the variable at a specific place in the snowpack. It is given with a height (in centimeter) and a direction.
     direction='up' means from ground to top of the snowpack
     direction='down' means from top of the snowpack to ground
     For example, if you want to plot 5 cm under the snowpack surface, you choose: height_cut=5 and direction_cut='down'

     :param ax: figure axis
     :type ax: matplotlib axis
     :param value: Value to be plot
     :type value: numpy array
     :param value_ep: thickness value for all the layers considered
     :type value_ep: numpy array
     :param time: Time values
     :type time: numpy array
     :param color: color name
     :type color: str
     :param legend: legend for the colorbar
     :type legend: str
     :param direction_cut: legend for the colorbar
     :type direction_cut: str
     :param height_cut: legend for the colorbar
     :type height_cut: int

    """
    if legend:
        ax.set_xlabel(legend)

    # 1) Prendre les épaisseurs et les sommer puis voir pour quel indice on dépasse la valeur height
    # 2) On fait cela pour chaque date
    y = []
    ep_from_ground = 100 * np.cumsum(value_ep[:, ::-1], axis=1)
    ep_from_topsnow = 100 * np.cumsum(value_ep, axis=1)

    # pas très pythonique: faire un truc avec np.apply_along_axis(np.searchsorted, 1, ep_from, height)
    if direction_cut == 'down':
        for i in np.arange(len(ep_from_topsnow)):
            if ep_from_topsnow[i, :].searchsorted(float(height_cut)) < int(value_ep.shape[1]) and \
                    float(height_cut) < ep_from_ground[i, -1]:
                y.append(value[i, ep_from_topsnow[i, :].searchsorted(float(height_cut))])
            else:
                y.append(None)
    if direction_cut == 'up':
        for i in np.arange(len(ep_from_ground)):
            if ep_from_ground[i, :].searchsorted(float(height_cut)) > 0 and \
                    float(height_cut) < ep_from_ground[i, -1]:
                y.append(value[i, int(value_ep.shape[1]) - ep_from_ground[i, :].searchsorted(float(height_cut))])
            else:
                y.append(None)

    y_out = [y[i] if y[i] != 0 else None for i in range(len(y))]

    xplot = range(value.shape[0])

    ax.plot(xplot, y_out, color)
    ax.set_xlim(0, value.shape[0])

    def format_ticks(x, pos):
        x = int(x)
        x = max(min(len(time) - 1, x), 0)
        if hasattr(time[0], 'strftime'):
            return time[x].strftime('%Y-%m-%d')
        else:
            return time[x]

    formatter = ticker.FuncFormatter(format_ticks)
    ax.xaxis.set_major_formatter(formatter)
    ax.xaxis.set_major_locator(ticker.MaxNLocator(4))
    plt.setp(ax.xaxis.get_majorticklabels(), size='small')
