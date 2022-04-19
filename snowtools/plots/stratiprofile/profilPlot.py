# -*- coding: utf-8 -*-

"""
Created on 6 avr. 2017
Modified 7. apr. 2017 viallon

:Authors:
    - Pascal Hagenmuller
    - Léo Viallon-Galinier
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import matplotlib.cm as cm
from matplotlib import collections
import matplotlib.colors as colors
from matplotlib.colors import BoundaryNorm
from snowtools.plots import Dictionnaries
import logging
logger = logging.getLogger()


def saisonProfil(ax, dz, value, list_legend, colormap='viridis', vmin=None, vmax=None, legend=None, cbar_show=True,
                 title=None, ylimit=None,):
    """
    Trace le profil de value en fonction du temps avec les epaisseurs reelles de couches

    Plot a snow profile along time taking into account layer thicknesses for a realistic
    plot.

    :param ax: figure axis
    :type ax: matplotlib axis
    :param dz: layer thicknesses
    :type dz: numpy array
    :param value: Value to be plot (color of the layer). Should have the same dimension as ``dz``
    :type value: numpy array
    :param list_legend: list for x axis legend. The dates where there are some datas to be plotted for standard graphs
    :type list_legend: numpy array
    :param colormap: Colormap to use. Some custom colormaps are defined for specific variables:
                     ``grains``, ``echelle_log``, ``echelle_log_sahara``, ``ratio_cisaillement``,
                     ``tempK`` and ``lwc``.
    :type colormap: str or matplotlib colormap
    :param legend: legend for the colorbar
    :type legend: str
    :param vmin: seems to be use only for Temperature graph
    :type vmin: float
    :param vmax: seems to be use only for Temperature graph
    :type vmax: float
    :param cbar_show: Whether or not to plot the colorbar
    :type cbar_show: bool
    :param title: title (date for member plots for example)
    :type title: str
    :param ylimit: give the upper y-limit for the variable (= max of thickness in all the season normally)
    :type ylimit: float

    Note that ``dz`` should not contain ``nan`` values. Layers that are not used sould be filled with
    a zero value for depth.

    .. code-block:: python

       from snowtools.utils.prosimu import prosimu
       import matplotlib.pyplot as plt
       from snowtools.plots.EvoProfilPlot import plot_profil

       with prosimu('/rd/cenfic2/manto/viallonl/testbase/PRO/PRO_gdesRousses_2019-2020.nc') as ff:
           dz = ff.read('SNOWDZ', selectpoint=point, fill2zero=True)
           var = ff.read('SNOWTYPE', selectpoint=point)

       ax = plt.gca()
       plot_profil(ax, dz, var, colormap='grains')
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

    maxval = np.nanmax(value)
    minval = np.nanmin(value)
    if np.ma.is_masked(maxval):
        maxval = 1
    if np.ma.is_masked(minval):
        minval = 0.1

    extend = 'neither'
    if colormap == 'grains':
        cmap = Dictionnaries.grain_colormap
        bounds = np.linspace(-0.5, 14.5, 16)
        norm = colors.BoundaryNorm(bounds, cmap.N)
        vmin = -0.5
        vmax = 14.5
    elif colormap == 'echelle_log':
        cmap = cm.gray_r.copy()
        Vmin = max(minval, 0.0000000001)
        Vmax = min(max(0.000000001, maxval), 1)
        norm = colors.LogNorm(vmin=Vmin, vmax=Vmax)    
        cmap.set_under('#fff2fd')
        extend = 'min'
    elif colormap == 'echelle_log_sahara':
        cmap = cm.gist_heat_r.copy()
        Vmin = max(minval, 0.0000000001)
        Vmax = min(max(0.000000001, maxval), 1)
        value = value.clip(Vmin/2, Vmax)
        norm = colors.LogNorm(vmin=Vmin, vmax=Vmax)
        cmap.set_under('#fff2fd')
        extend = 'min'
    elif colormap == 'ratio_cisaillement':
        cmap = cm.get_cmap('viridis')
        Vmin = 0
        Vmax = 20
        norm = colors.Normalize(vmin=Vmin, vmax=Vmax, clip=True)
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
            if i == cmapl-1:
                x = 1
            else:
                x = start + i*(1-start)/cmapl
            iuse = cmapl - i - 1
            customcmap['red'].append((x, cmap.colors[iuse][0], cmap.colors[iuse][0]))
            customcmap['green'].append((x, cmap.colors[iuse][1], cmap.colors[iuse][1]))
            customcmap['blue'].append((x, cmap.colors[iuse][2], cmap.colors[iuse][2]))
        cmap = colors.LinearSegmentedColormap('ratio_cisaillment', customcmap)
    elif colormap == 'tempK':
        if vmax is None:
            Vmax = 273.15
            Vmin = Vmax-40
        else:
            Vmax = vmax
            Vmin = vmin
        norm = colors.Normalize(vmin=Vmin, vmax=Vmax)
        value[value < Vmin if ~np.isnan(Vmin) else False] = Vmin
        cmap = cm.get_cmap('RdBu_r').copy()
        cmap.set_over((0.32, 0.0, 0.097))
        extend = 'max'
    elif colormap == 'lwc':
        Vmin = 0
        Vmax = 35 if vmax is None else vmax
        norm = colors.Normalize(vmin=Vmin, vmax=Vmax)
        value[value > Vmax if ~np.isnan(Vmax) else False] = Vmax
        value[value == 0] = -1
        cmap = cm.get_cmap('viridis').copy()
        cmap.set_under('#fff2fd')
        extend = 'min'
    else:
        norm = None
        cmap = cm.get_cmap(colormap)

    rect = collections.PolyCollection(vertices, array=value[(dz > 0)].ravel(),
                                      cmap=cmap, norm=norm, edgecolors='none')
    rect.set_clim(vmin, vmax)
    ax.add_collection(rect)
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
        x = max(min(len(list_legend)-1, x), 0)
        if (type(list_legend[0]) == int or type(list_legend[0]) == str):
            return list_legend[x]
        else:
            return list_legend[x].strftime('%Y-%m-%d')

    formatter = ticker.FuncFormatter(format_ticks)
    ax.xaxis.set_major_formatter(formatter)
    ax.xaxis.set_major_locator(ticker.MaxNLocator(4))
    plt.setp(ax.xaxis.get_majorticklabels(), size='small')

    if title is not None:
        ax.set_title(title)

    if ylimit is not None:
        ax.set_ylim(0, ylimit)


def plot_grains1D(ax, dz, value, legend=None, cbar_show=True):
    """
    Trace le profil de type de grains selon la hauteur
    """

    bottom_y = np.cumsum(dz).ravel()
    top_y = bottom_y - dz.ravel()

    left_x = np.ones(shape=dz.shape, dtype='int')
    right_x = np.zeros(shape=dz.shape, dtype='int')

    vertices = np.zeros(shape=(bottom_y.shape[0], 4, 2))
    vertices[:, 0, 0] = right_x
    vertices[:, 0, 1] = bottom_y
    vertices[:, 1, 0] = right_x
    vertices[:, 1, 1] = top_y
    vertices[:, 2, 0] = left_x
    vertices[:, 2, 1] = top_y
    vertices[:, 3, 0] = left_x
    vertices[:, 3, 1] = bottom_y

    vertices = vertices[(dz > 0).ravel()]

    cmap = Dictionnaries.grain_colormap
    bounds = np.linspace(-0.5, 14.5, 16)
    norm = colors.BoundaryNorm(bounds, cmap.N)
    vmin = -0.5
    vmax = 14.5

    rect = collections.PolyCollection(vertices, array=value[(dz > 0)].ravel(),
                                      cmap=cmap, norm=norm, edgecolors='none')
    rect.set_clim(vmin, vmax)
    ax.add_collection(rect)
    ax.autoscale_view()

    if cbar_show:
        cbar = plt.colorbar(rect, ax=ax)
        labels = Dictionnaries.MEPRA_labels
        cbar.set_ticks(np.arange(np.shape(labels)[0]))
        cbar.ax.set_yticklabels(labels)
    if legend:
        ax.set_xlabel(legend)


def saison1d(ax, value, list_legend, legend=None, color='b.', title=None, ylimit=None):
    """
    Trace la variable demandee en fonction du temps
    :param ax: figure axis
    :type ax: matplotlib axis
    :param value: Value to be plot
    :type value: numpy array
    :param list_legend: list for x axis legend. The dates where there are some datas to be plotted for standard graphs
    :type list_legend: numpy array
    :param legend: legend for the colorbar
    :type legend: str
    :param color: color name
    :type color: str
    :param title: title (date for member plots for example)
    :type title: str
    :param ylimit: give the upper y-limit for the variable (= max of thickness in all the season normally)
    :type ylimit: float
    """
    if legend:
        ax.set_xlabel(legend)

    def format_ticks(x, pos):
        x = int(x)
        x = max(min(len(list_legend)-1, x), 0)
        if (type(list_legend[0]) == int or type(list_legend[0]) == str):
            return list_legend[x]
        else:
            return list_legend[x].strftime('%Y-%m-%d')

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
    Trace le profil de la variable avec type_de_grain et résistance si présent. Ce profil est effectué à une date fixée.
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
    :param ylimit: give the upper y-limit for the variable (= max of thickness in all the season normally)
    :type ylimit: float
    :param hauteur: y-value for a black line in order to better see the interaction with seasonal profile
    :type hauteur: float
    :param color: color name
    :type color: str
    :param cbar_show: show the colorbar for grain type
    :type color: boolean
    :param legend: legend (the date in standard plot)
    :type legend: datetime object or str
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

    if xlimit is not (None, None):
        Max = xlimit[1]
        Min = xlimit[0]
    else:
        Max = 0
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


def heightplot(ax, value, value_ep, list_legend, legend=None, color='b', direction_cut='up', height_cut=10.):
    """
     Trace la variable demandée au niveau de la hauteur du manteau neigeux. Cette hauteur est définie par une direction
     (direction "up" signifie hauteur mesurée depuis la terre, direction "down" signifie hauteur mesurée depuis le point
     le plus haut du manteau neigeux).
     :param ax: figure axis
     :type ax: matplotlib axis
     :param value: Value to be plot
     :type value: numpy array
     :param value_ep: thickness value for all the layers considered
     :type value_ep: numpy array
     :param list_legend: list for x axis legend. The dates where there are some datas to be plotted for standard graphs
     :type list_legend: numpy array
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
        for i in np.arange(np.alen(ep_from_topsnow)):
            if ep_from_topsnow[i, :].searchsorted(float(height_cut)) < int(value_ep.shape[1]) and \
                    float(height_cut) < ep_from_ground[i, -1]:
                y.append(value[i, ep_from_topsnow[i, :].searchsorted(float(height_cut))])
            else:
                y.append(None)
    if direction_cut == 'up':
        for i in np.arange(np.alen(ep_from_ground)):
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
        x = max(min(len(list_legend)-1, x), 0)
        if (type(list_legend[0]) == int or type(list_legend[0]) == str):
            return list_legend[x]
        else:
            return list_legend[x].strftime('%Y-%m-%d')

    formatter = ticker.FuncFormatter(format_ticks)
    ax.xaxis.set_major_formatter(formatter)
    ax.xaxis.set_major_locator(ticker.MaxNLocator(4))
    plt.setp(ax.xaxis.get_majorticklabels(), size='small')
