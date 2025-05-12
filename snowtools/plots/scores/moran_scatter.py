# -*- coding: utf-8 -*-
"""
Created on 23 July 2024

@author: radanovics

based on code from Ange Haddjeri thesis. (chapter 5 notebook)
"""

import numpy as np
import matplotlib
from matplotlib import cm
import matplotlib.pyplot as plt
from packaging.version import Version
from snowtools.plots.abstracts.figures import Mplfigure


def get_moran_palette():
    """
    get colors for the local moran Is plots.
    The color map is derived from the 'Paired' colormap, but only dark and light red
    and dark and light blue are selected. The under color is set to light grey.
    :return: a color palette
    :rtype: matplotlib.colors.LinearSegmentedColormap
    """
    try:
        if Version(matplotlib.__version__) < Version("3.6"):
            mypalette = matplotlib.cm.get_cmap('moran_colors')
        else:
            mypalette = plt.get_cmap('moran_colors')
    except ValueError:
        if Version(matplotlib.__version__) < Version("3.6"):
            palette = matplotlib.cm.get_cmap('Paired')
        else:
            palette = plt.get_cmap('Paired').copy()
        mypalette = matplotlib.colors.LinearSegmentedColormap.from_list('moran_colors',
                                                                        colors=['lightgrey'] + [palette.colors[i] for i in [5, 0, 1, 4]],
                                                                        N=5)
        # mypalette.set_under(color='lightgrey')
        if Version(matplotlib.__version__) >= Version("3.5"):
            matplotlib.colormaps.register(cmap=mypalette) # for matplotlib >= 3.5
        else:
            cm.register_cmap(cmap=mypalette) # for matplotlib 3.4

    return mypalette


class MoranScatter(Mplfigure):
    """
        Moran Scatter plot.
        Scatter plot between a variable and a spatially lagged variable.
        https://dces.wisc.edu/wp-content/uploads/sites/128/2013/08/W4_Anselin1996.pdf

        Example:

        .. code-block:: python

            import numpy as np
            import matplotlib.pyplot as plt
            from snowtools.plots.scores.moran_scatter import MoranScatter

            diag = MoranScatter('snow height')
            diag.plot_var(np.array([0.2, 0.5, 0.6, 0.9, 0.1, 0.4, 1.3, 0.3, 0.15, 0.25]),
             np.array([0.1, 0.5, 0.7, 1.1, 0.4, 0.5, 0.8, 0.9, 0.2, 0.15]))
            diag.addlogo()
            plt.show()
            diag.close()

        .. figure:: /images/moranscatter.png
           :align: center
    """
    def __init__(self, variable_name, figsize=(9, 9), title="Moran Scatterplot"):
        """

        :param variable_name: variable name used for axis labels
        :type variable_name: str
        :param figsize: optional figure size. Default: (9, 9)
        :type figsize: tuple
        :param title: figure title. Default: "Moran Scatterplot"
        :type title: str
        """
        self.fig = plt.figure(figsize=figsize)
        self.plot = plt.subplot(111)
        self.xlabel = variable_name
        self.ylabel = "Spatial lag of " + variable_name
        self.set_title(title, fontsize=14)
        self.plot.set_xlabel(self.xlabel)
        self.plot.set_ylabel(self.ylabel)

    @property
    def xlabel(self):
        return self._xlabel

    @xlabel.setter
    def xlabel(self, value):
        self._xlabel = value

    @property
    def ylabel(self):
        return self._ylabel

    @ylabel.setter
    def ylabel(self, value):
        self._ylabel = value

    def plot_var(self, variable, lagged_variable, color='firebrick',  marker='.', slopecolor='r'):
        """
        plot data.
        :param variable: data variable
        :type variable: np.array
        :param lagged_variable: lagged data variable
        :type lagged_variable: np.array
        :param color: color of points Default: 'firebrick'
        :param marker: marker type. Default: '.'
        :param slopecolor: color of regression line. Default: 'r' (red)
        """
        try:
            assert len(variable) == len(lagged_variable)
        except AssertionError:
            raise AssertionError("variable and lagged_variable arrays have to be same length")

        self.plot.scatter(variable, lagged_variable, marker=marker, color=color)
        # dashed vertical line at mean of the variable
        self.plot.vlines(np.nanmean(variable), np.nanmin(lagged_variable),
                         np.nanmax(lagged_variable), linestyle='--')
        # dashed horizontal line at mean of lagged variable
        self.plot.hlines(np.nanmean(lagged_variable), np.nanmin(variable),
                         np.nanmax(variable), linestyle='--')
        # line of best fit using global I as slope
        variable[np.isnan(lagged_variable)] = np.nan
        lagged_variable[np.isnan(variable)] = np.nan
        b, a = np.polyfit(np.ravel(variable[~np.isnan(variable)]),
                          np.ravel(lagged_variable[~np.isnan(lagged_variable)]), 1)
        # print(a, b)
        self.plot.plot(np.ravel(variable), a + b * np.ravel(variable), color=slopecolor)


class MoranScatterColored(MoranScatter):
    """
    class for colored Moran Scatter Plot. The points are colored according to the quandrant
    and non-significant local Moran values are plotted in grey.
    """

    @property
    def palette(self):
        """
        Moran scatter specific colormap
        :return: colormap instance
        :rtype: matplotlib.colors.LinearSegmentedColormap
        """
        return get_moran_palette()

    @property
    def norm(self):
        return matplotlib.colors.Normalize(vmax=4, vmin=0)

    @property
    def legend_params(self):
        """
        standard legend parameters for Moran Scatter plot
        :return: legend parameters
        :rtype: dict
        """
        return dict(loc='upper left', fontsize=12, framealpha=0.9, frameon=True)

    def plot_var(self, variable, lagged_variable, color=None, marker='.', slopecolor='r'):
        """
        plot data.
        :param variable: data variable
        :type variable: np.array
        :param lagged_variable: lagged data variable
        :type lagged_variable: np.array
        :param color: array of quadrant numbers to use for coloring
        :type color: np.array
        :param marker: marker type. Default: '.'
        :param slopecolor: color of regression line. Default: 'r' (red)
        """
        try:
            assert len(variable) == len(lagged_variable)
        except AssertionError:
            raise AssertionError("variable and lagged_variable arrays have to be same length")
        try:
            assert len(color) == len(variable)
        except AssertionError:
            raise AssertionError("variable and color (quadrants) arrays have to be same length")

        s = self.plot.scatter(variable, lagged_variable, marker=marker, c=color, cmap=self.palette,
                             norm=self.norm)
        # dashed vertical line at mean of the variable
        self.plot.vlines(np.nanmean(variable), np.nanmin(lagged_variable),
                         np.nanmax(lagged_variable), linestyle='--')
        # dashed horizontal line at mean of lagged variable
        self.plot.hlines(np.nanmean(lagged_variable), np.nanmin(variable),
                         np.nanmax(variable), linestyle='--')
        # line of best fit using global I as slope
        variable[np.isnan(lagged_variable)] = np.nan
        lagged_variable[np.isnan(variable)] = np.nan
        b, a = np.polyfit(np.ravel(variable[~np.isnan(variable)]),
                          np.ravel(lagged_variable[~np.isnan(lagged_variable)]), 1)
        # print(a, b)
        self.plot.plot(np.ravel(variable), a + b * np.ravel(variable), color=slopecolor)

        self.add_legend()

    def add_legend(self):
        """
        add a legend to the moran scatter plot
        """
        from matplotlib.patches import Patch
        legend_elements = [Patch(color=self.palette(0),
                                 label='not significant'),
                           Patch(color=self.palette(1), label='hot spot'),
                           Patch(color=self.palette(2), label='doughnut'),
                           Patch(color=self.palette(3), label='cold spot'),
                           Patch(color=self.palette(4), label='diamond')]
        legend2 = plt.legend(handles=legend_elements, **self.legend_params)


