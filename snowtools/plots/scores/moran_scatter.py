# -*- coding: utf-8 -*-
"""
Created on 23 July 2024

@author: radanovics

based on code from Ange Haddjeri thesis. (chapter 5 notebook)
"""

import numpy as np
import matplotlib.pyplot as plt
from snowtools.plots.abstracts.figures import Mplfigure


class MoranScatter(Mplfigure):
    """
        Moran Scatter plot.
        Scatter plot between a variable and a spatially lagged variable.

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

    def plot_var(self, variable, lagged_variable, color='firebrick', marker='.', slopecolor='r'):
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
        b, a = np.polyfit(variable, lagged_variable, 1)
        self.plot.scatter(variable, lagged_variable, marker=marker, color=color)

        # dashed vertical line at mean of the variable
        self.plot.vlines(np.nanmean(variable), np.nanmin(lagged_variable),
                         np.nanmax(lagged_variable), linestyle='--')
        # dashed horizontal line at mean of lagged variable
        self.plot.hlines(np.nanmean(lagged_variable), np.nanmin(variable),
                         np.nanmax(variable), linestyle='--')
        # line of best fit using global I as slope
        self.plot.plot(variable, a + b * variable, color=slopecolor)
