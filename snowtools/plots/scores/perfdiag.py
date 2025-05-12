# -*- coding: utf-8 -*-
"""
Created on 24 April 2024

@author: radanovics

based on code from Ange Haddjeri thesis.
"""

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.colors as colors
import matplotlib.cm as cm
from snowtools.plots.abstracts.figures import Mplfigure


def truncate_colormap(cmap, minval=0.0, maxval=1.0, n=100):
    new_cmap = colors.LinearSegmentedColormap.from_list(
        'trunc({n},{a:.2f},{b:.2f})'.format(n=cmap.name, a=minval, b=maxval),
        cmap(np.linspace(minval, maxval, n)))
    return new_cmap


class PerfDiag(Mplfigure):
    """
    Performance diagram
    Plot 1-FAR on x-axis, POD on y-axis and iso-lines for frequency bias and CSI

    Example:

    .. code-block:: python

        import matplotlib.pyplot as plt
        from snowtools.plots.scores.perfdiag import PerfDiag

        diag = PerfDiag()
        diag.plot_scores([0.2, 0.5, 0.6, 0.9], [0.1, 0.5, 0.7, 0.4])
        diag.add_legend()
        diag.addlogo()
        plt.show()
        diag.close()

    .. figure:: /images/perfdiag.png
       :align: center

    """
    default_colors = plt.get_cmap('Paired')  #: default colors for the different experiments

    def __init__(self, figsize=(9, 8), csi_cmap='Purples', title="Performance diagram"):
        """

        :param figsize: figure size
        :param csi_cmap: colormap for CSI shading. Will be truncated
        :param title: Figure title
        """
        self.fig = plt.figure(figsize=figsize)
        self.plot = plt.subplot(111)
        self.xlabel = "Success Ratio (1-FAR)"
        self.ylabel = "Probability of Detection (POD)"
        self.ticks = np.arange(0, 1.1, 0.1)
        self.dpi = 300
        self.base_cmap = plt.get_cmap(csi_cmap)
        self.csi_cmap = truncate_colormap(self.base_cmap, 00.5, 1)
        self.csi_cmap2 = truncate_colormap(self.base_cmap, 0, 0.5)
        self.set_title(title, fontsize=14)
        self.csi_label = "Critical Success Index (CSI)"
        self.legend_params = dict(loc='upper left', fontsize=12, framealpha=0.9, frameon=True)
        self.diagram_background()
        self.add_labels()

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

    @property
    def csi_label(self):
        return self._csi_label

    @csi_label.setter
    def csi_label(self, value):
        self._csi_label = value

    def diagram_background(self):
        """
        create diagram background with CSI iso-lines and shading and frequency bias iso-lines
        """
        grid_ticks = np.arange(0., 1.01, 0.01)
        sr_g, pod_g = np.meshgrid(grid_ticks, grid_ticks)
        bias = np.empty((len(grid_ticks), len(grid_ticks)))
        bias.fill(np.nan)
        bias[:, 1:] = pod_g[:, 1:] / sr_g[:, 1:]
        csi = np.zeros((len(grid_ticks), len(grid_ticks)))
        # csi.fill(np.nan)
        csi[1:, 1:] = 1.0 / (1.0 / sr_g[1:, 1:] + 1.0 / pod_g[1:, 1:] - 1.0)

        csi_contourf = self.plot.contourf(sr_g, pod_g, csi, np.arange(0.1, 1.1, 0.05),
                                    extend="max", cmap=self.csi_cmap2)
        csi_contour = self.plot.contour(sr_g, pod_g, csi, np.arange(0.1, 1.1, 0.05),
                                  extend="max", cmap=self.csi_cmap)
        self.cbar = plt.colorbar(csi_contourf) #, ax=self.plot)
        self.cbar.set_label(self.csi_label, fontsize=14)
        b_contour = self.plot.contour(sr_g, pod_g, bias, [0.3, 0.5, 0.8, 1, 1.2, 1.5, 2, 3, 5, 10],
                                colors="k", linestyles="dashed")

    def add_datapoint(self, pod, far, marker='*', color='#6ACC64', label=None,
                      linestyle='None', markersize=10):
        """

        :param pod: Probability of detection
        :param far: False Alarm Ratio
        :param marker: matplotlib marker type
        :param color: matplotlib compatible color specification
        :param label: experiment label for the legend
        :param linestyle: matplotlib linestyle
        :param markersize: marker size
        :return:
        """
        self.plot.plot(1-far, pod, marker=marker, color=color, label=label, linestyle=linestyle, markersize=markersize)

    def plot_scores(self, pod, far, list_colors=None, list_markers=None, list_labels=None, list_markersizes=None):
        """

        :param pod: list or 1d array of Probability of detection values
        :param far: list or 1d array of False Alarm Ratio
        :param list_colors: colors for the different points. List of length 1
        (all points will be plotted with the same color)
         or same length as pod and far. If None, default colours `self.default_colors` will be used.
        :type list_colors: list
        :param list_markers: Markers for the different points. List of length 1
        (all points will be plotted with the same marker) or same length as pod and far. If None, all points will
        be plotted with the '*' marker
        :type list_markers: list
        :param list_labels: List of the same length as pod and far with labels to put on the legend.
        If None, the labels will be "experiment 1", "experiment 2 etc.
        :type list_labels: list
        :param list_markersizes: Marker sizes for the different points. List of length 1
        (all points will be plotted with the same marker size) or same length as pod and far. If None, all points will
        be plotted with a marker size of 10.
        :type list_markersizes: list
        :return:
        """
        try:
            assert len(pod) == len(far)
        except AssertionError:
            raise AssertionError("POD and FAR lists have to be same length")
        if list_colors:
            if len(list_colors) == 1:
                colors = np.repeat(list_colors[0], len(pod))
            else:
                assert len(list_colors) == len(pod)
                colors = list_colors
        else:
            colors = self.default_colors.colors[:len(pod)]
        if list_markers:
            if len(list_markers) == 1:
                markers = np.repeat(list_markers[0], len(pod))
            else:
                assert len(list_markers) == len(pod)
                markers = list_markers
        else:
            markers = np.repeat('*', len(pod))
        if list_labels:
            if len(list_labels) == 1:
                labels = np.repeat(list_labels[0], len(pod))
            else:
                assert len(list_labels) == len(pod)
                labels = list_labels
        else:
            labels = ['experiment {0}'.format(i) for i in range(1, len(pod)+1)]
        if list_markersizes:
            if len(list_markersizes) == 1:
                markersizes = np.repeat(list_markersizes[0], len(pod))
            else:
                assert len(list_markersizes) == len(pod)
                markersizes = list_markersizes
        else:
            markersizes = np.repeat(10, len(pod))

        for ii, (ipod, ifar, icolor, imarker, ilabel, isize) in enumerate(zip(pod, far, colors,
                                                                            markers, labels, markersizes)):
            self.add_datapoint(ipod, ifar, color=icolor, marker=imarker, label=ilabel, markersize=isize)

    def add_labels(self):
        """
        add axis labels and tick markers. Annotate the background lines with "Frequency Bias" and "CSI score".
        :return:
        """
        self.plot.set_xlabel(self.xlabel)
        self.plot.set_ylabel(self.ylabel)
        self.plot.set_xticks(self.ticks)
        self.plot.set_yticks(self.ticks)
        self.plot.text(0.78, 0.37, "Frequency Bias", fontdict=dict(fontsize=14, rotation=23))
        self.plot.text(0.5, 0.08, "CSI score", fontdict=dict(fontsize=14, rotation=0), color='#6B51A3')

    def add_legend(self):
        """
        add a legend.
        """
        self.plot.legend(**self.legend_params)
#
# import matplotlib.patches as mpatches
#
# green_patch = mpatches.Patch(color='#6ACC64', label='Safran precipitation')
# orange_patch = mpatches.Patch(color='#EE854A', label='Safran HR precipitation')
# red_patch = mpatches.Patch(color='#D65F5F', label='Antilope precipitation')
# blue_patch = mpatches.Patch(color='#4878D0', label='Arome precipitation')
# # csii=Line2D([0], [0], color='mediumpurple', lw=3, label='CSI score')
# legend1 = plt.legend(handles=[green_patch, orange_patch, red_patch, blue_patch], loc='lower right')
#
# from matplotlib.lines import Line2D
#
# legend_elements = [Line2D([0], [0], marker='*', color='w',
#                           label='Simulation with snow transport\nNeighborhood radius of 0\n(equivalent to pixel-pixel)',
#                           markerfacecolor='dimgray', markersize=10),
#                    Line2D([0], [0], marker='p', color='w',
#                           label='Simulation without snow transport\nNeighborhood radius of 0\n(equivalent to pixel-pixel)',
#                           markerfacecolor='dimgray', markersize=10),
#                    Line2D([0], [0], marker='P', color='w',
#                           label='Simulation with snow transport\nNeighborhood radius of 1', markerfacecolor='dimgray',
#                           markersize=10),
#                    Line2D([0], [0], marker='s', color='w',
#                           label='Simulation without snow transport\nNeighborhood radius of 1',
#                           markerfacecolor='dimgray', markersize=10),
#                    ]
# legend2 = plt.legend(handles=legend_elements, loc='upper left')
# plt.gca().add_artist(legend1)
# plt.gca().add_artist(legend2)
# plt.clabel(csi_contour, inline=True, fontsize=9)
# plt.clabel(b_contour, fmt="%1.1f", )  # manual=[(0.2, 0.9), (0.4, 0.9), (0.6, 0.9), (0.7, 0.7)])
# plt.savefig("CHAP4/perfdiagram.pdf", bbox_inches="tight")


class FuzzyScoreDiagram(Mplfigure):

    def __init__(self, figsize=(9, 8), cmap='winter', title="Fuzzy Score Diagram"):
        self.fig = plt.figure(figsize=figsize)
        self.plot = plt.subplot(111)
        self.xlabel = "Thresholds"
        self.ylabel = "Neighborhood size"
        self.cmap = cmap
        self.set_title(title, fontsize=14)

    def draw(self, data, xlabels, ylabels):
        self.plot.pcolormesh(data, cmap=self.cmap)
