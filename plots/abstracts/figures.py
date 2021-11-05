# -*- coding: utf-8 -*-


'''
Created on 4 déc. 2018

@author: lafaysse
'''

import os
from PIL import Image
import matplotlib.pyplot as plt


class Mplfigure(object):

    @property
    def cbar(self):
        """Colorbar of the figure"""
        return self._cbar

    @cbar.setter
    def cbar(self, value):
        self._cbar = value

    def set_title(self, title):
        if hasattr(self, 'map'):
            plt.title(title, fontsize=10)
        elif hasattr(self, 'plot'):
            self.plot.set_title(title, fontsize=10)

    def set_suptitle(self, suptitle):
        self.fig.suptitle(suptitle, fontsize=10)

    def set_figsize(self, width, height):
        fig = plt.gcf()
        fig.set_size_inches(width, height)

    def getlogo(self):
        return Image.open(os.environ["SNOWTOOLS_CEN"] + "/plots/logos/logoMF15.jpg")

    def addlogo(self):
        logo = self.getlogo()
        width, height = logo.size
        sizefig = self.fig.get_size_inches()
        widthfig = sizefig[0] * 100
        heightfig = sizefig[1] * 100

        if "map" in dir(self):
            self.fig.figimage(logo, widthfig - width, int(0.96 * heightfig) - height)
        elif "maps" in dir(self):
            self.fig.figimage(logo, widthfig - width, int(0.96 * heightfig) - height)
        else:
            self.fig.figimage(logo, widthfig - width, 0)

    def save(self, figname, formatout="pdf"):
        plt.savefig(figname, format=formatout)

    def close(self):
        self.fig.clear()
        plt.close(self.fig)


class MultiPlots(Mplfigure):
    def __init__(self, **kwargs):
        kwargs.setdefault("nrows", 1)
        kwargs.setdefault("ncols", 1)
        kwargs.setdefault("figsize", (5 * kwargs["ncols"], 4 * kwargs["nrows"]))
        self.fig, self.subplots = plt.subplots(nrows=kwargs["nrows"], ncols=kwargs["ncols"], figsize=kwargs["figsize"])


class MultiPlotsMassifs(MultiPlots):
    def __init__(self, **kwargs):
        super(MultiPlotsMassifs, self).__init__(self, **kwargs)
