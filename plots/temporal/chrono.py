#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 4 déc. 2018

@author: lafaysse
'''

import six

from plots.abstracts.figures import Mplfigure
from matplotlib import pyplot as plt
from matplotlib.dates import HourLocator, DayLocator, MonthLocator, YearLocator, DateFormatter


class temporalplot(Mplfigure):

    figsize = (5, 4)

    def __init__(self, *args, **kwargs):

        self.fig = plt.figure(figsize=self.figsize)
        self.plot = plt.subplot(111)

    def finalize(self, timeOut, **kwargs):

        self.set_xaxis(timeOut)
        self.set_yaxis(**kwargs)
        self.plot.legend(loc="upper left", fontsize="x-small")

    def set_yaxis(self, **kwargs):

        if 'forcemin' in list(kwargs.keys()) and 'forcemax' in list(kwargs.keys()):
            self.plot.set_ylim([kwargs['forcemin'], kwargs['forcemax']])

        if 'ylabel' in list(kwargs.keys()):
            self.plot.set_ylabel(kwargs['ylabel'])

    def draw(self, timeOut, *args, **kwargs):
        self.finalize(timeOut, **kwargs)

    def add_line(self, timeOut, varOut, **kwargs):

        linesargs = {}
        for key, value in six.iteritems(kwargs):
            if key not in ['forcemin', 'forcemax', 'ylabel', 'fillcolor']:
                linesargs[key] = value

        self.set_default("fmt", "-", linesargs)
        self.set_default("color", "red", linesargs)
        self.plot.plot_date(timeOut, varOut, **linesargs)

    def add_points(self, timeOut, varOut, **kwargs):
        self.set_default("fmt", "s", kwargs)
        self.set_default("color", "black", kwargs)
        self.set_default("markersize", 3, kwargs)
        self.add_line(timeOut, varOut, **kwargs)

    def set_default(self, varname, value, kwargs):
        if varname not in list(kwargs.keys()):
            kwargs[varname] = value

    def set_xaxis(self, timeOut):

        duration = timeOut[-1] - timeOut[0]
        ndays = duration.days

        if ndays <= 5:
            self.plot.xaxis.set_major_locator(HourLocator([0, 12]))
            formatDate = '%d/%m %Hh'
        elif ndays <= 90:
            self.plot.xaxis.set_major_locator(DayLocator([1, 11, 21]))
            formatDate = '%d %b\n%Y'
        elif ndays <= 366:
            self.plot.xaxis.set_major_locator(MonthLocator(list(range(1, 13, 2)), 1))
            formatDate = '%d %b\n%Y'
        elif ndays <= (5 * 365):
            interval = ndays / 365 + 1
            self.plot.xaxis.set_major_locator(MonthLocator(list(range(1, 13, interval)), 1))
            formatDate = '%d %b\n%Y'
        else:
            interval = ndays / (15 * 365) + 1
            self.plot.xaxis.set_major_locator(YearLocator(interval))
            formatDate = '%d %b\n%Y'

        self.plot.xaxis.set_major_formatter(DateFormatter(formatDate))

    def save(self, *args, **kwargs):
        super(temporalplot, self).save(*args, **kwargs)
        self.plot.cla()


class temporalplotSim(temporalplot):
    def draw(self, timeSim, varSim, **kwargs):
        if 'label' in list(kwargs.keys()):
            label = kwargs['label']
        else:
            label = 'S2M'
        self.add_line(timeSim, varSim, label=label)
        super(temporalplotSim, self).draw(timeSim, **kwargs)


class temporalplotObsSim(temporalplot):
    def draw(self, timeObs, varObs, timeSim, varSim, **kwargs):
        self.add_points(timeObs, varObs, label="Observations")
        if 'label' in list(kwargs.keys()):
            label = kwargs['label']
        else:
            label = 'S2M'
        self.add_line(timeSim, varSim, label=label)
        super(temporalplotObsSim, self).draw(timeSim, **kwargs)


class temporalplotObsMultipleSims(temporalplot):
    def draw(self, timeObs, varObs, timeSim, varSim, **kwargs):
        self.add_points(timeObs, varObs, label="Observations")
        self.set_default("label", "S2M", kwargs)
        self.add_line(timeSim, varSim, **kwargs)


class temporalplot2Axes(temporalplot):

    def addAx2(self, labelAx2):
        self.ax2 = self.plot.twinx()
        self.ax2.set_ylabel(labelAx2)

    def addVarAx2(self, timeOut, varAx2, label, color='red'):

        self.ax2.plot_date(timeOut, varAx2, "-", label=label, color=color)

    def addLegendAx2(self, location="upper right"):

        self.ax2.legend(loc=location)


class spaghettis(temporalplot):

    figsize = (10, 3)

    def __init__(self, *args, **kwargs):
        super(spaghettis, self).__init__(*args, **kwargs)
        self.fig.subplots_adjust(top=0.85)

    def draw(self, timeSim, ensemble, qmin, qmed, qmax, **kwargs):

        if 'colorquantiles' not in list(kwargs.keys()):
            kwargs['colorquantiles'] = 'red'
        if 'colormembers' not in list(kwargs.keys()):
            kwargs['colormembers'] = 'blue'

        medianlabel = "Médiane"
        quantileslabel = "Q20-Q80"
        if 'commonlabel' in list(kwargs.keys()):
            medianlabel += " " + kwargs['commonlabel']
            quantileslabel += " " + kwargs['commonlabel']

        self.add_line(timeSim, ensemble, color=kwargs['colormembers'], linewidth=0.5)
        self.plot.plot_date(timeSim, qmed, "-", color=kwargs['colorquantiles'], linewidth=2, label=medianlabel)
        self.plot.fill_between(timeSim, qmin, qmax, color=kwargs['colorquantiles'], alpha=0.25, label=quantileslabel)
        super(spaghettis, self).draw(timeSim, **kwargs)


class spaghettis_with_det(spaghettis):
    def draw(self, timeSim, deterministic, ensemble, qmin, qmed, qmax, **kwargs):

        if 'commonlabel' in list(kwargs.keys()):
            detlabel = "Dét."  + " " + kwargs['commonlabel']
        else:
            detlabel = "Déterministe"

        self.add_line(timeSim, deterministic, color="black", linewidth=2, label=detlabel, zorder=100)
        super(spaghettis_with_det, self).draw(timeSim, ensemble, qmin, qmed, qmax, **kwargs)
