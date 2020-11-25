#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 6 févr. 2019

@author: lafaysse
'''

import numpy as np
import six

from matplotlib import pyplot as plt
import matplotlib.ticker as mticker

from plots.abstracts.figures import Mplfigure


class boxplots(Mplfigure):
    '''
    '''
    figsize = (10, 8)

    def __init__(self, *args, **kwargs):

        self.fig = plt.figure(figsize=self.figsize)
        self.plot = plt.subplot(111)
        self.bp = []
        self.firstboxposition = 1
        self.indsimu = self.firstboxposition

    def draw(self, list_scores, **kwargs):

        boxplotargs = dict()
        for key, value in six.iteritems(kwargs):
            if key not in ['forcemin', 'forcemax', 'label', 'ylabel', 'fillcolor']:
                boxplotargs[key] = value

        self.bp.append(self.plot.boxplot(list_scores, notch=True, bootstrap=1000, showfliers=False, patch_artist=True, **boxplotargs))

        if 'fillcolor' in kwargs.keys():

            for element in ['boxes', 'whiskers', 'fliers', 'means', 'medians', 'caps']:
                plt.setp(self.bp[-1][element], color='black')

            for patch in self.bp[-1]['boxes']:
                patch.set_facecolor(kwargs['fillcolor'])
                patch.set_alpha(0.5)

    def finalize(self, nsimu=1, **kwargs):

        nboxes = 0
        for bp in self.bp:
            nboxes += len(bp['boxes'])

        firsttick = self.firstboxposition + (nsimu - 1) / 2.

        self.plot.set_xticks(np.arange(firsttick, nboxes + 1, nsimu))

        self.plot.set_xlim((self.firstboxposition - 1, nboxes + 1))

        self.set_yaxis(**kwargs)
        self.plot.grid(axis='y')
        plt.tight_layout()

        list_legend = []
        for bp in self.bp:
            list_legend.append(bp['boxes'][0])

        if 'label' in kwargs.keys():
            self.plot.legend(list_legend, kwargs['label'], loc="upper left", fontsize="small")

    def set_yaxis(self, **kwargs):

        if 'forcemin' in kwargs.keys() and 'forcemax' in kwargs.keys():
            self.plot.set_ylim([kwargs['forcemin'], kwargs['forcemax']])

        if 'ylabel' in kwargs.keys():
            self.plot.set_ylabel(kwargs['ylabel'])


class boxplots_bydepartment(boxplots):

    def draw(self, stations, scores, nsimu = 1, **kwargs):

        list_scores = []
        stringstations = ['%08i' % s for s in stations]
        list_dep = [s[0:2] for s in stringstations]

        france = [len(s) == 8 for s in stringstations]

#         list_dep_uniq = list(set(list_dep))
#         list_dep_uniq.sort()

        list_dep_uniq = dict(
                Alps = ['74', '73', '38, 26', '05', '04, 06'],
                Pyrenees = ['64, 65', '31, 09', '66, 99'],
                Corsica = ['20'],
            )

        for dep in list_dep_uniq:

            if ',' in dep:
                deps = dep.split(',')
                inddep = np.array(list_dep) == -999
                for d in deps:
                    inddep = (np.array(list_dep) == d.strip()) | inddep
            else:
                inddep = np.array(list_dep) == dep

            inddep = inddep & france

            print dep, np.sum(inddep)

            list_scores.append(scores[inddep])

        kwargs['labels'] = list_dep_uniq

        kwargs['positions'] = range(self.indsimu, 1 + len(list_dep_uniq) * nsimu, nsimu)

        print kwargs['positions']

        self.plot.set_xlabel(u'Département')
        super(boxplots_bydepartment, self).draw(list_scores, **kwargs)
        self.indsimu += 1


class boxplots_byelevation(boxplots):

    def label_elevation(self, tuple_elevations):
        return str(tuple_elevations[0]) + " - " + str(tuple_elevations[1]) + " m"

    def draw(self, elevations, scores, nsimu = 1, **kwargs):

        list_scores = []

        list_levels = [(600, 1200), (1200, 1600), (1600, 2000), (2000, 2400), (2400, 3300)]

        for (minlevel, maxlevel) in list_levels:
            list_scores.append(scores[(elevations >= minlevel) & (elevations < maxlevel)])

        kwargs['labels'] = map(self.label_elevation, list_levels)

        kwargs['positions'] = range(self.indsimu, 1 + len(list_levels) * nsimu, nsimu)

        self.plot.set_xlabel(u'Elevation')
        super(boxplots_byelevation, self).draw(list_scores, **kwargs)
        self.indsimu += 1


class boxplots_byyear(boxplots):

    def draw(self, list_years, list_scores, nsimu = 1, **kwargs):

        nyears = len(list_years)
        stepticks = nyears / 15 + 1
        list_years_str = map(str, list_years)

        list_labels = []
        for y, year in enumerate(list_years_str):
            if y % stepticks == 0:
                list_labels.append(year)
            else:
                list_labels.append("")

        print list_labels

        kwargs['labels'] = list_labels

        kwargs['positions'] = range(self.indsimu, 1 + len(list_labels) * nsimu, nsimu)

        print ('debug')
        print (nsimu)
        print (kwargs['positions'])

        self.plot.set_xlabel(u'Year')
        super(boxplots_byyear, self).draw(list_scores, **kwargs)
        self.indsimu += 1

