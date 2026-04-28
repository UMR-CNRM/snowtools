#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 31 March 2026
Script to produce benchmark plots and scores of Crocus with ESM-SnowMIP dataset
@author: lafaysse
"""

import datetime

import numpy as np
import os

import matplotlib.colors as colors
import matplotlib.pyplot as plt

from snowtools.plots.abstracts.figures import MultiPlots
from snowtools.plots.temporal.chrono import temporalsubplot
from snowtools.utils.prosimu import prosimu

from snowtools.scripts.ESMSnowMIP.ESM_snowmip import bdate, edate, list_sites, dirsnowmip
from snowtools.plots.stratiprofile.profilPlot import saisonProfil
from snowtools.plots.stratiprofile import Dictionnaries
from snowtools.scores.deterministic import DeterministicScores_Heterogeneous

dirsnowmip = os.path.join(os.environ['HOME'], 'data/ESM-SnowMIP')
# list_sites = ['cdp', 'wfj']

plt.rcParams['font.size'] = 18


class BenchmarkPlotSnowMip(temporalsubplot):

    def __init__(self):
        self.formatplot = 'png'

        self.default_list_var = ['DSN_T_ISBA', 'WSN_T_ISBA', 'ASN_ISBA', 'TS_ISBA']

        self.attributes = dict(
            DSN_T_ISBA=dict(convert_unit=1., label=u'Snow depth (m)'),
            WSN_T_ISBA=dict(label=u'Snow water equivalent ($kg~m^{-2}$)'),
            ASN_ISBA=dict(label=u'Surface albedo', firsthour=12, timestep=24),
            TS_ISBA=dict(delta_unit=273.15, thresholdmax= 273.15, label=u'Surface temperature (K)', firsthour=12,
                         timestep=12)
        )

    def set_hour_albedo(self, hour):
        self.attributes['ASN_ISBA']['firsthour'] = hour

    def openobs(self, obsfilename):
        self.fileObs = prosimu(obsfilename)
        self.timeObs = self.fileObs.readtime()

    def opensim(self, simfilename):
        self.fileSim = prosimu(simfilename)
        self.timeSim = self.fileSim.readtime()

    def close(self):
        self.fileObs.close()
        self.fileSim.close()

    def getobs(self, varname):
        ESMSnowMIP_dicvarnames = dict(DSN_T_ISBA="snd_auto", WSN_T_ISBA="snw_auto", ASN_ISBA='albs', TS_ISBA='ts')
        ESMSnowMIP_alternatevarnames = dict(DSN_T_ISBA="snd_can_auto", WSN_T_ISBA="snw_man")

        if ESMSnowMIP_dicvarnames[varname] in self.fileObs.listvar():
            varObs = self.fileObs.read(ESMSnowMIP_dicvarnames[varname])
        elif ESMSnowMIP_alternatevarnames[varname] in self.fileObs.listvar():
            varObs = self.fileObs.read(ESMSnowMIP_alternatevarnames[varname])
        else:
            print('MISSING VARIABLE ' + varname)
            return np.empty_like(self.timeObs)

        if 'convert_unit' in self.attributes[varname].keys():
            varObs = varObs * self.attributes[varname]['convert_unit']

        if 'delta_unit' in self.attributes[varname].keys():
            varObs = varObs + self.attributes[varname]['delta_unit']

        if 'thresholdmax' in self.attributes[varname].keys():
            varObs = np.where((varObs < self.attributes[varname]['thresholdmax']) | np.isnan(varObs),
                              varObs, self.attributes[varname]['thresholdmax'])
        return varObs

    def getsim(self, varname):
        varSim = self.fileSim.read(varname, selectpoint=0)
        return varSim

    def extractvar(self, time, variable, firsthour, timestep):
        firstind = self.getfirstind(time, firsthour)
        return time[firstind::timestep], variable[firstind::timestep]

    @staticmethod
    def getfirstind(time, firsthour):
        firstind = 0
        for t in time:
            if t.hour == firsthour:
                return firstind
            else:
                firstind += 1
        return None

    def load_filter_data(self, listvar=None):
        # Read all variables
        if not listvar:
            listvar = self.default_list_var

        self.dataSim = dict()
        self.dataObs = dict()
        self.timeSim_byvar = dict()
        self.timeObs_byvar = dict()

        for varname in listvar:
            # Read data in files:
            self.dataSim[varname] = self.getsim(varname)
            self.dataObs[varname] = self.getobs(varname)

        # Identify period without snow :
        nosnowperiodSim = self.dataSim['DSN_T_ISBA'] < 0.2
        nosnowperiodObs = ((self.dataObs['DSN_T_ISBA'] < 0.2) | (np.isnan(self.dataObs['DSN_T_ISBA'])) |
                           (self.dataObs['ASN_ISBA'] < 0.6))

        for varname in listvar:
            # Invalid values if no snow on the ground (this must be done before reducing time steps for some variables)
            if varname in ['TS_ISBA', 'ASN_ISBA']:
                self.dataSim[varname][nosnowperiodSim] = np.nan
                self.dataObs[varname][nosnowperiodObs] = np.nan

            # Extract time steps and save variable
            if 'timestep' in self.attributes[varname].keys() and 'firsthour' in self.attributes[varname].keys():
                self.timeObs_byvar[varname], self.dataObs[varname] = (
                    self.extractvar(self.timeObs, self.dataObs[varname], self.attributes[varname]['firsthour'],
                                    self.attributes[varname]['timestep']))
                self.timeSim_byvar[varname], self.dataSim[varname] = (
                    self.extractvar(self.timeSim, self.dataSim[varname], self.attributes[varname]['firsthour'],
                                    self.attributes[varname]['timestep']))
            else:
                self.timeObs_byvar[varname] = self.timeObs
                self.timeSim_byvar[varname] = self.timeSim

    def select_period(self, varname, datebegin, dateend):

        # Reduce dataset to a given period
        periodSim = (self.timeSim_byvar[varname] > datebegin) & (self.timeSim_byvar[varname] < dateend)
        periodObs = (self.timeObs_byvar[varname] > datebegin) & (self.timeObs_byvar[varname] < dateend)
        return (self.timeSim_byvar[varname][periodSim], self.dataSim[varname][periodSim],
                self.timeObs_byvar[varname][periodObs], self.dataObs[varname][periodObs])

    def getstrati(self, datebegin, dateend):
        periodSim = (self.timeSim > datebegin) & (self.timeSim < dateend)
        snowdz = self.fileSim.read('SNOWDZ', selectpoint=0, fill2zero=True)[periodSim, :]
        snowtype = self.fileSim.read('SNOWTYPE', selectpoint=0)[periodSim, :]

        return self.timeSim[periodSim], snowdz, snowtype

    def scores(self, datebegin=None, dateend=None, listvar=None):

        if not datebegin:
            datebegin = self.timeSim[0]
        if not dateend:
            dateend = self.timeSim[-1]

        if not listvar:
            listvar = self.default_list_var

        scores_output = dict()
        for varname in listvar:
            timeSim, varSim, timeObs, varObs = self.select_period(varname, datebegin, dateend)
            scores = DeterministicScores_Heterogeneous(timeObs, timeSim, varObs, varSim)
            nvalues = scores.nvalues()

            if varname == 'DSN_T_ISBA':
                convert_score = 100.
            else:
                convert_score = 1.

            if varname == 'ASN_ISBA':
                format_score = '%.2f'
            else:
                format_score = '%.1f'

            if nvalues > 10:
                scores_output[varname] = ' & '.join([format_score % (scores.bias * convert_score),
                                                     format_score % (scores.rmse * convert_score), '%i' % nvalues])
            else:
                scores_output[varname] = ' \ & \ & \ '
        return scores_output

    def plot(self, datebegin=None, dateend=None, listvar=None, diroutput='.', filename='BenchmarkPlot'):

        if not datebegin:
            datebegin = self.timeSim[0]
        if not dateend:
            dateend = self.timeSim[-1]

        if not listvar:
            listvar = self.default_list_var

        MP = MultiPlots(nrows=len(listvar) + 1, ncols=1)
        MP.set_figsize(18, 4*(len(listvar)+1))

        timeStrati, snowdz, snowtype = self.getstrati(datebegin, dateend)

        maxSD = max(np.nanmax(self.dataSim['DSN_T_ISBA']), np.nanmax(self.dataObs['DSN_T_ISBA'])) * 1.05

        rect = saisonProfil(MP.subplots[0], snowdz, snowtype, timeStrati, colormap='grains', cbar_show=False,
                            ylimit=maxSD)
        MP.subplots[0].set_ylabel(self.attributes['DSN_T_ISBA']['label'])
        MP.subplots[0].set_xticks([])

        # cmap = Dictionnaries.grain_colormap
        # bounds = np.linspace(-0.5, 14.5, 16)
        # norm = colors.BoundaryNorm(bounds, cmap.N)

        cbar = plt.colorbar(rect, ax=MP.subplots[0], location='top', pad=0.05, aspect=30)

        labels = Dictionnaries.MEPRA_labels
        cbar.set_ticks(np.arange(np.shape(labels)[0]))
        cbar.ax.set_xticklabels(labels)

        TSP = dict()
        for v, varname in enumerate(listvar):
            print("Create plot for variable" + varname)
            TSP[v] = temporalsubplot(MP.subplots, v+1, 0)

            timeSimPlot, varSimPlot, timeObsPlot, varObsPlot = self.select_period(varname, datebegin, dateend)

            TSP[v].add_line(timeSimPlot, varSimPlot, color='black', label='Crocus3.0.1')
            TSP[v].add_points(timeObsPlot, varObsPlot, color='red', label='Observation', fillstyle=None,
                              markeredgewidth=0.1, fmt='o')
            # TSP[v].set_yaxis(ylabel=self.attributes[varname]['label'])

            TSP[v].plot.set_xlim(datebegin, dateend)

            if varname == 'DSN_T_ISBA':
                TSP[v].finalize(timeSimPlot, ylabel=self.attributes[varname]['label'], xformatting= v == len(listvar)-1,
                                forcemin=0, forcemax = maxSD)
            else:
                TSP[v].finalize(timeSimPlot, ylabel=self.attributes[varname]['label'], xformatting= v == len(listvar)-1,
                                displaylegend= False)

        plotname = os.path.join(diroutput, '.'.join([filename, self.formatplot]))
        MP.fig.tight_layout()
        MP.save(plotname, formatout=self.formatplot, dpi=300)
        print(plotname + " is available.")


if __name__ == "__main__":

    suptitle = ''

    firstdateplot = dict(
        cdp = datetime.datetime(2007, 9, 1),
        oas = datetime.datetime(2003, 9, 1),
        obs = datetime.datetime(2003, 9, 1),
        ojp = datetime.datetime(2003, 9, 1),
        rme = datetime.datetime(2001, 9, 1),
        sap = datetime.datetime(2007, 9, 1),
        snb = datetime.datetime(2007, 9, 1),
        sod = datetime.datetime(2007, 9, 1),
        swa = datetime.datetime(2007, 9, 1),
        wfj = datetime.datetime(2007, 9, 1),
    )

    lastdateplot = dict(
        cdp = datetime.datetime(2014, 9, 1),
        oas = datetime.datetime(2010, 9, 1),
        obs = datetime.datetime(2010, 9, 1),
        ojp = datetime.datetime(2010, 9, 1),
        rme = datetime.datetime(2008, 9, 1),
        sap = datetime.datetime(2014, 9, 1),
        snb = datetime.datetime(2014, 9, 1),
        sod = datetime.datetime(2014, 9, 1),
        swa = datetime.datetime(2014, 9, 1),
        wfj = datetime.datetime(2014, 9, 1),
    )

    listvarplot = dict(
        cdp = ['DSN_T_ISBA', 'WSN_T_ISBA', 'ASN_ISBA', 'TS_ISBA'],
        oas = ['DSN_T_ISBA'],
        obs = ['DSN_T_ISBA'],
        ojp = ['DSN_T_ISBA'],
        rme = ['DSN_T_ISBA'],
        sap = ['DSN_T_ISBA'],
        snb = ['DSN_T_ISBA'],
        sod = ['DSN_T_ISBA'],
        swa = ['DSN_T_ISBA'],
        wfj = ['DSN_T_ISBA', 'WSN_T_ISBA', 'ASN_ISBA', 'TS_ISBA'],
    )

    houralbedo = dict(
        cdp = 12,
        oas = 12,
        obs = 12,
        ojp = 12,
        rme = 12,
        sap = 3,
        snb = 19,
        sod = 12,
        swa = 19,
        wfj = 12,
    )

    scores_plot_period = dict()
    scores_full_period = dict()

    for site in list_sites:

        datebeginsite = firstdateplot[site]
        dateendsite = lastdateplot[site]

        obsfile = os.path.join(dirsnowmip, 'evaldata/obs_insitu_' + site + "_" + bdate[site][0:4]
                               + "_" + edate[site][0:4] + ".nc")
        simfile = os.path.join(dirsnowmip, 'Crocus_' + site + "_insitu3.0.1/pro",
                               "PRO_" + bdate[site] + "_" + edate[site] + ".nc")

        plotsnowmip = BenchmarkPlotSnowMip()
        plotsnowmip.openobs(obsfile)
        plotsnowmip.opensim(simfile)

        plotsnowmip.set_hour_albedo(houralbedo[site])

        plotsnowmip.load_filter_data()

        plotsnowmip.plot(datebegin=datebeginsite, dateend=dateendsite,
                         diroutput=os.path.join(dirsnowmip, 'Crocus_' + site + "_insitu3.0.1"),
                         listvar=listvarplot[site])
        scores_plot_period[site] = plotsnowmip.scores(datebegin=datebeginsite, dateend=dateendsite)
        # scores_full_period[site] = plotsnowmip.scores()

        print(site)
        print("plot period")
        print(scores_plot_period[site])
        # print("full period")
        # print(scores_full_period[site])
        # print(scores_full_period[site].keys())

        plotsnowmip.close()

    # for scores_by_period in scores_plot_period:#, scores_full_period:

    print("SCORES :")
    for site in list_sites:
        line_scores = site
        for var in plotsnowmip.default_list_var:
            line_scores = ' & '.join([line_scores, scores_plot_period[site][var]])
        line_scores += "\\\\ \hline"

        print(line_scores)
