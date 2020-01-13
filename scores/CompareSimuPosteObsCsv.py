#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 5 dec. 2018

@author: lafaysse
'''

from optparse import OptionParser
import sys
import os
import numpy as np
import datetime

import matplotlib
matplotlib.use('Agg')

from utils.prosimu import prosimu
from utils.obscsv import obscsv, multiplecsv
from utils.resources import absolute_path, get_file_period
from utils.dates import checkdateafter, check_and_convert_date
from utils.infomassifs import infomassifs
from plots.temporal.chrono import temporalplotObsMultipleSims
from plots.boxplots.boxplots import boxplots_bydepartment, boxplots_byelevation, boxplots_byyear
from scores.deterministic import DeterministicScores_Heterogeneous

usage = "CompareSimuPosteObsCsv.py [--scores] [--plot] -b YYYYMMDD -e YYYYMMDD --dirsim=dirsim1,dirsim2 --labels=label1,labe2 --dirplot=dirplot --format=pdf,png,eps --yearly"

default = dict(fileobs="/manto/lafaysse/data/csv/OBS_1983080100_2019053123.csv",
               dirsim='/era40/vortex/s2m/postes/reanalysis/pro')

IM = infomassifs()


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("--fileobs",
                      action="store", type="string", dest="fileobs", default=default["fileobs"],
                      help="geometry")

    parser.add_option("-b", "--begin",
                      action="store", type="string", dest="datebegin", default="1980080106",
                      help="First year of extraction")

    parser.add_option("-e", "--end",
                      action="store", type="string", dest="dateend", default="2019080106",
                      help="Last year of extraction")

    parser.add_option("--dirsim",
                      action="store", dest="dirsim", default=default["dirsim"],
                      help="Directory of simulation outputs or list of directories")

    parser.add_option("--labels",
                      action="store", dest="labels", default=None,
                      help="Directory of simulation outputs or list of directories")

    parser.add_option("--dirplot",
                      action="store", dest="dirplot", default=os.getcwd() + "/plot",
                      help="Directory where the figures are saved")

    parser.add_option("--format",
                      action="store", dest="format", default="png",
                      help="Format of plots")

    parser.add_option("--yearly",
                      action="store_true", dest="yearly", default=False,
                      help="Yearly plots")

    parser.add_option("--plot",
                      action="store_true", dest="plot", default=False,
                      help="Plot")

    parser.add_option("--scores",
                      action="store_true", dest="scores", default=False,
                      help="Compute scores")

    parser.add_option("--sim2",
                      action="store_true", dest="sim2", default=False,
                      help="Specific case of SIM2 evaluations")

    (options, args) = parser.parse_args(arguments)
    del args
    return options


def check_and_convert_options(options, vortex=False):

    # Conversions of local paths in absolute paths
    [options.fileobs, options.dirsim, options.dirplot] = \
        list(map(absolute_path, [options.fileobs, options.dirsim, options.dirplot]))

    if type(options.dirsim) is not list:
        options.dirsim = [options.dirsim]

    # Create plot directory
    if not os.path.isdir(options.dirplot):
        os.makedirs(options.dirplot)

    # Conversions of dates in datetime objects
    [options.datebegin, options.dateend] = list(map(check_and_convert_date, [options.datebegin, options.dateend]))
    checkdateafter(options.dateend, options.datebegin)

    return options


def build_title(station):
    lati, longi, alti = IM.infoposte(station)  # @UnusedVariable
    return unicode(IM.nameposte(station).decode("utf-8")) + u" " + unicode(int(alti)) + u" m"


def build_title_with_scores(station, bias, rmse):
    return build_title(station) + " Bias = " + '%.1f' % bias + " RMSE = " + '%.1f' % rmse


def yearlyplots(datebegin, dateend, dataObs):
    datepro = datebegin

    yearly_nvalues = []
    yearly_bias = []
    yearly_rmse = []
    list_years = []

    while True:
        C = ComparisonSimObs(dataObs)
        for d, dirsim in enumerate(options.dirsim):
            dateprobegin, dateproend = get_file_period("PRO", dirsim, datepro, dateend)

            if options.plot or options.scores:
                C.read_sim("PRO.nc")

        if options.labels:
            C.set_sim_labels(map(str.strip, options.labels.split(',')))

        if options.scores:
            C.scores()
            list_years.append(dateprobegin.year + 1)
            yearly_nvalues.append(C.nvalues)
            yearly_bias.append(C.bias)
            yearly_rmse.append(C.rmse)

        if options.plot:
            C.plot()

        datepro = dateproend

        if dateproend >= dateend:
            break

    if options.scores:
        boxplots_yearly(list_years, yearly_nvalues, yearly_bias, list_colors = C.list_colors, list_labels = C.list_labels, label='bias', ylabel='Bias (cm)')
        boxplots_yearly(list_years, yearly_nvalues, yearly_rmse, list_colors = C.list_colors, list_labels = C.list_labels, label='rmse', ylabel='RMSE (cm)')


def boxplots_yearly(list_years, list_nvalues, list_scores, list_colors, list_labels, label, **kwargs):

    nyears = len(list_years)
    (nsim, nstations) = list_nvalues[0].shape

    list_valid = []

    for y, year in enumerate(list_years):
        valid = list_nvalues[y][0, :] == list_nvalues[y][0, :]
        for sim in range(0, nsim):
            valid = list_nvalues[y][sim, :] > 5 & valid
        list_valid.append(valid)

    b = boxplots_byyear()
    for sim in range(0, nsim):
        list_scores_valid = []
        for y, year in enumerate(list_years):
            list_scores_valid.append(list_scores[y][sim, list_valid[y]])

        kwargs['fillcolor'] = list_colors[sim]

        b.draw(list_years, list_scores_valid, nsimu=nsim, **kwargs)

    kwargs['label'] = list_labels
    b.finalize(nsimu=nsim, **kwargs)

    plotfilename = options.dirplot + "/" + label + "_years." + options.format
    print ('plot ' + plotfilename)
    b.save(plotfilename, formatout=options.format)

    b.close()


def fullplots(datebegin, dateend, dataObs):

    datepro = datebegin

    list_list_pro = []

    for d, dirsim in enumerate(options.dirsim):
        list_list_pro.append([])

    print ("Get simulation files")

    while True:

        for d, dirsim in enumerate(options.dirsim):
            dateprobegin, dateproend = get_file_period("PRO", dirsim, datepro, dateend)
            i = len(list_list_pro[d])
            proname = "PRO_part" + str(i) + "_simu" + str(d) + ".nc"
            os.rename("PRO.nc", proname)
            list_list_pro[d].append(proname)

        datepro = dateproend
        if dateproend >= options.dateend:
            break

    C = ComparisonSimObs(dataObs)
    for d, dirsim in enumerate(options.dirsim):
        print ("Read simulation files from " + dirsim)
        C.read_sim(list_list_pro[d])

    if options.labels:
        C.set_sim_labels(map(str.strip, options.labels.split(',')))

    if options.scores:
        print ("Compute scores")
        C.scores()
        C.allboxplots()

    if options.plot:
        print ("Draw plots")
        C.plot()


def fullplotsSIM2(datebegin, dateend, dataObs):

    list_list_pro = []

    for d, dirsim in enumerate(options.dirsim):
        list_list_pro.append([])

    print ("Get simulation files")

    prefix = dict(sim2_interp = "SIM_NEW_",
                  sim2_nointerp = "SIM_NEW_NO_INTERP_",
                  simref = "")

    for year in range(datebegin.year, dateend.year):

        stringyear = str(year) + str(year + 1)

        for d, dirsim in enumerate(options.dirsim):

            proname = dirsim + "/" + prefix[os.path.basename(dirsim)] + "hauteur_neige_" + stringyear

            list_list_pro[d].append(proname)

    C = ComparisonSimObsSIM2(dataObs)

    for d, dirsim in enumerate(options.dirsim):
        print ("Read simulation files from " + dirsim)
        C.read_sim(list_list_pro[d])

    if options.labels:
        C.set_sim_labels(map(str.strip, options.labels.split(',')))

    if options.scores:
        print ("Compute scores")
        C.scores()
        C.allboxplots()

    if options.plot:
        print ("Draw plots")
        C.plot()


class ComparisonSimObs(object):

    def __init__(self, dataObs):

        self.dataObs = dataObs

        self.timeSim = []
        self.sdSim = []
        self.SimStations = []
        self.Simelevations = []

        listStations = dataObs.getListStations()

        listSitesMetadata = IM.getListSites()

        self.listStations = list(set(listStations) & set(listSitesMetadata))

        self.elevations = map(IM.altiposte, self.listStations)

        self.nstations = len(self.listStations)

        # Default labels
        self.set_sim_labels(['New', 'Old', '', '', ''])

        # Default colors
        self.set_sim_colors(['blue', 'red', 'grey', 'orange', 'green'])

    def set_sim_colors(self, list_colors):
        self.list_colors = list_colors

    def set_sim_labels(self, list_labels):
        self.list_labels = list_labels

    def plot(self):

        dateprobegin = self.timeSim[0][0]
        dateproend = self.timeSim[0][-1]

        myplot = temporalplotObsMultipleSims()

        if dateproend - dateprobegin > datetime.timedelta(days = 1000):
            myplot.set_figsize(15, 4)

        nivose = 0
        for s, station in enumerate(self.listStations):
            available_sim = []
            for indSim in range(0, self.nsim):
                available, timeObs, timeSim, sdObs, sdSim  = self.get_obs_sim(station, indSim)
                available_sim.append(available)
                if available:
                    if not any(available_sim[:-1]):
                        myplot.draw(timeObs, sdObs, timeSim, sdSim, color=self.list_colors[indSim], label=self.list_labels[indSim])
                        timeOut = timeSim[:]
                    else:
                        myplot.add_line(timeSim, sdSim, color=self.list_colors[indSim], label=self.list_labels[indSim])

            if any(available_sim):

                if 'NIVOSE' in build_title(station):
                    nivose += 1

                if self.nsim > 1:
                    myplot.set_title(build_title(station))
                if self.nsim == 1 and hasattr(self, 'bias') and hasattr(self, 'rmse'):
                    myplot.set_title(build_title_with_scores(station, self.bias[0, s], self.rmse[0, s]))

                plotfilename = options.dirplot + "/" + station + "_" + dateprobegin.strftime("%Y") + "_" + dateproend.strftime("%Y") + "." + options.format

                myplot.finalize(timeOut, ylabel="Snow depth (cm)")
                print ('plot ' + plotfilename)
                myplot.save(plotfilename, formatout=options.format)

        print ("NUMBER OF NIVOSE")
        print (nivose)

    def scores(self):

        self.nvalues = np.zeros((self.nsim, self.nstations))
        self.bias = np.zeros((self.nsim, self.nstations))
        self.rmse = np.zeros((self.nsim, self.nstations))

        for s, station in enumerate(self.listStations):

            for indSim in range(0, self.nsim):

                available, timeObs, timeSim, sdObs, sdSim = self.get_obs_sim(station, indSim)

                if available:
                    scores = DeterministicScores_Heterogeneous(timeObs, timeSim, sdObs, sdSim)
                    self.nvalues[indSim, s] = scores.nvalues()
                    self.bias[indSim, s] = scores.bias()
                    self.rmse[indSim, s] = scores.rmse()

    def allboxplots(self):

        arrayStations = np.array(map(int, self.listStations))

        self.boxplots_scores(arrayStations, np.array(self.elevations), self.bias, 'bias', ylabel='Bias (cm)')
        self.boxplots_scores(arrayStations, np.array(self.elevations), self.rmse, 'rmse', ylabel='RMSE (cm)')

    def get_obs_sim(self, station, indSim):

        timeObs = self.dataObs.get(station, "time")
        sdObs = self.dataObs.get(station, "SNOWDEPTH")  # cm

        periodObs = (timeObs > min(self.timeSim[indSim])) & (timeObs < max(self.timeSim[indSim]))

        winter = np.empty_like(periodObs)
        for i, t in enumerate(timeObs):
            winter[i] = (t.month >= 10) or (t.month <= 6)

        periodObs = periodObs & winter

        availObs = np.sum(sdObs[periodObs] > 0) > 5
        
        
        #print type(self.SimStations[indSim])
        
        ind = self.SimStations[indSim] == int(station)
        availSim = np.sum(ind) == 1

        availCommon = availObs and availSim

        print np.sum(availCommon)

        if availCommon:
            return availCommon, timeObs[periodObs], self.timeSim[indSim], sdObs[periodObs], np.squeeze(self.sdSim[indSim][:, ind])
        else:
            return availCommon, None, None, None, None

    def read_sim(self, pro):

        pro = prosimu(pro)

        self.SimStations.append(pro.read("station"))
        self.Simelevations.append(pro.read("ZS"))

        self.timeSim.append(pro.readtime())

        try:
            self.sdSim.append(pro.read("DSN_T_ISBA", fill2zero=True) * 100.)  # cm
        except:
            self.sdSim.append(pro.read("SNOWDEPTH", fill2zero=True) * 100.)  # cm

        pro.close()

        self.nsim = len(self.sdSim)

    def boxplots_scores(self, stations, elevations, list_scores, label, **kwargs):

        b1 = boxplots_bydepartment()

        valid = self.nvalues[0, :] >= 0

        print 'number of obs'
        print np.sum(valid)

        for indSim in range(0, self.nsim):
            valid = (self.nvalues[indSim, :] > 10) & (valid)
            print 'number of obs with ' + str(indSim + 1) + "available simulation(s)"
            print np.sum(valid)

        for indSim in range(0, self.nsim):
            kwargs['fillcolor'] = self.list_colors[indSim]
            b1.draw(stations[valid], list_scores[indSim, valid], nsimu=self.nsim, **kwargs)

            #print list_scores[indSim, valid].shape

        kwargs['label'] = self.list_labels
        b1.finalize(nsimu=self.nsim, **kwargs)
        plotfilename = options.dirplot + "/" + label + "_departments." + options.format
        print ('plot ' + plotfilename)
        b1.save(plotfilename, formatout=options.format)

        b1.close()

        b2 = boxplots_byelevation()

        for indSim in range(0, self.nsim):
            valid = self.nvalues[indSim, :] > 10
            kwargs['fillcolor'] = self.list_colors[indSim]
            kwargs['label'] = self.list_labels[indSim]

            b2.draw(elevations[valid], list_scores[indSim, valid], nsimu=self.nsim, **kwargs)

        kwargs['label'] = self.list_labels
        b2.finalize(nsimu=self.nsim, **kwargs)
        plotfilename = options.dirplot + "/" + label + "_elevations." + options.format
        print ('plot ' + plotfilename)
        b2.save(plotfilename, formatout=options.format)

        b2.close()


class ComparisonSimObsSIM2(ComparisonSimObs):

    def read_sim(self, listpro):

        self.dataSim = multiplecsv(listpro)
        self.dataSim.read()
        self.dataSim.close()

        listStationsSim = self.dataSim.getListStations()
        self.timeSim.append(self.dataSim.get(listStationsSim[0], "time"))

        sdSim = np.empty((len(self.timeSim[-1]), len(listStationsSim)))

        for s, station in enumerate(listStationsSim):
            sdSim[:, s] = self.dataSim.get(station, "SNOWDEPTH")  # cm

        self.sdSim.append(sdSim)

        self.SimStations.append(np.array(map(int, listStationsSim)))
        self.nsim = len(self.sdSim)


if __name__ == "__main__":
    options = parse_options(sys.argv)
    options = check_and_convert_options(options)

    # Ouverture et lecture du fichier observé
    dataObs = obscsv(options.fileobs)
    dataObs.read()
    dataObs.close()

    if options.sim2:
        print ("full comparison for SIM2")
        fullplotsSIM2(options.datebegin, options.dateend, dataObs)
    elif options.yearly:
        print ("yearly comparisons")
        yearlyplots(options.datebegin, options.dateend, dataObs)

    else:
        print ("full comparison")
        fullplots(options.datebegin, options.dateend, dataObs)

