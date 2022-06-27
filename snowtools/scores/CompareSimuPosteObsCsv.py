#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 5 dec. 2018

@author: lafaysse
'''

from optparse import OptionParser
import sys
import os
import datetime
import time

import numpy as np
import matplotlib
matplotlib.use('Agg')

from snowtools.utils.prosimu import prosimu
from snowtools.utils.obscsv import obscsv, multiplecsv
from snowtools.utils.resources import absolute_path, get_file_period
from snowtools.utils.dates import checkdateafter, check_and_convert_date
from snowtools.utils.infomassifs import infomassifs
from snowtools.plots.temporal.chrono import temporalplotObsMultipleSims
from snowtools.plots.boxplots.boxplots import boxplots_bydepartment, boxplots_byelevation, boxplots_byyear
from snowtools.scores.deterministic import DeterministicScores_Heterogeneous


usage = "CompareSimuPosteObsCsv.py [--scores] [--plot] -b YYYYMMDD -e YYYYMMDD --dirsim=dirsim1,dirsim2 --labels=label1,labe2 --dirplot=dirplot --format=pdf,png,eps --yearly"

default = dict(fileobs="/rd/cenfic3/mma/vernaym/extraction_obs_htn/OBS_ref.csv",
               dirsim='/rd/cenfic3/era40/vortex/s2m/postes/reanalysis/pro')

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

    parser.add_option("--decade",
                      action="store_true", dest="decade", default=False,
                      help="Decade plots")

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
    # nameposte provides unicode and matplotlib expects unicode
    return IM.nameposte(station) + u" %d m" % int(alti)


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
        boxplots_yearly(list_years, yearly_nvalues, yearly_rmse, list_colors = C.list_colors, list_labels = C.list_labels, label='rmse', ylabel='RMSD (cm)')

def decadeplots(datebegin, dateend, dataObs):
    # Il y a un problème avec les 'xticks' en python 3 : seul le 1er élément de la liste s'affiche...
    init = time.time()
    datepro = datebegin

    yearly_nvalues = []
    yearly_bias = []
    yearly_rmse = []
    yearly_meanSD = []
    list_years = [1980, 1990, 2000, 2010]
    
    for dec in list_years:
        print('Debut traitement decade {0:d} : {1:f}s'.format(dec, time.time()-init))
        C = ComparisonSimObs(dataObs)
        datepro = datetime.datetime(dec, 8, 1, 6)
        list_pro = []
        for d, dirsim in enumerate(options.dirsim):
            list_pro.append([])
        for y in range(10):
            for d, dirsim in enumerate(options.dirsim):
                dateprobegin, dateproend = get_file_period("PRO", dirsim, datepro, dateend)
                i = len(list_pro[d])
                proname = "PRO_" + str(dec+y) + "_" + str(dec+y+1) + "_simu" + str(d) + ".nc"
                os.rename("PRO.nc", proname)
                list_pro[d].append(proname)
            datepro = dateproend

        for d, dirsim in enumerate(options.dirsim):
            if options.plot or options.scores:
                C.read_sim(list_pro[d])
        print('Fin lecture fichiers PRO : {1:f}s'.format(dec, time.time()-init))

        if options.labels:
            C.set_sim_labels(map(str.strip, options.labels.split(',')))

        if options.scores:
            C.scores()
            yearly_nvalues.append(C.nvalues)
            yearly_bias.append(C.bias)
            yearly_rmse.append(C.rmse)
            yearly_meanSD.append(C.meansd)

        print('Fin traitement decade {0:d} : {1:f}s'.format(dec, time.time()-init))

    if options.scores:
        boxplots_yearly(list_years, yearly_nvalues, yearly_bias, list_colors = C.list_colors, list_labels = C.list_labels, label='bias', ylabel='Bias (cm)')
        print('Plot boxplot_biais : {0:f}s'.format(time.time()-init))
        boxplots_yearly(list_years, yearly_nvalues, yearly_rmse, list_colors = C.list_colors, list_labels = C.list_labels, label='rmsd', ylabel='RMSD (cm)')
        print('Plot boxplot_rmse : {0:f}s'.format(time.time()-init))
        boxplots_yearly(list_years, yearly_nvalues, yearly_meanSD, list_colors = C.list_colors, list_labels = C.list_labels, label='MeanSD', ylabel='Mean snow depth (cm)')
        print('Plot boxplot_mean_SD : {0:f}s'.format(time.time()-init))

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

        self.elevations = list(map(IM.altiposte, self.listStations))

        self.nstations = len(self.listStations)

        # Default labels
#        self.set_sim_labels(['New', 'Old', '', '', ''])
        self.set_massifs_labels(['Alps', 'Pyrenees', 'Corsica', '', ''])
        self.set_sim_labels(['Reference reanalysis with no assimilation', 'New guess with no temperature observation', 'Reference reanalysis with assimilation', 'New guess with assimilation', ''])

        # Default colors
        self.set_sim_colors(['red', 'blue', 'grey', 'orange', 'green'])

    def set_sim_colors(self, list_colors):
        self.list_colors = list_colors

    def set_sim_labels(self, list_labels):
        self.list_labels = list_labels

    def set_massifs_labels(self, list_labels):
        self.list_labels_massifs = list_labels

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
        init = time.time()
        self.nvalues = np.zeros((self.nsim, self.nstations))
        self.bias = np.zeros((self.nsim, self.nstations))
        self.rmse = np.zeros((self.nsim, self.nstations))
        self.meansd = np.zeros((self.nsim, self.nstations))

        print('Debut boucle stations : {0:f}s'.format(time.time()-init))
        for s, station in enumerate(self.listStations):
            print('Station n°{0:d} : {1:f}s'.format(s, time.time()-init))

            for indSim in range(0, self.nsim):
                t1 = time.time()
                available, timeObs, timeSim, sdObs, sdSim = self.get_obs_sim(station, indSim)
                t2 = time.time()
                print('Appel a get_obs_sim : {0:f}s'.format(t2-t1))

                if available:
                    t1 = time.time()
                    scores = DeterministicScores_Heterogeneous(timeObs, timeSim, sdObs, sdSim)
                    t2 = time.time()
                    print('Calcul scores sim {0:d} : {1:f}s'.format(indSim, t2-t1))
                    #self.nvalues[indSim, s] = scores.nvalues()
                    #self.bias[indSim, s] = scores.bias()
                    #self.rmse[indSim, s] = scores.rmse()
                    #self.meansd[indSim, s] = scores.meansim
                    #self.nvalues[indSim, s], self.bias[indSim, s], self.rmse[indSim, s], self.meansd[indSim, s] = scores.scores_with_positive_values_only()
                    self.nvalues[indSim, s], self.bias[indSim, s], self.rmse[indSim, s], self.meansd[indSim, s] = scores.scores_all_values()

    def allboxplots(self):

        arrayStations = np.array(list(map(int, self.listStations)))

        self.boxplots_scores(arrayStations, np.array(self.elevations), self.bias, 'bias', ylabel='Bias (cm)')
        self.boxplots_scores(arrayStations, np.array(self.elevations), self.rmse, 'rmse', ylabel='RMSD (cm)')
        self.boxplots_scores(arrayStations, np.array(self.elevations), self.meansd, 'mean_SD', ylabel='Mean Snow Depth (cm)')

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

        print ('number of obs')
        print (np.sum(valid))

        for indSim in range(0, self.nsim):
            valid = (self.nvalues[indSim, :] > 10) & (valid)
            print ('number of obs with ' + str(indSim + 1) + "available simulation(s)")
            print (np.sum(valid))

        for indSim in range(0, self.nsim):
            if self.nsim == 1:
                # Color Alps departments in red, Pyr ones in blue and Corsica ones in grey to avoid
                # blue and green colors and the same figure
                # Add legend (by adding a 'label' key to the args)
                kwargs['fillcolor'] = ['red']*5 + ['blue']*3 + ['grey']
                kwargs['label'] = self.list_labels_massifs

            b1.draw(stations[valid], list_scores[indSim, valid], nsimu=self.nsim, **kwargs)

            #print list_scores[indSim, valid].shape

        b1.finalize(nsimu=self.nsim, **kwargs)
        plotfilename = options.dirplot + "/" + label + "_departments." + options.format
        print ('plot ' + plotfilename)
        b1.save(plotfilename, formatout=options.format)

        b1.close()

        b2 = boxplots_byelevation()

        for indSim in range(0, self.nsim):
            # Dans le cas des boxplot par tranche d'altitude on n'ajoute pas de légende si
            # on trace une seule simulation
            if self.nsim > 1:
                kwargs['label'] = self.list_labels
                kwargs['fillcolor'] = self.list_colors[indSim]
            else:
                # Color all boxplots in blue if there is only 1 simulation
                kwargs['fillcolor'] = self.list_colors[1]
                kwargs.pop('label', None)
            valid = self.nvalues[indSim, :] > 10
            b2.draw(elevations[valid], list_scores[indSim, valid], nsimu=self.nsim, **kwargs)

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
    elif options.decade:
        print ("decade comparisons")
        decadeplots(options.datebegin, options.dateend, dataObs)
    else:
        print ("full comparison")
        fullplots(options.datebegin, options.dateend, dataObs)

