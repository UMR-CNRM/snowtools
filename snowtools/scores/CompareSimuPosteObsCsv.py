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

default = dict(fileobs="/rd/cenfic3/cenmod/home/vernaym/extraction_obs_htn/OBS_ref.csv",
               dirsim='/rd/cenfic3/cenmod/era40/vortex/s2m/postes/reanalysis/pro')

IM = infomassifs()

blacklist = [5023400,5023401,6088400,6088401,38185400,38185401,73054403,73054404,73054408,73150402,74056407,74056408,74191407, +
            4062001,5001003,5064002,5079005,5093002,5101002,5132003,5139005,5157002,38442005,4193007,4219004,5136002,73144001,73248003,+
            73304005,74208005,74236002,204010191,204010223,204010261,204040681,4102004,4173002,6050001,6154002,38191407,38567404, +
            74037002,74522001,204010731,204011141,204011181,204012021,4006401,4019400,4019402,4205400,5001401,5023404,5026401, +
            5027401,5058400,5063403,5063404,5063406,5096400,5098400,5114401,5120401,5161400,5177401,6073400,6073401,6119001,6120401, +
            6120403,6163005,6163401,26290401,38002400,38002404,38002405,38005400,38052400,38191402,38191408,38191409,38395404,38442401, +
            38469400,38548401,38567405,38567405,73004401,38567405,73004401,73040400,73047402,73071404,73123400,73123401,73194400,73206401, +
            73206402,73235401,73257401,73280403,73304400,73306404,73318401,73322400,74056401,74056415,74058400,74058402,74063400,74063403, +
            74085404,74190400,74191401,74279400,99130419,99130418,66136403,66136401,65440402,65440401,31555402,31555401,31555400,65123001, +
            65212001,65295001,66150002,9290001,9030014,9032006,9100004,9139003,9220002,9290005,203000454,203000456,203000463,203000464, +
            203000469,203000470,203000471,203000472,203000473,203000474,203000475,203000477,203000478,65481001,66018001,66025001,66060003, +
            66124001,66130002,66150012,66179001,66222001,66222003,9023401,9029401,9032401,9070401,9135401,9206401,9231401,31042401,31042403, +
            31042404,31042405,31042407,31085401,31508401,64320403,65059401,65099402,65138401,65188401,65258400,66067400,66147400,66220002, +
            20004400,20004401,20247006,20254004,20268001,20278400,20359400]

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
            C.set_sim_labels(list(map(str.strip, options.labels.split(','))))

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
            C.set_sim_labels(list(map(str.strip, options.labels.split(','))))

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
        C.set_sim_labels(list(map(str.strip, options.labels.split(','))))

    if options.scores:
        print ("Compute scores")
        C.scores(datemin=datebegin, datemax=dateend)
        C.allboxplots()

    if options.plot:
        print ("Draw plots")
        print(datebegin, dateend)
        C.plot(datemin=datebegin, datemax=dateend)


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
        C.set_sim_labels(list(map(str.strip, options.labels.split(','))))

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

        StationsObs = dataObs.getListStations()

        listSitesMetadata = IM.getListSites()

        listStations = list(set(StationsObs) & set(listSitesMetadata))
        print('Nombre de stations AVANT blacklist : ', len(listStations))
        self.listStations = [station for station in listStations if int(station) not in blacklist]
        print('Nombre de stations APRES blacklist : ', len(self.listStations))

        self.elevations = list(map(IM.altiposte, self.listStations))

        self.nstations = len(self.listStations)

        # Default labels
#        self.set_sim_labels(['New', 'Old', '', '', ''])
        self.set_massifs_labels(['Alps', 'Pyrenees', 'Corsica', '', ''])
        #self.set_sim_labels(['Reference reanalysis', 'Reanalysis with no temperature observation', 'Reanalysis without evaluation observations', '', ''])
        # self.set_sim_labels(['Reference reanalysis without observations', 'New guess reanalysis without observations', 'Reference reanalysis', 'New guess reanalysis', ''])

        # Default colors
        #self.set_sim_colors(['red', 'blue', 'grey', 'orange', 'green'])
        self.set_sim_colors(['maroon', 'darkgreen', 'grey', 'blue'])

    def set_sim_colors(self, list_colors):
        self.list_colors = list_colors

    def set_sim_labels(self, list_labels):
        self.list_labels = list_labels

    def set_massifs_labels(self, list_labels):
        self.list_labels_massifs = list_labels

    def plot(self, datemin=None, datemax=None):

        dateprobegin = self.timeSim[0][0]
        dateproend = self.timeSim[0][-1]

        if datemin:
            dateplotbegin = max(dateprobegin, datemin)
        else:
            dateplotbegin = dateprobegin
        if datemax:
            dateplotend = min(dateproend, datemax)
        else:
            dateplotend = dateproend

        myplot = temporalplotObsMultipleSims()

        if dateproend - dateprobegin > datetime.timedelta(days = 1000):
            myplot.set_figsize(15, 4)

        nivose = 0
        for s, station in enumerate(self.listStations):
            available_sim = []
            for indSim in range(0, self.nsim):
                available, timeObs, timeSim, sdObs, sdSim  = self.get_obs_sim(station, indSim, datemin=datemin,
                                                                              datemax=datemax)
                available_sim.append(available)

                if available:
                    periodplot = (timeSim >= dateplotbegin) & (timeSim <= dateplotend)
                    timeplot = timeSim[periodplot]
                    sdSimplot = sdSim[periodplot]
                    if not any(available_sim[:-1]):
                        myplot.draw(timeObs, sdObs, timeplot, sdSimplot, color=self.list_colors[indSim],
                                    label=self.list_labels[indSim])
                        timeOut = timeSim[:]
                    else:
                        myplot.add_line(timeplot, sdSimplot, color=self.list_colors[indSim],
                                        label=self.list_labels[indSim])

            if any(available_sim):

                if 'NIVOSE' in build_title(station):
                    nivose += 1

                if self.nsim == 1 and hasattr(self, 'bias') and hasattr(self, 'rmse'):
                    myplot.set_title(build_title_with_scores(station, self.bias[0, s], self.rmse[0, s]))
                else:
                    myplot.set_title(build_title(station))

                plotfilename = (options.dirplot + "/" + station + "_" + dateprobegin.strftime("%Y") + "_" +
                                dateproend.strftime("%Y") + "." + options.format)

                myplot.finalize(timeOut, ylabel="Snow depth (cm)")
                print ('plot ' + plotfilename)
                myplot.save(plotfilename, formatout=options.format)

        print ("NUMBER OF NIVOSE")
        print (nivose)

    def scores(self, datemin=None, datemax=None):
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
                available, timeObs, timeSim, sdObs, sdSim = self.get_obs_sim(station, indSim, datemin=datemin,
                                                                             datemax=datemax)
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
        self.boxplots_scores(arrayStations, np.array(self.elevations), self.meansd, 'mean_SD', ylabel='Mean simulated Snow Depth (cm)')

    def get_obs_sim(self, station, indSim, datemin=None, datemax=None):

        timeObs = self.dataObs.get(station, "time")
        sdObs = self.dataObs.get(station, "SNOWDEPTH")  # cm

        periodObs = (timeObs > min(self.timeSim[indSim])) & (timeObs < max(self.timeSim[indSim]))

        winter = np.empty_like(periodObs)
        for i, t in enumerate(timeObs):
            winter[i] = (t.month >= 10) or (t.month <= 6)

        periodObs = periodObs & winter

        if datemin:
            periodObs = periodObs & (timeObs >= datemin)

        if datemax:
            periodObs = periodObs & (timeObs <= datemax)

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
            else:
                kwargs['fillcolor'] = self.list_colors[indSim] 
                kwargs['label'] = self.list_labels

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

