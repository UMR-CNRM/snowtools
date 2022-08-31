#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 5 dec. 2018

@author: lafaysse
"""

import argparse
import os
import datetime
import time

import numpy as np
import matplotlib

from snowtools.utils.prosimu import prosimu
from snowtools.utils.obscsv import obscsv, multiplecsv
from snowtools.utils.resources import absolute_path, get_file_period
from snowtools.utils.dates import checkdateafter, check_and_convert_date
from snowtools.utils.infomassifs import infomassifs
from snowtools.plots.temporal.chrono import temporalplotObsMultipleSims
from snowtools.plots.boxplots.boxplots import boxplots_bydepartment, boxplots_byelevation,\
    boxplots_byyear
from snowtools.scores.deterministic import DeterministicScores_Heterogeneous

matplotlib.use('Agg')

USAGE = "usage: CompareSimuPosteObsCsv.py [--scores] [--plot] -b YYYYMMDD -e YYYYMMDD " \
        "--dirsim=dirsim1,dirsim2 " \
        "--labels=label1,labe2 --dirplot=dirplot --format=pdf,png,eps --yearly"

DEFAULT = dict(fileobs="/rd/cenfic3/mma/vernaym/extraction_obs_htn/OBS_ref.csv",
               dirsim='/rd/cenfic3/era40/vortex/s2m/postes/reanalysis/pro')

IM = infomassifs()

BLACKLIST = [5023400, 5023401, 6088400, 6088401, 38185400, 38185401, 73054403, 73054404, 73054408,
             73150402, 74056407, 74056408, 74191407, 4062001, 5001003, 5064002, 5079005, 5093002,
             5101002, 5132003, 5139005, 5157002, 38442005, 4193007, 4219004, 5136002, 73144001,
             73248003, 73304005, 74208005, 74236002, 204010191, 204010223, 204010261, 204040681,
             4102004, 4173002, 6050001, 6154002, 38191407, 38567404, 74037002, 74522001, 204010731,
             204011141, 204011181, 204012021, 4006401, 4019400, 4019402, 4205400, 5001401, 5023404,
             5026401, 5027401, 5058400, 5063403, 5063404, 5063406, 5096400, 5098400, 5114401,
             5120401, 5161400, 5177401, 6073400, 6073401, 6119001, 6120401, 6120403, 6163005,
             6163401, 26290401, 38002400, 38002404, 38002405, 38005400, 38052400, 38191402,
             38191408, 38191409, 38395404, 38442401, 38469400, 38548401, 38567405, 38567405,
             73004401, 38567405, 73004401, 73040400, 73047402, 73071404, 73123400, 73123401,
             73194400, 73206401, 73206402, 73235401, 73257401, 73280403, 73304400, 73306404,
             73318401, 73322400, 74056401, 74056415, 74058400, 74058402, 74063400, 74063403,
             74085404, 74190400, 74191401, 74279400, 99130419, 99130418, 66136403, 66136401,
             65440402, 65440401, 31555402, 31555401, 31555400, 65123001, 65212001, 65295001,
             66150002, 9290001, 9030014, 9032006, 9100004, 9139003, 9220002, 9290005, 203000454,
             203000456, 203000463, 203000464, 203000469, 203000470, 203000471, 203000472,
             203000473, 203000474, 203000475, 203000477, 203000478, 65481001, 66018001, 66025001,
             66060003, 66124001, 66130002, 66150012, 66179001, 66222001, 66222003, 9023401,
             9029401, 9032401, 9070401, 9135401, 9206401, 9231401, 31042401, 31042403, 31042404,
             31042405, 31042407, 31085401, 31508401, 64320403, 65059401, 65099402, 65138401,
             65188401, 65258400, 66067400, 66147400, 66220002, 20004400, 20004401, 20247006,
             20254004, 20268001, 20278400, 20359400]

PARSER = argparse.ArgumentParser(description=USAGE)

PARSER.add_argument("--fileobs", action="store", type=str, dest="fileobs",
                    default=DEFAULT["fileobs"], help="geometry")

PARSER.add_argument("-b", "--begin", action="store", type=str, dest="datebegin",
                    default="1980080106", help="First year of extraction")

PARSER.add_argument("-e", "--end", action="store", type=str, dest="dateend",
                    default="2019080106", help="Last year of extraction")

PARSER.add_argument("--dirsim", action="store", dest="dirsim", default=DEFAULT["dirsim"],
                    help="Directory of simulation outputs or list of directories")

PARSER.add_argument("--labels", action="store", dest="labels", default=None,
                    help="Directory of simulation outputs or list of directories")

PARSER.add_argument("--dirplot", action="store", dest="dirplot", default=os.getcwd() + "/plot",
                    help="Directory where the figures are saved")

PARSER.add_argument("--format", action="store", dest="format", default="png",
                    help="Format of plots")

PARSER.add_argument("--yearly", action="store_true", dest="yearly", default=False,
                    help="Yearly plots")

PARSER.add_argument("--decade", action="store_true", dest="decade", default=False,
                    help="Decade plots")

PARSER.add_argument("--plot", action="store_true", dest="plot", default=False,
                    help="Plot")

PARSER.add_argument("--scores", action="store_true", dest="scores", default=False,
                    help="Compute scores")

PARSER.add_argument("--sim2", action="store_true", dest="sim2", default=False,
                    help="Specific case of SIM2 evaluations")

OPTIONS = PARSER.parse_args()


def check_and_convert_options(options, vortex=False):
    """
    convert paths and dates
    """

    # Conversions of local paths in absolute paths
    [options.fileobs, options.dirsim, options.dirplot] = \
        list(map(absolute_path, [options.fileobs, options.dirsim, options.dirplot]))

    if not isinstance(options.dirsim, list):
        options.dirsim = [options.dirsim]

    # Create plot directory
    if not os.path.isdir(options.dirplot):
        os.makedirs(options.dirplot)

    # Conversions of dates in datetime objects
    [options.datebegin, options.dateend] = list(map(check_and_convert_date,
                                                    [options.datebegin, options.dateend]))
    checkdateafter(options.dateend, options.datebegin)

    return options


def build_title(station):
    """
    create title string containing station name and station altitude.

    :param station: station number
    :type station: str
    :return: title
    :rtype: str
    """
    alti = IM.altiposte(station)
    return "{0} {1:d} m".format(IM.nameposte(station), int(alti))


def build_title_with_scores(station, bias, rmse):
    """
    create a title string containing station name, station altitude, Bias and RMSE.

    :param station: station number
    :type station: str
    :param bias: bias score for the given station
    :type bias: float
    :param rmse: RMSE score for the given station
    :type rmse: float
    :return: title string
    :rtype: str
    """
    return build_title(station) + " Bias = " + '%.1f' % bias + " RMSE = " + '%.1f' % rmse


def yearlyplots(datebegin, dateend, dataObs):
    """
    create time series plots for each year and yearly boxplots for scores
    (if the --scores option was given)

    :param datebegin: start date
    :type datebegin: `bronx.stdtypes.date.Date` object
    :param dateend: end date
    :type dateend: `bronx.stdtypes.date.Date` object
    :param dataObs: observation data
    :type dataObs: `snowtools.utils.obscsv.obscsv` object
    """
    datepro = datebegin

    yearly_nvalues = []
    yearly_bias = []
    yearly_rmse = []
    list_years = []

    while True:
        c_so = ComparisonSimObs(dataObs)
        for dirsim in OPTIONS.dirsim:
            dateprobegin, dateproend = get_file_period("PRO", dirsim, datepro, dateend)

            if OPTIONS.plot or OPTIONS.scores:
                c_so.read_sim("PRO.nc")

        if OPTIONS.labels:
            c_so.set_sim_labels(map(str.strip, OPTIONS.labels.split(',')))

        if OPTIONS.scores:
            c_so.scores()
            list_years.append(dateprobegin.year + 1)
            yearly_nvalues.append(c_so.nvalues)
            yearly_bias.append(c_so.bias)
            yearly_rmse.append(c_so.rmse)

        if OPTIONS.plot:
            c_so.plot()

        datepro = dateproend

        if dateproend >= dateend:
            break

    if OPTIONS.scores:
        boxplots_yearly(list_years, yearly_nvalues, yearly_bias, list_colors=c_so.list_colors,
                        list_labels=c_so.list_labels, label='bias', ylabel='Bias (cm)')
        boxplots_yearly(list_years, yearly_nvalues, yearly_rmse, list_colors=c_so.list_colors,
                        list_labels=c_so.list_labels, label='rmse', ylabel='RMSD (cm)')


def decadeplots(datebegin, dateend, dataObs):
    """
    score plots by decade.

    :param datebegin: start date
    :type datebegin: `bronx.stdtypes.date.Date` object
    :param dateend: end date
    :type dateend: `bronx.stdtypes.date.Date` object
    :param dataObs: observation data
    :type dataObs: `snowtools.utils.obscsv.obscsv` object
    """

    init = time.time()

    yearly_nvalues = []
    yearly_bias = []
    yearly_rmse = []
    yearly_meanSD = []
    list_years = [1980, 1990, 2000, 2010]

    for dec in list_years:
        print('Debut traitement decade {0:d} : {1:f}s'.format(dec, time.time()-init))
        c_so = ComparisonSimObs(dataObs)
        datepro = datetime.datetime(dec, 8, 1, 6)
        list_pro = [[] for x in range(len(OPTIONS.dirsim))]
        for y_i in range(10):
            for d_i, dirsim in enumerate(OPTIONS.dirsim):
                dateprobegin, dateproend = get_file_period("PRO", dirsim, datepro, dateend)
                proname = "PRO_" + str(dec+y_i) + "_" + str(dec+y_i+1) + "_simu" + str(d_i) + ".nc"
                os.rename("PRO.nc", proname)
                list_pro[d_i].append(proname)
            datepro = dateproend

        for d_i, dirsim in enumerate(OPTIONS.dirsim):
            if OPTIONS.plot or OPTIONS.scores:
                c_so.read_sim(list_pro[d_i])
        print('Fin lecture fichiers PRO for decade {0}: {1:f}s'.format(dec, time.time()-init))

        if OPTIONS.labels:
            c_so.set_sim_labels(map(str.strip, OPTIONS.labels.split(',')))

        if OPTIONS.scores:
            c_so.scores()
            yearly_nvalues.append(c_so.nvalues)
            yearly_bias.append(c_so.bias)
            yearly_rmse.append(c_so.rmse)
            yearly_meanSD.append(c_so.meansd)

        print('Fin traitement decade {0:d} : {1:f}s'.format(dec, time.time()-init))

    if OPTIONS.scores:
        boxplots_yearly(list_years, yearly_nvalues, yearly_bias, list_colors=c_so.list_colors,
                        list_labels=c_so.list_labels, label='bias', ylabel='Bias (cm)')
        print('Plot boxplot_biais : {0:f}s'.format(time.time()-init))
        boxplots_yearly(list_years, yearly_nvalues, yearly_rmse, list_colors=c_so.list_colors,
                        list_labels=c_so.list_labels, label='rmsd', ylabel='RMSD (cm)')
        print('Plot boxplot_rmse : {0:f}s'.format(time.time()-init))
        boxplots_yearly(list_years, yearly_nvalues, yearly_meanSD, list_colors=c_so.list_colors,
                        list_labels=c_so.list_labels, label='MeanSD', ylabel='Mean snow depth (cm)')
        print('Plot boxplot_mean_SD : {0:f}s'.format(time.time()-init))


def boxplots_yearly(list_years, list_nvalues, list_scores, list_colors, list_labels, label,
                    **kwargs):
    """
    do boxplots by year.

    :param list_years: list of years
    :param list_nvalues: ?
    :param list_scores: score values to plot
    :param list_colors: fill color for each simulation
    :param list_labels: list of labels used for the legend
    :param label: some string used in the plot filename.
    :param kwargs: keyword arguments passed to the boxplots draw method and finalize method
    """

    (nsim, nstations) = list_nvalues[0].shape

    list_valid = []

    for y_i in range(len(list_years)):
        valid = list_nvalues[y_i][0, :] == list_nvalues[y_i][0, :]
        for sim in range(0, nsim):
            valid = list_nvalues[y_i][sim, :] > 5 & valid
        list_valid.append(valid)

    b_p = boxplots_byyear()
    for sim in range(0, nsim):
        list_scores_valid = []
        for y_i in range(len(list_years)):
            list_scores_valid.append(list_scores[y_i][sim, list_valid[y_i]])

        kwargs['fillcolor'] = list_colors[sim]

        b_p.draw(list_years, list_scores_valid, nsimu=nsim, **kwargs)

    kwargs['label'] = list_labels
    b_p.finalize(nsimu=nsim, **kwargs)

    plotfilename = OPTIONS.dirplot + "/" + label + "_years." + OPTIONS.format
    print('plot ' + plotfilename)
    b_p.save(plotfilename, formatout=OPTIONS.format)

    b_p.close()


def fullplots(datebegin, dateend, dataObs):

    datepro = datebegin

    list_list_pro = [[] for x in range(len(OPTIONS.dirsim))]

    print("Get simulation files")

    while True:

        for d, dirsim in enumerate(OPTIONS.dirsim):
            dateprobegin, dateproend = get_file_period("PRO", dirsim, datepro, dateend)
            i = len(list_list_pro[d])
            proname = "PRO_part" + str(i) + "_simu" + str(d) + ".nc"
            os.rename("PRO.nc", proname)
            list_list_pro[d].append(proname)

        datepro = dateproend
        if dateproend >= OPTIONS.dateend:
            break

    C = ComparisonSimObs(dataObs)
    for d, dirsim in enumerate(OPTIONS.dirsim):
        print("Read simulation files from " + dirsim)
        C.read_sim(list_list_pro[d])

    if OPTIONS.labels:
        C.set_sim_labels(map(str.strip, OPTIONS.labels.split(',')))

    if OPTIONS.scores:
        print("Compute scores")
        C.scores()
        C.allboxplots()

    if OPTIONS.plot:
        print("Draw plots")
        C.plot()


def fullplotsSIM2(datebegin, dateend, dataObs):

    list_list_pro = []

    for d, dirsim in enumerate(OPTIONS.dirsim):
        list_list_pro.append([])

    print("Get simulation files")

    prefix = dict(sim2_interp="SIM_NEW_",
                  sim2_nointerp="SIM_NEW_NO_INTERP_",
                  simref="")

    for year in range(datebegin.year, dateend.year):

        stringyear = str(year) + str(year + 1)

        for d, dirsim in enumerate(OPTIONS.dirsim):

            proname = dirsim + "/" + prefix[os.path.basename(dirsim)] + "hauteur_neige_" + stringyear

            list_list_pro[d].append(proname)

    C = ComparisonSimObsSIM2(dataObs)

    for d, dirsim in enumerate(OPTIONS.dirsim):
        print ("Read simulation files from " + dirsim)
        C.read_sim(list_list_pro[d])

    if OPTIONS.labels:
        C.set_sim_labels(map(str.strip, OPTIONS.labels.split(',')))

    if OPTIONS.scores:
        print ("Compute scores")
        C.scores()
        C.allboxplots()

    if OPTIONS.plot:
        print ("Draw plots")
        C.plot()


class ComparisonSimObs:
    """Class for comparing Simulations to observations by calculating
    deterministic scores (bias, rmse)
    and plotting timeseries plots and boxplots of scores"""

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
        self.listStations = [station for station in listStations if int(station) not in BLACKLIST]
        print('Nombre de stations APRES blacklist : ', len(self.listStations))

        self.elevations = list(map(IM.altiposte, self.listStations))

        self.nstations = len(self.listStations)

        # Default labels
#        self.set_sim_labels(['New', 'Old', '', '', ''])
        self.set_massifs_labels(['Alps', 'Pyrenees', 'Corsica', '', ''])
        self.set_sim_labels(['Reference reanalysis with no assimilation', 'New guess with no temperature observation',
                             'Reference reanalysis with assimilation', 'New guess with assimilation', ''])

        # Default colors
        self.list_colors = ['red', 'blue', 'grey', 'orange', 'green']

    @property
    def list_colors(self):
        """list of colors"""
        return self._list_colors

    @list_colors.setter
    def list_colors(self, value):
        self._list_colors = value

    @property
    def nvalues(self):
        """number of available values for each simulation and each station"""
        return self._nvalues

    @nvalues.setter
    def nvalues(self, value):
        self._nvalues = value

    @property
    def bias(self):
        """bias for each simulation and each station"""
        return self._bias

    @bias.setter
    def bias(self, value):
        self._bias = value

    @property
    def rmse(self):
        """rmse for each simulation and each station"""
        return self._rmse

    @rmse.setter
    def rmse(self, value):
        self._rmse = value

    @property
    def meansd(self):
        """mean snow depth for each simulation and each station"""
        return self._meansd

    @meansd.setter
    def meansd(self, value):
        self._meansd = value

    def set_sim_labels(self, list_labels):
        self.list_labels = list_labels

    def set_massifs_labels(self, list_labels):
        self.list_labels_massifs = list_labels

    def plot(self):

        dateprobegin = self.timeSim[0][0]
        dateproend = self.timeSim[0][-1]

        myplot = temporalplotObsMultipleSims()

        if dateproend - dateprobegin > datetime.timedelta(days=1000):
            myplot.set_figsize(15, 4)

        nivose = 0
        for s, station in enumerate(self.listStations):
            available_sim = []
            for indSim in range(0, self.nsim):
                available, timeObs, timeSim, sdObs, sdSim = self.get_obs_sim(station, indSim)
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

                plotfilename = OPTIONS.dirplot + "/" + station + "_" + dateprobegin.strftime("%Y")\
                               + "_" + dateproend.strftime("%Y") + "." + OPTIONS.format

                myplot.finalize(timeOut, ylabel="Snow depth (cm)")
                print('plot ' + plotfilename)
                myplot.save(plotfilename, formatout=OPTIONS.format)
                myplot.close()

        print("NUMBER OF NIVOSE")
        print(nivose)

    def scores(self):
        """
        calculate bias, rmse and mean snowdepth including zeros.
        """
        init = time.time()
        self.nvalues = np.zeros((self.nsim, self.nstations))
        self.bias = np.zeros((self.nsim, self.nstations))
        self.rmse = np.zeros((self.nsim, self.nstations))
        self.meansd = np.zeros((self.nsim, self.nstations))

        print('Debut boucle stations : {0:f}s'.format(time.time()-init))
        for s_i, station in enumerate(self.listStations):
            print('Station n°{0:d} : {1:f}s'.format(s_i, time.time()-init))

            for ind_sim in range(0, self.nsim):
                t_1 = time.time()
                available, time_obs, time_sim, sd_obs, sd_sim = self.get_obs_sim(station, ind_sim)
                t_2 = time.time()
                print('Appel a get_obs_sim : {0:f}s'.format(t_2-t_1))

                if available:
                    t_1 = time.time()
                    scores = DeterministicScores_Heterogeneous(time_obs, time_sim, sd_obs, sd_sim)
                    t_2 = time.time()
                    print('Calcul scores sim {0:d} : {1:f}s'.format(ind_sim, t_2-t_1))
                    #self.nvalues[indSim, s] = scores.nvalues()
                    #self.bias[indSim, s] = scores.bias()
                    #self.rmse[indSim, s] = scores.rmse()
                    #self.meansd[indSim, s] = scores.meansim
                    #self.nvalues[indSim, s], self.bias[indSim, s], self.rmse[indSim, s],
                    # self.meansd[indSim, s] = scores.scores_with_positive_values_only()
                    self.nvalues[ind_sim, s_i], self.bias[ind_sim, s_i], self.rmse[ind_sim, s_i], \
                        self.meansd[ind_sim, s_i] = scores.scores_all_values()

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

        print('number of obs')
        print(np.sum(valid))

        for indSim in range(0, self.nsim):
            valid = (self.nvalues[indSim, :] > 10) & (valid)
            print('number of obs with ' + str(indSim + 1) + "available simulation(s)")
            print(np.sum(valid))

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
        plotfilename = OPTIONS.dirplot + "/" + label + "_departments." + OPTIONS.format
        print('plot ' + plotfilename)
        b1.save(plotfilename, formatout=OPTIONS.format)

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
        plotfilename = OPTIONS.dirplot + "/" + label + "_elevations." + OPTIONS.format
        print ('plot ' + plotfilename)
        b2.save(plotfilename, formatout=OPTIONS.format)

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
    OPTIONS = check_and_convert_options(OPTIONS)

    # Ouverture et lecture du fichier observé
    dataObs = obscsv(OPTIONS.fileobs)
    dataObs.read()
    dataObs.close()

    if OPTIONS.sim2:
        print("full comparison for SIM2")
        fullplotsSIM2(OPTIONS.datebegin, OPTIONS.dateend, dataObs)
    elif OPTIONS.yearly:
        print("yearly comparisons")
        yearlyplots(OPTIONS.datebegin, OPTIONS.dateend, dataObs)
    elif OPTIONS.decade:
        print("decade comparisons")
        decadeplots(OPTIONS.datebegin, OPTIONS.dateend, dataObs)
    else:
        print("full comparison")
        fullplots(OPTIONS.datebegin, OPTIONS.dateend, dataObs)

