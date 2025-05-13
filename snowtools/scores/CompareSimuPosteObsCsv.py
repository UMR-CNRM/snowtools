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
from snowtools.plots.boxplots.boxplots import boxplots_bydepartment, boxplots_byelevation, boxplots_byyear
from snowtools.scores.deterministic import DeterministicScores_Heterogeneous
from snowtools.DATA import REANALYSIS_DIR
matplotlib.use('Agg')


usage = ("CompareSimuPosteObsCsv.py [--scores] [--plot] -b YYYYMMDD -e YYYYMMDD --dirsim=dirsim1,"
         "dirsim2 --labels=label1,labe2 --dirplot=dirplot --format=pdf,png,eps --yearly")

default = dict(fileobs="/rd/cenfic3/cenmod/home/vernaym/extraction_obs_htn/OBS_ref.csv",
               dirsim=REANALYSIS_DIR+","+REANALYSIS_DIR)

IM = infomassifs()

blacklist = [5023400, 5023401, 6088400, 6088401, 38185400, 38185401, 73054403, 73054404, 73054408,
             73150402, 74056407, 74056408, 74191407, 4062001, 5001003, 5064002, 5079005, 5093002,
             5101002, 5132003, 5139005, 5157002, 38442005, 4193007, 4219004, 5136002, 73144001, 73248003,
             73304005, 74208005, 74236002, 204010191, 204010223, 204010261, 204040681, 4102004, 4173002,
             6050001, 6154002, 38191407, 38567404, 74037002, 74522001, 204010731, 204011141, 204011181,
             204012021, 4006401, 4019400, 4019402, 4205400, 5001401, 5023404, 5026401, 5027401,
             5058400, 5063403, 5063404, 5063406, 5096400, 5098400, 5114401, 5120401, 5161400,
             5177401, 6073400, 6073401, 6119001, 6120401, 6120403, 6163005, 6163401, 26290401,
             38002400, 38002404, 38002405, 38005400, 38052400, 38191402, 38191408, 38191409,
             38395404, 38442401, 38469400, 38548401, 38567405, 38567405, 73004401, 38567405,
             73004401, 73040400, 73047402, 73071404, 73123400, 73123401, 73194400, 73206401,
             73206402, 73235401, 73257401, 73280403, 73304400, 73306404, 73318401, 73322400,
             74056401, 74056415, 74058400, 74058402, 74063400, 74063403, 74085404, 74190400,
             74191401, 74279400, 99130419, 99130418, 66136403, 66136401, 65440402, 65440401,
             31555402, 31555401, 31555400, 65123001, 65212001, 65295001, 66150002, 9290001,
             9030014, 9032006, 9100004, 9139003, 9220002, 9290005, 203000454, 203000456, 203000463,
             203000464, 203000469, 203000470, 203000471, 203000472, 203000473, 203000474,
             203000475, 203000477, 203000478, 65481001, 66018001, 66025001, 66060003, 66124001,
             66130002, 66150012, 66179001, 66222001, 66222003, 9023401, 9029401, 9032401, 9070401,
             9135401, 9206401, 9231401, 31042401, 31042403, 31042404, 31042405, 31042407, 31085401,
             31508401, 64320403, 65059401, 65099402, 65138401, 65188401, 65258400, 66067400,
             66147400, 66220002, 20004400, 20004401, 20247006, 20254004, 20268001, 20278400,
             20359400]


def parse_options():
    parser = argparse.ArgumentParser(usage)

    parser.add_argument("--fileobs", type=str, dest="fileobs",
                        default=default["fileobs"], help="csv file with observations")

    parser.add_argument("-b", "--begin", type=str, dest="datebegin", default="1980080106",
                        help="First year of extraction")

    parser.add_argument("-e", "--end", type=str, dest="dateend", default="2019080106",
                        help="Last year of extraction")

    parser.add_argument("--dirsim", dest="dirsim", default=default["dirsim"],
                        help="Directory of simulation outputs or list of directories")

    parser.add_argument("--labels", dest="labels", default=None,
                        help="list of labels. Lenght of list should correspond to the number of"
                             " simulation directories given")

    parser.add_argument("--dirplot", dest="dirplot", default=os.getcwd() + "/plot",
                        help="Directory where the figures are saved")

    parser.add_argument("--format", dest="format", default="png",
                        help="Format of plots")

    parser.add_argument("--yearly", action="store_true", dest="yearly",
                        help="Yearly plots")

    parser.add_argument("--decade", action="store_true", dest="decade",
                        help="Decade plots")

    parser.add_argument("--plot", action="store_true", dest="plot",
                        help="do plots")

    parser.add_argument("--scores", action="store_true", dest="scores",
                        help="Compute scores")

    parser.add_argument("--sim2", action="store_true", dest="sim2",
                        help="Specific case of SIM2 evaluations")
    args = parser.parse_args()
    return args


def check_and_convert_options(options):
    """
    does some operations on options

    * Converts local paths to absolute paths
    * put the simulation directory in a list if this is not the case
    * creates the plot directory
    * converts dates in datetime objects
    :param options: options parsed from commandline by argparse parser
    :return: converted options
    """

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
    """
    constructs a plot title containing the station name and its altitude.
    :param station: station number
    :type station: int
    :return: plot title
    :rtype: str
    """
    lati, longi, alti = IM.infoposte(station)  # @UnusedVariable
    # nameposte provides unicode and matplotlib expects unicode
    return IM.nameposte(station) + u" %d m" % int(alti)


def build_title_with_scores(station, bias, rmse):
    """
    construct a plot title containing the station name, station altitude,
    the bias and the RMSE of the plotted simulation.
    :param station: station number
    :type station: int
    :param bias:
    :type bias: float
    :param rmse:
    :type rmse: float
    :return: plot title
    :rtype: str
    """
    return build_title(station) + " Bias = " + '%.1f' % bias + " RMSE = " + '%.1f' % rmse


def yearlyplots(datebegin, dateend, data_obs):
    """

    :param datebegin:
    :param dateend:
    :param data_obs:
    :return:
    """
    datepro = datebegin

    yearly_nvalues = []
    yearly_bias = []
    yearly_rmse = []
    list_years = []

    while True:
        c = ComparisonSimObs(data_obs)
        for d, dirsim in enumerate(options.dirsim):
            dateprobegin, dateproend = get_file_period("PRO", dirsim, datepro, dateend)

            if options.plot or options.scores:
                c.read_sim("PRO.nc")

        if options.labels:
            c.list_labels = list(map(str.strip, options.labels.split(',')))

        if options.scores:
            c.scores()
            list_years.append(dateprobegin.year + 1)
            yearly_nvalues.append(c.nvalues)
            yearly_bias.append(c.bias)
            yearly_rmse.append(c.rmse)

        if options.plot:
            c.plot()

        datepro = dateproend

        if dateproend >= dateend:
            break

    if options.scores:
        boxplots_yearly(list_years, yearly_nvalues, yearly_bias, list_colors=c.list_colors,
                        list_labels=c.list_labels, label='bias', ylabel='Bias (cm)')
        boxplots_yearly(list_years, yearly_nvalues, yearly_rmse, list_colors=c.list_colors,
                        list_labels=c.list_labels, label='rmse', ylabel='RMSD (cm)')


def decadeplots(datebegin, dateend, data_obs):
    """

    :param datebegin:
    :param dateend:
    :param data_obs:
    :return:
    """
    # Il y a un problème avec les 'xticks' en python 3 : seul le 1er élément de la liste s'affiche...
    init = time.time()

    yearly_nvalues = []
    yearly_bias = []
    yearly_rmse = []
    yearly_mean_sd = []
    list_years = [1980, 1990, 2000, 2010]
    
    for dec in list_years:
        print('Debut traitement decade {0:d} : {1:f}s'.format(dec, time.time()-init))
        c = ComparisonSimObs(data_obs)
        datepro = datetime.datetime(dec, 8, 1, 6)
        list_pro = [[] for i in range(len(options.dirsim))]
        for y in range(10):
            for d, dirsim in enumerate(options.dirsim):
                dateprobegin, dateproend = get_file_period("PRO", dirsim, datepro, dateend)
                # i = len(list_pro[d])
                proname = "PRO_" + str(dec+y) + "_" + str(dec+y+1) + "_simu" + str(d) + ".nc"
                os.rename("PRO.nc", proname)
                list_pro[d].append(proname)
            datepro = dateproend

        for d, dirsim in enumerate(options.dirsim):
            if options.plot or options.scores:
                c.read_sim(list_pro[d])
        print('Fin lecture fichiers PRO : {1:f}s'.format(dec, time.time()-init))

        if options.labels:
            c.list_labels = list(map(str.strip, options.labels.split(',')))

        if options.scores:
            c.scores()
            yearly_nvalues.append(c.nvalues)
            yearly_bias.append(c.bias)
            yearly_rmse.append(c.rmse)
            yearly_mean_sd.append(c.meansd)

        print('Fin traitement decade {0:d} : {1:f}s'.format(dec, time.time()-init))

    if options.scores:
        boxplots_yearly(list_years, yearly_nvalues, yearly_bias, list_colors=c.list_colors,
                        list_labels=c.list_labels, label='bias', ylabel='Bias (cm)')
        print('Plot boxplot_biais : {0:f}s'.format(time.time()-init))
        boxplots_yearly(list_years, yearly_nvalues, yearly_rmse, list_colors=c.list_colors,
                        list_labels=c.list_labels, label='rmsd', ylabel='RMSD (cm)')
        print('Plot boxplot_rmse : {0:f}s'.format(time.time()-init))
        boxplots_yearly(list_years, yearly_nvalues, yearly_mean_sd, list_colors=c.list_colors,
                        list_labels=c.list_labels, label='MeanSD', ylabel='Mean snow depth (cm)')
        print('Plot boxplot_mean_SD : {0:f}s'.format(time.time()-init))


def boxplots_yearly(list_years, list_nvalues, list_scores, list_colors, list_labels, label, **kwargs):
    """

    :param list_years:
    :param list_nvalues:
    :param list_scores:
    :param list_colors:
    :param list_labels:
    :param label:
    :param kwargs:
    :return:
    """

    # nyears = len(list_years)
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
    print('plot ' + plotfilename)
    b.save(plotfilename, formatout=options.format)

    b.close()


def fullplots(datebegin, dateend, data_obs):
    """
    calculate scores and makes boxplots if the scores option is True
    makes time series plots for stations if the plot option is True
    :param datebegin: start date of analysis and plots
    :type datebegin: `bronx.stdtypes.date.Date`
    :param dateend: end date of analysis and plots
    :type dateend: `bronx.stdtypes.date.Date`
    :param data_obs: observation data
    """

    datepro = datebegin

    list_list_pro = [[] for i in range(len(options.dirsim))]

    print("Get simulation files")

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

    c = ComparisonSimObs(data_obs)
    for d, dirsim in enumerate(options.dirsim):
        print("Read simulation files from " + dirsim)
        c.read_sim(list_list_pro[d])

    if options.labels:
        c.list_labels = list(map(str.strip, options.labels.split(',')))

    if options.scores:
        print("Compute scores")
        c.scores(datemin=datebegin, datemax=dateend)
        c.allboxplots()

    if options.plot:
        print("Draw plots")
        print(datebegin, dateend)
        c.plot(datemin=datebegin, datemax=dateend)


def fullplotsSIM2(datebegin, dateend, data_obs):
    """
    Do boxplots of scores and time series plots for simulations from the SIM2 system.
    :param datebegin: start date of analysis
    :type datebegin: `bronx.stdtypes.date.Date`
    :param dateend: end date of analysis
    :type dateend: `bronx.stdtypes.date.Date`
    :param data_obs: observation data
    """

    list_list_pro = [[] for i in range(len(options.dirsim))]

    print("Get simulation files")

    prefix = dict(sim2_interp="SIM_NEW_",
                  sim2_nointerp="SIM_NEW_NO_INTERP_",
                  simref="")

    for year in range(datebegin.year, dateend.year):

        stringyear = str(year) + str(year + 1)

        for d, dirsim in enumerate(options.dirsim):

            proname = dirsim + "/" + prefix[os.path.basename(dirsim)] + "hauteur_neige_" + stringyear

            list_list_pro[d].append(proname)

    c = ComparisonSimObsSIM2(data_obs)

    for d, dirsim in enumerate(options.dirsim):
        print("Read simulation files from " + dirsim)
        c.read_sim(list_list_pro[d])

    if options.labels:
        c.list_labels = list(map(str.strip, options.labels.split(',')))

    if options.scores:
        print("Compute scores")
        c.scores()
        c.allboxplots()

    if options.plot:
        print("Draw plots")
        c.plot()


class ComparisonSimObs(object):

    def __init__(self, data_obs):
        """

        :param data_obs:
        """

        self.dataObs = data_obs

        self.timeSim = []
        self.sdSim = []
        self.SimStations = []
        self.Simelevations = []
        self.nsim = 0

        stations_obs = data_obs.getListStations()

        list_sites_metadata = IM.getListSites()

        list_stations = list(set(stations_obs) & set(list_sites_metadata))
        print('Nombre de stations AVANT blacklist : ', len(list_stations))
        self.listStations = [station for station in list_stations if int(station) not in blacklist]
        print('Nombre de stations APRES blacklist : ', len(self.listStations))

        self.elevations = list(map(IM.altiposte, self.listStations))

        self.nstations = len(self.listStations)

        # Default labels
        self.list_labels = ['New', 'Old', '', '', '']
        self.list_labels_massifs = ['Alps', 'Pyrenees', 'Corsica', '', '']
        # self.list_labels = ['Reference reanalysis', 'Reanalysis with no temperature observation',
        # 'Reanalysis without evaluation observations', '', '']
        # self.list_labels = ['Reference reanalysis without observations',
        # 'New guess reanalysis without observations', 'Reference reanalysis', 'New guess reanalysis', '']

        # Default colors
        # self.list_colors = ['red', 'blue', 'grey', 'orange', 'green']
        self.list_colors = ['maroon', 'darkgreen', 'grey', 'blue']
        self.nvalues = None
        self.bias = None
        self.rmse = None
        self.meansd = None

    @property
    def list_colors(self):
        """list of colors to use for plots"""
        return self._list_colors

    @list_colors.setter
    def list_colors(self, colors):
        """
        :param colors: list of colors
        :type colors: list of strings
        """
        self._list_colors = colors

    @property
    def list_labels(self):
        """list of labels to use on the plots"""
        return self._list_labels

    @list_labels.setter
    def list_labels(self, labels):
        self._list_labels = labels

    @property
    def list_labels_massifs(self):
        """list of labels for massifs"""
        return self._list_labels_massifs

    @list_labels_massifs.setter
    def list_labels_massifs(self, labels):
        self._list_labels_massifs = labels

    @property
    def nvalues(self):
        """number of available values"""
        return self._nvalues

    @nvalues.setter
    def nvalues(self, value):
        self._nvalues = value

    @property
    def bias(self):
        """
        bias of the simulations for each station.
        """
        return self._bias

    @bias.setter
    def bias(self, value):
        self._bias = value

    @property
    def rmse(self):
        """rmse of the simulations for each station"""
        return self._rmse

    @rmse.setter
    def rmse(self, value):
        self._rmse = value

    @property
    def meansd(self):
        """mean snow depth of the simulations for each station """
        return self._meansd

    @meansd.setter
    def meansd(self, value):
        self._meansd = value

    def plot(self, datemin=None, datemax=None):
        """
        plot time series
        :param datemin: start date of the plot
        :param datemax: end date of the plot

        """

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

        if dateproend - dateprobegin > datetime.timedelta(days=1000):
            myplot.set_figsize(15, 4)

        nivose = 0
        for s, station in enumerate(self.listStations):
            available_sim = []
            for indSim in range(0, self.nsim):
                available, time_obs, time_sim, sd_obs, sd_sim = self.get_obs_sim(station, indSim,
                                                                                 datemin=datemin,
                                                                                 datemax=datemax)
                available_sim.append(available)

                if available:
                    periodplot = (time_sim >= dateplotbegin) & (time_sim <= dateplotend)
                    timeplot = time_sim[periodplot]
                    sd_simplot = sd_sim[periodplot]
                    if not any(available_sim[:-1]):
                        myplot.draw(time_obs, sd_obs, timeplot, sd_simplot, color=self.list_colors[indSim],
                                    label=self.list_labels[indSim])
                        time_out = time_sim[:]
                    else:
                        myplot.add_line(timeplot, sd_simplot, color=self.list_colors[indSim],
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

                myplot.finalize(time_out, ylabel="Snow depth (cm)")
                print('plot ' + plotfilename)
                myplot.save(plotfilename, formatout=options.format)

        print("NUMBER OF NIVOSE")
        print(nivose)

    def scores(self, datemin=None, datemax=None):
        """
        compute bias, RMSE and mean snow depth for all available simulations and stations
        :param datemin: start date of analysis period
        :type datemin: `bronx.stdtypes.date.Date`
        :param datemax: end date of analysis period
        :type datemax: `bronx.stdtypes.date.Date`
        """
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
                available, time_obs, time_sim, sd_obs, sd_sim = self.get_obs_sim(station, indSim, datemin=datemin,
                                                                                 datemax=datemax)
                t2 = time.time()
                print('Appel a get_obs_sim : {0:f}s'.format(t2-t1))

                if available:
                    t1 = time.time()
                    scores = DeterministicScores_Heterogeneous(time_obs, time_sim, sd_obs, sd_sim)
                    t2 = time.time()
                    print('Calcul scores sim {0:d} : {1:f}s'.format(indSim, t2-t1))
                    # self.nvalues[indSim, s] = scores.nvalues()
                    # self.bias[indSim, s] = scores.bias()
                    # self.rmse[indSim, s] = scores.rmse()
                    # self.meansd[indSim, s] = scores.meansim
                    # self.nvalues[indSim, s], self.bias[indSim, s], self.rmse[indSim, s],
                    # self.meansd[indSim, s] = scores.scores_with_positive_values_only()
                    (self.nvalues[indSim, s], self.bias[indSim, s], self.rmse[indSim, s],
                     self.meansd[indSim, s]) = scores.scores_all_values()

    def allboxplots(self):
        """
        make boxplots for bias, RMSE and mean snow depth.
        """

        array_stations = np.array(list(map(int, self.listStations)))

        self.boxplots_scores(array_stations, np.array(self.elevations), self.bias, 'bias', ylabel='Bias (cm)')
        self.boxplots_scores(array_stations, np.array(self.elevations), self.rmse, 'rmse', ylabel='RMSD (cm)')
        self.boxplots_scores(array_stations, np.array(self.elevations), self.meansd, 'mean_SD',
                             ylabel='Mean simulated Snow Depth (cm)')

    def get_obs_sim(self, station, ind_sim, datemin=None, datemax=None):
        """
        get observed and simulated time series
        :param station: station number
        :type station: int
        :param ind_sim: simulation index
        :type ind_sim: int
        :param datemin: start date of analysis
        :param datemax: end date of analysis
        :return: available obs-fc pairs, observation times, simulation times, observed snow depth,
         simulated snowdepth
        """

        time_obs = self.dataObs.get(station, "time")
        sd_obs = self.dataObs.get(station, "SNOWDEPTH")  # cm

        period_obs = (time_obs > min(self.timeSim[ind_sim])) & (time_obs < max(self.timeSim[ind_sim]))

        winter = np.empty_like(period_obs)
        for i, t in enumerate(time_obs):
            winter[i] = (t.month >= 10) or (t.month <= 6)

        period_obs = period_obs & winter

        if datemin:
            period_obs = period_obs & (time_obs >= datemin)

        if datemax:
            period_obs = period_obs & (time_obs <= datemax)

        avail_obs = np.sum(sd_obs[period_obs] > 0) > 5

        # print type(self.SimStations[indSim])
        ind = self.SimStations[ind_sim] == int(station)
        avail_sim = np.sum(ind) == 1

        avail_common = avail_obs and avail_sim

        if avail_common:
            return (avail_common, time_obs[period_obs], self.timeSim[ind_sim], sd_obs[period_obs],
                    np.squeeze(self.sdSim[ind_sim][:, ind]))
        else:
            return avail_common, None, None, None, None

    def read_sim(self, pro):
        """
        read a simulation file.
        :param pro: file name for simulation output file in netcdf format
        """

        pro = prosimu(pro)

        self.SimStations.append(pro.read("station"))
        self.Simelevations.append(pro.read("ZS"))

        self.timeSim.append(pro.readtime())

        try:
            self.sdSim.append(pro.read("DSN_T_ISBA", fill2zero=True) * 100.)  # cm
        except Exception:
            self.sdSim.append(pro.read("SNOWDEPTH", fill2zero=True) * 100.)  # cm

        pro.close()

        self.nsim = len(self.sdSim)

    def boxplots_scores(self, stations, elevations, list_scores, label, **kwargs):
        """
        do boxplots of a given score by department and by elevation.
        :param stations: array of station numbers
        :param elevations: array of station altitudes
        :param list_scores: array of score values for each simulation and station dimensions (simulation index, station)
        :param label: name of the score to put in the file name.
        :type label: str
        :param kwargs: keyword arguments to be passed to the draw and finalize methods of boxplots_bydepartment
         and boxplots_byelevation in snowtools.plots.boxplots.boxplots. Anything valid for pyplot.boxplot.
        """

        b1 = boxplots_bydepartment()

        valid = self.nvalues[0, :] >= 0

        print('number of obs')
        print(np.sum(valid))

        for indSim in range(0, self.nsim):
            # print(self.nvalues[indSim, :], "nvalues indSim")
            valid = (self.nvalues[indSim, :] >= 10) & valid
            print('number of obs with ' + str(indSim + 1) + "available simulation(s)")
            print(np.sum(valid))

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

            # print list_scores[indSim, valid].shape

        b1.finalize(nsimu=self.nsim, **kwargs)
        plotfilename = options.dirplot + "/" + label + "_departments." + options.format
        print('plot ' + plotfilename)
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
            valid = self.nvalues[indSim, :] >= 10
            b2.draw(elevations[valid], list_scores[indSim, valid], nsimu=self.nsim, **kwargs)

        b2.finalize(nsimu=self.nsim, **kwargs)
        plotfilename = options.dirplot + "/" + label + "_elevations." + options.format
        print('plot ' + plotfilename)
        b2.save(plotfilename, formatout=options.format)

        b2.close()


class ComparisonSimObsSIM2(ComparisonSimObs):

    def __init__(self, data_obs):
        """
        Class to compare observations with data from the SIM2 system.
        :param data_obs:
        """
        self.dataSim = []
        super().__init__(data_obs)

    def read_sim(self, listpro):
        """
        read simulations
        :param listpro: list of file names
        """

        self.dataSim = multiplecsv(listpro)
        self.dataSim.read()
        self.dataSim.close()

        list_stations_sim = self.dataSim.getListStations()
        self.timeSim.append(self.dataSim.get(list_stations_sim[0], "time"))

        sd_sim = np.empty((len(self.timeSim[-1]), len(list_stations_sim)))

        for s, station in enumerate(list_stations_sim):
            sd_sim[:, s] = self.dataSim.get(station, "SNOWDEPTH")  # cm

        self.sdSim.append(sd_sim)

        self.SimStations.append(np.array(map(int, list_stations_sim)))
        self.nsim = len(self.sdSim)


if __name__ == "__main__":
    options = parse_options()
    options = check_and_convert_options(options)

    # Ouverture et lecture du fichier observé
    dataObs = obscsv(options.fileobs)
    dataObs.read()
    dataObs.close()

    if options.sim2:
        print("full comparison for SIM2")
        fullplotsSIM2(options.datebegin, options.dateend, dataObs)
    elif options.yearly:
        print("yearly comparisons")
        yearlyplots(options.datebegin, options.dateend, dataObs)
    elif options.decade:
        print("decade comparisons")
        decadeplots(options.datebegin, options.dateend, dataObs)
    else:
        print("full comparison")
        fullplots(options.datebegin, options.dateend, dataObs)
