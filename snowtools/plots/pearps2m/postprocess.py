#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 6 déc. 2018

@author: lafaysse

usage: python postprocess.py [-b YYYYMMDD] [-e YYYYMMDD] [-o diroutput]

    #) extracts operational simulation results
    #) Plots maps for the Alps, the Pyrenees the Corse, Vosges, Massif Central, Jura
    #) Creates spaghetti plots for all massifs and stations
"""

import argparse
import os
import datetime

import numpy as np
import pandas as pd
from scipy.stats import gamma

#from snowtools.plots.pearps2m.postprocess_plot import Config
#from snowtools.plots.pearps2m.postprocess_plot import main
#from snowtools.plots.pearps2m.postprocess_plot import pp_plots
from snowtools.utils.prosimu import prosimu
from snowtools.utils.infomassifs import infomassifs
from snowtools.DATA import LUSTRE_NOSAVE_USER_DIR, SNOWTOOLS_DATA

from bronx.stdtypes.date import today


def build_filename(massif, alti):
    """
    Construct a filename from massif and altitude information.

    :param massif: a massif number
    :param alti: an altitude level
    :return: filename
    :rtype: str
    """
    filename = str(massif)
    if alti:
        filename += "_{:d}".format(int(alti))
    return filename


class Ensemble:
    """
    Describes an ensemble of simulations

    """

    def __init__(self):
        """
        """
        self.ensemble = {}  #: data dict with variable names as keys and np.arrays as values
        self.simufiles = []  #: list of prosimu objects with simulation file
        self.nech = None  #: number of time steps (forecast steps). type int
        self.nmembers = None  #: number of members type int
        self.time = None  #: :ivar ~.time: time variable from the first simulation file :vartype ~.time: numpy array
        self.indpoints = []  #: :ivar indpoints: list with spatial indices :vartype indpoints: list
        self.npoints = None  #: :ivar npoints: total number of spatial points :vartype npoints: int

    @property
    def spatialdim(self):
        """Name of spatial dimension"""
        return "Number_of_points"

    def open(self, listmembers):
        """
        Opens simulation files

        :param listmembers: list of ensemble members (filenames)
        :type listmembers: list
        :ivar inddeterministic: index of the deterministic member
        :vartype inddeterministic: int
        """
        print(listmembers)
        for m, member in enumerate(listmembers):
            p = prosimu(member)
            if m == 0:
                self.nech = p.getlendim("time")
            ntime = p.getlendim("time")
            if ntime == self.nech:
                self.simufiles.append(p)
                if 'mb035' in member:
                    self.inddeterministic = len(self.simufiles) - 1

        self.nmembers = len(self.simufiles)
        print(self.nmembers)
        self.time = self.simufiles[0].readtime()
        self.indpoints = self.select_points()
        self.npoints = self.get_npoints()

    def select_points(self):
        """
        Get a list of spatial indices from the spatial dimension
        length in the first simulation file.

        :return: list of spatial points (indices)
        :rtype: list
        """
        indpoints = range(0, self.simufiles[0].getlendim(self.spatialdim))
        return indpoints

    def get_npoints(self):
        """
        Get the number of spatial points.

        :return: total number of spatial points
        :rtype: int
        """
        if isinstance(self.indpoints, tuple):
            npoints = 0
            for indpoints in self.indpoints:
                npoints += len(indpoints)
        else:
            npoints = len(self.indpoints)

        return npoints

    def read(self, varname):
        """
        Read a variable and store it in :py:attr:`~.ensemble`

        :param varname: name of the variable to be read
            (corresponds to the variable name of the simulation NetCDF files)
        :type varname: str
        """

        self.ensemble[varname] = np.empty([self.nech, self.npoints, self.nmembers])

        kwargs = dict()

        # import datetime
        for m, member in enumerate(self.simufiles):
            # print("read " + varname + " for member" + str(m))
            # before = datetime.datetime.today()

            if isinstance(self.indpoints, tuple):
                sections = []
                for indpoints in self.indpoints:
                    kwargs[self.spatialdim] = indpoints
                    sections.append(member.read_var(varname, **kwargs))

                self.ensemble[varname][:, :, m] = np.concatenate(tuple(sections), axis=1)
            else:
                kwargs[self.spatialdim] = self.indpoints
                self.ensemble[varname][:, :, m] = member.read_var(varname, **kwargs)

            # after = datetime.datetime.today()
            # print(after - before)

        # Verrues (à éviter)
        if varname == 'NAT_LEV':
            self.ensemble[varname][:, :, :] = np.where(self.ensemble[varname] == 6.,
                                                       0., self.ensemble[varname])

    def read_geovar(self, varname):
        """
        Read a variable from the first simulation file.

        :param varname: NetCDF variable name of the variable to read
        :type varname: str
        :return: data read
        :rtype: numpy array
        """

        kwargs = dict()
        if isinstance(self.indpoints, tuple):
            sections = []
            for indpoints in self.indpoints:
                kwargs[self.spatialdim] = indpoints
                sections.append(self.simufiles[0].read_var(varname, **kwargs))

            return np.concatenate(tuple(sections))
        else:
            kwargs[self.spatialdim] = self.indpoints
            return self.simufiles[0].read_var(varname, **kwargs)

    def probability(self, varname, seuilinf=-999999999., seuilsup=999999999.):
        """
        Calculates probability as the proportion of ensemble members with
        values larger than :py:attr:`seuilinf`
        and smaller than :py:attr:`seuilsup`.

        :param varname: name of the variable for which to calculate the probability
        :type varname: str
        :param seuilinf: lower threshold
        :param seuilsup: upper threshold
        :return: probability field (or np.nan if the ensemble is not defined)
        :rtype: numpy array
        """

        if varname not in self.ensemble.keys():
            self.read(varname)

        condition = (self.ensemble[varname] > seuilinf) & (self.ensemble[varname] < seuilsup)
        probability = np.sum(condition, axis=2) / (1. * self.nmembers)

        return np.where(np.isnan(self.ensemble[varname][:, :, 0]), np.nan, probability)
        # On renvoit des nan quand ensemble n'est pas défini

    def emos_csg_nonorm_allstations_newsnow(self, varname, level):
        """
        Applies Censored Shifted Gamma (CSG) EMOS trained for 24h new snow variable and all stations in the Alps without
        normalisation (internship Benoit Gacon 2024)
        to variable "varname" and returns the quantiles given in "levels" of the CSG distribution.

        :param varname: variable name
        :type varname: str
        :param level: quantiles (range 0 to 100)
        :type level: array
        :return: quantiles of predictive distribution
        :rtype: np.array
        """

        if varname not in self.ensemble.keys():
            self.read(varname)

        csgmean, csgstd, csgdelta = self.get_csg_predictive_dist_nonorm_allstations(varname)

        level = level / 100.
        quantile = self.quantiles_CSGD(csgmean, csgstd, csgdelta, level)

        return quantile

    def get_csg_predictive_dist_nonorm_allstations(self, varname):
        """
        get the censored shifted gamma regression coefficients and climatological
        paramters for each lead time and apply them to the ensemble forecast
        and return the parameters mu, sigma and delta of the predictive censored shifted gamma distribution.

        :param varname: variable name
        :return: mu, sigma, delta of the predictive CSG distribution
        """

        # filename = os.path.join(SNOWTOOLS_DATA, "emos_2000_2022_212_par_nonorm.Rdata")
        # robj = robjects.r.load(filename)  # pylint: disable=possibly-unused-variable
        # reg_coef = np.array(robjects.r[robj[0]])
        # clim_par = np.array(robjects.r[robj[1]])
        reg_coef = pd.read_csv(os.path.join(SNOWTOOLS_DATA, "matEmosPars.csv"), index_col=0).values
        clim_par = pd.read_csv(os.path.join(SNOWTOOLS_DATA, "matEmosParsClim.csv"), index_col=0).values

        ndays_leadtime = 4

        ntime = len(self.time)
        list_indtime = []

        delta = self.time - self.time[0].replace(hour=6)

        for day_leadtime in range(1, ndays_leadtime + 1):
            list_indtime.append(np.where(
                (delta <= datetime.timedelta(days=day_leadtime)) &
                (delta > datetime.timedelta(days=day_leadtime - 1)))[0])

        # Extract regression parameters
        a1, a2, a3, a4, b1 = np.empty((5, ntime, self.npoints))
        a1.fill(np.nan)
        a2.fill(np.nan)
        a3.fill(np.nan)
        a4.fill(np.nan)
        b1.fill(np.nan)
        muclim, sigmaclim, deltaclim = np.empty((3, ntime, self.npoints))
        muclim.fill(np.nan)
        sigmaclim.fill(np.nan)
        deltaclim.fill(np.nan)

        for leadtime in range(0, ndays_leadtime):
            # cst_a1, cst_a2, cst_a3, cst_a4, cst_b1 = reg_coef[leadtime, 0, 1, 0:5]
            # cst_muclim, cst_sigmaclim, cst_deltaclim = clim_par[leadtime, 0, 1, :]
            cst_a1, cst_a2, cst_a3, cst_a4, cst_b1 = reg_coef[leadtime, 0:5]
            cst_muclim, cst_sigmaclim, cst_deltaclim = clim_par[leadtime, :]

            begin = list_indtime[leadtime][0]
            end = list_indtime[leadtime][-1]

            a1[slice(begin, end + 1), :] = cst_a1
            a2[slice(begin, end + 1), :] = cst_a2
            a3[slice(begin, end + 1), :] = cst_a3
            a4[slice(begin, end + 1), :] = cst_a4
            b1[slice(begin, end + 1), :] = cst_b1

            muclim[slice(begin, end + 1), :] = cst_muclim
            sigmaclim[slice(begin, end + 1), :] = cst_sigmaclim
            deltaclim[slice(begin, end + 1), :] = cst_deltaclim

        # Extract raw ensemble predictors
        ensmean = self.mean(varname)
        ensPOP = self.probability(varname, seuilinf=1.E-6)

        # Compute CSGD parameters with regression laws
        csgmean = self.csg_mean_nonorm(a1, a2, a3, a4, ensmean, ensPOP)
        csgstd = self.csg_std_nonorm(b1, muclim, sigmaclim, csgmean)
        csgdelta = deltaclim

        return csgmean, csgstd, csgdelta

    def csg_mean_nonorm(self, a1, a2, a3, a4, ensmean, ensPOP):
        """
        Calculate mu of predictive censored shifted gamma distribution from regression coefficients alpha 1 to
        alpha 4, ensemble mean and probability of non-zero values of the ensemble.

        :param a1: regression coefficient alpha 1
        :type a1: float
        :param a2: regression coefficient alpha 2
        :type a2: float
        :param a3: regression coefficient alpha 3
        :type a3: float
        :param a4: regression coefficient alpha 4
        :type a4: float
        :param ensmean: ensemble mean
        :type ensmean: float
        :param ensPOP: probability of non-zero values of the ensemble [0., 1.]
        :type ensPOP: float
        :return: mu of predictive censored shifted gamma distribution
        :rtype: float
        """
        return np.log1p(np.expm1(a1) * (a2 + a3 * ensPOP + a4 * ensmean)) / a1

    def csg_std_nonorm(self, b1, muclim, sigmaclim, csgmean):
        """
        Calculate sigma of predictive censored shifted gamma distribution from regression coefficients
        beta 1, climatological mu and sigma and csg mu parameter.

        :param b1: regression coefficient beta 1
        :type b1: float
        :param muclim: climatological mu
        :type muclim: float
        :param sigmaclim: climatological sigma
        :type sigmaclim: float
        :param csgmean: csg mu parameter
        :type csgmean: float
        :return: sigma of predictive censored shifted gamma distribution
        :rtype: float
        """
        return b1 * sigmaclim * np.sqrt(csgmean / muclim)

    def quantiles_CSGD(self, csgmean, csgstd, csgdelta, quantiles):
        """
        get quantiles of the censored shifted gamma (CSG) distribution with given mu, sigma and delta.

        :param csgmean: mu parameter of CSG
        :type csgmean: array
        :param csgstd: sigma parameter of CSG
        :type csgstd: array
        :param csgdelta: delta parameter of CSG
        :type csgdelta: array
        :param quantiles: quantiles wanted
        :type quantiles: array of floats
        :return: array of quantiles
        :rtype: array of same length as quantiles.
        """

        vectorppf = np.vectorize(gamma.ppf, signature='(),(),()->(n)', excluded='q')

        tmp = vectorppf(q=quantiles, a=(csgmean / csgstd) ** 2, scale=(csgstd ** 2) / csgmean, loc=csgdelta)
        # print(tmp)

        # return np.where(tmp >= 0, tmp, 0)
        return np.where(tmp < 0., 0., tmp)

    def quantile(self, varname, level):
        """
        Calculates ensemble percentiles for a given variable and given percentile levels.

        :param varname: Variable name
        :type varname: str
        :param level: list of percentiles to calculate
        :type level: list of numbers between 0 and 100
        :return: array of percentiles
        :rtype: numpy array
        """

        if varname not in self.ensemble.keys():
            self.read(varname)

        quantile = np.where(np.isnan(self.ensemble[varname][:, :, 0]), np.nan,
                            np.percentile(self.ensemble[varname], level, axis=2))
        return quantile

    def mean(self, varname):
        """
        Calculate ensemble mean for a given variable.

        :param varname: variable name
        :type varname: str
        :return: ensemble mean
        :rtype: numpy array
        """

        if varname not in self.ensemble.keys():
            self.read(varname)

        return np.nanmean(self.ensemble[varname], axis=2)

    def spread(self, varname):
        """
        Calculate ensemble spread (standard deviation) for a given variable.

        :param varname: variable name
        :type varname: str
        :return: ensemble spread
        :rtype: numpy array
        :rtype: numpy array
        """

        if varname not in self.ensemble.keys():
            self.read(varname)

        return np.nanstd(self.ensemble[varname], axis=2)

    def close(self):
        """
        Close simulation files and remove data from :py:attr:`ensemble`.

        """
        for member in self.simufiles:
            member.close()
        self.ensemble.clear()

    def get_metadata(self):
        """
        Get a tuple with spatial indices.

        :return: indpoints, indpoints
        """
        indpoints = self.select_points()
        return indpoints, indpoints

    def get_alti(self):
        """
        Get altitude variable from the first simulation file.

        :return: altitude variable
        :rtype: numpy array
        """
        if not hasattr(self, "alti"):
            self.alti = self.read_geovar("ZS")
        return self.alti

    def get_aspect(self):
        """
        Get aspect variable from the first simulation file.

        :return: aspect variable
        :rtype: numpy array
        """
        if not hasattr(self, "aspect"):
            self.aspect = self.read_geovar("aspect")
        return self.aspect


class _EnsembleMassif(Ensemble):
    """
    Metaclass for ensemble simulations on a massif geometry (SAFRAN like).

    :ivar InfoMassifs: Information of Massifs
    """

    InfoMassifs = infomassifs()

    @property
    def geo(self):
        """
        Geometry

        :return: "massifs"
        """
        return "massifs"

    def read(self, varname):
        """
        Read data for a given variable name into the :py:attr:`ensemble` instance variable.

        :param varname: variable name
        :type varname: str

        """
        if varname == 'naturalIndex':
            nmassifs = len(self.get_massifvar())
            self.ensemble[varname] = np.empty([self.nech, nmassifs, self.nmembers])
            for m, member in enumerate(self.simufiles):
                self.ensemble[varname][:, :, m] = member.read_var(varname)
        else:
            super(_EnsembleMassif, self).read(varname)

    def get_massifdim(self):
        """
        Read massif_num variable from the first simulation file.

        :return: massif numbers
        :rtype: numpy array
        """
        if not hasattr(self, "massifdim"):
            self.massifdim = self.read_geovar("massif_num")
        return self.massifdim

    def get_massifvar(self):
        """
        Read "massif" variable from the first simulation file.

        :return: massif numbers
        :rtype: numpy array
        """
        if not hasattr(self, "massifvar"):
            self.massifvar = self.simufiles[0].read_var("massif")
        return self.massifvar

    def get_metadata(self, nolevel=False):
        """
        Construct filenames and plot titles from massif and altitude
        variables in the first simulation file.

        :param nolevel: if True the altitude is not included in the filenames and titles.
        :return: a list of filenames and a list of titles
        :rtype: two lists.
        """

        if nolevel:
            massif = self.get_massifvar()
            alti = [None] * len(massif)
        else:
            alti = self.get_alti()
            massif = self.get_massifdim()

        return [self.build_filename(mas, alt) for mas, alt in zip(massif, alti)], \
               [self.build_title(mas, alt) for mas, alt in zip(massif, alti)]

    def build_filename(self, massif, alti):
        """
        Construct a filename from massif and altitude information.

        :param massif: a massif number
        :param alti: an altitude level
        :return: filename
        :rtype: str
        """
        filename = str(massif)
        if alti:
            filename += "_" + str(int(alti))
        return filename

    def build_title(self, massif, alti):
        """
        Construct a figure title from massif and altitude information.

        :param massif: a massif number
        :param alti: an altitude level
        :return: a title
        :rtype: unicode str
        """
        title = self.InfoMassifs.getMassifName(massif)  # type unicode
        if alti:
            title += " {:d}m".format(int(alti))
        return title  # matplotlib needs unicode


class EnsembleFlatMassif(_EnsembleMassif):
    """
    Class for ensemble simulations on a massif geometry (SAFRAN like)
    where all data points are considered
    on flat terrain (zero slope and no orientation information).
    """

    def select_points(self):
        """
        Select spatial indices from the first simulation file
        where aspect=-1 (zero slope, no orientation information).

        :return: spatial indices to be read from simulation files
        :rtype: numpy boolean array
        """
        return self.simufiles[0].get_points(aspect=-1)


class EnsembleNorthSouthMassif(_EnsembleMassif):
    """
    Class for ensemble simulations on a massif geometry (SAFRAN like)
    where data points are considered at a
    slope of 40 degrees and two orientations: North and South.
    """

    def select_points(self):
        """
        Select spatial indices from the first simulation file
        where slope=40 and aspect either 0 or 180.

        :return: spatial indices to be read from simulation files
        :rtype: a tuple of numpy boolean array, where the first
                component corresponds to the Northern orientation
                and the second to the Southern one.
        """
        # return np.sort(np.concatenate((self.simufiles[0].get_points(aspect = 0, slope = 40),
        # self.simufiles[0].get_points(aspect = 180, slope = 40))))
        # TAKE CARE : It is extremely more efficient to read regular sections of the netcdf files
        return (self.simufiles[0].get_points(aspect=0, slope=40),
                self.simufiles[0].get_points(aspect=180, slope=40))


class EnsembleMassifPoint(_EnsembleMassif):
    """
    Class for extracting one specific point in massif geometry
    """

    def __init__(self, massif_num, alti, aspect, slope):
        self.massif_num = massif_num
        self.alti = alti
        self.aspect = aspect
        self.slope = slope

        super(EnsembleMassifPoint, self).__init__()

    def select_points(self):
        """Select index of the corresponding point"""

        return self.simufiles[0].get_points(massif_num=self.massif_num,
                                            aspect=self.aspect, slope=self.slope,
                                            ZS=self.alti)


class EnsembleStation(Ensemble):
    """
    Class for ensemble simulations at station locations.

    :ivar InfoMassifs: Information on the Massif the station is situated.
    """

    InfoMassifs = infomassifs()

    @property
    def geo(self):
        """
        Geometry information.

        :return: "stations"
        """
        return "stations"

    def get_station(self):
        """
        Read station numbers from the first simulation file.

        :return: station numbers
        :rtype: numpy array
        """
        return self.simufiles[0].read_var("station", Number_of_points=self.indpoints)

    def get_metadata(self, **kwargs):
        """
        Construct filenames and plot titles from altitude and station information

        :param kwargs:
        :return: a list of filenames and a list of plot titles
        """
        alti = self.simufiles[0].read_var("ZS", Number_of_points=self.indpoints)
        station = self.get_station()
        # print('alti type: ', type(alti), 'station type: ', type(station))
        return [self.build_filename(stat, alt) for stat, alt in zip(station, alti)], \
               [self.build_title(stat, alt) for stat, alt in zip(station, alti)]

    def build_filename(self, station, alti):
        """
        Construct a filename from a station number

        :param station: station number
        :param alti: altitude level (not used)
        :return: filename
        :rtype: station number as 8digit integer.
        """
        return '%08d' % station

    def build_title(self, station, alti):
        """
        Construct a title from a station number and altitude level.

        :param station: station number
        :param alti: station altitude
        :return: title string composed of the station name and the altitude.
        :rtype: unicode str
        """
        # nameposte gives unicode
        # matplotlib expects unicode
        return self.InfoMassifs.nameposte(station) + " %d m" % int(alti)


class EnsembleDiags(Ensemble):
    """
    Probabilities and quantiles from ensembles.
    """

    def __init__(self):
        """
        """
        self.proba = {}  #: probabilities
        self.quantiles = {}  #: percentiles
        super(EnsembleDiags, self).__init__()

    def diags(self, list_var, list_quantiles, list_seuils):
        """
        Calculate quantiles and exceedance probabilities
        for a list of variables, quantiles and thresholds.
        The quantiles are stored in :py:attr:`~.quantiles`
        and the probabilities in :py:attr:`~.proba`

        :param list_var: list of variables
        :type list_var: iterable
        :param list_quantiles: list of percentiles to be calculated
        :type list_quantiles: iterable
        :param list_seuils: list of thresholds for each variable
        :type list_seuils: dict of iterables with variable names as keys
        """
        for var in list_var:
            if var in list_seuils.keys():
                for seuil in list_seuils[var]:
                    self.proba[(var, seuil)] = self.probability(var, seuilinf=seuil)

        for var in list_var:
            self.quantiles[var] = []
            for quantile in list_quantiles:
                print("Compute quantile " + str(quantile) + " for variable " + var)
                self.quantiles[var].append(self.quantile(var, quantile))

    def close(self):
        """
        Close simulation files and remove data from
        :py:attr:`~.plots.pearps2m.postprocess.Ensemble.ensemble`,
        :py:attr:`.proba` and :py:attr:`.quantiles`
        """
        super(EnsembleDiags, self).close()
        self.proba.clear()
        self.quantiles.clear()


class EnsemblePostproc(_EnsembleMassif):
    """
    Class for ensemble post-processing.
    """
    def __init__(self, variables, inputfiles, decile=np.arange(10, 100, 10), outdir='.',
                 outfilename='PRO_post.nc', emosmethod=None):
        """
        :param variables: list of variables to process
        :param inputfiles: list of input files
        :param decile: list of percentiles
        :param outdir: Output directory
        :param outfilename: output file name
        :param emosmethod: emos method to use. The method is supposed to take a variable name and the wanted quantiles
               as arguments.
        :type outdir: str
        """
        print(inputfiles)
        super(EnsemblePostproc, self).__init__()
        # self.ensemble = self  #: ensemble data
        self.variables = variables  #: list of variables
        # self.ensemble.open(inputfiles)
        self.open(inputfiles)

        #: output filename
        self.outfile = os.path.join(outdir, outfilename)
        # self.outfile = os.path.join(outdir, 'PRO_post_{0}_{1}.nc'.format(datebegin.ymdh, dateend.ymdh))
        #: list of percentiles
        self.decile = decile
        if emosmethod:
            if emosmethod == 'emos':
                self.emosmethod = Ensemble.emos_csg_nonorm_allstations_newsnow
            elif emosmethod == 'quantiles':
                self.emosmethod = Ensemble.quantile
            else:
                raise Exception('EMOS method ' + emosmethod + ' not available')
            self.flipaxis = False
        else:
            self.emosmethod = Ensemble.quantile
            self.flipaxis = True
            # TODO: to test postprocessing with CSG EMOS inside the vortex Four_Seasons_Task (s2m_postproc algo)
            #  without changes in vortex, comment the two lines above and uncomment the two lines below.
            # self.emosmethod = Ensemble.emos_csg_nonorm_allstations_newsnow
            # self.flipaxis = False

    @property
    def standardvars(self):
        """variables always written to the output file"""
        return ['time', 'ZS', 'aspect', 'slope', 'massif_num', 'longitude', 'latitude']

    def create_outfile(self):
        """
        Create output data set.

        :ivar outdataset: output data set
        :vartype outdataset: :py:class:`utils.prosimu.prosimu`
        """
        # if not os.path.isdir(self.outfile):
        # print(self.outfile)
        #     raise DirNameException(self.outfile)
        self.outdataset = prosimu(self.outfile, ncformat='NETCDF4_CLASSIC', openmode='w')

    def init_outfile(self):
        """
        Copy global attributes, dimensions and standard variables from the
        first simulation file to the output file.
        """
        # copy global attributes all at once via dictionary
        self.outdataset.dataset.setncatts(self.simufiles[0].dataset.__dict__)
        # copy dimensions
        for name, dimension in self.simufiles[0].dataset.dimensions.items():
            self.outdataset.dataset.createDimension(
                name, (len(dimension) if not dimension.isunlimited() else None))
        # print(self.outdataset.listdim())

        # copy standard variables
        for name, variable in self.simufiles[0].dataset.variables.items():
            if name in self.standardvars:
                fillval = self.simufiles[0].getfillvalue(name)
                self.outdataset.dataset.createVariable(name, variable.datatype, variable.dimensions,
                    fill_value=fillval)
                # copy variable attributes without _FillValue since this causes an error
                for att in self.simufiles[0].listattr(name):
                    if att != '_FillValue':
                        # print(self.simufiles[0].getattr(name, att))
                        self.outdataset.dataset[name].setncatts({att: self.simufiles[0].getattr(name, att)})
                self.outdataset.dataset[name][:] = self.simufiles[0].dataset[name][:]
                # print('data copied')
        # print(self.outdataset.listvar())

    def postprocess(self):
        """
        Do postprocessing

        #) create output file
        #) copy global attributes and standard variables to the output file
        #) calculate deciles for the data variables and put them to the output file
        #) close all input and output data sets.

        Calls :py:meth:`.create_outfile`, :py:meth:`.init_outfile`,
        :py:meth:`.deciles` and close methods for all data sets.
        """
        self.create_outfile()
        self.init_outfile()
        print('init done')
        # self.median()
        # print('median done')
        self.deciles()
        self.outdataset.close()
        self.close()

    def deciles(self):
        """
        Calculates percentiles given in :py:attr:`.decile` for variables in :py:attr:`.variables` and adds them
        to the output data set including the corresponding dimension, coordinate variable and attributes applying the
        method in :py:attr:`.emosmethod` to obtain the percentiles.
        """
        # create decile dimension
        self.outdataset.dataset.createDimension('decile', len(self.decile))
        # create decile variable
        self.outdataset.dataset.createVariable('decile', 'i4', 'decile')
        self.outdataset.dataset['decile'][:] = self.decile[:]
        atts = {'long_name': "Percentiles of the ensemble forecast"}
        self.outdataset.dataset['decile'].setncatts(atts)
        for name, variable in self.simufiles[0].dataset.variables.items():
            if name in self.variables:
                # calculate deciles
                vardecile = self.emosmethod(self, varname=name, level=self.decile)
                if self.flipaxis:
                    # get decile axis in the right place
                    vardecile = np.moveaxis(vardecile, 0, -1)
                fillval = self.simufiles[0].getfillvalue(name)
                self.outdataset.dataset.createVariable(name, variable.datatype, variable.dimensions + ('decile',),
                    fill_value=fillval)
                # copy variable attributes all at once via dictionary, but without _FillValue
                attdict = self.simufiles[0].dataset[name].__dict__
                attdict.pop('_FillValue', None)
                attdict['emos_method'] = str(self.emosmethod).split()[1]
                self.outdataset.dataset[name].setncatts(attdict)
                # print(vardecile.shape)
                self.outdataset.dataset[name][:] = vardecile[:]

    def median(self):
        """
        Calculates the median for variables in :py:attr:`.variables` and adds the median
        to the output data set including the corresponding attributes.
        """
        print('entered median')
        for name, variable in self.simufiles[0].dataset.variables.items():
            if name in self.variables:
                # print(name)
                median = self.quantile(name, 50)
                fillval = self.simufiles[0].getfillvalue(name)
                self.outdataset.dataset.createVariable(name, variable.datatype, variable.dimensions,
                    fill_value=fillval)
                # copy variable attributes all at once via dictionary, but without _FillValue
                attdict = self.simufiles[0].dataset[name].__dict__
                attdict.pop('_FillValue', None)
                self.outdataset.dataset[name].setncatts(attdict)
                print(self.outdataset.dataset[name][:].size)
                print(median.size)
                self.outdataset.dataset[name][:] = median[:]


class EnsemblePostprocStation(EnsemblePostproc):
    """
    Class for ensemble postprocessing at station locations.
    """

    @property
    def standardvars(self):
        """variables always written to the output file"""
        return ['time', 'ZS', 'aspect', 'slope', 'station', 'longitude', 'latitude']


class EnsembleHydro(EnsemblePostproc):
    """
    Class to provide a synthesis of ensemble hydrological diagnostics of S2M
    """

    @property
    def spatialdim(self):
        return 'basin'

    @property
    def standardvars(self):
        return ['time', 'basin']


if __name__ == "__main__":
    USAGE = "usage: python postprocess.py [-b YYYYMMDD] [-e YYYYMMDD] [-o diroutput]"

    PARSER = argparse.ArgumentParser(description="Postprocess new snow heights: "
                                                 "1) extracts operational simulation results,"
                                                 "2) Plots maps for the Alps, the Pyrenees the Corse,"
                                                 "3) Creates spaghetti plots "
                                                 "for all massifs and stations")

    PARSER.add_argument("-b", action="store", type=str, dest="datebegin", default=today().ymd,
                        help="First year of extraction")
    PARSER.add_argument("-e", action="store", type=str, dest="dateend", default=today().ymd,
                        help="Last year of extraction")
    PARSER.add_argument("-o", action="store", type=str, dest="diroutput",
                        default=os.path.join(LUSTRE_NOSAVE_USER_DIR, "PEARPS2M"),
                        help="Output directory")
    PARSER.add_argument("--dev", action="store_true", dest="dev", default=False)
    PARSER.add_argument("--reforecast", action="store_true", dest="reforecast", default=False)
    PARSER.add_argument("--dble", action="store_true", dest="dble", default=False)
    OPTIONS = PARSER.parse_args()  # @UnusedVariable
    c = Config(OPTIONS)
    main(c)
    if OPTIONS.dev:
        pp_plots(c)
