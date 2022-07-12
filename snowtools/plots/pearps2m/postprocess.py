#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 6 déc. 2018

@author: lafaysse

usage: python postprocess.py [-b YYYYMMDD] [-e YYYYMMDD] [-o diroutput]

    #) extracts operational simulation results
    #) Plots maps for the Alps, the Pyrenees the Corse
    #) Creates spaghetti plots for all massifs and stations
"""

import argparse
import locale
import os
from collections import Counter, defaultdict
import numpy as np
import datetime
import matplotlib
matplotlib.use('Agg')
import matplotlib.style

from abc import ABC, abstractmethod
from bronx.stdtypes.date import today
from bronx.syntax.externalcode import ExternalCodeImportChecker
from snowtools.utils.prosimu import prosimu
from snowtools.utils.dates import check_and_convert_date, pretty_date
from snowtools.plots.temporal.chrono import spaghettis_with_det, spaghettis
from snowtools.utils.infomassifs import infomassifs
from snowtools.utils.FileException import DirNameException
from snowtools.DATA import LUSTRE_NOSAVE_USER_DIR

ECHECKER = ExternalCodeImportChecker('plots.maps.cartopy')
with ECHECKER:
    from snowtools.plots.maps.cartopy import Map_alpes, Map_pyrenees, \
        Map_corse, Map_vosges, Map_jura, Map_central

if 'fast' in matplotlib.style.available:
    matplotlib.style.use('fast')
matplotlib.rcParams['agg.path.chunksize'] = 100
matplotlib.rcParams['axes.xmargin'] = 0
matplotlib.rcParams['axes.ymargin'] = 0
# matplotlib.rcParams["figure.dpi"] = 75


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

OPTIONS = PARSER.parse_args()  # @UnusedVariable


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


class Config:
    """
    Configuration passed to S2MExtractor and to be used in vortex toolboxes.

    """
    previ = True  #: False for analysis, True for forecast
    xpid = "oper"  #: Operational chain
    alternate_xpid = ["OPER@lafaysse"]  #: Alternative experiment id
    # alternate_xpid = ["oper"]
    #: List of geometries
    list_geometry = ['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']
    # list_geometry = ['alp', 'pyr', 'cor', 'postes']
    #: Alternative list of geometries (corresponding to alternative experiment ids)
    # alternate_list_geometry = [['alp', 'pyr', 'cor', 'postes']]
    alternate_list_geometry = [['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']]
    # Development chain
    # xpid = "OPER@lafaysse"  # To be changed with IGA account when operational
    # list_geometry = ['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']
    #: 35 for determinstic member, 36 for sytron, 0-34 for PEARP members
    list_members = list(range(0, 36))

    def __init__(self):
        """
        #) checks and converts dates
        #) creates output directories if they don't exist.
        """
        OPTIONS.datebegin, OPTIONS.dateend = [check_and_convert_date(dat)
                                              for dat in [OPTIONS.datebegin, OPTIONS.dateend]]
        if OPTIONS.datebegin.hour == 0:
            #: Date of model run  class:`bronx.stdtypes.date.Date`
            self.rundate = OPTIONS.datebegin.replace(hour=6)
        else:
            #: Date of model run  :class:`bronx.stdtypes.date.Date`
            self.rundate = OPTIONS.datebegin
        #: output directory
        self.diroutput = OPTIONS.diroutput + "/" + self.rundate.strftime("%Y%m%d%H")
        self.diroutput_maps = self.diroutput + "/maps"  #: output directory for maps
        self.diroutput_plots = self.diroutput + "/plots"  #: output directory for other plots

        for required_directory in [self.diroutput, self.diroutput_maps, self.diroutput_plots]:
            if not os.path.isdir(required_directory):
                os.mkdir(required_directory)

        self.dev = OPTIONS.dev
        if OPTIONS.dev:
            self.xpid = "nouveaux_guess@lafaysse"
            delattr(Config, 'alternate_xpid')
            self.list_geometry = ['jur4_allslopes', 'mac11_allslopes',
                                  'vog3_allslopes', 'cor', 'alp', 'pyr'] # , 'postes'
        self.reforecast = OPTIONS.reforecast
        if OPTIONS.reforecast:
            self.xpid = "reforecast_double2021@vernaym"
            delattr(Config, 'alternate_xpid')
            self.list_geometry = ['jur4_allslopes_reforecast', 'mac11_allslopes_reforecast',
                                  'vog3_allslopes_reforecast', 'alp27_allslopes',
                                  'pyr24_allslopes', 'cor2_allslopes']


class Ensemble:
    """
    Describes an ensemble of simulations

    """

    def __init__(self):
        """
        """
        self.spatialdim = "Number_of_points"  #: spatial dimension
        self.ensemble = {}  #: data dict with variable names as keys and np.arrays as values
        self.simufiles = []  #: list of prosimu objects with simulation file

    @property
    def nech(self):
        """number of leadtimes (forecast steps)"""
        return self._nech

    @nech.setter
    def nech(self, value):
        self._nech = value

    @property
    def inddeterministic(self):
        """index of the deterministic member"""
        return self._inddeterministic

    @inddeterministic.setter
    def inddeterministic(self, value):
        self._inddeterministic = value

    @property
    def nmembers(self):
        """number of ensemble members"""
        return self._nmembers

    @nmembers.setter
    def nmembers(self, value):
        self._nmembers = value

    @property
    def time(self):
        """time variable from the first simulation file (leadtime array)"""
        return self._time

    @time.setter
    def time(self, value):
        self._time = value

    @property
    def indpoints(self):
        """list with spatial indices"""
        return self._indpoints

    @indpoints.setter
    def indpoints(self, value):
        self._indpoints = value

    @property
    def npoints(self):
        """total number of spatial points"""
        return self._npoints

    @npoints.setter
    def npoints(self, value):
        self._npoints = value

    @property
    def alti(self):
        """
        altitude variable from the first simulation file.

        :return: altitude variable
        :rtype: numpy array
        """
        return self._alti

    @alti.setter
    def alti(self, value):
        self._alti = value

    @property
    def aspect(self):
        """
        aspect variable from the first simulation file.

        :return: aspect variable
        :rtype: numpy array
        """
        return self._aspect

    @aspect.setter
    def aspect(self, value):
        self._aspect = value

    def open(self, listmembers):
        """
        Opens simulation files

        :param listmembers: list of ensemble members (filenames)
        :type listmembers: list
        """
        print(listmembers)
        for m_i, member in enumerate(listmembers):
            p_i = prosimu(member)
            if m_i == 0:
                self.nech = p_i.getlendim("time")
            ntime = p_i.getlendim("time")
            if ntime == self.nech:
                self.simufiles.append(p_i)
                if 'mb035' in member:
                    self.inddeterministic = len(self.simufiles) - 1

        self.nmembers = len(self.simufiles)
        print(self.nmembers)
        self.time = self.simufiles[0].readtime()

        self.indpoints = self.select_points()
        self.npoints = self.get_npoints()
        self.alti = self.read_geovar("ZS")
        self.aspect = self.read_geovar("aspect")

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
        for m_i, member in enumerate(self.simufiles):
            print("read " + varname + " for member" + str(m_i))
            before = datetime.datetime.today()

            if isinstance(self.indpoints, tuple):
                sections = []
                for indpoints in self.indpoints:
                    sections.append(member.read_var(varname, Number_of_points=indpoints))

                self.ensemble[varname][:, :, m_i] = np.concatenate(tuple(sections), axis=1)
            else:
                self.ensemble[varname][:, :, m_i] = member.read_var(varname,
                                                                    Number_of_points=self.indpoints)

            after = datetime.datetime.today()
            print(after - before)

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
        if isinstance(self.indpoints, tuple):
            sections = []
            for indpoints in self.indpoints:
                sections.append(self.simufiles[0].read_var(varname, Number_of_points=indpoints))

            return np.concatenate(tuple(sections))

        return self.simufiles[0].read_var(varname, Number_of_points=self.indpoints)

    def probability(self, varname, seuilinf=-999999999, seuilsup=999999999):
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

    @property
    def massifdim(self):
        """
        Read massif_num variable from the first simulation file.

        :return: massif numbers
        :rtype: numpy array
        """
        if not hasattr(self, "_massifdim"):
            self.massifdim = self.read_geovar("massif_num")
        return self._massifdim

    @massifdim.setter
    def massifdim(self, value):
        self._massifdim = value

    @property
    def massifvar(self):
        """
        "massif" variable from the first simulation file.

        :return: massif numbers
        :rtype: numpy array
        """
        if not hasattr(self, "_massifvar"):
            self.massifvar = self.simufiles[0].read_var("massif")
        return self._massifvar

    @massifvar.setter
    def massifvar(self, value):
        self._massifvar = value

    def read(self, varname):
        """
        Read data for a given variable name into the :py:attr:`ensemble` instance variable.

        :param varname: variable name
        :type varname: str

        """
        if varname == 'naturalIndex':
            nmassifs = len(self.massifvar)
            self.ensemble[varname] = np.empty([self.nech, nmassifs, self.nmembers])
            for m_i, member in enumerate(self.simufiles):
                self.ensemble[varname][:, :, m_i] = member.read_var(varname)
        else:
            super(_EnsembleMassif, self).read(varname)

    def get_metadata(self, nolevel=False):
        """
        Construct filenames and plot titles from massif and altitude
        variables in the first simulation file.

        :param nolevel: if True the altitude is not included in the filenames and titles.
        :return: a list of filenames and a list of titles
        :rtype: two lists.
        """

        if nolevel:
            massif = self.massifvar
            alti = [None] * len(massif)
        else:
            alti = self.alti
            massif = self.massifdim

        return [build_filename(mas, alt) for mas, alt in zip(massif, alti)], \
               [self.build_title(mas, alt) for mas, alt in zip(massif, alti)]

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
            title += "{:d}m".format(int(alti))
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

    def get_metadata(self, nolevel=False):
        """
        Construct filenames and plot titles from altitude and station information

        :param nolevel: ignored parameter for this class, but needed for coherence with
        other classes.
        :return: a list of filenames and a list of plot titles
        """
        if nolevel:
            print('warning: nolevel ignored for EnsembleStation class filenames and plot titles')
        alti = self.simufiles[0].read_var("ZS", Number_of_points=self.indpoints)
        station = self.get_station()
        # nameposte gives unicode
        # matplotlib expects unicode
        return ['{0:08d}'.format(stat) for stat, alt in zip(station, alti)], \
               ["{0}{1:d}m".format(self.InfoMassifs.nameposte(stat),
                                   int(alt)) for stat, alt in zip(station, alti)]


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


class EnsembleOperDiags(ABC, EnsembleDiags):
    """
    Class for operationally used plots.
    """
    #: plot format
    formatplot = 'png'
    #: dict of plot attributes for each variable
    attributes = dict(
        PP_SD_1DY_ISBA=dict(convert_unit=1., forcemin=0., forcemax=60.,
                            palette='YlGnBu', seuiltext=50.,
                            label=u'Epaisseur de neige fraîche en 24h (cm)'),
        SD_1DY_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60.,
                         palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)'),
        SD_3DY_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60.,
                         palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 72h (cm)'),
        RAMSOND_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60.,
                          palette='YlGnBu', seuiltext=50.,
                          label=u'Epaisseur mobilisable (cm)'),
        NAT_LEV=dict(forcemin=-0.5, forcemax=5.5, palette='YlOrRd',
                     ncolors=6, label=u'Risque naturel',
                     ticks=[u'Très faible', u'Faible', u'Mod. A',
                            u'Mod. D', u'Fort', u'Très fort']),
        naturalIndex=dict(forcemin=0., forcemax=8., palette='YlOrRd',
                          label=u'Indice de risque naturel', format='%.1f',
                          nolevel=True),
        DSN_T_ISBA=dict(convert_unit=100., label=u'Hauteur de neige (cm)'),
        WSN_T_ISBA=dict(label=u'Equivalent en eau (kg/m2)'),
        SNOMLT_ISBA=dict(convert_unit=3. * 3600., forcemin=0.,
                         forcemax=60., palette='YlGnBu', seuiltext=50.,
                         label=u'Ecoulement en 3h (kg/m2/3h)'),
        WET_TH_ISBA=dict(convert_unit=100., forcemin=0.,
                         forcemax=60., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur humide (cm)'),
        REFRZTH_ISBA=dict(convert_unit=100., forcemin=0.,
                          forcemax=60., palette='YlGnBu', seuiltext=50.,
                          label=u'Epaisseur regelée (cm)'),
        RAINF_ISBA=dict(convert_unit=3. * 3600., forcemin=0.,
                        forcemax=60., palette='YlGnBu', seuiltext=50.,
                        label=u'Pluie en 3h (kg/m2/3h)'),
    )
    #: list of quantiles
    list_q = [20, 50, 80]

    @abstractmethod
    def get_metadata(self, nolevel=False):
        raise NotImplementedError("Must override get_metadata")

    def alldiags(self, list_var_spag, list_var_map):
        """
        Calculate percentiles for all variables in
        :py:attr:`list_var_spag` and :py:attr:`list_var_map`.

        :param list_var_spag: list of variables for spaghetti plots
        :type list_var_spag: list
        :param list_var_map: list of variables for map plots
        :type list_var_map: list
        """
        super(EnsembleOperDiags, self).diags(set(list_var_spag + list_var_map),
                                             self.list_q, {})

    def pack_spaghettis(self, list_var_spag, suptitle_s, diroutput="."):
        """
        Produce spaghetti plots for all variables in :py:attr:`list_var_spag`.

        :param list_var_spag: variable list
        :type list_var_spag: list of strings
        :param suptitle_s: Suptitle for all plots.
        :type suptitle_s: unicode string
        :param diroutput: directory to save the plots
        """

        for var in list_var_spag:

            if 'nolevel' not in self.attributes[var].keys():
                self.attributes[var]['nolevel'] = False

            list_filenames, list_titles = self.get_metadata(nolevel=self.attributes[var]['nolevel'])

            if hasattr(self, 'inddeterministic'):
                # print('has inddet')
                sp_h = spaghettis_with_det(self.time)
            else:
                # print('no inddet')
                sp_h = spaghettis(self.time)
            settings = self.attributes[var].copy()
            if 'label' in self.attributes[var].keys():
                settings['ylabel'] = self.attributes[var]['label']
            npoints = self.quantiles[var][0][0, :].shape[0]

            for point in range(0, npoints):
                if 'convert_unit' in self.attributes[var].keys():
                    allmembers = self.ensemble[var][:, point, :] \
                                 * self.attributes[var]['convert_unit']
                    qmin = self.quantiles[var][0][:, point] * self.attributes[var]['convert_unit']
                    qmed = self.quantiles[var][1][:, point] * self.attributes[var]['convert_unit']
                    qmax = self.quantiles[var][2][:, point] * self.attributes[var]['convert_unit']
                else:
                    allmembers = self.ensemble[var][:, point, :]
                    qmin = self.quantiles[var][0][:, point]
                    qmed = self.quantiles[var][1][:, point]
                    qmax = self.quantiles[var][2][:, point]

                if hasattr(self, '_inddeterministic'):
                    sp_h.draw(self.time, allmembers, qmin, qmed, qmax,
                              deterministic=allmembers[:, self.inddeterministic],
                              **settings)
                else:
                    sp_h.draw(self.time, allmembers, qmin, qmed, qmax, **settings)

                sp_h.set_title(list_titles[point])
                sp_h.set_suptitle(suptitle_s)
                sp_h.addlogo()
                plotname = diroutput + "/" + var + "_" + list_filenames[point] + \
                    "." + self.formatplot
                sp_h.save(plotname, formatout=self.formatplot)
                print(plotname + " is available.")

            sp_h.close()

    def pack_spaghettis_multipoints(self, list_var_spag_2points, list_pairs,
                                    suptitle_s, diroutput=".", **kwargs):
        """
        Produce spaghetti plots with up to four points per plot for
        variables in :py:attr:`list_var_spag_2points`.

        Each point gets a different color.
        :param list_var_spag_2points: list of variables to plot
        :type list_var_spag_2points: list of strings
        :param list_pairs: list of indices to plot together on each plot
        :type list_pairs: list of list
        :param suptitle_s: common suptitle for all plots
        :type suptitle_s: unicode str
        :param diroutput: output directory
        :param kwargs: arguments to be passed to plotting routines, notably "labels"
        """

        list_colors = ['blue', 'red', 'green', 'orange']

        for var in list_var_spag_2points:
            # print(var)
            # print(self.attributes[var])
            if 'nolevel' not in self.attributes[var].keys():
                self.attributes[var]['nolevel'] = False

            list_filenames, list_titles = self.get_metadata(nolevel=self.attributes[var]['nolevel'])

            if hasattr(self, 'inddeterministic'):
                sp_h = spaghettis_with_det(self.time)
            else:
                sp_h = spaghettis(self.time)

            settings = self.attributes[var].copy()
            if 'label' in self.attributes[var].keys():
                settings['ylabel'] = self.attributes[var]['label']

            for pair in list_pairs:
                for p_i, point in enumerate(pair):
                    if 'convert_unit' in self.attributes[var].keys():
                        allmembers = self.ensemble[var][:, point, :] \
                                     * self.attributes[var]['convert_unit']
                        qmin = self.quantiles[var][0][:, point] \
                            * self.attributes[var]['convert_unit']
                        qmed = self.quantiles[var][1][:, point] \
                            * self.attributes[var]['convert_unit']
                        qmax = self.quantiles[var][2][:, point] \
                            * self.attributes[var]['convert_unit']
                    else:
                        allmembers = self.ensemble[var][:, point, :]
                        qmin = self.quantiles[var][0][:, point]
                        qmed = self.quantiles[var][1][:, point]
                        qmax = self.quantiles[var][2][:, point]
                    settings['colorquantiles'] = list_colors[p_i]
                    settings['colormembers'] = list_colors[p_i]
                    if 'labels' in kwargs.keys():
                        settings['commonlabel'] = kwargs['labels'][p_i]

                    if hasattr(self, 'inddeterministic'):
                        sp_h.draw(self.time, allmembers, qmin, qmed, qmax,
                                  deterministic=allmembers[:, self.inddeterministic], **settings)
                    else:
                        sp_h.draw(self.time, allmembers, qmin, qmed, qmax, **settings)

                sp_h.set_title(list_titles[pair[0]])
                sp_h.set_suptitle(suptitle_s)
                sp_h.addlogo()
                plotname = diroutput + "/" + var + "_" + list_filenames[pair[0]] \
                    + "." + self.formatplot
                sp_h.save(plotname, formatout=self.formatplot)
                print(plotname + " is available.")

            sp_h.close()


@ECHECKER.disabled_if_unavailable
class EnsembleOperDiagsFlatMassif(EnsembleFlatMassif, EnsembleOperDiags):
    """
    Class for operationally plotting maps and spaghetti plots for
    ensembles with massif geometry and for the zero slope case.
    """
    #: maximum height level to be treated
    levelmax = 3900
    #: minimum heigth level to be treated
    levelmin = 0
    #: list of variables to plot maps for
    list_var_map = ['naturalIndex', 'SD_1DY_ISBA', 'SD_3DY_ISBA', 'SNOMLT_ISBA']
    #: list of variables to do spaghetti plots for
    list_var_spag = ['naturalIndex', 'DSN_T_ISBA', 'WSN_T_ISBA', 'SNOMLT_ISBA']
    # list_var_spag = ['DSN_T_ISBA', 'WSN_T_ISBA', 'SNOMLT_ISBA']

    def pack_maps(self, domain_m, suptitle_m, diroutput="."):
        """
        Produce maps for the variables given in :py:attr:`list_var_map`
        over the regions given in :py:attr:`domain`
        for each available altitude level and time step.

        Each massif is colored corresponding to the second percentile value defined in
        :py:attr:`.list_q` and
        the color map defined in :py:attr:`.attributes`.
        At the center of each massif the values corresponding to
        the first three
        percentiles in :py:attr:`.list_q` are marked.

        :param domain_m: list of region identifiers (e.g., "alp", "pyr", "cor")
        :param suptitle_m: common suptitle for all plots
        :type suptitle_m: str
        :param diroutput: output directory to save the maps
        """

        map_generic = dict(alp=Map_alpes, pyr=Map_pyrenees, cor=Map_corse, jur=Map_jura,
                           mac=Map_central, vog=Map_vosges)

        m_g = map_generic[domain_m[0:3]]()
        for var in self.list_var_map:
            m_g.init_massifs(**self.attributes[var])
            m_g.addlogo()
            if 'nolevel' not in self.attributes[var].keys():
                self.attributes[var]['nolevel'] = False
            if self.attributes[var]['nolevel']:
                list_loop_alti = [0]
                massif = self.massifvar
                indalti = np.ones_like(self.quantiles[var][0][0, :], dtype=bool)
            else:
                list_loop_alti = list(set(self.alti))
                for level in list_loop_alti:
                    if level < self.levelmin or level > self.levelmax:
                        list_loop_alti.remove(level)

                massif = self.massifdim

            for level in list_loop_alti:
                if not self.attributes[var]['nolevel']:
                    indalti = self.alti == level

                for t_i in range(0, self.nech):
                    qmin = self.quantiles[var][0][t_i, indalti]
                    qmed = self.quantiles[var][1][t_i, indalti]
                    qmax = self.quantiles[var][2][t_i, indalti]

                    m_g.draw_massifs(massif[indalti], qmed, **self.attributes[var])

                    m_g.plot_center_massif(massif[indalti], qmin, qmed, qmax,
                                           **self.attributes[var])

                    title = "pour le " + pretty_date(self.time[t_i])
                    if not self.attributes[var]['nolevel']:
                        title += " - Altitude : " + str(int(level)) + "m"

                    m_g.set_title(title)
                    m_g.set_suptitle(suptitle_m)
                    ech = self.time[t_i] - self.time[0] + self.time[1] - self.time[0]
                    ech_str = '+%02d' % (ech.days * 24 + ech.seconds / 3600)
                    plotname = diroutput + "/" + domain_m[0:3] + "_" + var + "_" \
                        + str(int(level)) + ech_str + "." + self.formatplot
                    m_g.save(plotname, formatout=self.formatplot)
                    print(plotname + " is available.")
                    m_g.reset_massifs(rmcbar=False)
            m_g.reset_massifs()


@ECHECKER.disabled_if_unavailable
class EnsembleOperDiagsNorthSouthMassif(EnsembleNorthSouthMassif, EnsembleOperDiags):
    """
    Class for operationally plot maps and spaghetti plots
    distinguishing between northern and southern orientation
    at each massif.
    """
    #: maximum height level to plot
    levelmax = 3900
    #: minimum height level to plot
    levelmin = 0
    #: labels for the two orientations
    versants = [u'Nord 40°', u'Sud 40°']
    #: variable list for standard spaghetti plots
    list_var_spag = []
    #: variable list for spaghetti plots combining northern and southern orientation
    list_var_spag_2points = ['RAMSOND_ISBA', 'NAT_LEV', 'WET_TH_ISBA', 'REFRZTH_ISBA']
    #: variable list for producing maps
    list_var_map = ['RAMSOND_ISBA', 'NAT_LEV', 'WET_TH_ISBA', 'REFRZTH_ISBA']
    #: ensemble data
    ensemble = {}

    @property
    def list_pairs(self):
        """Pair of indices corresponding to values for the northern and
        southern slopes for each massif and altitude the ."""
        if not hasattr(self, '_list_pairs'):
            self.list_pairs = self.get_pairs_ns()
        return self._list_pairs

    @list_pairs.setter
    def list_pairs(self, value):
        self._list_pairs = value

    def alldiags(self, list_var_spag, list_var_map):
        """
        Calculate percentiles for all variables in :py:attr:`.list_var_spag_2points`
        and :py:attr:`.list_var_map`.

        :param list_var_spag: variable list for spaghetti plots
        combining northern and southern orientation
        :param list_var_map: variable list for producing maps
        """
        super(EnsembleOperDiagsNorthSouthMassif, self).diags(set(list_var_spag + list_var_map),
                                                             self.list_q, {})

    def get_pairs_ns(self):
        """
        Get for each massif and altitude the pair of indices corresponding to
        values for the northern and southern slopes.

        :return: indices corresponding to northern and southern slopes
        :rtype: list of lists
        """
        aspect = np.array([int(asp) for asp in self.aspect])

        self.list_pairs = []

        for point in range(0, np.shape(self.alti)[0]):
            if aspect[point] == 0:
                indsouth = np.where((self.alti == self.alti[point]) & (aspect == 180)
                                    & (self.massifdim == self.massifdim[point]))
                if len(indsouth) == 1:
                    self.list_pairs.append([point, indsouth[0][0]])

        return self.list_pairs

    def pack_spaghettis_ns(self, suptitle_s, diroutput="."):
        """
        Do spaghetti plots with values for the northern and southern slope on the same plot.

        :param suptitle_s: common suptitle for all plots
        :param diroutput: directory to save the plots
        """

        super(EnsembleOperDiagsNorthSouthMassif, self).pack_spaghettis_multipoints(self.list_var_spag_2points,
                                                                                   self.list_pairs,
                                                                                   suptitle_s,
                                                                                   diroutput,
                                                                                   labels=self.versants)

    def pack_maps(self, domain_m, suptitle_m, diroutput):
        """
        Produce maps for the variables given in :py:attr:`.list_var_map`
        over the regions given in :py:attr:`.domain`
        for each available altitude level and time step.

        For each massif a table with background colors and values corresponding to the
        percentiles in :py:attr:`.list_q` and the color map defined in
        :py:attr:`.attributes` is plotted near the center of each massif.

        :param domain_m: list of region identifiers (e.g., "alp", "pyr", "cor")
        :param suptitle_m: common suptitle for all plots
        :type suptitle_m: str
        :param diroutput: output directory to save the maps
        """

        map_generic = dict(alp=Map_alpes, pyr=Map_pyrenees, cor=Map_corse,
                           jur=Map_jura, vog=Map_vosges, mac=Map_central)

        m_g = map_generic[domain_m[0:3]]()
        for var in self.list_var_map:
            m_g.init_massifs(**self.attributes[var])
            m_g.empty_massifs()
            m_g.add_north_south_info()
            m_g.addlogo()

            list_loop_alti = list(set(self.alti))
            for level in list_loop_alti:
                if level < self.levelmin or level > self.levelmax:
                    list_loop_alti.remove(level)

            for level in list_loop_alti:

                list_indalti = []
                for plotaspect in [180, 0]:  # du bas vers le haut

                    list_indalti.append((self.alti == level) & (self.aspect == plotaspect))

                for t_i in range(0, self.nech):
                    list_values = []
                    for indalti in list_indalti:
                        for q_i in range(len(self.list_q)):
                            list_values.append(self.quantiles[var][q_i][t_i, indalti])
                    # print(len(massif[indalti]))
                    m_g.rectangle_massif(self.massifdim[indalti], list_values, ncol=2,
                                         **self.attributes[var])
                    title = "pour le " + pretty_date(self.time[t_i])
                    title += " - Altitude : " + str(int(level)) + "m"

                    m_g.set_title(title)
                    m_g.set_suptitle(suptitle_m)
                    ech = self.time[t_i] - self.time[0] + self.time[1] - self.time[0]
                    ech_str = '+%02d' % (ech.days * 24 + ech.seconds / 3600)
                    plotname = diroutput + "/" + domain_m[0:3] + "_" + var + "_" \
                        + str(int(level)) + ech_str + "." + self.formatplot
                    m_g.save(plotname, formatout=self.formatplot)
                    print(plotname + " is available.")
                    m_g.reset_massifs(rmcbar=False, rminfobox=False)
            m_g.reset_massifs()


class EnsembleOperDiagsStations(EnsembleStation, EnsembleOperDiags):
    """
    Class for operationally plotting spaghetti plots for station based simulations.
    """
    #: list of variables to plot maps for
    list_var_map = []
    #: list of variables to plot spaghetti plots for.
    list_var_spag = ['DSN_T_ISBA', 'WSN_T_ISBA', 'RAMSOND_ISBA',
                     'WET_TH_ISBA', 'REFRZTH_ISBA', 'SNOMLT_ISBA']


class EnsemblePostproc(_EnsembleMassif):
    """
    Class for ensemble post-processing.
    """

    def __init__(self, ensemble, variables, inputfiles, datebegin, dateend,
                 decile=range(10, 100, 10)):
        """
        :param ensemble: object to hold the ensemble data
        :type ensemble: instance of :py:class:`.Ensemble` or inheriting from :py:class:`.Ensemble`
        :param variables: list of variables to process
        :param inputfiles: list of input files
        :param datebegin: start date
        :type datebegin: :py:class:`~.bronx.stdtypes.date.Date`
        :param dateend: end date
        :type dateend: :py:class:`~.bronx.stdtypes.date.Date`
        :param decile: list of percentiles
        """
        print(inputfiles)
        super(EnsemblePostproc, self).__init__()
        self.ensemble = ensemble  #: ensemble data
        self.variables = variables  #: list of variables
        self.ensemble.open(inputfiles)
        #: output filename
        self.outfile = 'PRO_post_{0}_{1}.nc'.format(datebegin.ymdh, dateend.ymdh)
        #: variables always written to the output file
        #: ['time', 'ZS', 'aspect', 'slope', 'massif_num', 'longitude', 'latitude']
        self.standardvars = ['time', 'ZS', 'aspect', 'slope', 'massif_num', 'longitude', 'latitude']
        #: list of percentiles
        self.decile = decile

    @property
    def outdataset(self):
        """output data set. (Prosimu instance)"""
        return self._outdataset

    @outdataset.setter
    def outdataset(self, value):
        self._outdataset = value

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
        self.outdataset.dataset.setncatts(self.ensemble.simufiles[0].dataset.__dict__)
        # copy dimensions
        for name, dimension in self.ensemble.simufiles[0].dataset.dimensions.items():
            self.outdataset.dataset.createDimension(
                name, (len(dimension) if not dimension.isunlimited() else None))
        print(self.outdataset.listdim())

        # copy standard variables
        for name, variable in self.ensemble.simufiles[0].dataset.variables.items():
            if name in self.standardvars:
                fillval = self.ensemble.simufiles[0].getfillvalue(name)
                x_od = self.outdataset.dataset.createVariable(name, variable.datatype,
                                                              variable.dimensions,
                                                              fill_value=fillval)
                # copy variable attributes without _FillValue since this causes an error
                for att in self.ensemble.simufiles[0].listattr(name):
                    if att != '_FillValue':
                        print(self.ensemble.simufiles[0].getattr(name, att))
                        self.outdataset.dataset[name].setncatts({att: self.ensemble.simufiles[0].getattr(name, att)})
                self.outdataset.dataset[name][:] = self.ensemble.simufiles[0].dataset[name][:]
                print('data copied')
        print(self.outdataset.listvar())

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
        self.ensemble.close()

    def deciles(self):
        """
        Calculates percentiles given in :py:attr:`.decile` for variables in
        :py:attr:`.variables` and adds them to the output data set including
        the corresponding dimension, coordinate variable and attributes.
        """
        # create decile dimension
        self.outdataset.dataset.createDimension('decile', len(self.decile))
        # create decile variable
        x_ds = self.outdataset.dataset.createVariable('decile', 'i4', 'decile')
        self.outdataset.dataset['decile'][:] = self.decile[:]
        atts = {'long_name': "Percentiles of the ensemble forecast"}
        self.outdataset.dataset['decile'].setncatts(atts)
        for name, variable in self.ensemble.simufiles[0].dataset.variables.items():
            if name in self.variables:
                # calculate deciles
                vardecile = self.ensemble.quantile(name, self.decile)
                # get decile axis in the right place
                vardecile = np.moveaxis(vardecile, 0, -1)
                fillval = self.ensemble.simufiles[0].getfillvalue(name)
                x_ds = self.outdataset.dataset.createVariable(name, variable.datatype,
                                                              variable.dimensions + ('decile',),
                                                              fill_value=fillval)
                # copy variable attributes all at once via dictionary, but without _FillValue
                attdict = self.ensemble.simufiles[0].dataset[name].__dict__
                attdict.pop('_FillValue', None)
                self.outdataset.dataset[name].setncatts(attdict)
                print(vardecile.shape)
                self.outdataset.dataset[name][:] = vardecile[:]

    def median(self):
        """
        Calculates the median for variables in :py:attr:`.variables` and adds the median
        to the output data set including the corresponding attributes.
        """
        print('entered median')
        for name, variable in self.ensemble.simufiles[0].dataset.variables.items():
            if name in self.variables:
                median = self.ensemble.quantile(name, 50)
                fillval = self.ensemble.simufiles[0].getfillvalue(name)
                x_ds = self.outdataset.dataset.createVariable(name, variable.datatype,
                                                              variable.dimensions,
                                                              fill_value=fillval)
                # copy variable attributes all at once via dictionary, but without _FillValue
                attdict = self.ensemble.simufiles[0].dataset[name].__dict__
                attdict.pop('_FillValue', None)
                self.outdataset.dataset[name].setncatts(attdict)
                print(self.outdataset.dataset[name][:].size)
                print(median.size)
                self.outdataset.dataset[name][:] = median[:]


if __name__ == "__main__":

    # The following class has a vortex-dependence
    # Should not import than above to avoid problems when importing the module from vortex
    from snowtools.tasks.oper.get_oper_files import S2MExtractor, FutureS2MExtractor

    C = Config()
    os.chdir(C.diroutput)
    if C.dev:
        S2ME = FutureS2MExtractor(C)
    elif C.reforecast:
        S2ME = FutureS2MExtractor(C)
    else:
        S2ME = S2MExtractor(C)
    SNOW_MEMBERS, SNOW_XPID = S2ME.get_snow()

    DICT_CHAINE = defaultdict(str)
    DICT_CHAINE['OPER'] = ' (oper)'
    DICT_CHAINE['MIRR'] = ' (miroir)'
    DICT_CHAINE['OPER@lafaysse'] = ' (dev)'
    DICT_CHAINE['nouveaux_guess@lafaysse'] = ' (dev)'
    # undefined xpid is possible because it is allowed by defaultdict

    locale.setlocale(locale.LC_TIME, 'fr_FR.UTF-8')

    LIST_DOMAINS = SNOW_MEMBERS.keys()
    print(LIST_DOMAINS)

    for domain in LIST_DOMAINS:  # ['alp_allslopes']: # ['postes']: #

        # S2ME.conf.rundate is a Date object --> strftime already calls decode method
        suptitle = u'Prévisions PEARP-S2M du ' + pretty_date(S2ME.conf.rundate)
        # Identify the prevailing xpid in the obtained resources and adapt the title
        count = Counter(SNOW_XPID[domain])
        print(count)
        prevailing_xpid = count.most_common(1)[0][0]
        suffixe_suptitle = DICT_CHAINE[prevailing_xpid]
        suptitle += suffixe_suptitle

        if domain == 'postes':
            E = EnsembleOperDiagsStations()
        else:
            E = EnsembleOperDiagsFlatMassif()
            ENS = EnsembleOperDiagsNorthSouthMassif()
            ENS.open(SNOW_MEMBERS[domain])
        print('number of member files', len(SNOW_MEMBERS[domain]))
        E.open(SNOW_MEMBERS[domain])

        print("domain " + domain + " npoints = " + str(E.npoints))

        E.alldiags(E.list_var_spag, E.list_var_map)

        print('Diagnostics have been computed for the following variables :')
        print(E.ensemble.keys())

        E.pack_spaghettis(E.list_var_spag, suptitle_s=suptitle, diroutput=C.diroutput_plots)
        if domain != 'postes':
            E.pack_maps(domain, suptitle, diroutput=C.diroutput_maps)

            ENS.alldiags(ENS.list_var_spag_2points, ENS.list_var_map)
            print('Diagnostics have been computed for the following variables :')
            print(ENS.ensemble.keys())
            ENS.pack_maps(domain, suptitle, diroutput=C.diroutput_maps)

            ENS.pack_spaghettis_ns(suptitle, diroutput=C.diroutput_plots)
            ENS.close()
            del ENS

            print(E.list_var_spag)
            print(E.list_var_map)

        E.close()
        del E
