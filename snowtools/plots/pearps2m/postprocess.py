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

import sys
import locale
import os
from optparse import OptionParser
from collections import Counter, defaultdict

import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.style

from snowtools.utils.prosimu import prosimu
from snowtools.utils.dates import check_and_convert_date, pretty_date
from snowtools.plots.temporal.chrono import spaghettis_with_det, spaghettis
from snowtools.utils.infomassifs import infomassifs
from snowtools.utils.FileException import DirNameException
from snowtools.DATA import LUSTRE_NOSAVE_USER_DIR

from bronx.stdtypes.date import today
from bronx.syntax.externalcode import ExternalCodeImportChecker

echecker = ExternalCodeImportChecker('plots.maps.cartopy')
with echecker:
    from snowtools.plots.maps.cartopy import Map_alpes, Map_pyrenees, Map_corse, Map_vosges, Map_jura, Map_central

if 'fast' in matplotlib.style.available:
    matplotlib.style.use('fast')
matplotlib.rcParams['agg.path.chunksize'] = 100
matplotlib.rcParams['axes.xmargin'] = 0
matplotlib.rcParams['axes.ymargin'] = 0
# matplotlib.rcParams["figure.dpi"] = 75


usage = "usage: python postprocess.py [-b YYYYMMDD] [-e YYYYMMDD] [-o diroutput]"


def parse_options(arguments):
    """
    Treat options passed to the script.

    :param arguments: arguments from calling the script
    :return: options
    """
    parser = OptionParser(usage)

    parser.add_option("-b",
                      action="store", type="string", dest="datebegin", default=today().ymd,
                      help="First year of extraction")

    parser.add_option("-e",
                      action="store", type="string", dest="dateend", default=today().ymd,
                      help="Last year of extraction")

    parser.add_option("-o",
                      action="store", type="string", dest="diroutput",
                      default=os.path.join(LUSTRE_NOSAVE_USER_DIR, "PEARPS2M"),
                      help="Output directory")

    parser.add_option("--dev",
                      action="store_true", dest="dev", default=False)
    parser.add_option("--dble", action="store_true", dest="dble", default=False)

    parser.add_option("--reforecast", action="store_true", dest="reforecast", default=False)

    (options, args) = parser.parse_args(arguments)  # @UnusedVariable

    return options


class config(object):
    """
    Configuration passed to S2MExtractor and to be used in vortex toolboxes.

    """
    previ = True  #: False for analysis, True for forecast
    xpid = "oper"  #: Operational chain
    alternate_xpid = ["OPER@lafaysse"]  #: Alternative experiment id
    # alternate_xpid = ["oper"]
    list_geometry = ['alp', 'pyr', 'cor', 'jur', 'mac', 'vog', 'postes']
    # list_geometry = ['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes'] #: List of geometries
    # list_geometry = ['alp', 'pyr', 'cor', 'postes']
    #: Alternative list of geometries (corresponding to alternative experiment ids)
    # alternate_list_geometry = [['alp', 'pyr', 'cor', 'postes']]
    alternate_list_geometry = [['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']]
    # Development chain
    # xpid = "OPER@lafaysse"  # To be changed with IGA account when operational
    # list_geometry = ['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']

    list_members = list(range(0, 36))  #: 35 for determinstic member, 36 for sytron, 0-34 for PEARP members

    def __init__(self):
        """
        #) checks and converts dates
        #) creates output directories if they don't exist.

        """
        options = parse_options(sys.argv)
        options.datebegin, options.dateend = [check_and_convert_date(dat) for dat in [options.datebegin, options.dateend]]
        if options.datebegin.hour == 0:
            self.rundate = options.datebegin.replace(hour=6)  #: Date of model run  class:`bronx.stdtypes.date.Date`
        else:
            self.rundate = options.datebegin  #: Date of model run  :class:`bronx.stdtypes.date.Date`
        self.diroutput = options.diroutput + "/" + self.rundate.strftime("%Y%m%d%H")  #: output directory
        self.diroutput_maps = self.diroutput + "/maps"  #: output directory for maps
        self.diroutput_plots = self.diroutput + "/plots"  #: output directory for other plots

        for required_directory in [self.diroutput, self.diroutput_maps, self.diroutput_plots]:
            if not os.path.isdir(required_directory):
                os.mkdir(required_directory)

        self.dev = options.dev
        if options.dev:
            self.xpid = "nouveaux_guess@lafaysse"
            delattr(config, 'alternate_xpid')
            self.list_geometry = ['jur', 'mac', 'vog', 'cor', 'alp', 'pyr', 'postes']
        self.dble = options.dble
        if options.dble:
            self.xpid = "dble"
            delattr(config, 'alternate_xpid')
            self.list_geometry = ['alp', 'pyr', 'cor', 'jur', 'mac', 'vog', 'postes']
        self.reforecast = options.reforecast
        if options.reforecast:
            self.xpid = "reforecast_double2021@vernaym"
            delattr(config, 'alternate_xpid')
            self.list_geometry = ['jur4_allslopes_reforecast', 'mac11_allslopes_reforecast',
                                  'vog3_allslopes_reforecast', 'alp27_allslopes',
                                  'pyr24_allslopes', 'cor2_allslopes']


class Ensemble(object):
    """
    Describes an ensemble of simulations

    """

    def __init__(self):
        """
        """
        self.spatialdim = "Number_of_points"  #: spatial dimension
        self.ensemble = {}  #: data dict with variable names as keys and np.arrays as values

    def open(self, listmembers):
        """
        Opens simulation files

        :param listmembers: list of ensemble members (filenames)
        :type listmembers: list
        :ivar simufiles: list of prosimu objects with simulation file
        :vartype simufiles: list
        :ivar inddeterministic: index of the deterministic member
        :vartype inddeterministic: int
        :ivar nmembers:  number of members
        :vartype nmemeber: int
        :ivar ~.time: time variable from the first simulation file
        :vartype ~.time: numpy array
        :ivar indpoints: list with spatial indices
        :vartype indpoints: list
        :ivar npoints: total number of spatial points
        :vartype npoints: int
        :ivar nech: number of time steps (forecast steps)
        :vartype nech: int
        """
        print(listmembers)
        self.simufiles = []
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
        Get a list of spatial indices from the spatial dimension length in the first simulation file.

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
        if type(self.indpoints) is tuple:
            npoints = 0
            for indpoints in self.indpoints:
                npoints += len(indpoints)
        else:
            npoints = len(self.indpoints)

        return npoints

    def read(self, varname):
        """
        Read a variable and store it in :py:attr:`~.ensemble`

        :param varname: name of the variable to be read (corresponds to the variable name of the simulation NetCDF files)
        :type varname: str

        """

        self.ensemble[varname] = np.empty([self.nech, self.npoints, self.nmembers])
        for m, member in enumerate(self.simufiles):
            print("read " + varname + " for member" + str(m))
            import datetime
            before = datetime.datetime.today()

            if type(self.indpoints) is tuple:
                sections = []
                for indpoints in self.indpoints:
                    sections.append(member.read_var(varname, Number_of_points=indpoints))

                self.ensemble[varname][:, :, m] = np.concatenate(tuple(sections), axis=1)
            else:
                self.ensemble[varname][:, :, m] = member.read_var(varname, Number_of_points=self.indpoints)

            after = datetime.datetime.today()
            print(after - before)

        # Verrues (à éviter)
        if varname == 'NAT_LEV':
            self.ensemble[varname][:, :, :] = np.where(self.ensemble[varname] == 6., 0., self.ensemble[varname])

    def read_geovar(self, varname):
        """
        Read a variable from the first simulation file.

        :param varname: NetCDF variable name of the variable to read
        :type varname: str
        :return: data read
        :rtype: numpy array
        """
        if type(self.indpoints) is tuple:
            sections = []
            for indpoints in self.indpoints:
                sections.append(self.simufiles[0].read_var(varname, Number_of_points=indpoints))

            return np.concatenate(tuple(sections))
        else:
            return self.simufiles[0].read_var(varname, Number_of_points=self.indpoints)

    def probability(self, varname, seuilinf=-999999999, seuilsup=999999999):
        """
        Calculates probability as the proportion of ensemble members with values larger than :py:attr:`seuilinf`
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
        Construct filenames and plot titles from massif and altitude variables in the first simulation file.

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
            title += u" %d m" % int(alti)
        return title  # matplotlib needs unicode


class EnsembleFlatMassif(_EnsembleMassif):
    """
    Class for ensemble simulations on a massif geometry (SAFRAN like) where all data points are considered
    on flat terrain (zero slope and no orientation information).
    """

    def select_points(self):
        """
        Select spatial indices from the first simulation file where aspect=-1 (zero slope, no orientation information).

        :return: spatial indices to be read from simulation files
        :rtype: numpy boolean array
        """
        return self.simufiles[0].get_points(aspect=-1)


class EnsembleNorthSouthMassif(_EnsembleMassif):
    """
    Class for ensemble simulations on a massif geometry (SAFRAN like) where data points are considered at a
    slope of 40 degrees and two orientations: North and South.
    """

    def select_points(self):
        """
        Select spatial indices from the first simulation file where slope=40 and aspect either 0 or 180.

        :return: spatial indices to be read from simulation files
        :rtype: a tuple of numpy boolean array, where the first component corresponds to the Northern orientation
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

        return self.simufiles[0].get_points(massif_num= self.massif_num, aspect=self.aspect, slope=self.slope,
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
        Calculate quantiles and exceedance probabilities for a list of variables, quantiles and thresholds.
        The quantiles are stored in :py:attr:`~.quantiles` and the probabilities in :py:attr:`~.proba`

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
        Close simulation files and remove data from :py:attr:`~.plots.pearps2m.postprocess.Ensemble.ensemble`,
        :py:attr:`.proba` and :py:attr:`.quantiles`
        """
        super(EnsembleDiags, self).close()
        self.proba.clear()
        self.quantiles.clear()


class EnsembleOperDiags(EnsembleDiags):
    """
    Class for operationally used plots.
    """
    #: plot format
    formatplot = 'png'
    #: dict of plot attributes for each variable
    attributes = dict(
        PP_SD_1DY_ISBA=dict(convert_unit=1., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                            label=u'Epaisseur de neige fraîche en 24h (cm)'),
        SD_1DY_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 24h (cm)'),
        SD_3DY_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur de neige fraîche en 72h (cm)'),
        RAMSOND_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                          label=u'Epaisseur mobilisable (cm)'),
        NAT_LEV=dict(forcemin=-0.5, forcemax=5.5, palette='YlOrRd', ncolors=6, label=u'Risque naturel',
                     ticks=[u'Très faible', u'Faible', u'Mod. A', u'Mod. D', u'Fort', u'Très fort']),
        naturalIndex=dict(forcemin=0., forcemax=8., palette='YlOrRd', label=u'Indice de risque naturel', format='%.1f',
                          nolevel=True),
        DSN_T_ISBA=dict(convert_unit=100., label=u'Hauteur de neige (cm)'),
        WSN_T_ISBA=dict(label=u'Equivalent en eau (kg/m2)'),
        SNOMLT_ISBA=dict(convert_unit=3. * 3600., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                         label=u'Ecoulement en 3h (kg/m2/3h)'),
        WET_TH_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                         label=u'Epaisseur humide (cm)'),
        REFRZTH_ISBA=dict(convert_unit=100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                          label=u'Epaisseur regelée (cm)'),
        RAINF_ISBA=dict(convert_unit=3. * 3600., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50.,
                        label=u'Pluie en 3h (kg/m2/3h)'),
    )
    #: list of quantiles
    list_q = [20, 50, 80]

    def alldiags(self):
        """
        Calculate percentiles for all variables in :py:attr:`list_var_spag` and :py:attr:`list_var_map`.
        """
        super(EnsembleOperDiags, self).diags(set(self.list_var_spag + self.list_var_map), self.list_q, {})

    def pack_spaghettis(self, suptitle, diroutput = "."):
        """
        Produce spaghetti plots for all variables in :py:attr:`list_var_spag`.

        :param suptitle: Suptitle for all plots.
        :type suptitle: unicode string
        :param diroutput: directory to save the plots
        """

        for var in self.list_var_spag:

            if 'nolevel' not in self.attributes[var].keys():
                self.attributes[var]['nolevel'] = False

            list_filenames, list_titles = self.get_metadata(nolevel=self.attributes[var]['nolevel'])

            if hasattr(self, 'inddeterministic'):
                # print('has inddet')
                s = spaghettis_with_det(self.time)
            else:
                # print('no inddet')
                s = spaghettis(self.time)
            settings = self.attributes[var].copy()
            if 'label' in self.attributes[var].keys():
                settings['ylabel'] = self.attributes[var]['label']
            npoints = self.quantiles[var][0][0, :].shape[0]

            for point in range(0, npoints):
                if 'convert_unit' in self.attributes[var].keys():
                    allmembers = self.ensemble[var][:, point, :] * self.attributes[var]['convert_unit']
                    qmin = self.quantiles[var][0][:, point] * self.attributes[var]['convert_unit']
                    qmed = self.quantiles[var][1][:, point] * self.attributes[var]['convert_unit']
                    qmax = self.quantiles[var][2][:, point] * self.attributes[var]['convert_unit']
                else:
                    allmembers = self.ensemble[var][:, point, :]
                    qmin = self.quantiles[var][0][:, point]
                    qmed = self.quantiles[var][1][:, point]
                    qmax = self.quantiles[var][2][:, point]

                if hasattr(self, 'inddeterministic'):
                    s.draw(self.time, allmembers, qmin, qmed, qmax, deterministic=allmembers[:, self.inddeterministic],
                           **settings)
                else:
                    s.draw(self.time, allmembers, qmin, qmed, qmax, **settings)

                s.set_title(list_titles[point])
                s.set_suptitle(suptitle)
                s.addlogo()
                plotname = diroutput + "/" + var + "_" + list_filenames[point] + "." + self.formatplot
                s.save(plotname, formatout=self.formatplot)
                print(plotname + " is available.")

            s.close()

    def pack_spaghettis_multipoints(self, list_pairs, suptitle, diroutput=".", **kwargs):
        """
        Produce spaghetti plots with up to four points per plot for variables in :py:attr:`list_var_spag_2points`.

        Each point gets a different color.

        :param list_pairs: list of indices to plot together on each plot
        :type list_pairs: list of list
        :param suptitle: common suptitle for all plots
        :type suptitle: unicode str
        :param diroutput: output directory
        :param kwargs: arguments to be passed to plotting routines, notably "labels"
        """

        list_colors = ['blue', 'red', 'green', 'orange']

        for var in self.list_var_spag_2points:

            if 'nolevel' not in self.attributes[var].keys():
                self.attributes[var]['nolevel'] = False

            list_filenames, list_titles = self.get_metadata(nolevel = self.attributes[var]['nolevel'])

            if hasattr(self, 'inddeterministic'):
                s = spaghettis_with_det(self.time)
            else:
                s = spaghettis(self.time)

            settings = self.attributes[var].copy()
            if 'label' in self.attributes[var].keys():
                settings['ylabel'] = self.attributes[var]['label']

            for pair in list_pairs:
                for p, point in enumerate(pair):
                    if 'convert_unit' in self.attributes[var].keys():
                        allmembers = self.ensemble[var][:, point, :] * self.attributes[var]['convert_unit']
                        qmin = self.quantiles[var][0][:, point] * self.attributes[var]['convert_unit']
                        qmed = self.quantiles[var][1][:, point] * self.attributes[var]['convert_unit']
                        qmax = self.quantiles[var][2][:, point] * self.attributes[var]['convert_unit']
                    else:
                        allmembers = self.ensemble[var][:, point, :]
                        qmin = self.quantiles[var][0][:, point]
                        qmed = self.quantiles[var][1][:, point]
                        qmax = self.quantiles[var][2][:, point]
                    settings['colorquantiles'] = list_colors[p]
                    settings['colormembers'] = list_colors[p]
                    if 'labels' in kwargs.keys():
                        settings['commonlabel'] = kwargs['labels'][p]

                    if hasattr(self, 'inddeterministic'):
                        s.draw(self.time, allmembers, qmin, qmed, qmax,
                               deterministic=allmembers[:, self.inddeterministic], **settings)
                    else:
                        s.draw(self.time, allmembers, qmin, qmed, qmax, **settings)

                s.set_title(list_titles[point])
                s.set_suptitle(suptitle)
                s.addlogo()
                plotname = diroutput + "/" + var + "_" + list_filenames[point] + "." + self.formatplot
                s.save(plotname, formatout=self.formatplot)
                print(plotname + " is available.")

            s.close()


@echecker.disabled_if_unavailable
class EnsembleOperDiagsFlatMassif(EnsembleOperDiags, EnsembleFlatMassif):
    """
    Class for operationally plotting maps and spaghetti plots for ensembles with massif geometry and for the zero slope case.
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

    def pack_maps(self, domain, suptitle, diroutput="."):
        """
        Produce maps for the variables given in :py:attr:`list_var_map` over the regions given in :py:attr:`domain`
        for each available altitude level and time step.

        Each massif is colored corresponding to the second percentile value defined in
        :py:attr:`.list_q` and
        the color map defined in :py:attr:`.attributes`. At the center of each massif the values corresponding to
        the first three
        percentiles in :py:attr:`.list_q` are marked.

        :param domain: list of region identifiers (e.g., "alp", "pyr", "cor")
        :param suptitle: common suptitle for all plots
        :type suptitle: str
        :param diroutput: output directory to save the maps
        """

        map_generic = dict(alp=Map_alpes, pyr=Map_pyrenees, cor=Map_corse, jur=Map_jura, mac=Map_central,
                           vog=Map_vosges)

        alti = self.get_alti()
        list_alti = list(set(alti))
        m = map_generic[domain[0:3]]()
        for var in self.list_var_map:
            m.init_massifs(**self.attributes[var])
            m.addlogo()
            if 'nolevel' not in self.attributes[var].keys():
                self.attributes[var]['nolevel'] = False
            if self.attributes[var]['nolevel']:
                list_loop_alti = [0]
                massif = self.get_massifvar()
                indalti = np.ones_like(self.quantiles[var][0][0, :], dtype=bool)
            else:
                list_loop_alti = list_alti[:]
                for level in list_loop_alti:
                    if level < self.levelmin or level > self.levelmax:
                        list_loop_alti.remove(level)

                massif = self.get_massifdim()

            for level in list_loop_alti:
                if not self.attributes[var]['nolevel']:
                    indalti = alti == level

                for t in range(0, self.nech):
                    qmin = self.quantiles[var][0][t, indalti]
                    qmed = self.quantiles[var][1][t, indalti]
                    qmax = self.quantiles[var][2][t, indalti]

                    m.draw_massifs(massif[indalti], qmed, **self.attributes[var])

                    m.plot_center_massif(massif[indalti], qmin, qmed, qmax, **self.attributes[var])

                    title = "pour le " + pretty_date(self.time[t])
                    if not self.attributes[var]['nolevel']:
                        title += " - Altitude : " + str(int(level)) + "m"

                    m.set_title(title)
                    m.set_suptitle(suptitle)
                    ech = self.time[t] - self.time[0] + self.time[1] - self.time[0]
                    ech_str = '+%02d' % (ech.days * 24 + ech.seconds / 3600)
                    plotname = diroutput + "/" + domain[0:3] + "_" + var + "_" + str(int(level)) + ech_str + "." + self.formatplot
                    m.save(plotname, formatout=self.formatplot)
                    print(plotname + " is available.")
                    m.reset_massifs(rmcbar=False)
            m.reset_massifs()


@echecker.disabled_if_unavailable
class EnsembleOperDiagsNorthSouthMassif(EnsembleOperDiags, EnsembleNorthSouthMassif):
    """
    Class for operationally plot maps and spaghetti plots distinguishing between northern and southern orientation
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

    def alldiags(self):
        """
        Calculate percentiles for all variables in :py:attr:`.list_var_spag_2points` and :py:attr:`.list_var_map`.
        """
        super(EnsembleOperDiagsNorthSouthMassif, self).diags(set(self.list_var_spag_2points + self.list_var_map),
                                                             self.list_q, {})

    def get_pairs_ns(self):
        """
        Get for each massif and altitude the pair of indices corresponding to values for the northern and southern
        slopes.

        :return: indices corresponding to northern and southern slopes
        :rtype: list of lists
        """
        alti = self.get_alti()
        aspect = np.array([int(asp) for asp in self.get_aspect()])
        massif = self.get_massifdim()

        if not hasattr(self, 'list_pairs'):
            self.list_pairs = []

            for point in range(0, np.shape(alti)[0]):
                if aspect[point] == 0:
                    indsouth = np.where((alti == alti[point]) & (aspect == 180) & (massif == massif[point]))
                    if len(indsouth) == 1:
                        self.list_pairs.append([point, indsouth[0][0]])

        return self.list_pairs

    def pack_spaghettis_ns(self, suptitle, diroutput="."):
        """
        Do spaghetti plots with values for the northern and southern slope on the same plot.

        :param suptitle: common suptitle for all plots
        :param diroutput: directory to save the plots
        """

        list_pairs = self.get_pairs_ns()

        super(EnsembleOperDiagsNorthSouthMassif, self).pack_spaghettis_multipoints(list_pairs, suptitle, diroutput,
                                                                                   labels=self.versants)

    def pack_maps(self, domain, suptitle, diroutput):
        """
        Produce maps for the variables given in :py:attr:`.list_var_map` over the regions given in :py:attr:`.domain`
        for each available altitude level and time step.

        For each massif a table with background colors and values corresponding to the percentiles in
        :py:attr:`.list_q` and
        the color map defined in :py:attr:`.attributes` is plotted near the center of each massif.

        :param domain: list of region identifiers (e.g., "alp", "pyr", "cor")
        :param suptitle: common suptitle for all plots
        :type suptitle: str
        :param diroutput: output directory to save the maps
        """

        map_generic = dict(alp=Map_alpes, pyr=Map_pyrenees, cor=Map_corse, jur=Map_jura, vog=Map_vosges,
                           mac=Map_central)

        list_pairs = self.get_pairs_ns()  # pylint: disable=possibly-unused-variable

        alti = self.get_alti()
        aspect = self.get_aspect()

        list_alti = list(set(alti))

        m = map_generic[domain[0:3]]()
        for var in self.list_var_map:
            m.init_massifs(**self.attributes[var])
            m.empty_massifs()
            m.add_north_south_info()
            m.addlogo()

            list_loop_alti = list_alti[:]
            for level in list_loop_alti:
                if level < self.levelmin or level > self.levelmax:
                    list_loop_alti.remove(level)

            massif = self.get_massifdim()

            for level in list_loop_alti:

                list_indalti = []
                for plotaspect in [180, 0]:  # du bas vers le haut

                    list_indalti.append((alti == level) & (aspect == plotaspect))

                for t in range(0, self.nech):
                    list_values = []
                    for indalti in list_indalti:
                        for q, quantile in enumerate(self.list_q):  # pylint: disable=possibly-unused-variable
                            list_values.append(self.quantiles[var][q][t, indalti])
                    # print(len(massif[indalti]))
                    m.rectangle_massif(massif[indalti], list_values, ncol=2, **self.attributes[var])
                    title = "pour le " + pretty_date(self.time[t])
                    title += " - Altitude : " + str(int(level)) + "m"

                    m.set_title(title)
                    m.set_suptitle(suptitle)
                    ech = self.time[t] - self.time[0] + self.time[1] - self.time[0]
                    ech_str = '+%02d' % (ech.days * 24 + ech.seconds / 3600)
                    plotname = diroutput + "/" + domain[0:3] + "_" + var + "_" + str(int(level)) + ech_str + "." + self.formatplot
                    m.save(plotname, formatout=self.formatplot)
                    print(plotname + " is available.")
                    m.reset_massifs(rmcbar=False, rminfobox=False)
            m.reset_massifs()


class EnsembleOperDiagsStations(EnsembleOperDiags, EnsembleStation):
    """
    Class for operationally plotting spaghetti plots for station based simulations.
    """
    #: list of variables to plot maps for
    list_var_map = []
    #: list of variables to plot spaghetti plots for.
    list_var_spag = ['DSN_T_ISBA', 'WSN_T_ISBA', 'RAMSOND_ISBA', 'WET_TH_ISBA', 'REFRZTH_ISBA', 'SNOMLT_ISBA']


class EnsemblePostproc(_EnsembleMassif):
    """
    Class for ensemble post-processing.
    """

    def __init__(self, ensemble, variables, inputfiles, datebegin, dateend, decile=range(10, 100, 10), outdir='.'):
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
        :param outdir: Output directory
        :type outdir: str
        """
        print(inputfiles)
        super(EnsemblePostproc, self).__init__()
        self.ensemble = ensemble  #: ensemble data
        self.variables = variables  #: list of variables
        self.ensemble.open(inputfiles)
        #: output filename
        self.outfile = os.path.join(outdir, 'PRO_post_{0}_{1}.nc'.format(datebegin.ymdh, dateend.ymdh))
        #: variables always written to the output file
        #: ['time', 'ZS', 'aspect', 'slope', 'massif_num', 'longitude', 'latitude']
        self.standardvars = ['time', 'ZS', 'aspect', 'slope', 'massif_num', 'longitude', 'latitude']
        #: list of percentiles
        self.decile = decile

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
        Copy global attributes, dimensions and standard variables from the first simulation file to the output file.
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
                x = self.outdataset.dataset.createVariable(name, variable.datatype, variable.dimensions,
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

        Calls :py:meth:`.create_outfile`, :py:meth:`.init_outfile`, :py:meth:`.deciles` and close methods for
        all data sets.

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
        Calculates percentiles given in :py:attr:`.decile` for variables in :py:attr:`.variables` and adds them
        to the output data set including the corresponding dimension, coordinate variable and attributes.
        """
        # create decile dimension
        self.outdataset.dataset.createDimension('decile', len(self.decile))
        # create decile variable
        x = self.outdataset.dataset.createVariable('decile', 'i4', 'decile')
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
                x = self.outdataset.dataset.createVariable(name, variable.datatype, variable.dimensions + ('decile',),
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
                x = self.outdataset.dataset.createVariable(name, variable.datatype, variable.dimensions,
                                                           fill_value=fillval)
                # copy variable attributes all at once via dictionary, but without _FillValue
                attdict = self.ensemble.simufiles[0].dataset[name].__dict__
                attdict.pop('_FillValue', None)
                self.outdataset.dataset[name].setncatts(attdict)
                print(self.outdataset.dataset[name][:].size)
                print(median.size)
                self.outdataset.dataset[name][:] = median[:]

def main(c):
    """
    :param c: config
    """
    # The following class has a vortex-dependence
    # Should not import than above to avoid problems when importing the module from vortex
    from snowtools.tasks.oper.get_oper_files import S2MExtractor, FutureS2MExtractor

    os.chdir(c.diroutput)
    # if c.dev:
    #     S2ME = FutureS2MExtractor(c)
    # elif c.dble:
    #     S2ME = FutureS2MExtractor(c)
    # elif c.reforecast:
    #     S2ME = FutureS2MExtractor(c)
    # else:
    S2ME = FutureS2MExtractor(c)
    snow_members, snow_xpid = S2ME.get_snow()


    dict_chaine = defaultdict(str)
    dict_chaine['OPER'] = ' (oper)'
    dict_chaine['DBLE'] = ' (double)'
    dict_chaine['MIRR'] = ' (miroir)'
    dict_chaine['OPER@lafaysse'] = ' (dev)'
    dict_chaine['nouveaux_guess@lafaysse'] = ' (dev)'
    # undefined xpid is possible because it is allowed by defaultdict

    locale.setlocale(locale.LC_TIME, 'fr_FR.UTF-8')

    list_domains = snow_members.keys()
    print(list_domains)

    for domain in list_domains:  # ['alp_allslopes']: #

        # S2ME.conf.rundate is a Date object --> strftime already calls decode method
        suptitle = u'Prévisions PEARP-S2M du ' + pretty_date(S2ME.conf.rundate)
        # Identify the prevailing xpid in the obtained resources and adapt the title
        count = Counter(snow_xpid[domain])
        print(count)
        prevailing_xpid = count.most_common(1)[0][0]
        suffixe_suptitle = dict_chaine[prevailing_xpid]
        suptitle += suffixe_suptitle

        if domain == 'postes':
            E = EnsembleOperDiagsStations()
        else:
            E = EnsembleOperDiagsFlatMassif()
            ENS = EnsembleOperDiagsNorthSouthMassif()
            ENS.open(snow_members[domain])
        print('number of member files', len(snow_members[domain]))
        E.open(snow_members[domain])

        print("domain " + domain + " npoints = " + str(E.npoints))

        E.alldiags()

        print('Diagnostics have been computed for the following variables :')
        print(E.ensemble.keys())

        E.pack_spaghettis(suptitle, diroutput=c.diroutput_plots)
        if domain != 'postes':
            E.pack_maps(domain, suptitle, diroutput=c.diroutput_maps)

            ENS.alldiags()
            print('Diagnostics have been computed for the following variables :')
            print(ENS.ensemble.keys())
            ENS.pack_maps(domain, suptitle, diroutput=c.diroutput_maps)

            ENS.pack_spaghettis_ns(suptitle, diroutput=c.diroutput_plots)
            ENS.close()
            del ENS

            print(E.list_var_spag)
            print(E.list_var_map)

        E.close()
        del E

if __name__ == "__main__":
    c = config()
    main(c)
