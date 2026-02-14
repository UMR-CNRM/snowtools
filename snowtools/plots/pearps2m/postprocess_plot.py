#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 6 déc. 2018

@author: lafaysse

usage: python postprocess_plot.py [-b YYYYMMDD] [-e YYYYMMDD] [-o diroutput]

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

from snowtools.utils.dates import check_and_convert_date, pretty_date
from snowtools.plots.temporal.chrono import spaghettis_with_det, spaghettis
from snowtools.DATA import LUSTRE_NOSAVE_USER_DIR
from snowtools.tools.postprocess import EnsembleFlatMassif, EnsembleNorthSouthMassif
from snowtools.tools.postprocess import EnsembleStation, EnsembleDiags

from bronx.stdtypes.date import today
from bronx.syntax.externalcode import ExternalCodeImportChecker

echecker = ExternalCodeImportChecker('cartopy')
with echecker:
    import cartopy
echecker_pyproj = ExternalCodeImportChecker('pyproj')
with echecker_pyproj as echecker_register:
    import pyproj
    echecker_register.update(version=pyproj.__version__)

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


@echecker_pyproj.disabled_if_unavailable(version='2.0.0')
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

    def __init__(self):
        from snowtools.plots.maps.cartopy_massifs import Map_alpes, Map_pyrenees, Map_corse, Map_vosges, Map_jura, Map_central
        super(self, EnsembleOperDiags).__init__()

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
@echecker_pyproj.disabled_if_unavailable(version='2.0.0')
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
@echecker_pyproj.disabled_if_unavailable(version='2.0.0')
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


@echecker.disabled_if_unavailable
@echecker_pyproj.disabled_if_unavailable(version='2.0.0')
class EnsembleOperDiagsStations(EnsembleOperDiags, EnsembleStation):
    """
    Class for operationally plotting spaghetti plots for station based simulations.
    """
    #: list of variables to plot maps for
    list_var_map = []
    #: list of variables to plot spaghetti plots for.
    list_var_spag = ['DSN_T_ISBA', 'WSN_T_ISBA', 'RAMSOND_ISBA', 'WET_TH_ISBA', 'REFRZTH_ISBA', 'SNOMLT_ISBA']


@echecker.disabled_if_unavailable
@echecker_pyproj.disabled_if_unavailable(version='2.0.0')
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
