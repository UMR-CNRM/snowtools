#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 6 déc. 2018

@author: lafaysse
'''

import locale

import os
import sys
from optparse import OptionParser
import numpy as np

import matplotlib
matplotlib.use('Agg')

from bronx.stdtypes.date import Date, today
from tasks.oper.get_oper_files import S2MExtractor
from utils.prosimu import prosimu
from utils.dates import check_and_convert_date, pretty_date
from plots.temporal.chrono import spaghettis_with_det
from plots.maps.basemap import Map_alpes, Map_pyrenees, Map_corse
from utils.infomassifs import infomassifs

usage = "usage: python postprocess.py [-b YYYYMMDD] [-e YYYYMMDD] [-o diroutput]"


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("-b",
                      action="store", type="string", dest="datebegin", default=today().ymd,
                      help="First year of extraction")

    parser.add_option("-e",
                      action="store", type="string", dest="dateend", default=today().ymd,
                      help="Last year of extraction")

    parser.add_option("-o",
                      action="store", type="string", dest="diroutput", default="/cnrm/cen/users/NO_SAVE/lafaysse/PEARPS2M",
                      help="Output directory")

    (options, args) = parser.parse_args(arguments)  # @UnusedVariable

    return options


class config(object):
    previ = True  # False for analysis, True for forecast
    xpid = "OPER@lafaysse"  # To be changed with IGA account when operational
    list_geometry = ['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']
#     list_geometry = ['pyr_allslopes']

    list_members = range(0, 36)  # 35 for determinstic member, 36 for sytron, 0-34 for PEARP members

    def __init__(self):
        options = parse_options(sys.argv)
        options.datebegin, options.dateend = map(check_and_convert_date, [options.datebegin, options.dateend])
        if options.datebegin.hour == 0:
            self.rundate = options.datebegin.replace(hour=6)
        else:
            self.rundate = options.datebegin
        self.diroutput = options.diroutput + "/" + self.rundate.strftime("%Y%m%d%H")
        self.diroutput_maps = self.diroutput + "/maps"
        self.diroutput_plots = self.diroutput + "/plots"

        for required_directory in [self.diroutput, self.diroutput_maps, self.diroutput_plots]:
            if not os.path.isdir(required_directory):
                os.mkdir(required_directory)


class Ensemble(object):

    spatialdim = "Number_of_points"
    ensemble = {}

    def open(self, listmembers):
        self.simufiles = []
        for m, member in enumerate(listmembers):
            self.simufiles.append(prosimu(member))
            if 'mb035' in member:
                self.inddeterministic = m

        self.nmembers = len(self.simufiles)
        self.time = self.simufiles[0].readtime()

        self.indpoints = self.select_points()
        self.npoints = self.get_npoints()
        self.nech = len(self.time)

    def select_points(self):
        indpoints = range(0, self.simufiles[0].getlendim(self.spatialdim))
        return indpoints

    def get_npoints(self):
        if type(self.indpoints) is tuple:
            npoints = 0
            for indpoints in self.indpoints:
                npoints += len(indpoints)
        else:
            npoints = len(self.indpoints)

        return npoints

    def read(self, varname):

        self.ensemble[varname] = np.empty([self.nech, self.npoints, self.nmembers])
        for m, member in enumerate(self.simufiles):
            print ("read " + varname + " for member" + str(m))
            import datetime
            before = datetime.datetime.today()

            if type(self.indpoints) is tuple:
                sections = []
                for indpoints in self.indpoints:
                    sections.append(member.read_var(varname, Number_of_points = indpoints))

                self.ensemble[varname][:, :, m] = np.concatenate(tuple(sections), axis=1)
            else:
                self.ensemble[varname][:, :, m] = member.read_var(varname, Number_of_points = self.indpoints)

            after = datetime.datetime.today()
            print(after - before)

        # Verrues (à éviter)
        if varname == 'NAT_LEV':
            self.ensemble[varname][:, :, :] = np.where(self.ensemble[varname] == 6., 0., self.ensemble[varname])

    def read_geovar(self, varname):
        if type(self.indpoints) is tuple:
            sections = []
            for indpoints in self.indpoints:
                sections.append(self.simufiles[0].read_var(varname, Number_of_points = indpoints))

            return np.concatenate(tuple(sections))
        else:
            return self.simufiles[0].read_var(varname, Number_of_points = self.indpoints)

    def proba(self, varname, seuilinf=-999999999, seuilsup=999999999):

        if varname not in self.ensemble.keys():
            self.read(varname)

        condition = (self.ensemble[varname] > seuilinf) & (self.ensemble[varname] < seuilsup)
        probability = np.sum(condition, axis=2) / (1. * self.nmembers)

        return np.where(np.isnan(self.ensemble[varname][:, :, 0]), np.nan, probability)  # On renvoit des nan quand ensemble n'est pas défini

    def quantile(self, varname, level):

        if varname not in self.ensemble.keys():
            self.read(varname)

        quantile = np.where(np.isnan(self.ensemble[varname][:, :, 0]), np.nan, np.percentile(self.ensemble[varname], level, axis=2))
        return quantile

    def close(self):
        for member in self.simufiles:
            member.close()
        self.ensemble.clear()

    def get_metadata(self):
        indpoints = self.select_points()
        return indpoints, indpoints


class _EnsembleMassif(Ensemble):

    InfoMassifs = infomassifs()

    def read(self, varname):
        if varname == 'naturalIndex':
            nmassifs = len(self.get_massifvar())
            self.ensemble[varname] = np.empty([self.nech, nmassifs, self.nmembers])
            for m, member in enumerate(self.simufiles):
                self.ensemble[varname][:, :, m] = member.read_var(varname)
        else:
            super(_EnsembleMassif, self).read(varname)

    def get_alti(self):
        if not hasattr(self, "alti"):
            self.alti = self.read_geovar("ZS")
        return self.alti

    def get_aspect(self):
        if not hasattr(self, "aspect"):
            self.aspect = self.read_geovar("aspect")
        return self.aspect

    def get_massifdim(self):
        if not hasattr(self, "massifdim"):
            self.massifdim = self.read_geovar("massif_num")
        return self.massifdim

    def get_massifvar(self):
        if not hasattr(self, "massifvar"):
            self.massifvar = self.simufiles[0].read_var("massif")
        return self.massifvar

    def get_metadata(self, nolevel=False):

        if nolevel:
            massif = self.get_massifvar()
            alti = [None] * len(massif)
        else:
            alti = self.get_alti()
            massif = self.get_massifdim()

        return map(self.build_filename, massif, alti), map(self.build_title, massif, alti)

    def build_filename(self, massif, alti):
        filename = str(massif)
        if alti:
            filename += "_" + str(int(alti))
        return filename

    def build_title(self, massif, alti):
        title = unicode(self.InfoMassifs.getMassifName(massif).decode("utf-8"))
        if alti:
            title += u" " + unicode(int(alti)) + u" m"
        return title


class EnsembleFlatMassif(_EnsembleMassif):

    def select_points(self):
        return self.simufiles[0].get_points(aspect = -1)


class EnsembleNorthSouthMassif(_EnsembleMassif):

    def select_points(self):
        # return np.sort(np.concatenate((self.simufiles[0].get_points(aspect = 0, slope = 40), self.simufiles[0].get_points(aspect = 180, slope = 40))))
        # TAKE CARE : It is extremely more efficient to read regular sections of the netcdf files
        return (self.simufiles[0].get_points(aspect = 0, slope = 40), self.simufiles[0].get_points(aspect = 180, slope = 40))


class EnsembleStation(Ensemble):

    InfoMassifs = infomassifs()

    def get_metadata(self, **kwargs):
        alti = self.simufiles[0].read_var("ZS", Number_of_points = self.indpoints)
        station = self.simufiles[0].read_var("station", Number_of_points = self.indpoints)

        return map(self.build_filename, station, alti), map(self.build_title, station, alti)

    def build_filename(self, station, alti):
        return '%08d' % station

    def build_title(self, station, alti):
        return unicode(self.InfoMassifs.nameposte(station).decode("utf-8")) + u" " + unicode(int(alti)) + u" m"


class EnsembleDiags(Ensemble):

    proba = {}
    quantiles = {}

    def diags(self, list_var, list_quantiles, list_seuils):
        for var in list_var:
            if var in list_seuils.keys():
                for seuil in list_seuils[var]:
                    self.proba[(var, seuil)] = self.proba(var, seuilsup=seuil)

        for var in list_var:
            self.quantiles[var] = []
            for quantile in list_quantiles:
                print ("Compute quantile " + str(quantile) + " for variable " + var)
                self.quantiles[var].append(self.quantile(var, quantile))

    def close(self):
        super(EnsembleDiags, self).close()
        self.proba.clear()
        self.quantiles.clear()


class EnsembleOperDiags(EnsembleDiags):
    formatplot = 'png'

    attributes = dict(
        SD_1DY_ISBA = dict(convert_unit= 100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50., label=u'Epaisseur de neige fraîche en 24h (cm)'),
        SD_3DY_ISBA = dict(convert_unit= 100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50., label=u'Epaisseur de neige fraîche en 72h (cm)'),
        RAMSOND_ISBA = dict(convert_unit= 100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50., label=u'Epaisseur mobilisable (cm)'),
        NAT_LEV = dict(forcemin=-0.5, forcemax=5.5, palette='YlOrRd', ncolors=6, label=u'Risque naturel', ticks=[u'Très faible', u'Faible', u'Mod. A', u'Mod. D', u'Fort', u'Très fort']),
        naturalIndex = dict(forcemin=0., forcemax=8., palette='YlOrRd', label=u'Indice de risque naturel', format= '%.1f', nolevel=True),
        DSN_T_ISBA  = dict(convert_unit= 100., label=u'Hauteur de neige (cm)'),
        WSN_T_ISBA  = dict(label=u'Equivalent en eau (kg/m2)'),
        SNOMLT_ISBA  = dict(convert_unit= 3. * 3600., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50., label=u'Ecoulement en 3h (kg/m2/3h)'),
        WET_TH_ISBA  = dict(convert_unit= 100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50., label=u'Epaisseur humide (cm)'),
        REFRZTH_ISBA  = dict(convert_unit= 100., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50., label=u'Epaisseur regelée (cm)'),
        RAINF_ISBA   = dict(convert_unit= 3. * 3600., forcemin=0., forcemax=60., palette='YlGnBu', seuiltext=50., label=u'Pluie en 3h (kg/m2/3h)'),
    )

    list_q = [20, 50, 80]

    def alldiags(self):
        super(EnsembleOperDiags, self).diags(set(self.list_var_spag + self.list_var_map), self.list_q, {})

    def pack_spaghettis(self, suptitle, diroutput = "."):

        for var in self.list_var_spag:

            if 'nolevel' not in self.attributes[var].keys():
                self.attributes[var]['nolevel'] = False

            list_filenames, list_titles = self.get_metadata(nolevel = self.attributes[var]['nolevel'])

            s = spaghettis_with_det(self.time)
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

                s.draw(self.time, allmembers[:, self.inddeterministic], allmembers, qmin, qmed, qmax, **settings)
                s.set_title(list_titles[point])
                s.set_suptitle(suptitle)
                s.addlogo()
                plotname = diroutput + "/" + var + "_" + list_filenames[point] + "." + self.formatplot
                s.save(plotname, formatout=self.formatplot)
                print (plotname + " is available.")

            s.close()

    def pack_spaghettis_multipoints(self, list_pairs, suptitle, diroutput=".", **kwargs):

        list_colors = ['blue', 'red', 'green', 'orange']

        for var in self.list_var_spag_2points:

            if 'nolevel' not in self.attributes[var].keys():
                self.attributes[var]['nolevel'] = False

            list_filenames, list_titles = self.get_metadata(nolevel = self.attributes[var]['nolevel'])

            s = spaghettis_with_det(self.time)
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

                    s.draw(self.time, allmembers[:, self.inddeterministic], allmembers, qmin, qmed, qmax, **settings)
                s.set_title(list_titles[point])
                s.set_suptitle(suptitle)
                s.addlogo()
                plotname = diroutput + "/" + var + "_" + list_filenames[point] + "." + self.formatplot
                s.save(plotname, formatout=self.formatplot)
                print (plotname + " is available.")

            s.close()


class EnsembleOperDiagsFlatMassif(EnsembleOperDiags, EnsembleFlatMassif):

    levelmax = 4800
    levelmin = 0

    list_var_map = 'naturalIndex', 'SD_1DY_ISBA', 'SD_3DY_ISBA', 'SNOMLT_ISBA' 
    list_var_spag = 'naturalIndex', 'DSN_T_ISBA', 'WSN_T_ISBA', 'SNOMLT_ISBA'


    def pack_maps(self, domain, suptitle, diroutput = "."):

        map_generic = dict(alp = Map_alpes, pyr = Map_pyrenees, cor = Map_corse)

        alti = self.get_alti()
        list_alti = list(set(alti))

        m = map_generic[domain[0:3]]()
        for var in self.list_var_map:
            m.init_massifs(**self.attributes[var])
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

                    title = "pour le " + pretty_date(self.time[t]).decode('utf-8')
                    if not self.attributes[var]['nolevel']:
                        title += " - Altitude : " + str(int(level)) + "m"

                    m.set_title(title)
                    m.set_suptitle(suptitle)
                    m.addlogo()
                    ech = self.time[t] - self.time[0] + self.time[1] - self.time[0]
                    ech_str = '+%02d' % (ech.days * 24 + ech.seconds / 3600)
                    plotname = diroutput + "/" + domain[0:3] + "_" + var + "_" + str(int(level)) + ech_str + "." + self.formatplot
                    m.save(plotname, formatout=self.formatplot)
                    print (plotname + " is available.")
            m.reset_massifs()


class EnsembleOperDiagsNorthSouthMassif(EnsembleOperDiags, EnsembleNorthSouthMassif):

    levelmax = 4800
    levelmin = 0
    versants = [u'Nord 40°', u'Sud 40°']
    list_var_spag = []
    list_var_spag_2points = ['RAMSOND_ISBA', 'NAT_LEV', 'WET_TH_ISBA', 'REFRZTH_ISBA']
    list_var_map = ['RAMSOND_ISBA', 'NAT_LEV', 'WET_TH_ISBA', 'REFRZTH_ISBA']
    ensemble = {}

    def alldiags(self):
        super(EnsembleOperDiagsNorthSouthMassif, self).diags(set(self.list_var_spag_2points + self.list_var_map), self.list_q, {})

    def get_pairs_ns(self):
        alti = self.get_alti()
        aspect = np.array(map(int, self.get_aspect()))
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

        list_pairs = self.get_pairs_ns()

        return self.pack_spaghettis_multipoints(list_pairs, suptitle, diroutput, labels=self.versants)

    def pack_maps(self, domain, suptitle, diroutput):

        map_generic = dict(alp = Map_alpes, pyr = Map_pyrenees, cor = Map_corse)

        list_pairs = self.get_pairs_ns()

        alti = self.get_alti()
        aspect = self.get_aspect()

        list_alti = list(set(alti))

        m = map_generic[domain[0:3]]()
        for var in self.list_var_map:
            m.init_massifs(**self.attributes[var])
            m.empty_massifs()
            m.add_north_south_info()

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
                        for q, quantile in enumerate(self.list_q):
                            list_values.append(self.quantiles[var][q][t, indalti])

                    m.rectangle_massif(massif[indalti], self.list_q, list_values, ncol=2, **self.attributes[var])

                    title = "pour le " + pretty_date(self.time[t]).decode('utf-8')
                    title += " - Altitude : " + str(int(level)) + "m"

                    m.set_title(title)
                    m.set_suptitle(suptitle)
                    m.addlogo()
                    ech = self.time[t] - self.time[0] + self.time[1] - self.time[0]
                    ech_str = '+%02d' % (ech.days * 24 + ech.seconds / 3600)
                    plotname = diroutput + "/" + domain[0:3] + "_" + var + "_" + str(int(level)) + ech_str + "." + self.formatplot
                    m.save(plotname, formatout=self.formatplot)
                    print (plotname + " is available.")
            m.reset_massifs()


class EnsembleOperDiagsStations(EnsembleOperDiags, EnsembleStation):
    list_var_map = []
    list_var_spag = ['DSN_T_ISBA', 'WSN_T_ISBA', 'RAMSOND_ISBA', 'WET_TH_ISBA', 'REFRZTH_ISBA', 'SNOMLT_ISBA']


if __name__ == "__main__":

    c = config()
    os.chdir(c.diroutput)
    S2ME = S2MExtractor(c)
    snow_members = S2ME.get_snow()

    locale.setlocale(locale.LC_TIME, 'fr_FR.UTF-8')
    suptitle = u'Prévisions PEARP-S2M du ' + pretty_date(S2ME.conf.rundate).decode('utf-8')

    list_domains = snow_members.keys()

    for domain in list_domains:
        if domain == 'postes':
            E = EnsembleOperDiagsStations()
        else:
            E = EnsembleOperDiagsFlatMassif()
            ENS = EnsembleOperDiagsNorthSouthMassif()
            ENS.open(snow_members[domain])

        E.open(snow_members[domain])

        print ("domain " + domain + " npoints = " + str(E.npoints))

        E.alldiags()

        print ('Diagnostics have been computed for the following variables :')
        print (E.ensemble.keys())

        E.pack_spaghettis(suptitle, diroutput = c.diroutput_plots)
        if domain != 'postes':
            E.pack_maps(domain, suptitle, diroutput = c.diroutput_maps)

            ENS.alldiags()
            print ('Diagnostics have been computed for the following variables :')
            print (ENS.ensemble.keys())
            ENS.pack_maps(domain, suptitle, diroutput = c.diroutput_maps)

            ENS.pack_spaghettis_ns(suptitle, diroutput = c.diroutput_plots)
            ENS.close()
            del ENS

            print E.list_var_spag
            print E.list_var_map

        E.close()
        del E
