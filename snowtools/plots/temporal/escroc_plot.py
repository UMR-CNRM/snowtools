#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 4 déc. 2018

@author: lafaysse
"""

import datetime

import numpy as np
import os

from snowtools.plots.temporal.chrono import prettyensemble
from snowtools.plots.pearps2m.postprocess import EnsembleDiags
from snowtools.utils.prosimu import prosimu
from snowtools.DATA import LUSTRE_NOSAVE_DIR

class EnsembleEscrocDiags(EnsembleDiags):

    def __init__(self):
        self.formatplot = 'png'

        self.list_var_spag = 'DSN_T_ISBA', 'WSN_T_ISBA'

        self.attributes = dict(
            DSN_T_ISBA=dict(convert_unit=100., label=u'Snow depth (cm)'),
            gap=dict(convert_unit=100., label=u'Snow depth (cm)'),
            WSN_T_ISBA=dict(label=u'Snow water equivalent (kg/m2)'),
        )

        self.list_q = [10, 50, 90]

        super(EnsembleEscrocDiags, self).__init__()

    def alldiags(self):
        super(EnsembleEscrocDiags, self).diags(set(self.list_var_spag), self.list_q, {})

    def getobs(self, obsfile, varname):
        ESMSnowMIP_dicvarnames = dict(DSN_T_ISBA="snd_can_auto", WSN_T_ISBA="snw_man", gap="snd_gap_auto")

        print("open obs")
        dataObs = prosimu(obsfile)
        timeObs = dataObs.readtime()
        varObs = dataObs.read(ESMSnowMIP_dicvarnames[varname])

        if 'convert_unit' in self.attributes[varname].keys():
            varObs = varObs * self.attributes[varname]['convert_unit']

        dataObs.close()
        print("close obs")

        return timeObs, varObs

    def getsnouf(self, dirsnouf, varname):

        # Routine dégueu parce qu'il faut mettre au propre les données SNOUF qui sont dans des formats crado

        SNOUF_col2017 = dict(DSN_T_ISBA=7)
        SNOUF_col2016 = dict(DSN_T_ISBA=1)

        obsfile = dirsnouf + "/2017-2018/CR3000_SNOUF_2017-2018_corrYves_foret.csv"
        str2date = lambda x: datetime.datetime.strptime(x.decode("utf-8"), '%m/%d/%Y %H:%M')
        timeObs2017 = np.genfromtxt(obsfile, delimiter=",", skip_header=1, usecols=(0,), converters = {0: str2date})
        varObs2017 = np.genfromtxt(obsfile, delimiter=",", skip_header=1, usecols=(SNOUF_col2017[varname],))

        obsfile = dirsnouf + "/2016-2017/Obs_HTN_1h.csv"
        str2date = lambda x: datetime.datetime.strptime(x.decode("utf-8"), '%Y-%m-%d %H:%M:%S')
        timeObs2016 = np.genfromtxt(obsfile, delimiter="\t", skip_header=1,usecols=(0,), converters = {0: str2date})

        varObs2016 = 2.07 - np.genfromtxt(obsfile, delimiter="\t", skip_header=1,usecols=(1,), converters = {0: str2date})
        varObs2016 = np.where(varObs2016 < 0, 0 , varObs2016) * 100.

        indfoireux = (timeObs2016 > datetime.datetime(2017,4,30,10)) | ((timeObs2016 < datetime.datetime(2017,4,28, 0)) & (timeObs2016 > datetime.datetime(2017,3 ,14, 0)))

        varObs2016[indfoireux] = 0

        timeObs = np.concatenate((timeObs2016, timeObs2017))
        varObs = np.concatenate((varObs2016, varObs2017))

        return timeObs, varObs

    def getsnoufprairie(self, dirsnouf, varname):

        SNOUF_col2017 = dict(gap=3)

        obsfile = dirsnouf + "/CR3000_SNOUF_2017-2018_corrYves_prairie.csv"
        str2date = lambda x: datetime.datetime.strptime(x.decode("utf-8"), '%m/%d/%Y %H:%M')
        timeObs2017 = np.genfromtxt(obsfile, delimiter=",", skip_header=1, usecols=(0,), converters = {0: str2date})
        varObs2017 = np.genfromtxt(obsfile, delimiter=",", skip_header=1, usecols=(SNOUF_col2017[varname],))

        ESMSnowMIP_dicvarnames = dict(DSN_T_ISBA="snow_depth_auto", WSN_T_ISBA="swe_auto", gap="snow_depth_auto")

        obsfile = dirsnouf + "/CDP_qot_20142017.nc"

        print("open obs")
        dataObs = prosimu(obsfile)
        timeObs2016 = dataObs.readtime()
        varObs2016 = dataObs.read(ESMSnowMIP_dicvarnames[varname])

        if 'convert_unit' in self.attributes[varname].keys():
            varObs2016 = varObs2016 * self.attributes[varname]['convert_unit']

        dataObs.close()
        print("close obs")

        timeObs = np.concatenate((timeObs2016, timeObs2017))
        varObs = np.concatenate((varObs2016, varObs2017))

        return timeObs, varObs

    def pack_pretty(self, secondensemble, diroutput = ".", filename = "spaghettis", obsfile=None):

        for var in self.list_var_spag:

            s = prettyensemble(self.time)

            settings = self.attributes[var].copy()
            if 'label' in self.attributes[var].keys():
                settings['ylabel'] = self.attributes[var]['label']
            npoints = self.quantiles[var][0][0, :].shape[0]

            for point in range(0, npoints):
                if 'convert_unit' in self.attributes[var].keys():
                    qmin = self.quantiles[var][0][:, point] * self.attributes[var]['convert_unit']
                    qmed = self.quantiles[var][1][:, point] * self.attributes[var]['convert_unit']
                    qmax = self.quantiles[var][2][:, point] * self.attributes[var]['convert_unit']
                else:
                    qmin = self.quantiles[var][0][:, point]
                    qmed = self.quantiles[var][1][:, point]
                    qmax = self.quantiles[var][2][:, point]


                if 'convert_unit' in secondensemble.attributes[var].keys():
                    qmin2 = secondensemble.quantiles[var][0][:, point] * secondensemble.attributes[var]['convert_unit']
                    qmed2 = secondensemble.quantiles[var][1][:, point] * secondensemble.attributes[var]['convert_unit']
                    qmax2 = secondensemble.quantiles[var][2][:, point] * secondensemble.attributes[var]['convert_unit']
                else:
                    qmin2 = secondensemble.quantiles[var][0][:, point]
                    qmed2 = secondensemble.quantiles[var][1][:, point]
                    qmax2 = secondensemble.quantiles[var][2][:, point]

                settings["colorquantiles"] = "blue"
                settings["linewidth"] = 1
                settings["alpha"] = 0.3
                settings["commonlabel"] = "Opened area"

                s.draw(secondensemble.time, qmin2, qmed2, qmax2, **settings)

                settings["colorquantiles"] = "green"
                settings["commonlabel"] = "Forest"
                settings["alpha"] = 0.6
                settings["linewidth"] = 2

                s.draw(self.time, qmin, qmed, qmax, **settings)

#                 indtime = (self.time >= datetime.datetime(2010, 1, 1)) & ( self.time <= datetime.datetime(2010, 1, 2))
#
#                 print qmed[indtime]
#                 print qmed2[indtime]
#                 import sys
#                 sys.exit(1)

                if obsfile:
                    if 'snouf' in obsfile:
                        if var == 'DSN_T_ISBA':
                            timeObs, varObs = self.getsnouf(obsfile, var)
                            timeGAP, varGAP = self.getsnoufprairie(obsfile, 'gap')
                            s.add_line(timeGAP, varGAP, label="Observations in meadow", linewidth=2)
                            s.add_line(timeObs, varObs, label="Observations below canopy", linewidth=2, color='black')
                    else:
                        timeObs, varObs = self.getobs(obsfile, var)

                        if var == 'WSN_T_ISBA':
                            s.add_points(timeObs, varObs, label="Observations")
                        elif var == 'DSN_T_ISBA':
                            timeObs, varGAP = self.getobs(obsfile, 'gap')
                            s.add_line(timeObs, varGAP, label="Observations in canopy gap", linewidth=2)
                            s.add_line(timeObs, varObs, label="Observations below canopy", linewidth=2, color='black')

#                 s.plot.set_xlim([datetime.datetime(2009, 10, 1), datetime.datetime(2010, 6, 1)])

                    s.finalize(secondensemble.time)

                plotname = diroutput + "/" + var + "_" + filename + "." + self.formatplot
                s.save(plotname, formatout=self.formatplot)
                print(plotname + " is available.")

            s.close()


if __name__ == "__main__":

    from snowtools.scripts.extract.vortex.get_escroc import S2MExtractor, config
    from snowtools.scripts.ESMSnowMIP.ESM_snowmip import bdate, edate

    c = config()
    S2ME = S2MExtractor(c)
    snow_members = S2ME.get()

    suptitle = ''

    list_domains = snow_members.keys()

    for domain in list_domains:

        list_ensembles = snow_members[domain].keys()

        if domain == 'cdp':
            obsfile = os.path.join(LUSTRE_NOSAVE_DIR, 'lafaysse/ESM-SnowMIP/evaldata/snouf')
        else:
            obsfile = os.path.join(LUSTRE_NOSAVE_DIR,
                                   'lafaysse/ESM-SnowMIP/evaldata/obs_insitu_' + domain + "_" +
                                   bdate[domain][0:4] + "_" + edate[domain][0:4] + ".nc")

        E = dict()

        for e, ensemble in enumerate(list_ensembles):

            E[ensemble] = EnsembleEscrocDiags()

            print("on domain " + domain + ", open ensemble " + ensemble)
            print(snow_members[domain][ensemble])
            E[ensemble].open(snow_members[domain][ensemble])

    #         print ("domain " + domain + " npoints = " + str(E.npoints))

            print(E[ensemble].ensemble.keys())

            E[ensemble].alldiags()

            print('Diagnostics have been computed for the following variables :')
#             print (E.ensemble.keys())

        if domain == 'cdp':
            # j'ai inversé E2 et E2open au CDP comme c'est un site dégagé dans snowmip
            E["E2open"].pack_pretty(E["E2"], diroutput=".", filename=domain, obsfile=obsfile)
        else:
            E["E2"].pack_pretty(E["E2open"], diroutput=".", filename=domain, obsfile=obsfile)

        for key, value in E.items():
            value.close()

        del E
