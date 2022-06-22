#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 25 mar 2020

@author: lafaysse
'''

import matplotlib

from snowtools.utils.prosimu import prosimu
from snowtools.utils.infomassifs import infomassifs
from snowtools.plots.temporal.chrono import temporalsubplot
from snowtools.plots.abstracts.figures import MultiPlots
from bronx.stdtypes.date import Date
from snowtools.DATA import LUSTRE_NOSAVE_USER_DIR
import os

matplotlib.use('Agg')

commonpath = os.path.join(LUSTRE_NOSAVE_USER_DIR, "oper/alp")# commonpath = "/home/lafaysse"
# simuls = [commonpath + "/reanalyse_nouveau_listeo_obs_temps_reel/PRO_2020080106_2021013006.nc",
#           commonpath + "/reanalyse_nouveaux_listeo/PRO_2020080106_2021013006.nc",
#           commonpath + "/reanalyse_nouveaux_listeo_AVEC_EDFNIVO/PRO_2020080106_2021013006.nc",
#           commonpath + "/reanalyse_anciens_listeo/PRO_2020080106_2021013006.nc",
#           commonpath + "/reanalyse_anciens_liste_obs_temps_reel/PRO_2020080106_2021013006.nc",
#           commonpath + "/oper/PRO_2021011406_2021013006.nc",
#           commonpath + "/2021012909/mb035/PRO_2021012806_2021012906.nc"]
#
# labelsims = [u'Nouveaux listings obs temps réel', u'Nouveaux listings sans EDF-NIVO',
#              u'Nouveaux listings avec EDF-NIVO', u'Anciens listings',
#              u'Anciens listings, obs temps réel', u'Oper 03TU', u'Oper 09TU']
# colorsims = ['green', 'cyan', 'blue', 'red', 'orange', 'black', 'gray']

simuls = [commonpath + "/oper/PRO_2020112606_2021042106.nc",
          commonpath + "/2021042212/PRO_2020080106_2021041806.nc"]

labelsims = [u'Ana oper ', u'Réa mens ']



colorsims = ['green', 'orange', 'blue', 'red', 'cyan', 'black', 'gray']


linestyles = ['-', '--']

dept = {0: u"Haute-Savoie", 1: u"Savoie", 2: u"Isère", 3: u"Hautes-Alpes", 4: u"Alpes-Maritimes"}
list_massifs = {0: [1, 2, 3], 1: [4, 5, 6, 9, 10, 11], 2: [7, 8, 12, 14, 15], 3: [13, 16, 17, 18, 19],
                4: [20, 21, 22, 23]}

#dept = {0: u"Haute-Tarentaise"}
#list_massifs = {0: [6]}

nrows=len(list_massifs.keys())

list_alti = [1800, 2700]
ncols = len(list_alti)

MP = MultiPlots(nrows=nrows, ncols=ncols)
TSP = dict()

for i in range(0, nrows):
    for j in range(0, ncols):
        TSP[(i,j)] = temporalsubplot(MP.subplots, i, j)

I = infomassifs()


for s, simul in enumerate(simuls):
    pro = prosimu(simul)

    for massif in I.getListMassif_of_region('alp'):

        i = -999
        for key in list_massifs.keys():
            print (key, massif in list_massifs[key])
            if massif in list_massifs[key]:
                i = key
                index = list_massifs[key].index(massif)
                break
        if i == -999:
            continue

        print (i, index)

        for j, alti in enumerate(list_alti):

            try:
                selectpoint = pro.get_point(massif_num=massif, ZS=alti, aspect=-1)
            except IndexError:
                continue

            timeSim = pro.readtime()
            period = timeSim > Date(2020, 12, 1, 0)
            timeSim = timeSim[period]

            swe = pro.read_var('WSN_T_ISBA', Number_of_points=selectpoint)
            swe = swe[period]

            if s==0:
                label = I.getMassifName(massif)
            else:
                label = ""

            TSP[(i, j)].add_line(timeSim, swe, label= label, color=colorsims[index], linestyle=linestyles[s])


            TSP[(i, j)].set_yaxis(ylabel=u"SWE ($kg \quad m^{-2}$)")

for i in range(0, nrows):
    for j, alti in enumerate(list_alti):
        TSP[(i, j)].set_title(dept[i] + " - " + str(alti) + "m")
        TSP[(i, j)].finalize(timeSim, fontsize='xx-small')

MP.save(commonpath + "/oper/impact_comod5324.pdf", formatout="pdf")
