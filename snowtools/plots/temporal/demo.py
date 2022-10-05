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

commonpath = os.path.join(LUSTRE_NOSAVE_USER_DIR, "oper/alp")
# commonpath = "/home/lafaysse"
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

simuls = [commonpath + "/reanalyse_nouveau_listeo_obs_temps_reel/PRO_2020080106_2021013006.nc",
          commonpath + "/reanalyse_anciens_liste_obs_temps_reel/PRO_2020080106_2021013006.nc",]

labelsims = [u'Réanalyse, sans EDF-NIVO',
             u'Run oper, avec EDF-NIVO']



colorsims = ['green', 'orange', 'blue', 'red', 'cyan', 'black', 'gray']
colorsims = ['cyan', 'red']


linestyles = ['-', '--']

dept = {0: u"Haute-Savoie", 1: u"Savoie", 2: u"Isère", 3: u"Hautes-Alpes", 4: u"Alpes-Maritimes"}
list_massifs = {0: [1, 2, 3], 1: [4, 5, 6, 9, 10, 11], 2: [7, 8, 12, 14, 15], 3: [13, 16, 17, 18, 19],
                4: [20, 21, 22, 23]}

dept = {0: u"Haute-Tarentaise"}
list_massifs = {0: [6]}

nrows=len(list_massifs.keys())


MP = MultiPlots(nrows=nrows, ncols=1)
TSP = dict()
for i in range(0, nrows):
    TSP[i] = temporalsubplot(MP.subplots, i, 1)

# plot.addAx2(u'SWE ($kg m^{-2}$)')
# plot.addAx2(u'Epaisseur mobilisable à 2400 m (m)')

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


        selectpoint = pro.get_point(massif_num=massif, ZS=2400, aspect=-1)
        selectpoint2 = pro.get_point(massif_num=6, ZS=1800, aspect=-1)
        timeSim = pro.readtime()
        period = timeSim > Date(2020, 12, 1, 0)
        timeSim = timeSim[period]

        # epmobil = pro.read_var('RAMSOND_ISBA', Number_of_points=selectpoint)

        swe = pro.read_var('WSN_T_ISBA', Number_of_points=selectpoint)
        swe1800 = pro.read_var('WSN_T_ISBA', Number_of_points=selectpoint2)
        # risk = pro.read_var('naturalIndex', massif=6)

        # epmobil = epmobil[period]
        swe = swe[period]
        swe1800 = swe1800[period]
        # risk = risk[period]

        print("Massif:")
        print (massif)



        # plot.add_line(timeSim, risk, label=labelsims[s], color=colorsims[s])
        #if s==0:
        #    TSP[i].add_line(timeSim, swe, label= I.getMassifName(massif), color=colorsims[index], linestyle=linestyles[s])
        #else:
        #    TSP[i].add_line(timeSim, swe, color=colorsims[index], linestyle=linestyles[s])

        TSP[i].add_line(timeSim, swe, label= labelsims[s] + u" 2400 m", color=colorsims[s], linestyle='-')


        TSP[i].add_line(timeSim, swe1800, label=labelsims[s] + u" 1800 m", color=colorsims[s], linestyle='dotted')
        #  plot.addVarAx2(timeSim, epmobil, label='NEW', color=colorsims[s], linestyle="--")

        # if s == len(simuls) - 2:
        #     nextinitswe = swe[timeSim == Date(2021, 1, 28, 9)] - swe[0]
        #
        # if s == len(simuls) - 1:
        #     initswe = nextinitswe
        # else:
        #     initswe = 0

        # plot.addVarAx2(timeSim, swe - swe[0] + initswe, label='NEW', color=colorsims[s], linestyle="--")

        # plot.set_yaxis(ylabel="IRNAM", forcemin=0, forcemax=8)

        TSP[i].set_yaxis(ylabel=u"SWE ($kg \quad m^{-2}$)")

for i in range(0, nrows):
    TSP[i].set_title(dept[i])
    TSP[i].finalize(timeSim, fontsize='xx-small')

MP.save(commonpath + "/reanalyse_nouveaux_listeo/plot.pdf", formatout="pdf")
