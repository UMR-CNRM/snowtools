#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 25 mar 2020

@author: lafaysse
'''

import matplotlib
matplotlib.use('Agg')

import six

import numpy as np
from utils.prosimu import prosimu
from plots.temporal.chrono import temporalplot2Axes
from bronx.stdtypes.date import Date

commonpath = "/cnrm/cen/users/NO_SAVE/lafaysse/oper/alp"
# commonpath = "/home/lafaysse"
simuls = [commonpath + "/reanalyse_nouveaux_listeo/PRO_2020080106_2021013006.nc",
          commonpath + "/reanalyse_nouveaux_listeo_AVEC_EDFNIVO/PRO_2020080106_2021013006.nc",
          commonpath + "/reanalyse_anciens_listeo/PRO_2020080106_2021013006.nc",
          commonpath + "/oper/PRO_2021011406_2021013006.nc"]

labelsims = ["Nouveaux listings sans EDF-NIVO", "Nouveaux listings avec EDF-NIVO", "Anciens listings avec EDF-NIVO", "Oper"]
colorsims = ["green", "blue", "red", 'black']

plot = temporalplot2Axes()
plot.addAx2(u'SWE ($kg m^{-2}$)')
# plot.addAx2(u'Epaisseur mobilisable à 2400 m (m)')

for s, simul in enumerate(simuls):
    pro = prosimu(simul)

    selectpoint = pro.get_point(massif_num=6, ZS=2400, aspect=-1)
    timeSim = pro.readtime()
    period = timeSim > Date(2021, 1, 27, 0)
    timeSim = timeSim[period]

    epmobil = pro.read_var('RAMSOND_ISBA', Number_of_points=selectpoint)
    swe = pro.read_var('WSN_T_ISBA', Number_of_points=selectpoint)
    risk = pro.read_var('naturalIndex', massif=6)

    epmobil = epmobil[period]
    swe = swe[period]
    risk = risk[period]

    plot.add_line(timeSim, risk, label=labelsims[s], color=colorsims[s])
    #  plot.addVarAx2(timeSim, epmobil, label='NEW', color=colorsims[s], linestyle="--")
    plot.addVarAx2(timeSim, swe - swe[0], label='NEW', color=colorsims[s], linestyle="--")

plot.set_yaxis(ylabel="IRNAM")

plot.finalize(timeSim)

plot.save(commonpath + "/reanalyse_nouveaux_listeo/plot.pdf", formatout="pdf")
