#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 25 June 2018

@author: lafaysse
'''

import numpy as np
from utils.prosimu import prosimu


class massif_simu(prosimu):

    SurfexNatRiskName = 'NAT_LEV'
    MassifRiskName = 'naturalIndex'
    massif_dim_name = 'massif'
    massif_var_name = 'massif_num'

    def massif_natural_risk(self):

        if self.SurfexNatRiskName not in self.listvar():
            return

        slope = self.read("slope")

        if not np.any(slope == 40.):
            return

        print "Compute massif-scale natural avalanche hazard indexes"

        slope_natural_risk = self.read(self.SurfexNatRiskName).astype('int')
        aspect = self.read("aspect")
        altitude = self.read("ZS")
        massif_number = self.read(self.massif_var_name).astype('int')
        list_massifs = np.unique(massif_number)
        list_aspects = np.unique(aspect[aspect >= 0])
        minlevel = 1500.
        maxlevel = 3000.

        ntime = slope_natural_risk.shape[0]
        naspects = len(list_aspects)
        nmassifs = len(list_massifs)
        nlevels = int((maxlevel - minlevel) / 300.) + 1

        weights = [0, 0, 1, 2, 4, 8, 0]

        if self.massif_dim_name not in self.listdim():
            self.dataset.createDimension(self.massif_dim_name, nmassifs)
            var = self.dataset.createVariable(self.massif_dim_name, 'i4', ["massif"], fill_value=0)
            var[:] = list_massifs[:]

        risk_array = np.empty((ntime, naspects, nlevels, nmassifs))
        for m, massif in enumerate(list_massifs):
            for l, level in enumerate(np.arange(minlevel, maxlevel, 300)):
                indslopes = (massif_number == massif) & (slope == 40.) & (altitude == level)

                if np.sum(indslopes) > 1:
                    risk_array[:, :, l, m] = np.take(weights, slope_natural_risk[:, indslopes])
                else:
                    risk_array[:, :, l, m] = np.nan

        var = self.dataset.createVariable(self.MassifRiskName, 'i4', ["time", "massif"], fill_value=-1)
        var[:] = np.max(np.nanmean(risk_array, axis=2), axis=1)
