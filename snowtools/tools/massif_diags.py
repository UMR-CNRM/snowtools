# -*- coding: utf-8 -*-

'''
Created on 25 June 2018

@author: lafaysse
'''

import numpy as np
from snowtools.utils.prosimu import prosimu


class massif_simu(prosimu):
    """Class overriding prosimu to add massif-scale diagnostics"""
    SurfexNatRiskName = 'NAT_LEV'
    MassifRiskName = 'naturalIndex'
    massif_dim_name = 'massif'
    massif_var_name = 'massif_num'
    aspect_var_name = 'aspect'
    elevation_var_name = 'ZS'
    slope_var_name = 'slope'

    def massif_natural_risk(self):
        """Compute massif-scale natural hazard index and add it to the file"""
        if self.SurfexNatRiskName not in self.listvar() or self.massif_var_name not in self.listvar():
            return

        slope = self.read(self.slope_var_name, keepfillvalue=True)

        if not np.any(slope == 40.):
            return

        print("Compute massif-scale natural avalanche hazard indexes")

        slope_natural_risk = self.read(self.SurfexNatRiskName, keepfillvalue=True).astype('int')
        fillvalue = self.getfillvalue(self.SurfexNatRiskName)
        aspect = self.read(self.aspect_var_name, keepfillvalue=True)
        altitude = self.read(self.elevation_var_name, keepfillvalue=True)
        massif_number = self.read(self.massif_var_name, keepfillvalue=True).astype('int')
        list_massifs = np.unique(massif_number)
        list_aspects = np.unique(aspect[aspect >= 0])

        minlevel = 1500.
        maxlevel = 3000.

        ntime = slope_natural_risk.shape[0]
        naspects = len(list_aspects)

        self.warnings(np.min(altitude), np.max(altitude), minlevel, maxlevel, naspects, list_aspects)

        nmassifs = len(list_massifs)
        nlevels = int((maxlevel - minlevel) / 300.) + 1

        weights = [0, 0, 1, 2, 4, 8, 0]

        if self.massif_dim_name not in self.listdim():
            self.dataset.createDimension(self.massif_dim_name, nmassifs)
            var = self.dataset.createVariable(self.massif_dim_name, 'i4', [self.massif_dim_name], fill_value=0)
            var[:] = list_massifs[:]

        risk_array = np.empty((ntime, naspects, nlevels, nmassifs))
        for m, massif in enumerate(list_massifs):
            for L, level in enumerate(np.arange(minlevel, maxlevel + 1, 300)):
                indslopes = (massif_number == massif) & (slope == 40.) & (altitude == level)

                if np.sum(indslopes) > 1:
                    risk_array[:, :, L, m] = np.take(weights, slope_natural_risk[:, indslopes])
                else:
                    risk_array[:, :, L, m] = 0

        var = self.dataset.createVariable(self.MassifRiskName, 'float',
                                          ["time", self.massif_dim_name], fill_value=fillvalue)
        var.long_name = ('Massif-scale index of natural avalanche hazard.'
                         ' Definition provided in http://dx.doi.org/10.3189/172756401781819292')
        var.units = '0-8'

        var[:, :] = np.max(np.nanmean(risk_array, axis=2), axis=1)

    def warnings(self, minaltitude, maxaltitude, minlevel, maxlevel, naspects, list_aspects):
        """Check if all elevations and aspects are present in the computation of massif-scale natural hazard index

        :param minaltitude: Minimum elevation present in the PRO file
        :type minaltitude: float
        :param maxaltitude: Maximum elevation present in the PRO file
        :type maxaltitude: float
        :param minlevel: Minimum elevation considered in massif-scale natural hazard index
        :type minlevel: float
        :param maxlevel: Maximum elevation considered in massif-scale natural hazard index
        :type maxlevel: float
        :param naspects: Number of aspects present in the PRO file
        :type naspects: int
        :param list_aspects: List of aspects present in the PRO file
        :type list_aspects: list
        """
        if minaltitude > minlevel:
            print("WARNING: the massif-scale natural avalanche"
                  " hazard index is not computed with all expected elevations")
            print("Lowest available level: " + str(minaltitude))

        if maxaltitude < maxlevel:
            print("WARNING: the massif-scale natural avalanche"
                  "hazard index is not computed with all expected elevations")
            print("Highest available level: " + str(maxaltitude))

        if naspects != 8:
            print("WARNING: the massif-scale natural avalanche hazard index is not computed with 8 aspect classes")
            print("Available aspects: " + str(list_aspects))
