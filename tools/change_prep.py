#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 23 Aug. 2017

@author: lafaysse
'''

import os
# import sys
import numpy as np
# import datetime

# For compatibility python 2 / python 3
# import six

from utils.prosimu import prosimu
from utils.FileException import FileNameException, VarNameException


class prep_tomodify(object):
    def __init__(self, prepfile):
        '''Generic method to open a PREP.nc file ready to be modified'''

        # Names of the prognostic variables in the PREP netcdf file
        self.dict_prep = {'nsnowlayer': 'SN_VEG_N',
                          'year': 'DTCUR-YEAR',
                          'month': 'DTCUR-MONTH',
                          'day': 'DTCUR-DAY',
                          'time': 'DTCUR-TIME',
                          'swe': 'WSN_VEG',
                          'rho': 'RSN_VEG',
                          'pheat': 'HSN_VEG',
                          'dopt': 'SG1_VEG',
                          'spher': 'SG2_VEG',
                          'hist': 'SHI_VEG',
                          'age': 'SAG_VEG',
                          'albedo': 'ASN_VEG',
                          'tg': 'TG'
                          }

        if not os.path.isfile(prepfile):
            raise FileNameException(prepfile)

        self.prepfile = prosimu(prepfile, openmode="a")
        self.nsnowlayer = self.prepfile.read(self.dict_prep['nsnowlayer'])[0]
        try:
            _, _ = self.prepfile.read(self.dict_prep['swe'] + str(1), keepfillvalue=True, removetile=False, needmodif=True)
            self.layerdim = False
        except VarNameException:  # layer is a dimension in the file
            self.layerdim = True

    def apply_swe_threshold(self, swe_threshold, closefile=False):
        '''Method to apply a threshold on snow water equivalent in a PREP.nc file'''
        if not self.layerdim:
            for i in range(self.nsnowlayer):
                swe_layer, swe_layer_nc = self.prepfile.read(self.dict_prep['swe'] + str(i + 1), keepfillvalue=True, removetile=False, needmodif=True)
                if i == 0:
                    swe_fromsurface = np.zeros_like(swe_layer)
                swe_layer = np.where(swe_fromsurface > swe_threshold, 0, swe_layer)
                swe_layer_nc[:] = swe_layer[:]

                swe_fromsurface += swe_layer
        else:
            swe_layer, swe_layer_nc = self.prepfile.read(self.dict_prep['swe'], keepfillvalue=True, removetile=False, needmodif=True)
            swe_layer = np.squeeze(swe_layer)
            swe_fromsurface = np.zeros_like(swe_layer[0, :])
            for i in range(self.nsnowlayer):
                swe_layer[i, :] = np.where(swe_fromsurface > swe_threshold, 0, swe_layer[i, :])
                swe_layer_nc[0, i, :] = swe_layer[i, :]
                swe_fromsurface += swe_layer[i, :]
        if closefile:
            self.close()

    def change_date(self, newdate, closefile=False):
        '''Method to change the date of a PREP file because a spinup is used to initialize the simulation
           Input : newdate must be a datetime.datetime object'''
        year, yearnc = self.prepfile.read(self.dict_prep['year'], needmodif=True)
        month, monthnc = self.prepfile.read(self.dict_prep['month'], needmodif=True)
        day, daync = self.prepfile.read(self.dict_prep['day'], needmodif=True)
        seconds, secondsnc = self.prepfile.read(self.dict_prep['time'], needmodif=True)

        del year, month, day, seconds

        yearnc[:] = newdate.year
        monthnc[:] = newdate.month
        daync[:] = newdate.day
        secondsnc[:] = newdate.hour * 3600

        if closefile:
            self.close()

    def close(self):
        self.prepfile.close()


if __name__ == "__main__":
    prep = prep_tomodify("PREP.nc")
    prep.apply_swe_threshold(swe_threshold=400, closefile=True)
