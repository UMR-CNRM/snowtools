#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 23 Aug. 2017

@author: lafaysse
'''

import os
# import sys
import numpy as np

# For compatibility python 2 / python 3
# import six

from utils.prosimu import prosimu
from utils.FileException import FileNameException

class prep_tomodify(object):
    def __init__(self,prepfile):
        '''Generic method to open a PREP.nc file ready to be modified'''     

        # Names of the prognostic variables in the PREP netcdf file
        self.dict_prep = {'nsnowlayer': 'SN_VEG_N',
                   'year': 'STCUR-YEAR',
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
            
        self.prepfile = prosimu(prepfile,openmode="a")
        self.nsnowlayer=self.prepfile.read(self.dict_prep['nsnowlayer'])[0]

    def apply_swe_threshold(self,swe_threshold):
        '''Method to apply a threshold on snow water equivalent in a PREP.nc file'''
        for i in range(self.nsnowlayer):
            swe_layer,swe_layer_nc=self.prepfile.read(self.dict_prep['swe']+str(i+1),keepfillvalue=True,removetile=False,needmodif=True)
            print swe_layer.shape
            if i==0:
                swe_fromsurface=np.zeros_like(swe_layer)
            swe_layer=np.where(swe_fromsurface>swe_threshold,0,swe_layer)
            print swe_layer_nc.shape, swe_layer.shape
            swe_layer_nc[:]=swe_layer[:]

            swe_fromsurface+=swe_layer
        
        self.close()       
        
    def close(self):
        self.prepfile.close()
        
if __name__ == "__main__":
    prep=prep_tomodify("PREP.nc")
    prep.apply_swe_threshold(swe_threshold=400)   
        
        