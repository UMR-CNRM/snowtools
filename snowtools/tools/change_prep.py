# -*- coding: utf-8 -*-

'''
Created on 23 Aug. 2017

@author: lafaysse
'''


import os
import csv

import netCDF4
import numpy as np

from snowtools.utils.prosimu import prosimu_base
from snowtools.utils.FileException import FileNameException


class prep_tomodify(object):
    """This class represents a PREP.nc file for SURFEX initial conditions
    which has to be modified before SURFEX execution

    :param prepfile: Address of prep file to modify
    :type prepfile: str
    """

    def __init__(self, prepfile):
        """Init method opens a PREP.nc file ready to be modified"""

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
                          'tg': 'TG',
                          }

        if not os.path.isfile(prepfile):
            raise FileNameException(prepfile)

        self.prepfile = prosimu_base(prepfile, openmode="a")
        self.nsnowlayer = self.prepfile.read(self.dict_prep['nsnowlayer'])[0]

    def apply_swe_threshold(self, swe_threshold, closefile=False):
        """Method to apply a threshold on snow water equivalent in a PREP.nc file

        :param swe_threshold: Maximum allowed Snow Water Equivalent (kg/m2)
        :type swe_threshold: int or float
        :param closefile: Close PREP file after modification. Defaults to False
        :type closefile: bool, optional
        """
        for i in range(self.nsnowlayer):
            swe_layer, swe_layer_nc = self.prepfile.read(
                self.dict_prep['swe'] + str(i + 1), keepfillvalue=True, removetile=False, needmodif=True)
            if i == 0:
                swe_fromsurface = np.zeros_like(swe_layer)
            swe_layer = np.where(swe_fromsurface > swe_threshold, 0, swe_layer)
            swe_layer_nc[:] = swe_layer[:]

            swe_fromsurface += swe_layer
        if closefile:
            self.close()

    def change_date(self, newdate, closefile=False):
        """Method to change the date of a PREP file because a spinup is used to initialize the simulation

        :param newdate: New initial date of simulation
        :type newdate: class:`datetime.datetime`
        :param closefile: Close PREP file after modification. Defaults to False
        :type closefile: bool, optional
        """
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
        """Close the PREP file"""
        self.prepfile.close()

    def insert_snow_depth(self, my_name_SRU, my_name_SNOWSAT, my_name_OBS,
                          my_name_extprep50, my_name_extprep5, my_name_var, my_name_PREP):
        ''' This function was implemented by C. Carmagnola in March 2019 (PROSNOW project).
        It modifies a PREP file by inserting measured snow depth values.'''

        swe = 'WSN_VEG'
        rho = 'RSN_VEG'
        tg = 'TG1'
        snd = 'DSN_T_ISBA'

        # 1) Read files

        # my_name_SRU -> slopes of SRUs
        name_SRU = my_name_SRU
        SRUlist = open(name_SRU)
        slope = []
        for line in SRUlist:
            maliste = line.split()
            slope = slope + [maliste[7]]
        for i in range(len(slope)):
            slope[i] = float(slope[i])

        # my_name_SNOWSAT -> SNOWSAT measurements
        name_SNOWSAT = my_name_SNOWSAT

        # my_name_OBS -> OBS.nc to be filled
        name_OBS = my_name_OBS

        # my_name_extprep50, my_name_extprep5 and my_name_var ->
        # extprep50, extprep5 and variables (for model guess <= 10 cm)
        extprep50 = my_name_extprep50
        extprep5 = my_name_extprep5
        variables_list = open(my_name_var)
        variables = []
        for line in variables_list:
            maliste = line.split()
            variables = variables + [maliste[0]]
        for i in range(len(variables)):
            variables[i] = str(variables[i])

        # my_name_PREP -> PREP to be modified
        name_PREP = my_name_PREP

        # 2) Fill OBS.nc

        with open(name_SNOWSAT, 'r') as readfile:

            spamreader = csv.reader(readfile)

            for row in spamreader:

                r = row[0].split()
                with netCDF4.Dataset(name_OBS, 'a') as Fnc:
                    dsn = Fnc.variables[snd]

                    ind = int(r[0]) - 1
                    dsn[0, ind] = float(r[2]) / 100.

        # 3) Change PREP

        # Check if assimilation is possible
        if os.path.exists(name_OBS):

            # OBS.nc
            OBS_nc = netCDF4.Dataset(name_OBS, 'r')
            dsno = OBS_nc.variables[snd][:]

            # PREP
            PREP = netCDF4.Dataset(name_PREP, 'a')
            nlayer = self.prepfile.read(self.dict_prep['nsnowlayer'])[:]
            tg1 = [self.prepfile.read(self.dict_prep['tg'] + str(1))[:]]

            old_swe = np.zeros((np.shape(tg1)[0], np.shape(tg1)[1], nlayer))  # [1,point,layer]
            new_swe = np.zeros((np.shape(tg1)[0], np.shape(tg1)[1], nlayer))  # [1,point,layer]
            old_rho = np.zeros((np.shape(tg1)[0], np.shape(tg1)[1], nlayer))  # [1,point,layer]
            dsn = np.zeros((np.shape(tg1)[0], np.shape(tg1)[1]))         # [1,point]

            for j in range(nlayer):
                var = self.prepfile.read(self.dict_prep['swe'] + str(j + 1))[:]
                old_swe[:, :, j] = var[:]
                var = self.prepfile.read(self.dict_prep['rho'] + str(j + 1))[:]
                old_rho[:, :, j] = var[:]

            # Loop on points
            for k in range(np.shape(tg1)[1]):

                for m in range(nlayer):
                    if old_swe[0, k, m] > 0:
                        dsn[0, k] = dsn[0, k] + old_swe[0, k, m]/old_rho[0, k, m]

                dsno[0][k] = dsno[0][k] * np.cos(slope[k] * np.pi / 180.)

                # Observation exists
                if (dsno[0, k] >= 0.):

                    # Model guess > 10 cm -> fine!
                    if (dsn[0, k] > 0.10):

                        new_swe[0, k, :] = (dsno[0, k] / dsn[0, k]) * old_swe[0, k, :]

                        for m in range(nlayer):
                            var = PREP.variables[swe + str(m + 1)]
                            var[0, k] = new_swe[0, k, m]

                    # Model guess <= 10 cm -> use other profiles!
                    else:

                        # Observation >= 10 cm
                        if (dsno[0, k] >= .10):
                            PREP_ext = netCDF4.Dataset(extprep50, 'r')

                        # Observation < 10 cm
                        else:
                            PREP_ext = netCDF4.Dataset(extprep5, 'r')

                        utg1 = PREP_ext.variables[tg][:]

                        uswe = np.zeros((np.shape(utg1)[0], np.shape(utg1)[1], nlayer))
                        urho = np.zeros((np.shape(utg1)[0], np.shape(utg1)[1], nlayer))

                        for j in range(nlayer):
                            var = PREP_ext.variables[swe + str(j + 1)]
                            uswe[:, :, j] = var[:, :]
                            var = PREP_ext.variables[rho + str(j + 1)]
                            urho[:, :, j] = var[:, :]

                        udsn = np.sum(uswe / urho, 2)

                        ana = dsno[0, k]
                        unew_swe = (ana / udsn[:, 0]) * uswe[:, 0, :].copy()

                        for m in range(nlayer):
                            var = PREP.variables[swe + str(m + 1)]
                            var[0, k] = unew_swe[0, m]

                        # Fill PREP with variables of PREP_ext (except for WSN_VEG, already filled in)
                        for v in variables:
                            if swe not in v:
                                var = PREP.variables[v]
                                x = PREP_ext.variables[v][0, 0]
                                if str(x) == "--":
                                    x = 1.
                                var[0, k] = x

                        PREP_ext.close()

            OBS_nc.close()
            PREP.close()
