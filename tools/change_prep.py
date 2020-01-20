#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 23 Aug. 2017

@author: lafaysse
'''


import netCDF4, os, glob, csv
import numpy as np
from netCDF4 import *
from datetime import *


# For compatibility python 2 / python 3
# import six

from utils.prosimu import prosimu
from utils.FileException import FileNameException


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

    def apply_swe_threshold(self, swe_threshold, closefile=False):
        '''Method to apply a threshold on snow water equivalent in a PREP.nc file'''
        for i in range(self.nsnowlayer):
            swe_layer, swe_layer_nc = self.prepfile.read(self.dict_prep['swe'] + str(i + 1), keepfillvalue=True, removetile=False, needmodif=True)
            if i == 0:
                swe_fromsurface = np.zeros_like(swe_layer)
            swe_layer = np.where(swe_fromsurface > swe_threshold, 0, swe_layer)
            swe_layer_nc[:] = swe_layer[:]

            swe_fromsurface += swe_layer
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


def insert_snow_depth(date_obs, folder_3_points, folder_4_measurements, folder_5_obs, folder_6_gen_prep_snow, folder_8_out_assim):

    ''' This function was implemented by C. Carmagnola in March 2019 (PROSNOW project).
    It modifies a PREP file by inserting measured snow depth values.'''

    # 1) Read files

    # folder_3_points -> slopes of SRUs
    name_SRU = folder_3_points + '/SRUs_LesSaisies'
    SRUlist = open(name_SRU)
    slope = []
    for line in SRUlist:
        maliste = line.split()
        slope = slope + [maliste[7]]
    for i in range(len(slope)):
        slope[i] = float(slope[i])

    # folder_4_measurements -> SNOWSAT measurements
    name_SNOWSAT = folder_4_measurements + '/Les_Saisies_Snowsat.txt'

    # folder_5_obs -> OBS.nc to be filled
    name_OBS = folder_5_obs + '/OBS_' + date_obs.strftime('%Y%m%d') + '.nc'

    # folder_6_gen_prep_snow -> extprep50, extprep5 and variables (for model guess <= 10 cm)
    extprep50 = folder_6_gen_prep_snow + "/PREP_fillup_50.nc"
    extprep5 = folder_6_gen_prep_snow + "/PREP_fillup_5.nc"
    variables_list = open(folder_6_gen_prep_snow + "/variables")
    variables = []
    for line in variables_list:
        maliste = line.split()
        variables = variables + [maliste[0]]
    for i in range(len(variables)):
        variables[i] = str(variables[i])

    # folder_8_out_assim -> PREP to be modified
    name_PREP = folder_8_out_assim + '/prep/PREP_' + date_obs.strftime('%Y%m%d') + '06.nc'

    # 2) Fill OBS.nc

    f = glob.glob(name_SNOWSAT)[0]

    with open(f, 'rb') as readfile:

        spamreader = csv.reader(readfile)

        for row in spamreader:
            r = row[0].split(";")
            date = datetime(int(r[0]), int(r[1]), int(r[2]))

            if date == date_obs:
                Fic = folder_5_obs + '/OBS_' + date.strftime('%Y%m%d') + '.nc'
                Fnc = Dataset(Fic, 'a', format = "NETCDF4")
                DSN_T_ISBA = Fnc.variables['DSN_T_ISBA']

                if float(r[5]) >= 0.:
                    ind = int(r[3]) - 1
                    DSN_T_ISBA[0, ind] = float(r[5]) / 100.

                Fnc.close()

    # 3) Change PREP

    # Check if assimilation is possible
    if os.path.exists(name_OBS):

        # OBS.nc
        OBS_nc      = netCDF4.Dataset(name_OBS, 'r')
        DSN_T_ISBAo = OBS_nc.variables['DSN_T_ISBA'][:]

        # PREP
        PREP   = netCDF4.Dataset(name_PREP, 'a')
        nlayer = PREP.variables['SN_VEG_N'][:]

        tg1 = PREP.variables['TG1'][:]

        swe = np.zeros((np.shape(tg1)[0], np.shape(tg1)[1], nlayer))
        rho = np.zeros((np.shape(tg1)[0], np.shape(tg1)[1], nlayer))

        for j in range(nlayer):
            var = PREP.variables['WSN_VEG' + str(j + 1)]
            swe[:, :, j] = var[:, :]
            var = PREP.variables['RSN_VEG' + str(j + 1)]
            rho[:, :, j] = var[:, :]

        DSN_T_ISBA  = np.sum(swe / rho, 2)

        # Loop on points
        for k in range(np.shape(tg1)[1]):

            # SWE corrected to account for the slope
            DSN_T_ISBAo[0][k] = DSN_T_ISBAo[0][k] * np.cos(slope[k] * np.pi / 180.)

            # Observation exists
            if (DSN_T_ISBAo[0, k] >= 0.):

                # Model guess > 10 cm -> fine!
                if (DSN_T_ISBA[0, k] > 0.10):

                    ana = DSN_T_ISBAo[0, k]
                    new_swe = (ana / DSN_T_ISBA[:, k]) * swe[:, k, :].copy()

                    for m in range(nlayer):
                        var = PREP.variables['WSN_VEG' + str(m + 1)]
                        var[0, k] = new_swe[0, m]

                # Model guess <= 10 cm -> use other profiles!
                else:

                    # Observation >= 10 cm
                    if (DSN_T_ISBAo[0, k] >= .10):
                        PREP_ext  = netCDF4.Dataset(extprep50, 'r')

                    # Observation < 10 cm
                    else:
                        PREP_ext  = netCDF4.Dataset(extprep5, 'r')

                    utg1 = PREP_ext.variables['TG1'][:]

                    uswe = np.zeros((np.shape(utg1)[0], np.shape(utg1)[1], nlayer))
                    urho = np.zeros((np.shape(utg1)[0], np.shape(utg1)[1], nlayer))

                    for j in range(nlayer):
                        var = PREP_ext.variables['WSN_VEG' + str(j + 1)]
                        uswe[:, :, j] = var[:, :]
                        var = PREP_ext.variables['RSN_VEG' + str(j + 1)]
                        urho[:, :, j] = var[:, :]

                    uDSN_T_ISBA  = np.sum(uswe / urho, 2)

                    ana = DSN_T_ISBAo[0, k]
                    unew_swe = (ana / uDSN_T_ISBA[:, 0]) * uswe[:, 0, :].copy()

                    for m in range(nlayer):
                        var = PREP.variables['WSN_VEG' + str(m + 1)]
                        var[0, k] = unew_swe[0, m]

                    # Fill PREP with variables of PREP_ext (except for WSN_VEG, already filled in)
                    for v in variables:
                        if "WSN_VEG" not in v:
                            var = PREP.variables[v]
                            x = PREP_ext.variables[v][0, 0]
                            if str(x) == "--":
                                x = 1.
                            var[0, k] = x

        OBS_nc.close()
        PREP.close()


# Test

# if __name__ == "__main__":
#     prep = prep_tomodify("PREP.nc")
#     prep.apply_swe_threshold(swe_threshold=400, closefile=True)

# if __name__ == "__main__":
#     insert_snow_depth(datetime(2016,10,15),"/home/carmagnolac/CMC/CEN/4_SIMUL/Insert_SD/3_points","/home/carmagnolac/CMC/CEN/4_SIMUL/Insert_SD/4_measurements","/home/carmagnolac/CMC/CEN/4_SIMUL/Insert_SD/5_obs","/home/carmagnolac/CMC/CEN/4_SIMUL/Insert_SD/6_gen_prep_snow","/home/carmagnolac/CMC/CEN/4_SIMUL/Insert_SD/8_out_assim")