#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This script creates FORCING files for the Col de Porte site.
It uses MET files (ie meteorogical files) from 1993 and s2m reanalysis before.

Values of MET files come from different sensor in the Col de Porte site and benefit from a human expertise.

The FORCING files which is created is also use to complete the doi of the article "57 years..."
(under the name CRYOBSCLIM.CDP.2018.MetInsitu.nc).

What has to be controled in Met2Netcdf.py script
------------------------------------------------
This script completes the MET files with values coming from different sources: bdniv database, s2m reanalasys.
The use of the different sources imply to define in the script some dates.
Roughly, before one date, we use a data source and after, another data source.
So, this is important to change dates inside the script before using it.

HARD CODED PART OF THE CODE
^^^^^^^^^^^^^^^^^^^^^^^^^^^
year of last MET file (to be changed each year)

year of name changing for cdp60mn database (to be changed each year)
(ie cdp60mn becomes cdp60mn_2223 for example)

path to MET files (normally stable path)
path to reanalysis files (normally stable path)

EXAMPLES OF USE
^^^^^^^^^^^^^^^

.. code-block:: bash

   python3 Met2Netcdf.py -b 2000080106 -e 2001080106
   python3 Met2Netcdf.py -b 1958080106 -e 2023080106 -o MAJ_MetInsitu.nc
   python3 Met2Netcdf.py -b 2000080106 -e 2001080106 --one_file -p partial_MET.txt

Options:

* -o output.nc -> The output file is named output.nc
* -c ( for constant) -> Avoid taking PSurf and Wind_DIR from cdp60mn. By default, -c is not activated.
* -b -> the starting date for the FORCING file
* -e -> the ending date for the FORCING file
* -s -> the number of the location site. By default '38472401' which is the Col de Porte site
* --one_file -> create a FORCING file only for a specific MET file
* -p -> the path for this only MET file (required when --one_file is activated)

"""
import sys
import datetime
import argparse
from datetime import timedelta

import psycopg2
import numpy as np
import netCDF4
import xarray as xr

from snowtools.utils.S2M_standard_file import StandardCDP
from snowtools.utils.resources import get_file_period
from snowtools.utils.prosimu import prosimu
from snowtools.utils.dates import check_and_convert_date, get_list_dates_files
from snowtools.utils.infomassifs import infomassifs
from bronx.meteo.thermo import Thermo
from bronx.meteo.constants import T0

###########################################################################
# MUST CHANGE EACH YEAR:
#
# year of last MET file
annee_last_MET = 2023
#
# END OF CHANGE
###############
# HARD CODE:
#
# CDP60mn starting day
annee_last_cdp60mn = str(annee_last_MET) + '080100'
path_safran = '/rd/cenfic3/cenmod/era40/vortex/s2m/postes/reanalysis/meteo'
path_met = '/rd/cenfic3/cenobs/mesure_data/col_de_porte/met/'
pas_par_defaut = 3600
#
###########################################################################

def recup_donnees_site(numero_site):
    """
    Give infos (Latitude, longitude, altitude, etc...) from a site (using site number)
    More precisely, the infos are: [LAT, LON, UREF, ZREF, ZS, aspect, slope, site number], site name

    :param numero_site: site of interest (number of 8 figures with string format)
    :type numero_site: str
    :returns: un tuple of 2 terms: one is the list of infos and other is the site name
    """
    IM = infomassifs()
    LAT, LON, ZS, UREF, ZREF, aspect, slope, name = IM.infoposte_extensive(numero_site)
    Tab_point = np.zeros(8)
    Tab_point[0] = float(LAT)
    Tab_point[1] = float(LON)
    Tab_point[2] = float(UREF)
    Tab_point[3] = float(ZREF)
    Tab_point[4] = float(ZS)
    Tab_point[5] = float(aspect)
    Tab_point[6] = float(slope)
    Tab_point[7] = int(numero_site)
    return Tab_point, name


def message_accueil(option_bool, nom_site):
    if option_bool:
        print('\nCompletion des donnees MET avec Pression et Direction du vent du site ' + str(nom_site) + '\n')
    print('#########################################################')
    print('# A verifier dans le code:')
    print('# - chemin des MET: ' + path_met + '\n')
    print('# - format des MET: MET_YYYY_YYYY+1_fmt (ex MET_2000_2001_fmt)\n')
    print('# - chemin des reanalyses SAFRAN: ' + path_safran + '\n')
    print('# A changer chaque année: année du dernier MET disponible')
    print('#########################################################')


def decoupe_periode(date_entree_debut, date_entree_fin):
    """
    split period of dates in yearly period and give the list of MET files
    MET file name is supposed to be in a fix format: MET_1996_1997_fmt

    :param date_entree_debut: starting date for the FORCING file
    :type date_entree_debut: Date (extension of datetime.datetime)
    :param date_entree_fin: ending date for the FORCING file
    :type date_entree_fin: Date (extension of datetime.datetime)
    :returns: un tuple of 3 lists: MET list, starting date list, ending date list
    """
    list_date_debut = get_list_dates_files(date_entree_debut, date_entree_fin, 'yearly')[0]
    list_date_fin = get_list_dates_files(date_entree_debut, date_entree_fin, 'yearly')[1]
    list_path_met = []
    for debut,fin in zip(list_date_debut,list_date_fin):
        list_path_met.append(path_met + 'MET_' + str(debut.year) + '_' + str(fin.year) + '_fmt')

    return list_path_met, list_date_debut, list_date_fin


def read_info_met(filename):
    """
    Get informations for an annual MET file: size of the header, begin date, end date.
    NB: we suppose that the lines of the header are beginning with a letter.
    NB2: 2 date format: YYYYMMDDHH or DD/MM/YYYY HH:M
    NB3: "old" MET doesn't have full yearly data -> needs to complete -> needs to know where to start and stop

    :param filename: path to MET file
    :type filename: str
    :returns: list: [number of lines in header, starting date for MET datas; ending date for MET datas]
    """
    metfile = open(filename, 'r')
    fileLines = metfile.readlines()
    metfile.close()
    last_line = fileLines[len(fileLines) - 1]
    nb_ligne_entete = 0
    # Idée: regarder les 30 premières lignes pour compter les lignes d'entêtes (qui ne commencent pas par un chiffre)
    # Ensuite, repérer le format de la date en fonction de la présence d'un "/" (format DD/MM/... au lieu de YYYYMM..)
    for line in fileLines[0:30]:
        if line[0] not in set([str(i) for i in range(10)]):
            nb_ligne_entete = nb_ligne_entete + 1
            continue
        elif (line[2] == '/'):
            first_date_met = line[6:10] + line[3:5] + line[0:2] + line[11:13]
            last_date_met = last_line[6:10] + last_line[3:5] + last_line[0:2] + last_line[11:13]
            break
        elif (line[2] != '/'):
            first_date_met = line[0:11]
            last_date_met = last_line[0:11]
            break

    L = [int(nb_ligne_entete), check_and_convert_date(first_date_met), check_and_convert_date(last_date_met)]
    return L


def recup_cdp(date_time_deb, date_time_fin):
    """
    Get field Wind Direction and Surface Pressure from cdp60mn database

    :param date_time_deb: starting date
    :type date_time_deb: datetime.datetime
    :param date_time_fin: ending date
    :type date_time_fin: datetime.datetime
    :returns: np array of dimension (list_of_dates,3) [list_of_dates, pressure, wind dir]
    """
    # Structure des bases: cdp60mn pour l'année en cours et un peu plus. Ensuite, mis dans cdp9293, cdp9394 etc... jusqu'à cdp1415
    # A partir de la saison 15-16, chgt de nom de base cdp60mn_1516 et changement de nom des variables
    date_change_2015 = check_and_convert_date(str(2015080100))  # EN DUR et fixe
    date_change_base60mn = check_and_convert_date(annee_last_cdp60mn)

    ###############################################################################################
    # A partir du MET_2015_2016, on a des années complètes 1er août 00h vers 1er août 00h pour les met
    # Le stockage dans les bases de données cdp*** correspond
    # Par contre, les FORCING sont du 1er août 6h vers 1er août 6h
    # Il faut ainsi gérer le cas où la date fin du fichier MET dépasse le 1er août minuit (future norme ?)
    date_1er_aout = datetime.datetime(date_time_fin.year, 8, 1, 0, 0)

    # Cas "standards": date_time_fin avant le 1er aout 00h
    if (date_change_2015 >= date_time_fin) and (date_time_fin <= date_1er_aout):
        nom_base = 'cdp' + str(date_time_fin.year - 1)[2:4] + str(date_time_fin.year)[2:4]
        nom_var = 'baro,dvent'
    elif (date_change_base60mn >= date_time_fin) and (date_time_fin <= date_1er_aout):
        nom_base = 'cdp60mn_' + str(date_time_fin.year - 1)[2:4] + str(date_time_fin.year)[2:4]
        nom_var = 'p_ptb330_v1,dd_meca_10mn_10m'
    elif date_time_fin <= date_1er_aout:
        nom_base = 'cdp60mn'
        nom_var = 'p_ptb330_v1,dd_meca_10mn_10m'
    # Cas où YYYY080106 => d'abord récupérer les données sur l'année passée
    elif (date_time_fin > date_1er_aout) and date_time_fin.year <= date_change_2015.year:
        nom_base = 'cdp' + str(date_time_fin.year - 1)[2:4] + str(date_time_fin.year)[2:4]
        nom_var = 'baro,dvent'
    elif (date_time_fin > date_1er_aout) and date_time_fin.year <= date_change_base60mn.year:
        nom_base = 'cdp60mn_' + str(date_time_fin.year - 1)[2:4] + str(date_time_fin.year)[2:4]
        nom_var = 'p_ptb330_v1,dd_meca_10mn_10m'
    else:
        nom_base = 'cdp60mn'
        nom_var = 'p_ptb330_v1,dd_meca_10mn_10m'

    # command est une requete SQL (en string)
    command = "select dat," + nom_var + " from bdniv." + nom_base + " where dat between '{0}' and '{1}' \
    order by dat".format(date_time_deb, date_time_fin)

    host = 'cenexp2.cen.meteo.fr'
    database = 'bdniv'
    user = 'consult'
    port = '5433'
    conn = psycopg2.connect(database=database, user=user, host=host, port=port)
    cursor = conn.cursor()
    cursor.execute(command)
    data = np.array(cursor.fetchall())

    # Cas où YYYY080106 => ensuite récupérer les données sur l'année en cours
    # a) récupérer dans la base de données d'après les 5h manquantes
    # b) concaténer les données.
    if (date_time_fin > date_1er_aout and nom_base != 'cdp60mn' and date_time_fin.year < date_change_base60mn.year):
        nom_var = 'p_ptb330_v1,dd_meca_10mn_10m'
        if date_time_fin.year < date_change_base60mn.year:
            nom_base = 'cdp60mn_' + str(date_time_fin.year)[2:4] + str(date_time_fin.year + 1)[2:4]
        else:
            nom_base = 'cdp60mn'

        # command est une requete SQL (en string)
        command = "select dat," + nom_var + " from bdniv." + nom_base + " where dat between '{0}' and '{1}' \
        order by dat".format(date_time_deb, date_time_fin)

        host = 'cenexp2.cen.meteo.fr'
        database = 'bdniv'
        user = 'consult'
        port = '5433'
        conn = psycopg2.connect(database=database, user=user, host=host, port=port)
        cursor = conn.cursor()
        cursor.execute(command)
        data_supp = np.array(cursor.fetchall())

        # concatenation en enlevant une ligne pour éviter de dupliquer la valeur YYYY080100
        if data.shape == (0,):
            data = np.empty((0, 3))
        if data_supp.shape == (0,):
            data_supp = np.empty((0, 3))
        data = np.vstack((data, data_supp[1:, :]))

    # correction frustre des données pression + changement d'unité (hPa en Pa)
    if len(data[:]) != 0:
        for i in range(len(data[:, 0])):
            if data[i, 1] != None:
                if data[i, 1] > 890:
                    data[i, 1] = 863.827
                if data[i, 1] < 840:
                    data[i, 1] = 863.827
                data[i, 1] = 100 * data[i, 1]
            else:
                data[i, 1] = 86382.7
            # correction frustre des données de direction du vent
            if data[i, 2] != None:
                if data[i, 2] < 0:
                    data[i, 2] = 0
                if data[i, 2] > 360:
                    data[i, 2] = 360
            else:
                data[i, 2] = 0

    return data


def open_met_file_and_create_xr(filename, option_recup, site):
    """
    Get all differents fields in MET file

    :param filename: path to MET file
    :type filename: str
    :param option_recup: if True, use database cdp60mn to complete PSurf and windDIR
    :type option_recup: boolean
    :param site: site of interest (number of 8 figures with string format)
    :type site: str
    :returns: an xarray with all the fields, indexed by dates of MET file
    """
    # Les constantes à mettre:
    CO2air = 0.00062
    IM = infomassifs()
    LAT, LON, ZS, UREF, ZREF, aspect, slope, name = IM.infoposte_extensive(site)

    PSurf_cst = round(101325 * ((288 - 0.0065 * ZS) / 288) ** 5.255, 1)
    nb_ligne_entete, first_date, last_date = read_info_met(filename)

    try:
        metfile = open(filename, 'r')
    except:
        print('input meteorological file ' + metfile + ' not found')
        sys.exit()

    # Les fichiers sont générés avec possiblement pas le bon nombre d'espaces
    # Les MET à partir de 2016_2017 => 10 champs
    # date          tempe       wind     humrel   precip    phase    LWdown   DIR_SWdown  SCA_SWdown  NEB
    # 2017080100      20.0       1.7        45      0.00      0.00    334.40      0.47      1.31      0.00

    # Les MET de 2015_2016 => 12 champs
    #  date (2 champs)    nrart     Tair        ff        RH        RR     phase        LW     SWdir     SWdif       Neb
    # 01/08/2015 00:0         1      12.2       0.9        90       0.8       0.0     355.3       0.0       0.0       0.8

    # Les MET avant 2014_2015 (inclus) => 11 champs
    # Date          ART.  T   Vent Hum RRtot SS/RRtot IR  Soldr Soldf Neb
    # 2014092000      1  12.8  0.6  95  0.80 0.00   357.8   0.0   0.0 0.79
    fileLines = metfile.readlines()[nb_ligne_entete:]
    metfile.close()

    File_entree = np.loadtxt(fileLines, dtype='str')

    Tableau_retour_time_nbpoint = np.zeros((len(fileLines), 14))
    # Dans l'ordre: CO2air, DIR_SWdown, Flag_in_situ, Humrel, LWdown, NEB, PSurf, Qair, Rainf, SCA_SWdown, Snowf, Tair, Wind, Wind_DIR

    # Construction Liste_date pour compléter éventuellement les data du cdp
    Liste_date = []
    if len(fileLines) != 0:
        if len(File_entree[0, :]) != 12:
            for i in range(len(File_entree[:, 0])):
                Liste_date.append(check_and_convert_date(File_entree[i, 0]))
        elif len(File_entree[0, :]) == 12:
            for i in range(len(File_entree[:, 0])):
                line = File_entree[i, 0]
                hour = File_entree[i, 1]
                date = line[6:10] + line[3:5] + line[0:2] + hour[0:2]
                Liste_date.append(check_and_convert_date(date))

        # Recuperation de PSurf et Wind_DIR sur table CdP, sinon, valeurs par défaut
        if option_recup and site == '38472401':
            data_cdp = recup_cdp(first_date, last_date)
            # traitement du cas où des valeurs sont absentes de la table CdP
            if len(data_cdp[:]) == 0:
                Wind_DIR = np.zeros((len(fileLines)))
                PSurf = np.ones((len(fileLines))) * PSurf_cst
            elif len(data_cdp[:, 0]) < len(fileLines):
                Tab = np.zeros((len(fileLines), 2))
                Tab[:, 0] = PSurf_cst
                index = list(np.searchsorted(Liste_date, data_cdp[:, 0]))
                Tab[index, 0] = data_cdp[:, 1].astype(float)
                Tab[index, 1] = data_cdp[:, 2].astype(float)
                PSurf = Tab[:, 0]
                Wind_DIR = Tab[:, 1]
            else:
                PSurf = data_cdp[:, 1].astype(float)
                Wind_DIR = data_cdp[:, 2].astype(float)
        else:
            Wind_DIR = np.zeros((len(fileLines)))
            PSurf = np.ones((len(fileLines))) * PSurf_cst

        if len(File_entree[0, :]) == 10:
            for i in range(len(File_entree[:, 0])):
                Tableau_retour_time_nbpoint[i, 7] = Thermo(['v', 'c'], dict(P=PSurf[i], Huw=float(File_entree[i, 3]),
                                                                            T=float(File_entree[i, 1]) + T0, rc=0)).get(
                    'qv')
                Tableau_retour_time_nbpoint[i, 8] = float(File_entree[i, 4]) * (
                            1 - float(File_entree[i, 5])) / pas_par_defaut
                Tableau_retour_time_nbpoint[i, 10] = float(File_entree[i, 4]) * float(
                    File_entree[i, 5]) / pas_par_defaut

            Tableau_retour_time_nbpoint[:, 0] = float(CO2air)
            Tableau_retour_time_nbpoint[:, 1] = File_entree[:, 7].astype(float)
            Tableau_retour_time_nbpoint[:, 2] = float(1)  # flag à 1 dans ce cas-là
            Tableau_retour_time_nbpoint[:, 3] = File_entree[:, 3].astype(float)
            Tableau_retour_time_nbpoint[:, 4] = File_entree[:, 6].astype(float)
            Tableau_retour_time_nbpoint[:, 5] = File_entree[:, 9].astype(float)
            Tableau_retour_time_nbpoint[:, 6] = PSurf[:]
            Tableau_retour_time_nbpoint[:, 9] = File_entree[:, 8].astype(float)
            Tableau_retour_time_nbpoint[:, 11] = File_entree[:, 1].astype(float) + T0
            Tableau_retour_time_nbpoint[:, 12] = File_entree[:, 2].astype(float)
            Tableau_retour_time_nbpoint[:, 13] = Wind_DIR[:]

        elif len(File_entree[0, :]) == 11:
            for i in range(len(File_entree[:, 0])):
                Tableau_retour_time_nbpoint[i, 7] = Thermo(['v', 'c'], dict(P=PSurf[i], Huw=float(File_entree[i, 4]),
                                                                            T=float(File_entree[i, 2]) + T0, rc=0)).get(
                    'qv')
                Tableau_retour_time_nbpoint[i, 8] = float(File_entree[i, 5]) * (
                            1 - float(File_entree[i, 6])) / pas_par_defaut
                Tableau_retour_time_nbpoint[i, 10] = float(File_entree[i, 5]) * float(
                    File_entree[i, 6]) / pas_par_defaut
                year = int(File_entree[i, 0][0:4])
                month = int(File_entree[i, 0][4:6])
                if year < 2010 or (year == 2010 and month <= 11):
                    Tableau_retour_time_nbpoint[i, 4] = float(File_entree[i, 7]) - 10.0
                elif year < 2015 or (year == 2015 and month <= 7):
                    Tableau_retour_time_nbpoint[i, 4] = float(File_entree[i, 7]) + 10.0
                else:
                    Tableau_retour_time_nbpoint[i, 4] = float(File_entree[i, 7])

            Tableau_retour_time_nbpoint[:, 0] = float(CO2air)
            Tableau_retour_time_nbpoint[:, 1] = File_entree[:, 8].astype(float)
            Tableau_retour_time_nbpoint[:, 2] = float(1)  # flag à 1 dans ce cas-là
            Tableau_retour_time_nbpoint[:, 3] = File_entree[:, 4].astype(float)
            Tableau_retour_time_nbpoint[:, 5] = File_entree[:, 10].astype(float)
            Tableau_retour_time_nbpoint[:, 6] = PSurf[:]
            Tableau_retour_time_nbpoint[:, 9] = File_entree[:, 9].astype(float)
            Tableau_retour_time_nbpoint[:, 11] = File_entree[:, 2].astype(float) + T0
            Tableau_retour_time_nbpoint[:, 12] = File_entree[:, 3].astype(float)
            Tableau_retour_time_nbpoint[:, 13] = Wind_DIR[:]

        elif len(File_entree[0, :]) == 12:
            for i in range(len(File_entree[:, 0])):
                Tableau_retour_time_nbpoint[i, 7] = Thermo(['v', 'c'], dict(P=PSurf[i], Huw=float(File_entree[i, 5]),
                                                                            T=float(File_entree[i, 3]) + T0, rc=0)).get(
                    'qv')
                Tableau_retour_time_nbpoint[i, 8] = float(File_entree[i, 6]) * (
                            1 - float(File_entree[i, 7])) / pas_par_defaut
                Tableau_retour_time_nbpoint[i, 10] = float(File_entree[i, 6]) * float(
                    File_entree[i, 7]) / pas_par_defaut

            Tableau_retour_time_nbpoint[:, 0] = float(CO2air)
            Tableau_retour_time_nbpoint[:, 1] = File_entree[:, 9].astype(float)
            Tableau_retour_time_nbpoint[:, 2] = float(1)  # flag à 1 dans ce cas-là
            Tableau_retour_time_nbpoint[:, 3] = File_entree[:, 5].astype(float)
            Tableau_retour_time_nbpoint[:, 4] = File_entree[:, 8].astype(float)
            Tableau_retour_time_nbpoint[:, 5] = File_entree[:, 11].astype(float)
            Tableau_retour_time_nbpoint[:, 6] = PSurf[:]
            Tableau_retour_time_nbpoint[:, 9] = File_entree[:, 10].astype(float)
            Tableau_retour_time_nbpoint[:, 11] = File_entree[:, 3].astype(float) + T0
            Tableau_retour_time_nbpoint[:, 12] = File_entree[:, 4].astype(float)
            Tableau_retour_time_nbpoint[:, 13] = Wind_DIR[:]

    Liste_field = ['CO2air', 'DIR_SWdown', 'Flag_in_situ', 'Humrel', 'LWdown', 'NEB', 'PSurf',
                   'Qair', 'Rainf', 'SCA_SWdown', 'Snowf', 'Tair', 'Wind', 'Wind_DIR']
    data_xr = xr.DataArray(Tableau_retour_time_nbpoint,
                           coords={'time': Liste_date, 'fields': Liste_field}, dims=['time','fields'])

    return data_xr, Liste_date[0], Liste_date[-1]


def recup_safran(date_1, date_2, site):
    """
    Complete missing values of the MET file by Safran reanalysis

    :param site: site of interest (number of 8 figures with string format)
    :type site: str
    :param date_1: starting date for getting fields
    :type date_1: datetime.datetime
    :param date_2: ending date for getting fields
    :type date_2: datetime.datetime
    :returns: an xarray with all fields of interest
    """
    Liste_date = [date_1 + datetime.timedelta(seconds=x) for x in range(0, int((date_2 - date_1).total_seconds()),
                  pas_par_defaut)]
    date_b, date_e = get_file_period('FORCING', path_safran, date_1, date_2)

    forcing = path_safran + '/FORCING_' + date_b.strftime('%Y%m%d%H') + '_' + date_e.strftime('%Y%m%d%H') + '.nc'
    fic = prosimu(forcing)

    Tableau_retour_time_nbpoint = np.zeros((len(Liste_date), 14))
    # Dans l'ordre: CO2air, DIR_SWdown, Flag_in_situ, Humrel, LWdown, NEB, PSurf,
    #               Qair, Rainf, SCA_SWdown, Snowf, Tair, Wind, Wind_DIR

    index1 = np.where(fic.readtime() == date_1)[0][0]
    index2 = np.where(fic.readtime() == date_2)[0][0]
    point = np.where(fic.read_var('station') == int(site))[0][0]
    Tableau_retour_time_nbpoint[:, 0] = fic.read_var('CO2air', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 1] = fic.read_var('DIR_SWdown', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 2] = float(0)  # flag à 0 dans ce cas-là
    Tableau_retour_time_nbpoint[:, 3] = fic.read_var('HUMREL', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 4] = fic.read_var('LWdown', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 5] = fic.read_var('NEB', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 6] = fic.read_var('PSurf', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 7] = fic.read_var('Qair', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 8] = fic.read_var('Rainf', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 9] = fic.read_var('SCA_SWdown', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 10] = fic.read_var('Snowf', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 11] = fic.read_var('Tair', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 12] = fic.read_var('Wind', time=slice(index1, index2), Number_of_points=point)
    Tableau_retour_time_nbpoint[:, 13] = fic.read_var('Wind_DIR', time=slice(index1, index2), Number_of_points=point)
    fic.close()

    Liste_field = ['CO2air', 'DIR_SWdown', 'Flag_in_situ', 'Humrel', 'LWdown', 'NEB', 'PSurf',
                   'Qair', 'Rainf', 'SCA_SWdown', 'Snowf', 'Tair', 'Wind', 'Wind_DIR']
    xr_safran = xr.DataArray(Tableau_retour_time_nbpoint,
                             coords={'time': Liste_date, 'fields': Liste_field}, dims=['time','fields'])

    return xr_safran


def complete_obs_with_model_1period(filename, option_recup, date_entree_debut, date_entree_fin, site):
    """
    For a one year period (YYYY080106 - (YYYY+1)080106), return a complete file
    This file is made from the MET informations + eventually Psurf and WindDIR from cdp60mn database.
    Then, for the missing dates, this is completed with Safran reanalysis.

    :param filename: path to the MET file which is going to be completed
    :type filename: str
    :param site: site of interest (number of 8 figures with string format)
    :type site: str
    :param option_recup: PSurf et Wind DIR filled with database cdp60mn
    :type option_recup: boolean
    :param date_entree_debut: starting date of the year
    :type date_entree_debut: datetime.datetime
    :param date_entree_fin: ending date of the year
    :type date_entree_fin: datetime.datetime
    :returns: an xarray with all fields of interest
    """
    xr_MET, first_date, last_date = open_met_file_and_create_xr(filename, option_recup, site)

    if date_entree_debut < first_date:
        xr_safran = recup_safran(date_entree_debut, first_date, site)
        xr_MET = xr.concat([xr_safran,  xr_MET], dim='time')

    if date_entree_fin > last_date:
        xr_safran = recup_safran(last_date + timedelta(seconds=pas_par_defaut),date_entree_fin, site)
        xr_MET = xr.concat([xr_MET,  xr_safran], dim='time')

    xr_1period = xr_MET.drop_duplicates(dim='time')
    return xr_1period


def compilation_ttes_periodes(date_entree_debut, date_entree_fin, site, option_recup):
    """
    Compilation of all the different one_year period

    :param site: site of interest (number of 8 figures with string format)
    :type site: str
    :param option_recup: PSurf et Wind DIR filled with database cdp60mn
    :type option_recup: boolean
    :param date_entree_debut: starting date for the FORCING
    :type date_entree_debut: datetime.datetime
    :param date_entree_fin: ending date for the FORCING
    :type date_entree_fin: datetime.datetime
    :returns: an xarray with all fields of interest
    """
    date_first_met = datetime.datetime(1993, 8, 1, 6)  # EN DUR et fixe
    date_last_met = datetime.datetime(annee_last_MET, 8, 1, 6)
    list_path_met, list_date_debut, list_date_fin = decoupe_periode(date_entree_debut, date_entree_fin)
    Liste_field = ['CO2air', 'DIR_SWdown', 'Flag_in_situ', 'Humrel', 'LWdown', 'NEB', 'PSurf',
                   'Qair', 'Rainf', 'SCA_SWdown', 'Snowf', 'Tair', 'Wind', 'Wind_DIR']
    xr_out = xr.DataArray(np.empty((0,14)),coords={'time': [], 'fields': Liste_field}, dims=['time','fields'])

    for i in range(len(list_path_met)):
        print(str(list_date_debut[i]) + ' - ' + str(list_date_fin[i]))
        if list_date_debut[i] >= date_first_met and list_date_fin[i] <= date_last_met:
            xr_loop = complete_obs_with_model_1period(list_path_met[i], option_recup,
                                                         list_date_debut[i], list_date_fin[i], site)
        else:
            xr_loop = recup_safran(list_date_debut[i], list_date_fin[i], site)
        xr_out = xr.concat([xr_out, xr_loop], dim='time')

    xr_out.drop_duplicates(dim='time')

    return xr_out


def create_netcdf(output, xr, Tableau_valeurs_nbpoint):
    """
    Create NetCDF output file

    :param output: name of the output file
    :type output: str
    :param xr: fields of interest (PSurf, Rayt, Snowf, ...) 
    :type xr: xarray
    :param Tableau_valeurs_nbpoint: fields defining the site (LAT, LON, ZS, ...) 
    :type Tableau_valeurs_nbpoint: np array

    :returns: in a pythonic way, nothing (write a netCDF named out_met2netcdf.nc by default) 
    """
    newname = output
    fic_forcing = StandardCDP(newname, 'w', format='NETCDF4_CLASSIC')
    fic_forcing.createDimension('time', None)
    fic_forcing.createDimension('Number_of_points', 1)

    unit_time = 'seconds since 1970-01-01 00:00'
    # conversion from xarray time format (np.datetime64 to datetime.datetime
    dates = [check_and_convert_date(xr.time.values[i].astype(str)[:13]) for i in range(len(xr.time.values))]
    time = netCDF4.date2num(dates, unit_time)

    time_nc = fic_forcing.createVariable('time', 'd', ('time',), fill_value=-9999999)
    time_nc.units = unit_time
    time_nc[:] = time

    frc_nc = fic_forcing.createVariable('FRC_TIME_STP', 'f', fill_value=-9999999)
    frc_nc.units = 's'
    frc_nc[:] = pas_par_defaut

    Liste_nom_time_nbpoint = ['CO2air', 'DIR_SWdown', 'flag', 'HUMREL', 'LWdown', 'NEB', 'PSurf', 'Qair', 'Rainf',
                              'SCA_SWdown', 'Snowf', 'Tair', 'Wind', 'Wind_DIR']

    Liste_unite_time_nbpoint = ['kg/m3', 'W/m2', '0 or 1', '%', 'W/m2', 'between 0 and 1', 'Pa', 'Kg/Kg', 'kg/m2/s',
                                'W/m2', 'kg/m2/s', 'K', 'm/s', 'deg']

    Liste_longname_time_nbpoint = ['Near Surface CO2 Concentration', 'Surface Incident Direct Shortwave Radiation',
                                   'in situ flag', 'Relative Humidity', 'Surface Incident Longwave Radiation',
                                   'Nebulosity', 'Surface Pressure', 'Near Surface Specific Humidity',
                                   'Rainfall Rate', 'Surface Incident Diffuse Shortwave Radiation', 'Snowfall Rate',
                                   'Near Surface Air Temperature', 'Wind Speed', 'Wind Direction']

    Liste_nom_nbpoint = ['LAT', 'LON', 'UREF', 'ZREF', 'ZS', 'aspect', 'slope', 'station']
    Liste_unite_nbpoint = ['degrees_north', 'degrees_east', 'm', 'm', 'm', 'degrees from north',
                           'degrees from horizontal', '']
    Liste_longname_nbpoint = ['latitude', 'longitude', 'Reference_Height_for_Wind', 'Reference_Height', 'altitude',
                              'slope aspect', 'slope angle', 'OMM code of the station']

    for i in range(len(Liste_nom_time_nbpoint)):
        fic_nc = fic_forcing.createVariable(Liste_nom_time_nbpoint[i], 'f', ('time', 'Number_of_points'),
                                            fill_value=-9999999)
        fic_nc.units = Liste_unite_time_nbpoint[i]
        fic_nc.long_name = Liste_longname_time_nbpoint[i]
        fic_nc[:] = xr.data[:,i]

    for i in range(len(Liste_nom_nbpoint)):
        fic_nc = fic_forcing.createVariable(Liste_nom_nbpoint[i], 'f', ('Number_of_points',), fill_value=-9999999)
        fic_nc.units = Liste_unite_nbpoint[i]
        fic_nc.long_name = Liste_longname_nbpoint[i]
        fic_nc[:] = Tableau_valeurs_nbpoint[i]

    fic_forcing.close()


def parseArguments():
    # Create argument parser
    parser = argparse.ArgumentParser()

    # Optional arguments
    parser.add_argument('-o', '--output', help='Name for output file', type=str, default='out_met2netcdf.nc')
    parser.add_argument('-c', '--constant', help='PSurf and Wind_DIR are constant', action='store_true')
    parser.add_argument('-b', '--begin', help='Beginning date for nc file', type=str, default='1993080106')
    parser.add_argument('-e', '--end', help='Ending date for nc file', type=str, default='2023080106')
    parser.add_argument('-s', '--site', help='Site location', type=str, default='38472401')
    parser.add_argument('--one_file', help='Create forcing for one MET file', action='store_true')
    parser.add_argument('-p', '--path_MET', required='--one_file' in sys.argv, help='Path of the only MET', type=str)

    # Print version
    parser.add_argument('--version', action='version', version='%(prog)s - Version 1.0')
    # Parse arguments
    args = parser.parse_args()

    return args


if __name__ == '__main__':
    # Recuperer les  arguments
    args = parseArguments()
    pathout = args.output
    option_recup = not args.constant # By default, we get PSurf and Wind DIR from cdp60mn database
    date_entree_debut = check_and_convert_date(str(args.begin))
    date_entree_fin = check_and_convert_date(str(args.end))
    site = args.site
    # Construire les donnees et les editer
    Tab_point, name = recup_donnees_site(site)
    message_accueil(option_recup, name)
    if args.one_file:
        xr, date_beg, date_end = open_met_file_and_create_xr(args.path_MET, option_recup, site)
        create_netcdf('FORCING_'+ date_beg.ymdh + '_' + date_end.ymdh + '.nc', xr, Tab_point)
    else:
        xr_compil = compilation_ttes_periodes(date_entree_debut, date_entree_fin, site, option_recup)
        create_netcdf(pathout, xr_compil, Tab_point)
