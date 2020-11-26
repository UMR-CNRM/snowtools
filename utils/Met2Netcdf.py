# -*- coding: utf-8 -*-
# First version met file (format printable .prn or .txt) to netcdf

import sys
import datetime
import psycopg2
import numpy as np
import netCDF4
import argparse
from utils.S2M_standard_file import StandardCDP
from utils.resources import get_file_period
from utils.prosimu import prosimu
from datetime import timedelta
from dateutil.relativedelta import relativedelta
from utils.dates import check_and_convert_date
from bronx.meteo.thermo import Thermo
from bronx.meteo.constants import T0
from utils.infomassifs import infomassifs


# Tester avec par exemple: python Met2Netcdf.py -b 2000080106 -e 2001080106

def recup_donnees_site(nom_site):
    IM = infomassifs()
    LAT, LON, ZS, UREF, ZREF, aspect, slope, name = IM.infoposte_extensive(nom_site)
    Tab_point = np.zeros(8)
    Tab_point[0] = float(LAT)
    Tab_point[1] = float(LON)
    Tab_point[2] = float(UREF)
    Tab_point[3] = float(ZREF)
    Tab_point[4] = float(ZS)
    Tab_point[5] = float(aspect)
    Tab_point[6] = float(slope)
    Tab_point[7] = int(nom_site)
    return Tab_point, name

def message_accueil(option_bool, nom_site):
    if option_bool:
        print('\nCompletion des donnees MET avec Pression et Direction du vent du site ' + str(nom_site) +'\n')
    print('#########################################################')
    print('# A verifier/modifier dans le code:')
    print('# - chemin des MET: /mesure_data/col_de_porte/met\n')
    print('# - format des MET: MET_YYYY_YYYY+1_fmt (ex MET_2000_2001_fmt)\n')
    print('# - chemin des reanalyses SAFRAN: /era40/vortex/s2m/postes/reanalysis/meteo\n')
    print('# - etat de la base de donnees du Col de Porte: cdp60mn_1819 puis cdp60mn a partir du 01082019\n')
    print('# ! la base CdP change a priori tous les ans ! => modifier la seconde ligne de la fonction recup_cdp (vers ligne 165-170)')
    print('# Si vous faites des modifs, penser aussi à changer ce message (fonction message_accueil)')
    print('#########################################################')

def parseArguments():
    # Create argument parser
    parser = argparse.ArgumentParser()

    # Optional arguments
    parser.add_argument("-o", "--output", help="Name for output file", type=str, default='out.nc')
    parser.add_argument('-r', "--recup", help="Option for recup PSurf in database.", type=lambda x: (str(x).lower() in ['true', 'yes', '1']), default=True)
    parser.add_argument("-b", "--begin", help="Beginning date for nc file", type=str, default ='1993080106')
    parser.add_argument("-e", "--end", help="Ending date for nc file", type=str, default ='2019080106')
    parser.add_argument("-s", "--site", help="Site location", type=str, default='38472401')

    # Print version
    parser.add_argument("--version", action="version", version='%(prog)s - Version 1.0')
    # Parse arguments
    args = parser.parse_args()

    return args

def decoupe_periode(date_entree_debut, date_entree_fin):
    # Separe la periode de dates en periodes du type entree - 2000080106 puis 2000080106 - 2001080106 puis 2001080106 - 2002080106 puis...
    # En parallèle, donne la liste des fichiers MET correspondants
    # !!! Format des fichiers MET supposé fixe: MET_1996_1997_fmt
    first_year = date_entree_debut.year
    path = '/mesure_data/col_de_porte/met/' # EN DUR

    list_date_debut = [date_entree_debut]
    if date_entree_debut < datetime.datetime(first_year,8,1,6):
        list_path_met = [path + 'MET_' + str(first_year - 1) + '_' + str(first_year) + '_fmt']
        list_date_fin = [datetime.datetime(first_year,8,1,6)]
    else:
        list_path_met = [path + 'MET_' + str(first_year) + '_' + str(first_year + 1) + '_fmt']
        list_date_fin = [datetime.datetime(first_year + 1,8,1,6)]

    date_tour = date_entree_debut
    while (date_tour + relativedelta(months = +12)) < date_entree_fin:
        list_date_debut.append(list_date_fin[-1])
        list_date_fin.append(list_date_fin[-1] + relativedelta(months = +12))
        path_add = list_path_met[-1].replace(list_path_met[-1][-8:-4], str(int(list_path_met[-1][-8:-4])+1))
        path_add = path_add.replace(list_path_met[-1][-13:-9], str(int(list_path_met[-1][-13:-9])+1))
        list_path_met.append(path_add)
        date_tour = date_tour + relativedelta(months = +12)

    if list_date_fin[-1] < date_entree_fin:
        list_date_debut.append(list_date_fin[-1])
        list_date_fin.append(list_date_fin[-1] + relativedelta(months = +12))
        path_add = list_path_met[-1].replace(list_path_met[-1][-8:-4], str(int(list_path_met[-1][-8:-4])+1))
        path_add = path_add.replace(list_path_met[-1][-13:-9], str(int(list_path_met[-1][-13:-9])+1))
        list_path_met.append(path_add)

    if list_date_fin[-1] > date_entree_fin:
        list_date_fin[-1] = date_entree_fin

    return list_path_met, list_date_debut, list_date_fin

def read_info_met(filename, date_debut, date_fin):
    # suppose que les lignes de l'entête ne commencent pas par un chiffre
    # gère deux formats de date: YYYYMMDDHHMMSS (et plus petit YYYYMMDDHH si assez d'espaces pour ne pas lire la suite)
    #                            DD/MM/YYYY HH:MM:SS (et plus petit DD/MM/YYYY HH si assez d'espaces pour ne pas lire la suite)
    metfile = open(filename,'r')
    fileLines = metfile.readlines()
    metfile.close()
    last_line = fileLines[len(fileLines)-1]
    passage1 = False
    passage2 = False
    nb_ligne_entete = 0
    for line in fileLines:
        if line[0] not in {'0','1','2','3','4','5','6','7','8','9'}:
            nb_ligne_entete = nb_ligne_entete + 1
            continue
        elif (line[2] == '/' and passage1 == False):
            date_debut_met = int(line[6:10] + line[3:5] + line[0:2] + line[11:13] + line[14:16] + line[17:19])
            passage1 = True
        elif (line[2] != '/' and passage1 == False):
            date_debut_met = int(line[0:13])
            passage1 = True
        elif (line[2] == '/' and passage1 and passage2 == False):
            date2_met = int(line[6:10] + line[3:5] + line[0:2] + line[11:13] + line[14:16] + line[17:19])
            last_date_met = int(last_line[6:10] + last_line[3:5] + last_line[0:2] + last_line[11:13] + last_line[14:16] + last_line[17:19])
            passage2 = True
        elif (line[2] != '/' and passage1 and passage2 == False):
            date2_met = int(line[0:13])
            last_date_met = int(last_line[0:13])
            passage2 = True
        elif passage1 and passage2:
            break

    # for special case DD/MM/YYYY HH:M
    if len(str(date_debut_met))%2 != 0:
        date_debut_met = int(date_debut_met/10)
    if len(str(date2_met))%2 != 0:
        date2_met = int(date2_met/10)
    if len(str(last_date_met))%2 != 0:
        last_date_met = int(last_date_met/10)

    pas_de_temps = int( ( check_and_convert_date(str(date2_met)) - check_and_convert_date(str(date_debut_met)) ).total_seconds() )

    if date_debut > check_and_convert_date(str(date_debut_met)):
        rajout_entete = (date_debut - check_and_convert_date(str(date_debut_met))).total_seconds() / pas_de_temps
        date_sortie_debut = date_debut
    else:
        rajout_entete = 0
        date_sortie_debut = check_and_convert_date(str(date_debut_met))

    if date_fin < check_and_convert_date(str(last_date_met)):
        pied_page = (check_and_convert_date(str(last_date_met)) - date_fin).total_seconds() / pas_de_temps
        date_sortie_fin = date_fin
    else:
        pied_page = 0
        date_sortie_fin = check_and_convert_date(str(last_date_met))

    L = [pas_de_temps, int(nb_ligne_entete + rajout_entete), int(pied_page), date_sortie_debut, date_sortie_fin]
    return L

def recup_cdp(date_time_deb, date_time_fin, pas_de_temps):
    # Récupération sur les bases de données CDP:
    # Structure des bases: cdp60mn pour l'année en cours et un peu plus. Ensuite, mis dans cdp9293, cdp9394 etc... jusqu'à cdp1415
    # A partir de la saison 15-16, chgt de nom de base cdp60mn_1516 et changement de nom des variables
    # Au 4 septembre 2019, on est à cdp60mn_1617 jusqu'au 1er août 2017 et ensuite sur cdp60mn mais la bascule de données vers
    # cdp60mn_1718 est proche !!
    date_change_2015 = check_and_convert_date(str(2015080100)) # EN DUR et fixe
    date_change_base60mn = check_and_convert_date(str(2019080100)) # EN DUR mais doit changer à chaque bascule de données de cdp60mn vers cdp60mn_****

    ###############################################################################################
    # A partir du MET_2015_2016, on a des années complètes 1er août 00h vers 1er août 00h pour les met
    # Le stockage dans les bases de données cdp*** correspond
    # Par contre, les FORCING sont du 1er août 6h vers 1er août 6h
    # Il faut ainsi gérer le cas où la date fin du fichier MET dépasse le 1er août minuit (future norme ?)
    date_1er_aout = datetime.datetime(date_time_fin.year,8,1,0,0)

    # Cas "standards": date_time_fin avant le 1er aout 00h
    if (date_change_2015 >= date_time_fin) and (date_time_fin <= date_1er_aout):
        nom_base = 'cdp' + str(date_time_fin.year - 1)[2:4] + str(date_time_fin.year)[2:4]
        nom_var = 'baro,dvent'
    elif (date_change_base60mn >= date_time_fin) and (date_time_fin <= date_1er_aout):
        nom_base = 'cdp60mn_' + str(date_time_fin.year - 1)[2:4] + str(date_time_fin.year)[2:4]
        nom_var = 'p_ptb330_v1,dd_meca_10mn_10m'
    elif (date_time_fin <= date_1er_aout):
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
    command= "select dat," + nom_var +" from bdniv." + nom_base +" where dat between '{0}' and '{1}' \
    order by dat".format(date_time_deb,date_time_fin)

    host = 'cenexp2.cen.meteo.fr'
    database = 'bdniv'
    user ='consult'
    port = '5433'
    conn= psycopg2.connect(database=database, user=user, host = host, port=port)
    cursor= conn.cursor()
    cursor.execute(command)
    data = np.array(cursor.fetchall())

    # Cas où YYYY080106 => ensuite récupérer les données sur l'année en cours
    # a) récupérer dans la base de données d'après les 5h manquantes
    # b) concaténer les données.
    if (date_time_fin > date_1er_aout and nom_base != 'cdp60mn'):
        nom_var = 'p_ptb330_v1,dd_meca_10mn_10m'
        if date_time_fin.year < date_change_base60mn.year:
            nom_base = 'cdp60mn_' + str(date_time_fin.year)[2:4] + str(date_time_fin.year + 1)[2:4]
        else:
            nom_base = 'cdp60mn'

        # command est une requete SQL (en string)
        command= "select dat," + nom_var +" from bdniv." + nom_base +" where dat between '{0}' and '{1}' \
        order by dat".format(date_time_deb,date_time_fin)

        host = 'cenexp2.cen.meteo.fr'
        database = 'bdniv'
        user ='consult'
        port = '5433'
        conn= psycopg2.connect(database=database, user=user, host = host, port=port)
        cursor= conn.cursor()
        cursor.execute(command)
        data_supp = np.array(cursor.fetchall())

        # concatenation en enlevant une ligne pour éviter de dupliquer la valeur YYYY080100
        if data.shape == (0,):
            data = np.empty((0,3))
        if data_supp.shape == (0,):
            data_supp = np.empty((0,3))
        data = np.vstack((data,data_supp[1:,:]))

    # correction frustre des données pression + changement d'unité (hPa en Pa)
    if (len(data[:])  != 0):
        for i in range(len(data[:,0])):
            if data[i,1] != None:
                if data[i,1] > 890:
                    data[i,1] = 863.827
                if data[i,1] < 840:
                    data[i,1] = 863.827
                data[i,1] = 100 * data[i,1]
            else:
                data[i,1] = 86382.7
        # correction frustre des données de direction du vent       
            if data[i,2] != None:
                if data[i,2] < 0:
                    data[i,2] = 0
                if data[i,2] > 360:
                    data[i,2] = 360
            else:
                data[i,2] = 0
                    
    # correction frustre des données pression + changement d'unité (hPa en Pa) ne marche pas en python3 ?
    #if (len(data[:])  != 0):
        #data[:,1] = np.where(data[:,1] > 890, 863.827, data[:,1])
        #data[:,1] = np.where(data[:,1] < 840, 863.827, data[:,1])
        #data[:,1] = 100. * data[:,1].astype(float)

        # correction frustre des données de direction du vent ne marche pas en python3 ?
        #data[:,2] = np.where(data[:,2] < 0, 0, data[:,2])
        #data[:,2] = np.where(data[:,2] > 360, 360, data[:,2])

    return data

def open_met_file_and_create_tab(filename, option_recup, date_entree_debut, date_entree_fin, site):
    # Les constantes à mettre:
    CO2air = 0.00062
    IM = infomassifs()
    LAT, LON, ZS, UREF, ZREF, aspect, slope, name = IM.infoposte_extensive(site)

    PSurf_cst = round(101325 * ((288 - 0.0065 * ZS)/288)**5.255,1)
    L = read_info_met(filename, date_entree_debut, date_entree_fin)
    print(str(date_entree_debut) + ' - ' + str(date_entree_fin))

    pas_de_temps_met = L[0]
    nb_ligne_entete = L[1]
    nb_ligne_bas = L[2]
    first_date = L[3]
    last_date = L[4]

    try:
        metfile = open(filename,'r')
    except:
        print("input meteorological file " + metfile + " not found")
        sys.exit()

    # Les fichiers sont générés avec possiblement pas le bon nombre d'espaces
    # Les MET à partir de 2016_2017 => 10 champs
    # date          tempe       wind     humrel   precip    phase    LWdown   DIR_SWdown  SCA_SWdown  NEB
    #2017080100      20.0       1.7        45      0.00      0.00    334.40      0.47      1.31      0.00

    # Les MET de 2015_2016 => 12 champs
    #  date (2 champs)    nrart     Tair        ff        RH        RR     phase        LW     SWdir     SWdif       Neb
    #01/08/2015 00:0         1      12.2       0.9        90       0.8       0.0     355.3       0.0       0.0       0.8

    # Les MET avant 2014_2015 (inclus) => 11 champs
    # Date          ART.  T   Vent Hum RRtot SS/RRtot IR  Soldr Soldf Neb
    #2014092000      1  12.8  0.6  95  0.80 0.00   357.8   0.0   0.0 0.79
    if nb_ligne_bas > 0:
        fileLines=metfile.readlines()[nb_ligne_entete:-nb_ligne_bas]
    else:
        fileLines=metfile.readlines()[nb_ligne_entete:]
    metfile.close()

    #File_entree = np.genfromtxt(fileLines, dtype='string')
    File_entree = np.loadtxt(fileLines,dtype='str')
    
    if sys.version_info[0] == 3:
        for i in range(len(File_entree[:,0])):
            for j in range(len(File_entree[0,:])):
                File_entree[i,j] = File_entree[i,j][2:-1]
        '''if 'MET_2015' in filename: 
            for i in range(len(File_entree[:,0])):
                File_entree[i,0] = File_entree[i,0][:-2]'''
    
    
    Tableau_retour_time_nbpoint = np.zeros((len(fileLines),14))
    # Dans l'ordre: CO2air, DIR_SWdown, Flag_in_situ, Humrel, LWdown, NEB, PSurf, Qair, Rainf, SCA_SWdown, Snowf, Tair, Wind, Wind_DIR

    # Construction Liste_date pour compléter éventuellement les data du cdp
    Liste_date = []
    if len(fileLines) != 0:
        if len(File_entree[0,:]) == 10:
            for i in range(len(File_entree[:,0])):
                Liste_date.append(check_and_convert_date(File_entree[i,0]))
        elif len(File_entree[0,:]) == 11:
            for i in range(len(File_entree[:,0])):
                Liste_date.append(check_and_convert_date(File_entree[i,0] ))
        elif len(File_entree[0,:]) == 12:
            for i in range(len(File_entree[:,0])):
                line = File_entree[i,0]
                hour = File_entree[i,1]
                date = line[6:10] + line[3:5] + line[0:2] + hour[0:2]
                Liste_date.append(check_and_convert_date(date))

        # Recuperation de PSurf et Wind_DIR sur table CdP, sinon, valeurs par défaut
        if option_recup and site =='38472401':
            data_cdp = recup_cdp(first_date,last_date,pas_de_temps_met)
            # traitement du cas où des valeurs sont absentes de la table CdP
            if len(data_cdp[:]) == 0:
                Wind_DIR = np.zeros((len(fileLines)))
                PSurf = np.ones((len(fileLines)))*PSurf_cst
            elif len(data_cdp[:,0]) < len(fileLines):
                Tab = np.zeros((len(fileLines),2))
                Tab[:,0] = PSurf_cst
                index = list(np.searchsorted(Liste_date,data_cdp[:,0]))
                Tab[index,0] = data_cdp[:,1].astype(float)
                Tab[index,1] = data_cdp[:,2].astype(float)
                PSurf = Tab[:,0]
                Wind_DIR = Tab[:,1]
            else:
                PSurf = data_cdp[:,1].astype(float)
                Wind_DIR = data_cdp[:,2].astype(float)
        else:
            Wind_DIR = np.zeros((len(fileLines)))
            PSurf = np.ones((len(fileLines)))*PSurf_cst

        if len(File_entree[0,:]) == 10:
            for i in range(len(File_entree[:,0])):
                #Tableau_retour_time_nbpoint[i,7] = HUMrelHUMspec(float(File_entree[i,1]) + T0, float(File_entree[i,3]), PSurf[i])
                Tableau_retour_time_nbpoint[i,7] = Thermo(['v', 'c'], dict(P=PSurf[i], Huw=float(File_entree[i,3]), T=float(File_entree[i,1]) + T0, rc=0)).get('qv')
                Tableau_retour_time_nbpoint[i,8] = float(File_entree[i,4])*(1 - float(File_entree[i,5])) / pas_de_temps_met
                Tableau_retour_time_nbpoint[i,10] = float(File_entree[i,4])*float(File_entree[i,5]) / pas_de_temps_met

            Tableau_retour_time_nbpoint[:,0] = float(CO2air)
            Tableau_retour_time_nbpoint[:,1] = File_entree[:,7].astype(float)
            Tableau_retour_time_nbpoint[:,2] = float(1) #flag à 1 dans ce cas-là
            Tableau_retour_time_nbpoint[:,3] = File_entree[:,3].astype(float)
            Tableau_retour_time_nbpoint[:,4] = File_entree[:,6].astype(float)
            Tableau_retour_time_nbpoint[:,5] = File_entree[:,9].astype(float)
            Tableau_retour_time_nbpoint[:,6] = PSurf[:]
            Tableau_retour_time_nbpoint[:,9] = File_entree[:,8].astype(float)
            Tableau_retour_time_nbpoint[:,11] = File_entree[:,1].astype(float) + T0
            Tableau_retour_time_nbpoint[:,12] = File_entree[:,2].astype(float)
            Tableau_retour_time_nbpoint[:,13] = Wind_DIR[:]

        elif len(File_entree[0,:]) == 11:
            for i in range(len(File_entree[:,0])):
                #Tableau_retour_time_nbpoint[i,7] = HUMrelHUMspec(float(File_entree[i,2]) + T0, float(File_entree[i,4]), PSurf[i])
                Tableau_retour_time_nbpoint[i,7] = Thermo(['v', 'c'], dict(P=PSurf[i], Huw=float(File_entree[i,4]), T=float(File_entree[i,2]) + T0, rc=0)).get('qv')
                Tableau_retour_time_nbpoint[i,8] = float(File_entree[i,5])*(1 - float(File_entree[i,6])) / pas_de_temps_met
                Tableau_retour_time_nbpoint[i,10] = float(File_entree[i,5])*float(File_entree[i,6]) / pas_de_temps_met
                year = int(File_entree[i,0][0:4])
                month = int(File_entree[i,0][4:6])
                if year < 2010 or (year == 2010 and month <= 11):
                    Tableau_retour_time_nbpoint[i,4] = float(File_entree[i,7]) - 10.0
                elif year < 2015 or (year == 2015 and month <= 7):
                    Tableau_retour_time_nbpoint[i,4] = float(File_entree[i,7]) + 10.0
                else:
                    Tableau_retour_time_nbpoint[i,4] = float(File_entree[i,7])

            Tableau_retour_time_nbpoint[:,0] = float(CO2air)
            Tableau_retour_time_nbpoint[:,1] = File_entree[:,8].astype(float)
            Tableau_retour_time_nbpoint[:,2] = float(1) #flag à 1 dans ce cas-là
            Tableau_retour_time_nbpoint[:,3] = File_entree[:,4].astype(float)
            Tableau_retour_time_nbpoint[:,5] = File_entree[:,10].astype(float)
            Tableau_retour_time_nbpoint[:,6] = PSurf[:]
            Tableau_retour_time_nbpoint[:,9] = File_entree[:,9].astype(float)
            Tableau_retour_time_nbpoint[:,11] = File_entree[:,2].astype(float) + T0
            Tableau_retour_time_nbpoint[:,12] = File_entree[:,3].astype(float)
            Tableau_retour_time_nbpoint[:,13] = Wind_DIR[:]

        elif len(File_entree[0,:]) == 12:
            for i in range(len(File_entree[:,0])):
                #Tableau_retour_time_nbpoint[i,7] = HUMrelHUMspec(float(File_entree[i,3]) + T0, float(File_entree[i,5]), PSurf[i])
                Tableau_retour_time_nbpoint[i,7] = Thermo(['v', 'c'], dict(P=PSurf[i], Huw=float(File_entree[i,5]), T=float(File_entree[i,3]) + T0, rc=0)).get('qv')
                Tableau_retour_time_nbpoint[i,8] = float(File_entree[i,6])*(1 - float(File_entree[i,7])) / pas_de_temps_met
                Tableau_retour_time_nbpoint[i,10] = float(File_entree[i,6])*float(File_entree[i,7]) / pas_de_temps_met

            Tableau_retour_time_nbpoint[:,0] = float(CO2air)
            Tableau_retour_time_nbpoint[:,1] = File_entree[:,9].astype(float)
            Tableau_retour_time_nbpoint[:,2] = float(1) #flag à 1 dans ce cas-là
            Tableau_retour_time_nbpoint[:,3] = File_entree[:,5].astype(float)
            Tableau_retour_time_nbpoint[:,4] = File_entree[:,8].astype(float)
            Tableau_retour_time_nbpoint[:,5] = File_entree[:,11].astype(float)
            Tableau_retour_time_nbpoint[:,6] = PSurf[:]
            Tableau_retour_time_nbpoint[:,9] = File_entree[:,10].astype(float)
            Tableau_retour_time_nbpoint[:,11] = File_entree[:,3].astype(float) + T0
            Tableau_retour_time_nbpoint[:,12] = File_entree[:,4].astype(float)
            Tableau_retour_time_nbpoint[:,13] = Wind_DIR[:]

    return Liste_date, Tableau_retour_time_nbpoint

def recup_safran(date_1,date_2,pas, site):
    path = '/era40/vortex/s2m/postes/reanalysis/meteo/' # EN DUR

    Liste_date = [date_1 + datetime.timedelta(seconds=x) for x in range(0, int((date_2 - date_1).total_seconds()),pas)]
    date_b, date_e = get_file_period('FORCING', path, date_1, date_2)


    forcing = path + '/FORCING_' + date_b.strftime('%Y%m%d%H') + '_' + date_e.strftime('%Y%m%d%H') + '.nc'
    fic = prosimu(forcing)

    Tableau_retour_time_nbpoint = np.zeros((len(Liste_date),14))
    # Dans l'ordre: CO2air, DIR_SWdown, Flag_in_situ, Humrel, LWdown, NEB, PSurf, Qair, Rainf, SCA_SWdown, Snowf, Tair, Wind, Wind_DIR

    index1 = np.where( fic.readtime() == date_1 )[0][0]
    index2 = np.where( fic.readtime() == date_2 )[0][0]
    point = np.where(fic.read_var('station') == int(site))[0][0]
    Tableau_retour_time_nbpoint[:,0] = fic.read_var('CO2air', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,1] = fic.read_var('DIR_SWdown', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,2] = float(0) #flag à 0 dans ce cas-là
    Tableau_retour_time_nbpoint[:,3] = fic.read_var('HUMREL', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,4] = fic.read_var('LWdown', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,5] = fic.read_var('NEB', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,6] = fic.read_var('PSurf', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,7] = fic.read_var('Qair', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,8] = fic.read_var('Rainf', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,9] = fic.read_var('SCA_SWdown', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,10] = fic.read_var('Snowf', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,11] = fic.read_var('Tair', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,12] = fic.read_var('Wind', time = slice(index1,index2), Number_of_points = point )
    Tableau_retour_time_nbpoint[:,13] = fic.read_var('Wind_DIR', time = slice(index1,index2), Number_of_points = point )
    fic.close()
    return Liste_date, Tableau_retour_time_nbpoint

def recup_last_value_safran(date,site,date_1):
    path = '/era40/vortex/s2m/postes/reanalysis/meteo/'
    date_b, date_e = get_file_period('FORCING', path, date_1,date)
    forcing = path +'/FORCING_' + date_b.strftime('%Y%m%d%H') + '_' + date_e.strftime('%Y%m%d%H') + '.nc'
    fic = prosimu(forcing)
    Tab_retour = np.zeros((1,14))
    index = np.where(fic.readtime() == date )[0][0]
    point = np.where(fic.read_var('station') == int(site))[0][0]
    Tab_retour[0,0] = fic.read_var('CO2air', time = index, Number_of_points = point)
    Tab_retour[0,1] = fic.read_var('DIR_SWdown', time = index, Number_of_points = point)
    Tab_retour[0,2] = float(0) # flag à 0 dans ce cas là
    Tab_retour[0,3] = fic.read_var('HUMREL', time = index, Number_of_points = point)
    Tab_retour[0,4] = fic.read_var('LWdown', time = index, Number_of_points = point)
    Tab_retour[0,5] = fic.read_var('NEB', time = index, Number_of_points = point)
    Tab_retour[0,6] = fic.read_var('PSurf', time = index, Number_of_points = point)
    Tab_retour[0,7] = fic.read_var('Qair', time = index, Number_of_points = point)
    Tab_retour[0,8] = fic.read_var('Rainf', time = index, Number_of_points = point)
    Tab_retour[0,9] = fic.read_var('SCA_SWdown', time = index, Number_of_points = point)
    Tab_retour[0,10] = fic.read_var('Snowf', time = index, Number_of_points = point)
    Tab_retour[0,11] = fic.read_var('Tair', time = index, Number_of_points = point)
    Tab_retour[0,12] = fic.read_var('Wind', time = index, Number_of_points = point)
    Tab_retour[0,13] = fic.read_var('Wind_DIR', time = index, Number_of_points = point)
    fic.close()
    return date, Tab_retour

def complete_obs_with_model_1period(filename, option_recup, date_entree_debut, date_entree_fin, site):
    Liste_dates, Tableau_valeurs_time_nbpoint = open_met_file_and_create_tab(filename,option_recup, date_entree_debut, date_entree_fin, site)
    L = read_info_met(filename, date_entree_debut, date_entree_fin)
    pas_de_temps_met = L[0]
    date_MET_debut = L[3]
    date_MET_fin =L[4]

    if date_entree_debut < date_MET_debut:
        first_date = date_entree_debut
        Liste_avant, Tableau_avant = recup_safran(date_entree_debut, date_MET_debut, pas_de_temps_met, site)
        Liste_avant.extend(Liste_dates)
        Liste_dates = Liste_avant
        Tableau_valeurs_time_nbpoint = np.vstack((Tableau_avant,Tableau_valeurs_time_nbpoint))
    else:
        first_date = date_MET_debut

    if date_entree_fin > date_MET_fin:
        if date_MET_fin < date_MET_debut:
            date_MET_fin = date_MET_debut
        Liste_apres, Tableau_apres = recup_safran(date_MET_fin + timedelta(seconds = pas_de_temps_met), date_entree_fin, pas_de_temps_met, site)
        Liste_dates.extend(Liste_apres)
        Tableau_valeurs_time_nbpoint = np.vstack((Tableau_valeurs_time_nbpoint,Tableau_apres))

    return Liste_dates, Tableau_valeurs_time_nbpoint, pas_de_temps_met

def compilation_ttes_periodes(date_entree_debut, date_entree_fin, site, option_recup):
    date_first_met = datetime.datetime(1993,8,1,6) # EN DUR
    date_last_met = datetime.datetime(2020,8,1,6) # EN DUR
    list_path_met, list_date_debut, list_date_fin = decoupe_periode(date_entree_debut, date_entree_fin)
    pas = 3600 # EN DUR
    L=[]
    Tab_time_point = np.empty((0,14))
    for i in range(len(list_path_met)):
        if list_date_debut[i] >= date_first_met and list_date_fin[i] <= date_last_met:
            L_dates_1, Tab_time_nbpoint_1, pas_1 = complete_obs_with_model_1period(list_path_met[i], option_recup, list_date_debut[i], list_date_fin[i], site)
            pas = pas_1
        else:
            print(str(list_date_debut[i]) + ' - ' + str(list_date_fin[i]))
            L_dates_1, Tab_time_nbpoint_1 = recup_safran(list_date_debut[i], list_date_fin[i], pas, site)

        L.extend(L_dates_1)
        Tab_time_point = np.vstack((Tab_time_point,Tab_time_nbpoint_1))

    # Rajouter test de fin entre date_entree_fin et L[-1] a l'interieur de la fonction
    if L[-1] != date_entree_fin:
        dat, Tab = recup_last_value_safran(date_entree_fin,site,list_date_debut[-1])
        L.extend([dat])
        Tab_time_point = np.vstack((Tab_time_point, Tab))

    return L, Tab_time_point, pas

def create_netcdf(output, Liste_dates, Tableau_valeurs_time_nbpoint, Tableau_valeurs_nbpoint, first_date, pas):
    newname = output
    fic_forcing = StandardCDP(newname, 'w',format='NETCDF4_CLASSIC')
    fic_forcing.createDimension('time',None)
    fic_forcing.createDimension('Number_of_points',1)

    unit_time = 'seconds since '+ str(first_date)
    time = netCDF4.date2num(Liste_dates,unit_time)

    time_nc = fic_forcing.createVariable('time','d',('time',),fill_value=-9999999)
    time_nc.units = unit_time
    time_nc[:] = time

    frc_nc = fic_forcing.createVariable('FRC_TIME_STP','f',fill_value=-9999999)
    frc_nc.units = 's'
    frc_nc[:] = pas

    Liste_nom_time_nbpoint = ['CO2air', 'DIR_SWdown', 'flag', 'HUMREL', 'LWdown', 'NEB', 'PSurf', 'Qair', 'Rainf', 'SCA_SWdown', 'Snowf',
                 'Tair', 'Wind', 'Wind_DIR']
    Liste_unite_time_nbpoint = [ 'kg/m3', 'W/m2', '0 or 1', '%', 'W/m2', 'between 0 and 1', 'Pa', 'Kg/Kg', 'kg/m2/s', 'W/m2', 'kg/m2/s', 'K', 'm/s', 'deg']
    Liste_longname_time_nbpoint = ['Near Surface CO2 Concentration', 'Surface Incident Direct Shortwave Radiation', 'in situ flag',
                      'Relative Humidity', 'Surface Incident Longwave Radiation', 'Nebulosity', 'Surface Pressure', 'Near Surface Specific Humidity',
                      'Rainfall Rate', 'Surface Incident Diffuse Shortwave Radiation', 'Snowfall Rate','Near Surface Air Temperature',
                      'Wind Speed', 'Wind Direction']

    Liste_nom_nbpoint = ['LAT', 'LON', 'UREF', 'ZREF', 'ZS', 'aspect', 'slope', 'station']
    Liste_unite_nbpoint = [ 'degrees_north', 'degrees_east', 'm', 'm', 'm', 'degrees from north', 'degrees from horizontal', '']
    Liste_longname_nbpoint = ['latitude', 'longitude', 'Reference_Height_for_Wind', 'Reference_Height', 'altitude',
                               'slope aspect', 'slope angle', 'OMM code of the station']

    for i in range(len(Liste_nom_time_nbpoint)):
        fic_nc = fic_forcing.createVariable(Liste_nom_time_nbpoint[i],'f', ('time','Number_of_points'), fill_value=-9999999)
        fic_nc.units = Liste_unite_time_nbpoint[i]
        fic_nc.long_name = Liste_longname_time_nbpoint[i]
        fic_nc[:] = Tableau_valeurs_time_nbpoint[:,i]

    for i in range(len(Liste_nom_nbpoint)):
        fic_nc = fic_forcing.createVariable(Liste_nom_nbpoint[i],'f', ('Number_of_points',), fill_value=-9999999)
        fic_nc.units = Liste_unite_nbpoint[i]
        fic_nc.long_name = Liste_longname_nbpoint[i]
        fic_nc[:] = Tableau_valeurs_nbpoint[i]

    fic_forcing.close()

if __name__ == '__main__':
    # Recuperer les  arguments
    args = parseArguments()
    pathout = args.output
    option_recup = args.recup
    date_entree_debut = check_and_convert_date(str(args.begin))
    date_entree_fin = check_and_convert_date(str(args.end))
    site = args.site
    # Construire les donnees et les editer
    Tab_point, name = recup_donnees_site(site)
    message_accueil(option_recup, name)
    L, Tab_time_point, pas = compilation_ttes_periodes(date_entree_debut, date_entree_fin, site, option_recup)
    create_netcdf(pathout, L, Tab_time_point, Tab_point, date_entree_debut, pas)

