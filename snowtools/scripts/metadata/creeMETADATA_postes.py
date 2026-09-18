#!/usr/bin/env python3
# -*- coding: utf-8 -*

import os
import sys
import re
import csv

from snowtools.utils.infomassifs import infomassifs
from snowtools.utils.FileException import FileNameException, FileOpenException
from snowtools.DATA import SNOWTOOLS_DIR
################################################################################################
#                                                                                              #
#  I/  LECTURE DES SITES DANS UN FICHIER                                                       #
#                                                                                              #
################################################################################################


if __name__ == "__main__":

    if len(sys.argv) != 1:
        sys.exit("USAGE python creeMETADATA_postes.py")

    IXML=infomassifs()

    listeSites = []

    name = {}
    lat = {}
    lon = {}
    alti = {}
    slope = {}
    aspect = {}
    massif = {}

    namefile = SNOWTOOLS_DIR + "/DATA/carposts_info.csv"
    if os.path.isfile(namefile):
    
        with open(namefile, 'r') as fichier:
            reader = csv.DictReader(fichier, delimiter=' ')
            for row in reader:
                code = row['num_poste']
                name[code] = row['nom_usuel']
                lat[code] = float(row['lat_dg'])
                lon[code] = float(row['lon_dg'])
                alti[code] = float(row['alti'])
                slope[code] = float(row['pente_nivo'])
                aspect[code] = float(row['exposition_nivo'])
                massif[code] = int(row['massif_nivo'])
      
    else:
        raise FileNameException(namefile)

    # Quels sont les sites existants ?
    SitesExistants = IXML.getListSites()

    # Lecture de la liste des postes mal géolocalisés pour ne pas les prendre en compte
    # ---------------------------------------------------------------------------
    list_mal_geolocalises = []
    objcsv = open(SNOWTOOLS_DIR + "/DATA/blacklist.csv", "r")
    r = csv.reader(objcsv, delimiter=" ", skipinitialspace=True)
    for row in r:
        if re.match("^\d{7}$", row[0]):
            code = "0" + row[0]
        else:
            code = row[0]
        list_mal_geolocalises.append(code)

    ################################################################################################
    #                                                                                              #
    #  AJOUT DE NOUVEAUX SITES DANS LE FICHIER XML                                   #
    #                                                                                              #
    ################################################################################################
    # chemin d ecriture du fichier XML
    from snowtools.DATA import SNOWTOOLS_DIR
    chemxml = SNOWTOOLS_DIR + "/DATA"
    # ouverture  du fichier en mode "lecture"/"ecriture"
    metadata = open(chemxml + "/METADATA.xml", 'r')
    metadataout = open(chemxml + "/METADATA_withoutmask.xml", 'w')

    while True:
        line = metadata.readline()
        metadataout.write(line)
        if '<Sites>' in line:
            break

    list_update = []

    list_carpost = sorted(name.keys())

    for code in list_carpost:

        if code not in SitesExistants:
            if code not in list_mal_geolocalises:
                print("ajout du site : ", name[code])

                metadataout.write('\t<Site>\n')
                metadataout.write('\t\t<name> ' + name[code] + ' </name>\n')
                metadataout.write('\t\t<nameRed> ' + name[code] + ' </nameRed>\n')
                metadataout.write('\t\t<number> ' + code + ' </number>\n')
                metadataout.write('\t\t<lat> ' + str(lat[code]) + ' </lat>\n')
                metadataout.write('\t\t<lon> ' + str(lon[code]) + ' </lon>\n')
                metadataout.write('\t\t<altitude> ' + str(alti[code]) + ' </altitude>\n')
                metadataout.write('\t\t<aspect> ' + str(aspect[code]) + ' </aspect>\n')
                metadataout.write('\t\t<slope> ' + str(slope[code]) + ' </slope>\n')
                metadataout.write('\t\t<massif> ' + str(massif[code]) + ' </massif>\n')
                metadataout.write('\t\t<zref> ' + "1.5" + ' </zref>\n')
                metadataout.write('\t\t<uref> ' + "10.0" + ' </uref>\n')
                metadataout.write('\t\t<carpost> ' + "True" + ' </carpost>\n')
                metadataout.write('\t</Site>\n')

        else:

            lati_base, longi_base, alti_base = IXML.infoposte(code)
            expo_base, slope_base = IXML.exposlopeposte(code)
            try:
                # Ce truc va planter s'il n'y a pas encore de champ massif
                massif_base = IXML.massifposte(code)
            except Exception:
                massif_base = -1

            update_needed = False
            if alti[code] != alti_base:
                print("UPDATE ALTITUDE : " + code)
                print(alti[code], alti_base)
                update_needed = True

            if aspect[code] != expo_base:
                print("UPDATE ASPECT : " + code)
                print(aspect[code], expo_base)
                update_needed = True

            if slope[code] != slope_base:
                print("UPDATE SLOPE : " + code)
                print(slope[code], slope_base)
                update_needed = True

            if lat[code] != lati_base:
                print("UPDATE LATITUDE : " + code)
                print(lat[code], lati_base)
                update_needed = True

            if lon[code] != longi_base:
                print("UPDATE LONGITUDE : " + code)
                print(lon[code], longi_base)
                update_needed = True

            if massif[code] != massif_base:
                print("UPDATE MASSIF : " + code)
                print(massif[code], massif_base)            
                update_needed = True            
            
            if update_needed:
                print("UPDATE STATION : " + code)
                list_update.append(code)
                metadataout.write('\t<Site>\n')
                metadataout.write('\t\t<name> ' + name[code] + ' </name>\n')
                metadataout.write('\t\t<nameRed> ' + name[code] + ' </nameRed>\n')
                metadataout.write('\t\t<number> ' + code + ' </number>\n')
                metadataout.write('\t\t<lat> ' + str(lat[code]) + ' </lat>\n')
                metadataout.write('\t\t<lon> ' + str(lon[code]) + ' </lon>\n')
                metadataout.write('\t\t<altitude> ' + str(alti[code]) + ' </altitude>\n')
                metadataout.write('\t\t<aspect> ' + str(aspect[code]) + ' </aspect>\n')
                metadataout.write('\t\t<slope> ' + str(slope[code]) + ' </slope>\n')
                metadataout.write('\t\t<massif> ' + str(massif[code]) + ' </massif>\n')
                metadataout.write('\t\t<zref> ' + "1.5" + ' </zref>\n')
                metadataout.write('\t\t<uref> ' + "10.0" + ' </uref>\n')
                metadataout.write('\t\t<carpost> ' + "True" + ' </carpost>\n')
                metadataout.write('\t</Site>\n')

    # Gestion des autres postes

    for code in SitesExistants:
        if code not in list_carpost:
            name_base = IXML.nameposte(code)
            lati_base, longi_base, alti_base = IXML.infoposte(code)
            expo_base, slope_base = IXML.exposlopeposte(code)
            listazim, listmask = IXML.maskposte(code)
            liststrazim = list(map(str, listazim))
            liststrmask = list(map(str, listmask))
            try:
                # Ce truc va planter s'il n'y a pas encore de champ massif
                massif_base = IXML.massifposte(code)
            except Exception:
                massif_base = -1

            metadataout.write('\t<Site>\n')
            metadataout.write('\t\t<name> ' + name_base + ' </name>\n')
            metadataout.write('\t\t<nameRed> ' + name_base + ' </nameRed>\n')
            metadataout.write('\t\t<number> ' + code + ' </number>\n')
            metadataout.write('\t\t<lat> ' + str(lati_base) + ' </lat>\n')
            metadataout.write('\t\t<lon> ' + str(longi_base) + ' </lon>\n')
            metadataout.write('\t\t<altitude> ' + str(alti_base) + ' </altitude>\n')
            metadataout.write('\t\t<aspect> ' + str(expo_base) + ' </aspect>\n')
            metadataout.write('\t\t<slope> ' + str(slope_base) + ' </slope>\n')
            metadataout.write('\t\t<massif> ' + str(massif_base) + ' </massif>\n')
            metadataout.write('\t\t<zref> ' + "1.5" + ' </zref>\n')
            metadataout.write('\t\t<uref> ' + "10.0" + ' </uref>\n')
            metadataout.write('\t\t<carpost> ' + "False" + ' </carpost>\n')
            metadataout.write('\t\t<azimut> ' + ','.join(liststrazim) + ' </azimut>\n')
            metadataout.write('\t\t<mask> ' + ','.join(liststrmask) + ' </mask>\n')
            metadataout.write('\t</Site>\n')


    # for line in metadata.readlines():
    #     metadataout.write(line)

        # if "<number>"in line:
        #     if re.match("^.*(\d{8}).*$", line):
        #         codestation = re.split("^.*(\d{8}).*$", line)[1]
        #         if codestation in list_update:
        #             metadataout.write('\t\t<massif> ' + str(massif[codestation]) + ' </massif>\n')

    ################################################################################################
    #                                                                                              #
    #  V/  C EST FINI                                                                              #
    #                                                                                              #
    ################################################################################################
    metadataout.write('</Sites>\n')
    metadataout.write('</root>\n')
    metadata.close()
