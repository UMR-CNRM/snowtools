#!/usr/bin/env python3
# -*- coding: utf-8 -*

import os
import sys
import re

from snowtools.utils.infomassifs import infomassifs
from snowtools.utils.FileException import FileNameException, FileOpenException
################################################################################################
#                                                                                              #
#  I/  LECTURE DES SITES DANS UN FICHIER                                                       #
#                                                                                              #
################################################################################################


if __name__ == "__main__":

    if len(sys.argv) != 2:
        sys.exit("USAGE python creeMETADATA_postes.py LIST_POSTES")

    listeSites = []

    name = {}
    lat = {}
    lon = {}
    alti = {}
    slope = {}
    aspect = {}
    massif = {}

    namefile = sys.argv[1]
    if os.path.isfile(namefile):
        try:
            fichier = open(namefile, "r")
        except IOError:
            raise FileOpenException("can't open the file " + namefile)

        for line in fichier.readlines():
            listline = line.split(',')
            code = listline[0]
            name[code] = listline[1].strip()
            lat[code] = float(listline[2])
            lon[code] = float(listline[3])
            alti[code] = float(listline[4])
            slope[code] = float(listline[5])
            aspect[code] = float(listline[6])
            massif[code] = int(listline[7])

    else:
        raise FileNameException(namefile)

    # Quels sont les sites existants ?
    SitesExistants = infomassifs().getListSites()
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
    metadataout = open(chemxml + "/METADATA_out.xml", 'w')

    while True:
        line = metadata.readline()
        metadataout.write(line)
        if '<Sites>' in line:
            break

    list_update = []

    for code in sorted(name.keys()):

        if code not in SitesExistants:
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
            metadataout.write('\t</Site>\n')

        else:

            lati_base, longi_base, alti_base = infomassifs().infoposte(code)
            expo_base, slope_base = infomassifs().exposlopeposte(code)
            try:
                # Ce truc va planter s'il n'y a pas encore de champ massif
                massif_base = infomassifs().massifposte(code)
            except Exception:
                massif_base = -1

            if alti[code] != alti_base:
                print("WARNING ALTITUDE : " + code)
                print(alti[code], alti_base)

            if aspect[code] != expo_base:
                print("WARNING ASPECT : " + code)
                print(aspect[code], expo_base)

            if slope[code] != slope_base:
                print("WARNING SLOPE : " + code)
                print(slope[code], slope_base)

            if lat[code] != lati_base:
                print("WARNING LATITUDE : " + code)
                print(lat[code], lati_base)

            if lon[code] != longi_base:
                print("WARNING LONGITUDE : " + code)
                print(lon[code], longi_base)

            if massif[code] != massif_base:
                print("UPDATE MASSIF NUMBER : " + code)
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
                metadataout.write('\t</Site>\n')

    # On écrit la fin du fichier

    for line in metadata.readlines():
        metadataout.write(line)

        if "<number>"in line:
            if re.match("^.*(\d{8}).*$", line):
                codestation = re.split("^.*(\d{8}).*$", line)[1]
                if codestation in list_update:
                    metadataout.write('\t\t<massif> ' + str(massif[codestation]) + ' </massif>\n')

    ################################################################################################
    #                                                                                              #
    #  V/  C EST FINI                                                                              #
    #                                                                                              #
    ################################################################################################

    metadata.close()

    os.rename(chemxml + "/METADATA_out.xml", chemxml + "/METADATA.xml")
