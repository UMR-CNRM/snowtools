#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Extraction des données d'observation depuis la BDCLIM
# Matthieu Lafaysse 10 sept 2014
# Script simplifié pour obtenir un format obs.csv depuis la BDCLIM

import sys
import os
import csv

from snowtools.evals.extract.extract_obs import question


def usage():
    print("USAGE extract_metadata.py LIST_STATIONS")
    sys.exit(1)


if __name__ == "__main__":

    if len(sys.argv) != 2:
        usage()

    objcsv = open(sys.argv[1], "r")
    r = csv.reader(objcsv, delimiter=" ", skipinitialspace=True)

    fichierout = open("metadata_malgeo.csv", "w")

    writer = csv.writer(fichierout)

    for row in r:
        station = row[0].strip()

    # II.1.1 construction de la question pour tous les postes sauf les sondages pour la HTN
    # -------------------------------------------------------------------------------------
    # On prend toutes les heures


        question1 = question("question_metadata.q")
        question1.set_fileout(station + ".hist")
        question1.set_varout(["num_poste", "type_poste", "datdeb", "datfin"])
        question1.set_tables(["HIST_TYPE_POSTE"])
        question1.set_firstcondition("NUM_POSTE = " + station)
        question1.run()

    objcsv.close()
    os.system("sleep 10")

    objcsvbis = open(sys.argv[1], "r")
    rbis = csv.reader(objcsvbis, delimiter=" ", skipinitialspace=True)

    for row in rbis:
        station = row[0].strip()

        objcsv2 = open(station + ".hist", "r")
        info = csv.reader(objcsv2, delimiter="|", skipinitialspace=True)
        for rowinfo in info:
            pass

        print (station, rowinfo)
        writer.writerow(row[0:5] + rowinfo[1:])
        objcsv2.close()

    objcsvbis.close()
    fichierout.close()
        
    # Fichier final
