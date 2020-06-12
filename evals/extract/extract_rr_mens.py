#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Extraction des données d'observation depuis la BDCLIM
# Matthieu Lafaysse 10 sept 2014
# Script simplifié pour obtenir un format obs.csv depuis la BDCLIM

import sys, os
import string
import re
import datetime

import csv

from extract_obs import question 

def usage():
    print("USAGE extract_rr_mens.py datedeb datefin numero bydep")
    print("format des dates : YYYYMMDD")
    print("bydep=0 means numero is massif number ; bydep=1 means numero is department number")
    sys.exit(1)



if __name__ == "__main__":

    if len(sys.argv) != 3 and len(sys.argv) != 4:
        usage()
    datedeb = sys.argv[1] + "00"
    datefin = sys.argv[2] + "23"
    bydep = int(sys.argv[4])
    if (bydep == 0):
        massif_nivo = sys.argv[3]
    if (bydep == 1):
        department = sys.argv[3]

    # II.1.1 construction de la question pour les postes nivo pour la HTN
    # ---------------------------------------------------------------------------
    # On prend toutes les heures

    question1 = question("question_jesus.q")
    question1.set_fileout("RRmassif" + str(massif_nivo) + ".obs")
    if (bydep == 0):
        question1.set_varout(["dat", "mensq.num_poste", "poste_nivo.nom_usuel", "poste_nivo.alti", "rr", "nbrr", "rr_me"])
        question1.set_tables(["MENSQ", "POSTE_NIVO"])
        question1.set_joincondition("MENSQ.NUM_POSTE = POSTE_NIVO.NUM_POSTE and massif_nivo = " + str(massif_nivo))

    if (bydep == 1):
        question1.set_varout(["dat", "mensq.num_poste", "poste_nivo.nom_usuel", "poste_nivo.alti", "poste_nivo.massif_nivo", "rr", "nbrr", "rr_me"])
        question1.set_tables(["MENSQ", "POSTE_NIVO"])
        question1.set_joincondition("MENSQ.NUM_POSTE = POSTE_NIVO.NUM_POSTE and num_dep = " + str(department))

    question1.set_period_monthly(datedeb, datefin)
    question1.set_order(["dat", "num_poste"])
    question1.run()


