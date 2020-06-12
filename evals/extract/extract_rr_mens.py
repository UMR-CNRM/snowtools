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
    print("USAGE extract_rr_mens.py datedeb datefin massif")
    print("format des dates : YYYYMMDD")
    sys.exit(1)



if __name__ == "__main__":

    if len(sys.argv) != 3 and len(sys.argv) != 4:
        usage()
    datedeb = sys.argv[1] + "00"
    datefin = sys.argv[2] + "23"
    massif_nivo = sys.argv[3]

    # II.1.1 construction de la question pour les postes nivo pour la HTN
    # ---------------------------------------------------------------------------
    # On prend toutes les heures

    question1 = question("question_jesus.q")
    question1.set_fileout("RRmassif" + str(massif_nivo) + ".obs")
    question1.set_varout(["dat", "mensq.num_poste", "poste_nivo.nom_usuel", "rr", "rr_me", "nbrr"])
    question1.set_tables(["MENSQ", "POSTE_NIVO"])
    question1.set_joincondition("MENSQ.NUM_POSTE = POSTE_NIVO.NUM_POSTE and massif_nivo = " + str(massif_nivo))
    question1.set_period_monthly(datedeb, datefin)
    question1.set_order(["dat", "num_poste"])
    question1.run()


