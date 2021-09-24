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
    print("USAGE extract_obs_meteo_mb.py datedeb datefin")
    print("format des dates : YYYYMMDD")
    sys.exit(1)

stations = ['05079402', '05063402'] # GALIBIER et MEIJE

if __name__ == "__main__":

    if len(sys.argv) != 3:
        usage()
    datedeb = sys.argv[1] + "06"
    datefin = sys.argv[2] + "06"

    # II.1.1 construction de la question pour les postes nivo pour la HTN
    # ---------------------------------------------------------------------------
    # On prend toutes les heures

    question1 = question("question_hor.q")
    question1.set_fileout("obs_horaires.data")
    question1.set_varout(["dat", "h.num_poste", "poste.nom_usuel", "poste.alti", "t"]) # Ajouter les variables à extraire
    question1.set_tables(["H", "POSTE"])
    question1.set_joincondition("H.NUM_POSTE = POSTE.NUM_POSTE and POSTE.NUM_POSTE in (" + ','.join(stations) + ')')
    question1.set_period(datedeb, datefin)
    question1.set_order(["h.num_poste", "dat"])
    question1.run()
