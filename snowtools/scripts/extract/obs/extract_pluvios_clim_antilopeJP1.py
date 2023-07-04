#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Extraction des données d'observation depuis la BDCLIM
# Matthieu Lafaysse 10 sept 2014
# Script simplifié pour obtenir un format obs.csv depuis la BDCLIM

import sys, os
import string
import re
import datetime

import csv

from snowtools.scripts.extract.obs.bdquery import question


def usage():
    print("USAGE extract_pluvios_antilope.py datedeb datefin")
    print("format des dates : YYYYMMDDHH")
    sys.exit(1)

if __name__ == "__main__":

    if len(sys.argv) != 3:
        usage()
    datedeb = sys.argv[1]
    datefin = sys.argv[2]

# TODO : extraire obs horaires, puis cumul de 6h à 6h 

# 1. RR horaires
#-----------------
    question1 = question(
        listvar=["H.dat", "H.num_poste", "poste.nom_usuel", "poste.lat_dg", "poste.lon_dg", "poste.alti", "H.rr1", "hist_reseau_poste.reseau_poste"],
        table='H',
        listjoin=['POSTE ON H.NUM_POSTE=POSTE.NUM_POSTE ', 
            'HIST_RESEAU_POSTE ON HIST_RESEAU_POSTE.NUM_POSTE=POSTE.NUM_POSTE'],
        #listjoin=['POSTE ON H.NUM_POSTE=POSTE.NUM_POSTE', 'HIST_TYPE_POSTE ON HIST_TYPE_POSTE.NUM_POSTE=POSTE.NUM_POSTE and hist_type_poste.dat=h.dat and hist_type_poste.type_poste<=3'],
        # Pour la version "clim" :
        #listjoin=['POSTE ON H.NUM_POSTE=POSTE.NUM_POSTE', 
        #   'HIST_RESEAU_POSTE ON HIST_RESEAU_POSTE.NUM_POSTE=POSTE.NUM_POSTE and hist_reseau_poste.datdeb<=h.dat and (hist_reseau_poste.datfin>=h.dat or hist_reseau_poste.datfin is null) and hist_reseau_poste.reseau_poste in (10,11,40,41,42,63,71)'],
        #listjoin=['POSTE ON H.NUM_POSTE=POSTE.NUM_POSTE', 'HIST_TYPE_POSTE ON HIST_TYPE_POSTE.NUM_POSTE=POSTE.NUM_POSTE and hist_type_poste.dat=h.dat and hist_type_poste.type_poste<=4'],
        listconditions = [
            #"hist_reseau_poste.datdeb<=h.dat and (hist_reseau_poste.datfin>=h.dat or hist_reseau_poste.datfin is null) and hist_reseau_poste.reseau_poste in ('10','12','40','41','42','63','71')",
            "hist_reseau_poste.datdeb<=H.dat and (hist_reseau_poste.datfin>=H.dat or hist_reseau_poste.datfin is null) and hist_reseau_poste.reseau_poste in ('40','41','42','43','44','49','53')",  # postes clim
            "poste.lat_dg<=46.875 and poste.lat_dg>=42.75 and poste.lon_dg<=9. and poste.lon_dg>=4.5",
            ],
        #listconditions = ["hist_reseau_poste.datdeb<=h.dat and (hist_reseau_poste.datfin>=h.dat or hist_reseau_poste.datfin is null) and hist_reseau_poste.reseau_poste in ('10','11','40','41','42','63','71')"],
        period=[datedeb, datefin],
        listorder=['H.dat', 'H.num_poste'],
        )
    question1.run(outputfile="obs_horaires_clim_RR.data", header=['dat', 'num_poste', 'poste', 'lat', 'lon', 'alti', 'rr', 'reseau_poste'])

