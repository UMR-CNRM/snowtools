#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Extraction des données d'observation depuis la BDCLIM
# Matthieu Lafaysse 10 sept 2014
# Script simplifié pour obtenir un format obs.csv depuis la BDCLIM

import sys, os
import string
import re
import datetime
from bronx.stdtypes.date import Date

import csv

from snowtools.scripts.extract.obs.bdquery import question


def usage():
    print("USAGE extract_pluvios.py datedeb datefin")
    print("format des dates : YYYYMMDDHH")
    sys.exit(1)

if __name__ == "__main__":

    if len(sys.argv) != 3:
        usage()
    datedeb = sys.argv[1]
    datefin = sys.argv[2]
    begin = Date(datedeb)
    end = Date(datefin)

# 1. RR horaires
#-----------------
    question1 = question(
        listvar=["h.dat", "h.num_poste", "poste.nom_usuel", "poste.lat_dg", "poste.lon_dg", "poste.alti", "h.rr1", "hist_reseau_poste.reseau_poste"],
        table='H',
        listjoin=['POSTE ON H.NUM_POSTE=POSTE.NUM_POSTE ', 
            'HIST_RESEAU_POSTE ON HIST_RESEAU_POSTE.NUM_POSTE=POSTE.NUM_POSTE'],
        listconditions = [
            "hist_reseau_poste.datdeb<=h.dat and (hist_reseau_poste.datfin>=h.dat or hist_reseau_poste.datfin is null) and h.rr1 is not null",
            #"poste.lat_dg<=47.0 and poste.lat_dg>=42.75 and poste.lon_dg<=9. and poste.lon_dg>=4.5",
            "poste.lat_dg<=46.45 and poste.lat_dg>=44.1 and poste.lon_dg<=7.2 and poste.lon_dg>=5.4",
            ],
        period=[datedeb, datefin],
        listorder=['h.dat', 'h.num_poste'],
        )
    question1.run(outputfile=f"obs_horaires_RR_{begin.ymd}_{end.ymd}.csv", header=['date', 'num_poste', 'nom', 'lat', 'lon', 'alti', 'rr', 'reseau_poste'])

