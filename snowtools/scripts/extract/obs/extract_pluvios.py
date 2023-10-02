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

    latmin = dict(alp=44.1, pyr=41.5)
    latmax = dict(alp=46.45, pyr=44.0)
    lonmin = dict(alp=5.4, pyr=-2.5)
    lonmax = dict(alp=7.2, pyr=4.0)

# 1. RR horaires
#-----------------
    for domain in ['alp', 'pyr']:
        question1 = question(
            listvar=["h.dat", "h.num_poste", "poste.nom_usuel", "poste.lat_dg", "poste.lon_dg", "poste.alti", "h.rr1", "hist_reseau_poste.reseau_poste"],
            table='H',
            listjoin=['POSTE ON H.NUM_POSTE=POSTE.NUM_POSTE ', 
                'HIST_RESEAU_POSTE ON HIST_RESEAU_POSTE.NUM_POSTE=POSTE.NUM_POSTE'],
            listconditions = [
                "hist_reseau_poste.datdeb<=h.dat and (hist_reseau_poste.datfin>=h.dat or hist_reseau_poste.datfin is null) and h.rr1 is not null",
                #"poste.lat_dg<=47.0 and poste.lat_dg>=42.75 and poste.lon_dg<=9. and poste.lon_dg>=4.5",
                f"poste.lat_dg<={latmax[domain]} and poste.lat_dg>={latmin[domain]} and poste.lon_dg<={lonmax[domain]} and poste.lon_dg>={lonmin[domain]}",
                ],
            period=[datedeb, datefin],
            listorder=['h.dat', 'h.num_poste'],
            )
        question1.run(outputfile=f"obs_horaires_RR_{begin.ymd}_{end.ymd}_{domain}.csv", header=['date', 'num_poste', 'nom', 'lat', 'lon', 'alti', 'rr', 'reseau_poste'])

