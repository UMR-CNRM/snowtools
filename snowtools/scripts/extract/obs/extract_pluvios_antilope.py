#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Extraction des données d'observation depuis la BDCLIM
# Author : Matthieu Vernay
# Script simplifié pour extraire les pluvios utilisés par la LE ANTILOPE.

# Doc BDClim : http://dpnet.meteo.fr/DCLIM/doc/climsol.html
# Doc réseaux postes :
# http://gedmf/share/page/document-details?nodeRef=workspace://SpacesStore/b31a01c0-d252-4e3e-ba86-9e1c25b5d0d5

import sys
import pandas as pd
from snowtools.scripts.extract.obs.bdquery import question


def usage():
    print("USAGE extract_pluvios_antilope.py domain [datedeb datefin]")
    print("format des dates : YYYYMMDDHH")
    sys.exit(1)


domain_map = dict(
    alp            = (47., 41., 4., 10.),
    MercantourTheo = (44.7, 43.2, 6.3, 8.),
)

if __name__ == "__main__":

    domain = sys.argv[1]
    latmax, latmin, lonmin, lonmax = domain_map[domain]
    listconditions = [
        # Apply ANTILOPE filter for "reseau_poste" (Cf mail from O.Laurantin on may 2022)
        "hist_reseau_poste.reseau_poste not in ('00','01','02','03','04','09','40','42','43','44','50','51','52','53',"
        "'54','83','90','91','92','93','94','95','96','97')",
        # Filter out EDF-nivo observations
        "not nom_usuel like '%EDFNIVO'",
        # French Alps domain
        f"poste.lat_dg<={latmax} and poste.lat_dg>={latmin} and poste.lon_dg<={lonmax} and poste.lon_dg>={lonmin}",
    ]
    listvar = ["poste.num_poste", "poste.nom_usuel", "poste.lat_dg", "poste.lon_dg", "poste.alti",
            "hist_reseau_poste.reseau_poste"]
    listorder  = ['poste.num_poste']

    if len(sys.argv) == 2:
        period     = []
        table      = 'POSTE'
        listjoin   = ['HIST_RESEAU_POSTE ON HIST_RESEAU_POSTE.NUM_POSTE=POSTE.NUM_POSTE']
        header     = ['num_poste', 'poste', 'lat', 'lon', 'alti', 'reseau_poste']
        outputfile = 'liste_stations_ANTILOPE_{domain}.csv'

    elif len(sys.argv) == 4:
        datedeb = sys.argv[2]
        datefin = sys.argv[3]
        period = [datedeb, datefin]
        table      = 'H'
        listvar.extend(["h.dat", "h.rr1"])
        listjoin       = ['POSTE ON POSTE.NUM_POSTE=H.NUM_POSTE ',
            'HIST_RESEAU_POSTE ON HIST_RESEAU_POSTE.NUM_POSTE=H.NUM_POSTE']
        header = ['num_poste', 'poste', 'lat', 'lon', 'alti', 'reseau_poste', 'date', 'rr']
        outputfile = f"obs_quotidiennes_RR_{domain}_{datedeb}_{datefin}.data"

    else:
        usage()


# 1. RR horaires
# ---------------
    question1 = question(
        listvar        = listvar,
        table          = table,
        listjoin       = listjoin,
        listconditions = listconditions,
        period         = period,
        listorder      = listorder,
    )
    question1.run(outputfile=outputfile, header=header)

    if len(sys.argv) == 4:
        df = pd.read_csv(outputfile, sep=';')
        out = df.groupby('num_poste').agg({'poste': 'min', 'lat': 'max', 'lon': 'max', 'alti': 'max',
            'reseau_poste': 'max', 'rr': 'sum'})

        out.to_csv(outputfile.replace('obs_quotidiennes', 'CUMUL'), sep=';')
