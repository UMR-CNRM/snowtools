#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Extraction des données d'observation depuis la BDCLIM
# Author : Matthieu Vernay
# Script simplifié pour extraire les pluvios utilisés par la LE ANTILOPE.

# Doc BDClim : http://dpnet.meteo.fr/DCLIM/doc/climsol.html
# Doc réseaux postes : http://gedmf/share/page/document-details?nodeRef=workspace://SpacesStore/b31a01c0-d252-4e3e-ba86-9e1c25b5d0d5

import sys

from snowtools.scripts.extract.obs.bdquery import question


def usage():
    print("USAGE extract_pluvios_antilope.py [datedeb datefin]")
    print("format des dates : YYYYMMDDHH")
    sys.exit(1)


if __name__ == "__main__":

    listconditions = [
        # Apply ANTILOPE filter for "reseau_poste" (Cf mail from O.Laurantin on may 2022)
        "hist_reseau_poste.reseau_poste not in ('00','01','02','03','04','09','40','42','43','44','50','51','52','53',"
        "'54','83','90','91','92','93','94','95','96','97')",
        # Filter out EDF-nivo observations
        "not nom_usuel like '%EDFNIVO'",
        # French Alps domain
        "poste.lat_dg<=47.0 and poste.lat_dg>=41.0 and poste.lon_dg<=10. and poste.lon_dg>=4.0",
    ]
    listvar = ["poste.num_poste", "poste.nom_usuel", "poste.lat_dg", "poste.lon_dg", "poste.alti",
            "hist_reseau_poste.reseau_poste"]

    if len(sys.argv) == 1:
        period     = []
        table      = 'POSTE'
        listjoin   = ['HIST_RESEAU_POSTE ON HIST_RESEAU_POSTE.NUM_POSTE=POSTE.NUM_POSTE']
        listorder  = ['poste.num_poste']
        header     = ['num_poste', 'poste', 'lat', 'lon', 'alti', 'reseau_poste']
        outputfile = 'liste_stations_ANTILOPE.csv'

    elif len(sys.argv) == 3:
        datedeb = sys.argv[1]
        datefin = sys.argv[2]
        period = [datedeb, datefin]
        table      = 'H'
        listvar.extend(["h.dat", "h.rr1"])
        listjoin       = ['POSTE ON POSTE.NUM_POSTE=H.NUM_POSTE ',
            'HIST_RESEAU_POSTE ON HIST_RESEAU_POSTE.NUM_POSTE=H.NUM_POSTE']
        listorder      = ['h.dat', 'h.num_poste']
        header = ['dat', 'num_poste', 'poste', 'lat', 'lon', 'alti', 'reseau_poste', 'rr']
        outputfile = f"obs_quotidiennes_RR_{datedeb}_{datefin}.data"

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
