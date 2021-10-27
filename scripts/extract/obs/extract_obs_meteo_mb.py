#! /usr/bin/env python3
# -*- coding: utf-8 -*-
"""
# Extraction des données d'observation depuis la BDCLIM
# Matthieu Lafaysse 10 sept 2014
# Modified Léo Viallon-Galinier 22 oct 2021
# Script simplifié pour obtenir un format obs.csv depuis la BDCLIM
"""

import argparse

from snowtools.scripts.extract.obs.bdquery import question

parser = argparse.ArgumentParser(
        description="Read obs data (Temperature) from BDCLim from specified observation stations"
        )
parser.add_argument("date_min", help="Start date")
parser.add_argument("date_max", help="End date")
parser.add_argument("-o", "--output", help="Output filename", default='obs_horaires.csv', dest='output')
# Selection of stations, default is GALIBIER et MEIJE
parser.add_argument("-s", "--stations", nargs="+", default=['05079402', '05063402'], help="List of stations to be used",
                    dest='stations')
args = parser.parse_args()

# II.1.1 construction de la question pour les postes nivo pour la HTN
# ---------------------------------------------------------------------------
# On prend toutes les heures

question = question(
        listvar=["dat", "h.num_poste", "poste.nom_usuel", "poste.alti", "t"],  # Insert other variables here if needed
        table='H',
        listorder=['H.num_poste', 'dat'],
        listjoin=[
            'POSTE ON H.NUM_POSTE = POSTE.NUM_POSTE and POSTE.NUM_POSTE in (' + ','.join(args.stations) + ')'
            ],
        period=[args.date_min, args.date_max],
        )
question.run(outputfile=args.output)
