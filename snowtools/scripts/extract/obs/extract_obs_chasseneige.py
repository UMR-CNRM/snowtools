#!/usr/bin/env python3
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
        description="Read HTN (snow height) observations from BDClim for all available stations on a daily timestep \
                     at 6hUTC"
        )
parser.add_argument("date_min", help="Start date")
parser.add_argument("date_max", help="End date")
parser.add_argument("-o", "--output", help="Output file. If none selected, produce HTN.csv file",
                    default='HTN.csv', dest='output')
parser.add_argument("--append", action="store_true", default=False, help="Append to an existing output file")
args = parser.parse_args()

# II.1.1 construction de la question pour tous les postes sauf les sondages pour la HTN
# -------------------------------------------------------------------------------------
# On prend toutes les heures
question = question(
        # Insert other variables here if needed
        listvar=["h_nivo.num_poste", "NOM_USUEL", "to_char(dat,'YYYY-MM-DD-HH24-MI')", "cn"],
        table='H_NIVO',
        listorder=['h_nivo.num_poste','dat'],
        listjoin=[
            'POSTE_NIVO ON H_NIVO.NUM_POSTE = POSTE_NIVO.NUM_POSTE',
            ],
        listconditions=[
            "to_char(dat,'HH24') = '06'",  "cn is not null", 'POSTE_NIVO.TYPE_NIVO = 1', '(POSTE_NIVO.num_poste=73306403'
                                                                                         ' or POSTE_NIVO.num_poste=73173400'
                                                                                         ' or POSTE_NIVO.num_poste=73318400'
                                                                                         ' or POSTE_NIVO.num_poste=73280402'
                                                                                         ' or POSTE_NIVO.num_poste=38191400 '
                                                                                         'or POSTE_NIVO.num_poste=38191408'
                                                                                         ' or POSTE_NIVO.num_poste=38527400'
                                                                                         ' or POSTE_NIVO.num_poste=38020400'
                                                                                         ' or POSTE_NIVO.num_poste=05063407'
                                                                                         ' or POSTE_NIVO.num_poste=38253400'
                                                                                         ' or POSTE_NIVO.num_poste=38253407)'
            ],
        period=[args.date_min, args.date_max],
        )
question.run(outputfile=args.output,
             header=['NUMPOST', 'Date UTC', "CN"] if not args.append else False,
             mode='w' if not args.append else 'a')
