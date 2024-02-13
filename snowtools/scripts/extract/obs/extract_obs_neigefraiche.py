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
        listvar=["q.num_poste", "to_char(dat,'YYYY-MM-DD-HH24-MI')", "hneigef", "type_nivo"],
        table='Q',
        listorder=['dat', 'q.num_poste'],
        listjoin=[
            'POSTE_NIVO ON Q.NUM_POSTE = POSTE_NIVO.NUM_POSTE and type_nivo != 2',
            ],
        listconditions=[
            "hneigef is not null"
            ],
        period=[args.date_min, args.date_max],
        )
question.run(outputfile=args.output,
             header=['NUMPOST', 'Date UTC', "HN24 cm", "type_nivo"] if not args.append else False,
             mode='w' if not args.append else 'a')
