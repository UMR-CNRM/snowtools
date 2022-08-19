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
        listvar=["h.num_poste", "to_char(dat,'YYYY-MM-DD-HH24-MI')", "neigetot", "POSTE.LAT_DG", "POSTE.LON_DG",
                 "POSTE.ALTI", "POSTE.NOM_USUEL"],
        table='H',
        listorder=['h.num_poste', 'dat'],
        listjoin=[
            'POSTE ON H.NUM_POSTE = POSTE.NUM_POSTE',
            ],
        listconditions=[
            "to_char(dat,'HH24') = '06'", "neigetot is not null", "POSTE.ALTI > 300",
            "POSTE.NUM_DEP = 57" # 54 57 67 68 70 88 90
            ],
        period=[args.date_min, args.date_max],
        )
question.run(outputfile=args.output,
             header=['NUMPOST', 'Date UTC', "HTN cm", "LAT dg", "LON dg", "ALTI m", "NOM"] if not args.append else False,
             mode='w' if not args.append else 'a')

# Old code:
# -------------------------------------------------------------------------------------
# if append:
#     ficname = appendfile
#     if os.path.isfile(appendfile):
#         # Fichier déjà existant
#         os.rename(appendfile, "old.csv")
#     else:
#         append = False
# else:
#     ficname = "OBS_" + datedeb + "_" + datefin + ".csv"
#
# c = csv.writer(open(ficname, "wb"), delimiter=";")
# c.writerow(['NUMPOST', 'Date UTC', "HTN cm", "type_nivo"])
#
# if append:
#     csvold = open("old.csv", "r")
#     r = csv.reader(csvold, delimiter=";")
#     datedebtime = datetime.datetime(int(datedeb[0:4]), int(datedeb[4:6]), int(datedeb[6:8]), int(datedeb[8:10]))
#     datefintime = datetime.datetime(int(datefin[0:4]), int(datefin[4:6]), int(datefin[6:8]), int(datefin[8:10]))
#     for row in r:
#         #                9 pour obs espagnoles
#         if re.match("^\d{8}\d?$", row[0]):
#             listdate = row[1].split("-")
#             date = datetime.datetime(int(listdate[0]), int(listdate[1]), int(listdate[2]), int(listdate[3]), int(listdate[4]))
#             if date < datedebtime or date > datefintime:
#                 c.writerow(row)
#
# for ficin in ["HTN.obs"]:
#     csvfile = open(ficin)
#     reader = csv.reader(csvfile, delimiter="|", skipinitialspace=True)
#     for row in reader:
#         # Ajout d'un 0 si département < 10
#         if re.match("^\d{7}$", row[1]):
#             row[1] = "0" + row[1]
#         if len(row[2]) > 0 and row[2].strip() != "-999":
#             c.writerow([row[1], row[0], row[2], row[3]])
