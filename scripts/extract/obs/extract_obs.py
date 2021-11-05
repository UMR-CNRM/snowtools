#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Extraction des données d'observation depuis la BDCLIM
# Matthieu Lafaysse 10 sept 2014
# Modified Léo Viallon-Galinier 22 oct 2021
# Script simplifié pour obtenir un format obs.csv depuis la BDCLIM

import argparse

from snowtools.scripts.extract.obs.bdquery import question

parser = argparse.ArgumentParser(
        description="""
        Read HTN (snow height) observations from BDCLim
        for all available stations: both Nivoses and
        "nivo-meteo" network.
        """
        )
parser.add_argument("date_min", help="Start date")
parser.add_argument("date_max", help="End date")
parser.add_argument("-o", "--output", help="Output file. If none selected, produce NIVOMETEO.obs and NIVOSE.obs files",
                    default=None, dest='output')
args = parser.parse_args()

datedeb = args.date_min
datefin = args.date_max

if args.output:
    nivose_obs_fn = args.output
    nivomto_obs_fn = args.output
    nivomto_mode = 'a'
else:
    nivose_obs_fn = 'NIVOSE.obs'
    nivomto_obs_fn = 'NIVOMETEO.obs'
    nivomto_mode = 'w'

# II.1.1 construction de la question pour les postes nivo pour la HTN
# ---------------------------------------------------------------------------
# On prend toutes les heures

# Postes nivo-meteo
question1 = question(
        listvar=["to_char(dat,'YYYY-MM-DD-HH24-MI')", "to_char(h.num_poste, 'fm00000000')", "neigetot"],
        table='H',
        listjoin=['POSTE_NIVO ON H.NUM_POSTE = POSTE_NIVO.NUM_POSTE and type_nivo = 1'],
        period=[datedeb, datefin],
        listorder=['dat', 'h.num_poste'],
        )
question1.run(outputfile=nivomto_obs_fn, header=['dat', 'num_poste', 'neigetot'])

# Postes NIVOSE
question2 = question(
        listvar=["to_char(dat,'YYYY-MM-DD-HH24-MI')", "to_char(h.num_poste, 'fm00000000')", "neigetot"],
        table='H',
        listjoin=['POSTE_NIVO ON H.NUM_POSTE = POSTE_NIVO.NUM_POSTE and type_nivo = 3'],
        period=[datedeb, datefin],
        listorder=['dat', 'h.num_poste'],
        listconditions=["to_char(dat,'HH24') = '06'"]
        )
question2.run(outputfile=nivose_obs_fn, header=['dat', 'num_poste', 'neigetot'], mode=nivomto_mode)

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
# c.writerow(['NUMPOST', 'Date UTC', "HTN cm"])
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
# for ficin in ["NIVOMETEO.obs", "NIVOSE.obs"]:
#     csvfile = open(ficin)
#     reader = csv.reader(csvfile, delimiter="|", skipinitialspace=True)
#     for row in reader:
#         # Ajout d'un 0 si département < 10
#         if re.match("^\d{7}$", row[1]):
#             row[1] = "0" + row[1]
#         if len(row[2]) > 0 and row[2].strip() != "-999":
#             c.writerow([row[1], row[0], row[2]])
