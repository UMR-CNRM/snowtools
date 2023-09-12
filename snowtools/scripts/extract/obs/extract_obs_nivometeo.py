#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Extraction des données d'observation depuis la BDCLIM
# Matthieu Lafaysse 10 sept 2014
# Modified Léo Viallon-Galinier 22 oct 2021
# Modified Matthieu Vernay 15 mar 2022
# Script simplifié pour obtenir un format obs.csv depuis la BDCLIM

# rundir on sxcen : /home/vernaym/workdir/extraction_obs_eval_antilope

import argparse
import os

from snowtools.scripts.extract.obs.bdquery import question
from bronx.stdtypes.date import Date, Period

parser = argparse.ArgumentParser(
        description="Read precititation and temperature observations from BDCLim for all availables stations"
        )
parser.add_argument("date_min", help="Start date")
parser.add_argument("date_max", help="End date", default=None)
parser.add_argument("-o", "--output", help="Output folder",
                    default='.', dest='output')
parser.add_argument("--rr", action="store_true", default=True, help="Produce RR output")
parser.add_argument("--tn", action="store_true", default=False, help="Produce TN output")
parser.add_argument("--tx", action="store_true", default=False, help="Produce TX output")
parser.add_argument("--tt", action="store_true", default=False, help="Produce T output at hourly timestep")
parser.add_argument("--postes", action="store_true", default=False, help="Print list of stations")
parser.add_argument("--f", dest="frequency", action="store", default="daily", help="Observation frequency", choices=["monthly", "daily", "hourly"])
args = parser.parse_args()

if not os.path.isdir(args.output):
    os.mkdir(args.output)


if __name__ == "__main__":

    datedeb = args.date_min
    datefin = args.date_max
    begin = Date(datedeb)
    end = Date(datefin)

    # II.1.1 construction de la question pour les postes nivo pour la HTN
    # ---------------------------------------------------------------------------
    # On prend toutes les heures

if args.frequency == "monthly":
    table = "MENSQ"
    suffix = "_me"
elif args.frequency == "daily":
    table = "Q"
    suffix = ""
elif args.frequency == "hourly":
    table = "H"
    suffix = "1" 


if args.postes:
    question_postes = question(
            listvar=["poste_nivo.num_poste", "poste_nivo.nom_usuel", "poste_nivo.alti", "poste_nivo.lat_dg", "poste_nivo.lon_dg", "hist_reseau_poste.reseau_poste"],
            table='POSTE_NIVO',
            listjoin=["HIST_RESEAU_POSTE on (POSTE_NIVO.NUM_POSTE = HIST_RESEAU_POSTE.NUM_POSTE and HIST_RESEAU_POSTE.RESEAU_POSTE in ('51','53','64'))",],
            listconditions=["datouvr<to_date('{0:s}', 'YYYYMMDD')".format(datedeb), "(datferm>to_date('{0:s}', 'YYYYMMDD') OR datferm is Null)".format(datefin)],
            listorder=['num_poste'],
            )
    question_postes.run(outputfile=f'postes_nivometeo.csv')

# 1. RR
# ------

if args.rr:
    question1 = question(
            listvar=[f"{table}.dat", f"{table}.num_poste", "poste_nivo.nom_usuel", "poste_nivo.alti", "poste_nivo.lat_dg", "poste_nivo.lon_dg", 
                "poste_nivo.massif_nivo", f"{table}.rr"+suffix, "hist_reseau_poste.reseau_poste"],
            table=table,
            listorder=[f'{table}.num_poste', f'{table}.dat'],
            listjoin=[
                f"POSTE_NIVO ON {table}.NUM_POSTE = POSTE_NIVO.NUM_POSTE ", f" HIST_RESEAU_POSTE on ({table}.NUM_POSTE = HIST_RESEAU_POSTE.NUM_POSTE) ",
                ],
            listconditions=[f"HIST_RESEAU_POSTE.RESEAU_POSTE in ('51','53','64') AND {table}.rr{suffix} is not Null"],
            #period=[datedeb, datefin],
            period=[begin.ymdh, end.ymdh],
            )
    if args.frequency == 'daily':
        print("WARNING : The rr value corresponding to date 'ymd' is the observation of date 'ym(d+1)' covering ymd6h-->ym(d+1)6h")
    question1.run(outputfile=f'obs_nivometeo_{args.frequency}_RR_{begin.ymd}_{end.ymd}.csv', header=['date', 'num_poste', 'nom', 'alti', 'lat', 'lon', 'massif', 'rr', 'reseau_poste'])

    question2 = question(
            listvar=[f"H_NIVO.NUM_POSTE", "H_NIVO.ALTI_LPNX", "H_NIVO.DAT"], # ALTI_LPNX=altitude maximale de la LPN depuis la dernière obs
            table="H_NIVO",
            listorder=[f'H_NIVO.NUM_POSTE', f'H_NIVO.DAT'],
            listjoin=[
                f"HIST_RESEAU_POSTE on (H_NIVO.NUM_POSTE = HIST_RESEAU_POSTE.NUM_POSTE) ",
                ],
            listconditions=["HIST_RESEAU_POSTE.RESEAU_POSTE in ('51','53','64') ", "H_NIVO.ALTI_LPNX is not Null "],
            #period=[datedeb, datefin],
            period=[begin.ymdh, end.ymdh],
            )
    question2.run(outputfile=f'LPN_nivometeo_{begin.ymd}_{end.ymd}.csv')

# 2. TN
# ------

if args.tn:
    question2 = question(
            listvar=["dat", f"{table}.num_poste", "poste.nom_usuel", "poste.alti", "tn"+sufix],
            table=table,
            listorder=[f'{table}.num_poste', 'dat'],
            listjoin=[
                f'POSTE ON {table}.NUM_POSTE = POSTE.NUM_POSTE and POSTE.NUM_POSTE in (' + ','.join(stations_t) + ')'
                ],
            #period=[datedeb, datefin],
            period=[begin.ymdh, end.ymdh],
            dateformat=args.frequency,
            )
    question2.run(outputfile=f'raw_{args.frequency}_obs_TN_{begin.ymd}_{end.ymd}.csv')

# 3. TX
# ------

if args.tx:
    question3 = question(
            listvar=["dat", f"{table}.num_poste", "poste.nom_usuel", "poste.alti", "tx"+sufix],
            table=table,
            listorder=[f'{table}.num_poste', 'dat'],
            listjoin=[
                f'POSTE ON {table}.NUM_POSTE = POSTE.NUM_POSTE and POSTE.NUM_POSTE in (' + ','.join(stations_t) + ')'
                ],
            #period=[datedeb, datefin],
            period=[begin.ymdh, end.ymdh],
            dateformat=args.frequency,
            )
    question3.run(outputfile=f'raw_{args.frequency}_obs_TX_{begin.ymd}_{end.ymd}.csv')

# 4. T horaires
# ---------------

if args.tt:
    question4 = question(
            #listvar=["dat", "H.num_poste", "poste.nom_usuel", "poste.alti", "tn", "tx", "t"],
            listvar=["dat", "H.num_poste", "poste.nom_usuel", "poste.alti", "t"],
            table='H',
            listorder=['H.num_poste', 'dat'],
            listjoin=[
                #'POSTE ON H.NUM_POSTE = POSTE.NUM_POSTE and POSTE.NUM_POSTE in (' + ','.join(stations_t) + ')'
                f"POSTE ON H.NUM_POSTE = POSTE.NUM_POSTE ", f" HIST_RESEAU_POSTE on (H.NUM_POSTE = HIST_RESEAU_POSTE.NUM_POSTE) ",
                ],
            listconditions=["HIST_RESEAU_POSTE.RESEAU_POSTE in ('51','53','64')"],
#            listconditions=[
#                "tn is not NULL",
#                "tx is not NULL"
#                ],
#            period=[datedeb, datefin],
            )
    question4.run(outputfile='obs_brutes_T.csv')
