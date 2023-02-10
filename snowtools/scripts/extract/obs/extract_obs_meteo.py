#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Extraction des données d'observation depuis la BDCLIM
# Matthieu Lafaysse 10 sept 2014
# Modified Léo Viallon-Galinier 22 oct 2021
# Script simplifié pour obtenir un format obs.csv depuis la BDCLIM

import argparse
import os

from snowtools.scripts.extract.obs.bdquery import question
from bronx.stdtypes.date import Date

parser = argparse.ArgumentParser(
        description="Read precititation and temperature observations from BDCLim for all availables stations"
        )
parser.add_argument("date_min", help="Start date")
parser.add_argument("date_max", help="End date")
parser.add_argument("-o", "--output", help="Output folder",
                    default='.', dest='output')
parser.add_argument("--rr", action="store_true", default=False, help="Produce RR output")
parser.add_argument("--tn", action="store_true", default=False, help="Produce TN output")
parser.add_argument("--tx", action="store_true", default=False, help="Produce TX output")
parser.add_argument("--tt", action="store_true", default=False, help="Produce T output at hourly timestep")
parser.add_argument("--f", dest="frequency", action="store", default="daily", help="Observation frequency", choices=["monthly", "daily"])
args = parser.parse_args()

if not os.path.isdir(args.output):
    os.mkdir(args.output)

# List of stations with homoegenized series of mmonthly observations
#stations_t = ['04049001', '05046001', '05098001', '05157001', '05183001', '06029001', '06088001', '06127001',
#              '09023002', '09029001', '09122001', '09289001', '20004002', '20041001', '20050001', '20148001',
#              '20342001', '31042012', '31069001', '31157001', '31403002', '38130001', '38242001', '38299001',
#              '38421001', '38442001', '38548001', '64024001', '64189001', '64274001', '64320001', '64549001',
#              '65059009', '65344001', '66016001', '66136001', '66148001', '73026001', '73034002', '73054001',
#              '74093001', '74280001']
#stations_r = ['04024001', '04039001', '04041001', '04049001', '04070009', '04076001', '04088001', '04096002',
#              '04099001', '04113001', '04178001', '04226001', '04230001', '05004001', '05007001', '05013003',
#              '05026001', '05027001', '05032002', '05038001', '05046001', '05063001', '05064001', '05070001',
#              '05079001', '05090001', '05090002', '05093001', '05098001', '05101001', '05110001', '05127001',
#              '05132001', '05139002', '05139006', '05142001', '05157001', '05182001', '06004002', '06004004',
#              '06016001', '06023004', '06029001', '06050002', '06057001', '06071001', '06077006', '06086001',
#              '06088001', '06088003', '06088007', '06094002', '06118002', '06127001', '06148001', '06163001',
#              '06163007', '09027001', '09029001', '09047001', '09055001', '09100001', '09122001', '09181003',
#              '09194001', '09220001', '09225002', '09283002', '09289001', '09299003', '09306001', '09328001',
#              '20004002', '20004003', '20031001', '20040001', '20041001', '20050001', '20108001', '20108002',
#              '20211001', '20232001', '20249001', '20272001', '20312001', '31042001', '31069001', '31070004',
#              '31144001', '31157001', '31374001', '31403002', '31451001', '31540001', '38006001', '38021001',
#              '38034001', '38040001', '38053001', '38053003', '38064001', '38073001', '38082001', '38095001',
#              '38128001', '38130001', '38153001', '38162001', '38170001', '38226002', '38242001', '38248002',
#              '38285001', '38334001', '38421001', '38442001', '38501001', '38504001', '38517001', '38522001',
#              '38527001', '38548001', '64006001', '64024001', '64092002', '64109001', '64123001', '64157001',
#              '64189001', '64216001', '64301002', '64316001', '64320001', '64320003', '64324001', '64331002',
#              '64342001', '64416001', '64445008', '64450001', '64480001', '64537001', '64549001', '65031001',
#              '65032001', '65055001', '65059009', '65123001', '65129003', '65258001', '65295003', '65304001',
#              '65319001', '65344001', '65388001', '65452001', '66016001', '66024001', '66029001', '66037001',
#              '66038003', '66049001', '66062001', '66117001', '66127001', '66136001', '66137003', '66148001',
#              '66183001', '66230001', '73004001', '73034002', '73054001', '73055001', '73064001', '73132001',
#              '73146001', '73167001', '73181001', '73206001', '73215001', '73220001', '73232001', '73258001',
#              '73261001', '73330001', '74024001', '74034001', '74035001', '74051002', '74056001', '74080002',
#              '74081002', '74083002', '74085001', '74087001', '74093001', '74134002', '74137001', '74173001',
#              '74203001', '74211002', '74221001', '74256001', '74258002', '74280001', '74281001', '74285002',
#              '74286001', '74289001']

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
    sufix = "_me"
elif args.frequency == "daily":
    table = "Q"
    sufix = ""
else:
    table = "H"

# 1. RR
# ------

if args.rr:
    # WARNING 1 : l'extraction de la table Q fournit des cumuls entre 0:00 J-1 et 0:00 J
    # pour avoir des observations entre 6:00 J-1 et 6:00 J il faut extraire les valeurs horaires
    # puis faire le cumul à la main.
    # WARNING 2 : l'extraction pour la date y/m/d correspond au cumul entre y/m/d à 0:00
    # et y/m/(d+1) à 0:00

    question1 = question(
            listvar=["dat", f"{table}.num_poste", "poste.nom_usuel", "poste.alti", "poste.lat_dg", "poste.lon_dg", "rr"+sufix],
            table=table,
            listorder=[f'{table}.num_poste', 'dat'],
            listjoin=[
                f"POSTE ON {table}.NUM_POSTE = POSTE.NUM_POSTE ", f" HIST_RESEAU_POSTE on ({table}.NUM_POSTE = HIST_RESEAU_POSTE.NUM_POSTE) ",
                ],
            listconditions=["HIST_RESEAU_POSTE.RESEAU_POSTE in ('51','53','64')"],
            period=[datedeb, datefin],
            dateformat=args.frequency,
            )
    question1.run(outputfile=f'raw_{args.frequency}_obs_RR_{begin.ymd}_{end.ymd}.csv')

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
            period=[datedeb, datefin],
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
            period=[datedeb, datefin],
            dateformat=args.frequency,
            )
    question3.run(outputfile=f'raw_{args.frequency}_obs_TX_{begin.ymd}_{end.ymd}.csv')

# 4. T horaires
# ---------------

if args.tt:
    question4 = question(
            listvar=["dat", "H.num_poste", "poste.nom_usuel", "poste.alti", "t", "poste.lon_dg", "poste.lat_dg"],
            table='H',
            listorder=['H.num_poste', 'dat'],
            listjoin=[
                #'POSTE ON H.NUM_POSTE = POSTE.NUM_POSTE and POSTE.NUM_POSTE in (' + ','.join(stations_t) + ')'
                f"POSTE ON H.NUM_POSTE = POSTE.NUM_POSTE ", f" HIST_RESEAU_POSTE on (H.NUM_POSTE = HIST_RESEAU_POSTE.NUM_POSTE) ",
                ],
            listconditions=["HIST_RESEAU_POSTE.RESEAU_POSTE in ('51','53','64')"],
            period=[datedeb, datefin],
            )
    question4.run(outputfile='obs_brutes_T_horaires.csv')
