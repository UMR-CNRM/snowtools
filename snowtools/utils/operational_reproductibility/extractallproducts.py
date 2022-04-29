#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 1 oct 2014

@author: lafaysse
"""


import datetime
import os
import sys


def usage():
    print("USAGE extractallproducts.py datepivot suffixe")
    print("datepivot AAAAMMJJHHMMSS")
    sys.exit(1)


if len(sys.argv) != 3:
    usage()

datepivot = sys.argv[1]
suff = sys.argv[2]

if suff == "OPE":
    os.environ["BDPE_CIBLE_PREFEREE"] = 'SEC'
    os.environ["BDPE_CIBLE_INTERDITE"] = 'INT'
elif suff == 'INT':
    os.environ["BDPE_CIBLE_PREFEREE"] = 'INT'
    os.environ["BDPE_CIBLE_INTERDITE"] = 'SEC'

if len(datepivot) != 14:
    usage()

datejour = datetime.datetime(int(datepivot[0:4]), int(datepivot[4:6]), int(datepivot[6:8]), int(datepivot[8:10]),
                             int(datepivot[10:12]))
dateAna = datejour.replace(hour=6, minute=0)

list_prod_nomm = [11977, 11979, 11980, 11982, 11983, 11985, 11989, 11991]
list_prod_all = [11977, 11979, 11980, 11982, 11983, 11985, 11986, 11988, 11989, 11991]
list_prod_mm = [11986, 11988]
list_prod_arve = [13480, 13481, 13482, 13483]

if datepivot[8:10] == "03":
    list_ech = ["002400", "004800"]
else:
    list_ech = ["000000", "002400", "004800"]

if datepivot[8:10] == "06":
    list_prod = list_prod_arve + list_prod_all
elif datepivot[8:10] == "21":
    list_prod = list_prod_mm
elif datepivot[8:10] == "18":
    list_prod = list_prod_arve + list_prod_nomm
else:
    list_prod = list_prod_nomm

for prod in list_prod:

    if prod in [11977, 11979]:
        pathbase = os.environ['HOME'] + "/RW/alp"
        list_ech_test = list_ech[:]
    elif prod in [11980, 11982]:
        pathbase = os.environ['HOME'] + "/RW/pyr"
        list_ech_test = list_ech[:]
    elif prod in [11983, 11985]:
        pathbase = os.environ['HOME'] + "/RW/cor"
        list_ech_test = list_ech[:]
    elif prod in [11986, 11988]:
        pathbase = os.environ['HOME'] + "/RW/mm"
        list_ech_test = list_ech[:]
    elif prod in [11989, 11991]:
        pathbase = os.environ['HOME'] + "/RW/postes"
        list_ech_test = list_ech[:]
    elif prod in [13481, 13483]:
        pathbase = os.environ['HOME'] + "/RW/arve_noglacier"
        list_ech_test = ["000000"]
    elif prod in [13480, 13482]:
        pathbase = os.environ['HOME'] + "/RW/arve_glacier"
        list_ech_test = ["000000"]

    if prod in [11977, 11980, 11983, 11986, 11989]:
        directory = "meteo"
        fic = "FORCING"
    elif prod in [11979, 11982, 11985, 11988, 11991, 13480, 13481]:
        directory = "pro"
        fic = "PRO"
    elif prod in [13482]:
        directory = "prep"
        fic = "reservoirs_glacier"
    elif prod in [13483]:
        directory = "prep"
        fic = "reservoirs_noglacier"
    else:
        sys.exit("Unknown product:" + str(prod))

    for ech in list_ech_test:
        if suff == "DEV":
            if ech == "000000":
                datedeb = dateAna - datetime.timedelta(days=1)
                datefin = dateAna
                pathin = pathbase + "/ANA-" + datepivot[8:10] + "/" + directory + "/" + fic + "_" + \
                         datedeb.strftime('%Y%m%d%H') + "_" + datefin.strftime('%Y%m%d%H') + ".nc"
            elif ech == "002400":
                datedeb = dateAna
                datefin = dateAna + datetime.timedelta(days=1)
                pathin = pathbase + "/P24-" + datepivot[8:10] + "/" + directory + "/" + fic + "_" + \
                         datedeb.strftime('%Y%m%d%H') + "_" + datefin.strftime('%Y%m%d%H') + ".nc"
            elif ech == "004800":
                datedeb = dateAna + datetime.timedelta(days=1)
                datefin = dateAna + datetime.timedelta(days=2)
                pathin = pathbase + "/P48-" + datepivot[8:10] + "/" + directory + "/" + fic + "_" + \
                         datedeb.strftime('%Y%m%d%H') + "_" + datefin.strftime('%Y%m%d%H') + ".nc"

            os.symlink(pathin, str(prod) + "_" + datepivot + "_" + ech + "_" + suff + ".nc")
        else:
            os.system("lirepe " + str(prod) + " " + datepivot + " " + ech + " " + str(prod) + "_" +
                      datepivot + "_" + ech + "_" + suff + ".nc")
