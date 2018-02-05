#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 1 oct 2014

@author: lafaysse
'''


import sys
import datetime
import numpy as np
from utils.prosimu import prosimu
from scipy.stats import nanmean


def usage():
    print("USAGE compare2versions.py fichier1 fichier2")
    print("USAGE compare2versions.py DEV INT DATEPIVOT DATEPIVOT DATEPIVOT...")
    sys.exit(1)


def compare2files(name1, name2, checktime=True):

    file1 = prosimu(name1)
    file2 = prosimu(name2)

    listvar1 = file1.listvar()
    listvar2 = file2.listvar()

    if checktime:
        time1 = file1.readtime()
        time2 = file2.readtime()

        difftime = time1[:] - time2[:]
        mindiff, maxdiff = np.min(difftime), np.max(difftime)

        if mindiff == datetime.timedelta(0) and maxdiff == datetime.timedelta(0):
            print "TIME CONFORME"
        else:
            print "TIME NON CONFORME !!  min=" + str(mindiff) + " : max=" + str(maxdiff)

    for varname in listvar2:
        if varname not in listvar1:
            print varname + " is missing in first file"

    for varname in listvar1:
        if varname != "time":
            if varname in listvar2:
                var1 = file1.read(varname)
                var2 = file2.read(varname)

                if len(var1.shape) != 0:
                    diff = var1[:] - var2[:]

                    mindiff, maxdiff, meandiff = np.nanmin(diff), np.nanmax(diff), nanmean(diff.flatten())

                    if mindiff == 0 and maxdiff == 0:
                        print varname + " : CONFORME"
                    else:
                        print varname + " : mean=" + str(meandiff) + " : min=" + str(mindiff) + " : max=" + str(maxdiff)
            else:
                print varname + " is missing in second file"


if len(sys.argv) == 1:
    usage()
if len(sys.argv) == 3:
    compare2files(sys.argv[1], sys.argv[2])
else:
    mode1 = sys.argv[1]
    mode2 = sys.argv[2]
    list_dates = sys.argv[3:]

    for datepivot in list_dates:
        # list_prod_nomm=[11977,11979,11980,11982,11983,11985]
        # list_prod_mm=[11977,11979,11980,11982,11983,11985,11986,11988]

        list_prod_nomm = [11977, 11979, 11980, 11982, 11983, 11985, 11989, 11991]
        list_prod_all = [11977, 11979, 11980, 11982, 11983, 11985, 11986, 11988, 11989, 11991]
        list_prod_mm = [11986, 11988]
        list_prod_arve = [13480, 13481, 13482, 13483]

        if datepivot[8:10] == "06":
            list_prod = list_prod_arve + list_prod_all
        elif datepivot[8:10] == "21":
            list_prod = list_prod_mm
        elif datepivot[8:10] == "18":
            list_prod = list_prod_arve + list_prod_nomm
        else:
            list_prod = list_prod_nomm

        for prod in list_prod:
            if prod in list_prod_arve:
                list_ech = ["000000"]
                checktime = False
            elif datepivot[8:10] == "03":
                list_ech = ["002400", "004800"]
                checktime = True
            else:
                list_ech = ["000000", "002400", "004800"]
                checktime = True
#                    list_ech=["000000"]
            for ech in list_ech:
                basename = str(prod) + "_" + datepivot + "_" + ech + "_"

                intname = basename + mode1 + ".nc"
                opename = basename + mode2 + ".nc"

                print "\n***********************************************************************"
                print "PRODUIT " + str(prod) + " DATEPIVOT " + datepivot + " ECHEANCE " + ech
                print "***********************************************************************"
                compare2files(intname, opename, checktime=checktime)
