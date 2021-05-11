#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Extraction des données d'observation depuis la BDCLIM
# Matthieu Lafaysse 10 sept 2014
# Script simplifié pour obtenir un format obs.csv depuis la BDCLIM

import sys, os
import string
import re
import datetime

import csv

from extract_obs import question 

def usage():
    print("USAGE extract_rr_mens.py datedeb datefin")
    print("format des dates : YYYYMMDD")
    sys.exit(1)

stations_t = ['04049001', '05046001', '05098001', '05157001', '05183001', '06029001', '06088001', '06127001', '09023002', '09029001', '09122001', '09289001', '20004002', '20041001', '20050001', '20148001', '20342001', '31042012', '31069001', '31157001', '31403002', '38130001', '38242001', '38299001', '38421001', '38442001', '38548001', '64024001', '64189001', '64274001', '64320001', '64549001', '65059009', '65344001', '66016001', '66136001', '66148001', '73026001', '73034002', '73054001', '74093001', '74280001']
stations_r = ['04024001', '04039001', '04041001', '04049001', '04070009', '04076001', '04088001', '04096002', '04099001', '04113001', '04178001', '04226001', '04230001', '05004001', '05007001', '05013003', '05026001', '05027001', '05032002', '05038001', '05046001', '05063001', '05064001', '05070001', '05079001', '05090001', '05090002', '05093001', '05098001', '05101001', '05110001', '05127001', '05132001', '05139002', '05139006', '05142001', '05157001', '05182001', '06004002', '06004004', '06016001', '06023004', '06029001', '06050002', '06057001', '06071001', '06077006', '06086001', '06088001', '06088003', '06088007', '06094002', '06118002', '06127001', '06148001', '06163001', '06163007', '09027001', '09029001', '09047001', '09055001', '09100001', '09122001', '09181003', '09194001', '09220001', '09225002', '09283002', '09289001', '09299003', '09306001', '09328001', '20004002', '20004003', '20031001', '20040001', '20041001', '20050001', '20108001', '20108002', '20211001', '20232001', '20249001', '20272001', '20312001', '31042001', '31069001', '31070004', '31144001', '31157001', '31374001', '31403002', '31451001', '31540001', '38006001', '38021001', '38034001', '38040001', '38053001', '38053003', '38064001', '38073001', '38082001', '38095001', '38128001', '38130001', '38153001', '38162001', '38170001', '38226002', '38242001', '38248002', '38285001', '38334001', '38421001', '38442001', '38501001', '38504001', '38517001', '38522001', '38527001', '38548001', '64006001', '64024001', '64092002', '64109001', '64123001', '64157001', '64189001', '64216001', '64301002', '64316001', '64320001', '64320003', '64324001', '64331002', '64342001', '64416001', '64445008', '64450001', '64480001', '64537001', '64549001', '65031001', '65032001', '65055001', '65059009', '65123001', '65129003', '65258001', '65295003', '65304001', '65319001', '65344001', '65388001', '65452001', '66016001', '66024001', '66029001', '66037001', '66038003', '66049001', '66062001', '66117001', '66127001', '66136001', '66137003', '66148001', '66183001', '66230001', '73004001', '73034002', '73054001', '73055001', '73064001', '73132001', '73146001', '73167001', '73181001', '73206001', '73215001', '73220001', '73232001', '73258001', '73261001', '73330001', '74024001', '74034001', '74035001', '74051002', '74056001', '74080002', '74081002', '74083002', '74085001', '74087001', '74093001', '74134002', '74137001', '74173001', '74203001', '74211002', '74221001', '74256001', '74258002', '74280001', '74281001', '74285002', '74286001', '74289001']

if __name__ == "__main__":

    if len(sys.argv) != 3:
        usage()
    datedeb = sys.argv[1] + "00"
    datefin = sys.argv[2] + "23"

    # II.1.1 construction de la question pour les postes nivo pour la HTN
    # ---------------------------------------------------------------------------
    # On prend toutes les heures

# 1. RR mensuelles
#-----------------
#
#    question1 = question("question_r_mens.q")
#    question1.set_fileout("obs_mensuelles_brutes_RR.data")
#    question1.set_varout(["dat", "mensq.num_poste", "poste.nom_usuel", "poste.alti", "rr", "rr_me"])
#    question1.set_tables(["MENSQ", "POSTE"])
#    question1.set_joincondition("MENSQ.NUM_POSTE = POSTE.NUM_POSTE and POSTE.NUM_POSTE in (" + ','.join(stations_r) + ')')
#    question1.set_period_monthly(datedeb, datefin)
#    question1.set_order(["mensq.num_poste", "dat"])
#    question1.run()
#
# 2. TN mensuelles
#-----------------
#
#    question2 = question("question_tn_mens.q")
#    question2.set_fileout("obs_mensuelles_brutes_TN.data")
#    question2.set_varout(["dat", "mensq.num_poste", "poste.nom_usuel", "poste.alti", "tn","tn_me"])
#    question2.set_tables(["MENSQ", "POSTE"])
#    question2.set_joincondition("MENSQ.NUM_POSTE = POSTE.NUM_POSTE and POSTE.NUM_POSTE in (" + ','.join(stations_t) + ')')
#    question2.set_period_monthly(datedeb, datefin)
#    question2.set_order(["mensq.num_poste", "dat"])
#    question2.run()
#
# 3. TX mensuelles
#-----------------
#
#    question3 = question("question_tx_mens.q")
#    question3.set_fileout("obs_mensuelles_brutes_TX.data")
#    question3.set_varout(["dat", "mensq.num_poste", "poste.nom_usuel", "poste.alti", "tx","tx_me"])
#    question3.set_tables(["MENSQ", "POSTE"])
#    question3.set_joincondition("MENSQ.NUM_POSTE = POSTE.NUM_POSTE and POSTE.NUM_POSTE in (" + ','.join(stations_t) + ')')
#    question3.set_period_monthly(datedeb, datefin)
#    question3.set_order(["mensq.num_poste", "dat"])
#    question3.run()
#
# 4. T horaires 
#---------------
    question4 = question("question_t.q")
    question4.set_fileout("obs_brutes_T.data")
    question4.set_varout(["dat", "H.num_poste", "poste.nom_usuel", "poste.alti", "tn", "tx", "t"])
    question4.set_tables(["H", "POSTE"])
    question4.set_joincondition("H.NUM_POSTE = POSTE.NUM_POSTE and POSTE.NUM_POSTE in (" + ','.join(stations_t) + ')')
    question4.set_period(datedeb, datefin)
    question4.set_condition("tn is not NULL and tx is not NULL")
    question4.set_order(["H.num_poste", "dat"])
    question4.run()
