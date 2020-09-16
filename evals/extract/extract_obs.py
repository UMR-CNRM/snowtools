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


def usage():
    print("USAGE extract_obs.py datedeb datefin [appendfile]")
    print("format des dates : YYYYMMDD")
    sys.exit(1)


class question(object):
    def __init__(self, name):
        self.name = name
        self.fichier = open(name, "w")

# BDCLIM POSTGRESQL
        self.fichier.write("\\a\n")
        self.fichier.write("\\t\n")
        self.fichier.write("\\unset ECHO\n")
        self.fichier.write("\\pset footer\n")
        self.fichier.write("\\pset format unaligned\n")

    def run(self):
        self.fichier.write("\\q\n")
        self.fichier.close()
        os.environ["PGPASSWORD"] = "anonymous"
        os.system("psql -h bdclimop-usr  -p 5432 -d bdclim -U anonymous -q < " + self.name)

    def set_fileout(self, name):
        self.fichier.write("\o " + name + "\n")

    def set_varout(self, listvar):
        self.fichier.write("select " + string.join(listvar, ",") + "\n")

    def set_order(self, listvar):
        self.fichier.write("order by " + string.join(listvar, ",") + ";\n")

    def set_tables(self, listtables):
        self.fichier.write("from " + string.join(listtables, " join ") + "\n")

    def set_period(self, datedeb, datefin):
        self.fichier.write("    where DAT between to_timestamp('" + datedeb + "','YYYYMMDDHH24')\n")
        self.fichier.write("        and to_timestamp('" + datefin + "','YYYYMMDDHH24')\n")

    def set_period_monthly(self, datedeb, datefin):
        self.fichier.write("    where DAT >=" + datedeb[0:6] + "\n")
        self.fichier.write("        and DAT <=" + datefin[0:6] + "\n")

    def set_condition(self, condition):
        self.fichier.write("        and " + condition + "\n")

    def set_firstcondition(self, condition):
        self.fichier.write("        where " + condition + "\n")

    def set_joincondition(self, condition):
        self.fichier.write(" on " + condition + "\n")


if __name__ == "__main__":

    if len(sys.argv) != 3 and len(sys.argv) != 4:
        usage()
    datedeb = sys.argv[1] + "00"
    datefin = sys.argv[2] + "23"
    append = len(sys.argv) == 4
    if append:
        appendfile = sys.argv[3]

    # II.1.1 construction de la question pour les postes nivo pour la HTN
    # ---------------------------------------------------------------------------
    # On prend toutes les heures

    question1 = question("question_nivometeo.q")
    question1.set_fileout("NIVOMETEO.obs")
    question1.set_varout(["to_char(dat,'YYYY-MM-DD-HH24-MI')", "h.num_poste", "neigetot"])
    question1.set_tables(["H", "POSTE_NIVO"])
    question1.set_joincondition("H.NUM_POSTE = POSTE_NIVO.NUM_POSTE and type_nivo = 1")
    question1.set_period(datedeb, datefin)
    question1.set_order(["dat", "num_poste"])
    question1.run()

    question2 = question("question_nivose.q")
    question2.set_fileout("NIVOSE.obs")
    question2.set_varout(["to_char(dat,'YYYY-MM-DD-HH24-MI')", "h.num_poste", "neigetot"])
    question2.set_tables(["H", "POSTE_NIVO"])
    question2.set_joincondition("H.NUM_POSTE = POSTE_NIVO.NUM_POSTE and type_nivo = 3")
    question2.set_period(datedeb, datefin)
    question2.set_condition("to_char(dat,'HH24') = '06'")
    question2.set_order(["dat", "num_poste"])
    question2.run()

    # Fichier final

    if append:
        ficname = appendfile
        if os.path.isfile(appendfile):
            # Fichier déjà existant
            os.rename(appendfile, "old.csv")
        else:
            append = False
    else:
        ficname = "OBS_" + datedeb + "_" + datefin + ".csv"

    c = csv.writer(open(ficname, "wb"), delimiter=";")
    c.writerow(['NUMPOST', 'Date UTC', "HTN cm"])

    if append:
        csvold = open("old.csv", "r")
        r = csv.reader(csvold, delimiter=";")
        datedebtime = datetime.datetime(int(datedeb[0:4]), int(datedeb[4:6]), int(datedeb[6:8]), int(datedeb[8:10]))
        datefintime = datetime.datetime(int(datefin[0:4]), int(datefin[4:6]), int(datefin[6:8]), int(datefin[8:10]))
        for row in r:
            #                9 pour obs espagnoles
            if re.match("^\d{8}\d?$", row[0]):
                listdate = row[1].split("-")
                date = datetime.datetime(int(listdate[0]), int(listdate[1]), int(listdate[2]), int(listdate[3]), int(listdate[4]))
                if date < datedebtime or date > datefintime:
                    c.writerow(row)

    for ficin in ["NIVOMETEO.obs", "NIVOSE.obs"]:
        csvfile = open(ficin)
        reader = csv.reader(csvfile, delimiter="|", skipinitialspace=True)
        for row in reader:
            # Ajout d'un 0 si département < 10
            if re.match("^\d{7}$", row[1]):
                row[1] = "0" + row[1]
            if len(row[2]) > 0 and row[2].strip() != "-999":
                c.writerow([row[1], row[0], row[2]])
