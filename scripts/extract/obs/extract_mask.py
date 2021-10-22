#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# Extraction des masques dans BDLIM

import os
import string
import re
import xml.etree.ElementTree as ET
import csv

from evals.extract.extract_rr_mens import question

# Extraction des masques dans la BDCLIM
# ---------------------------------------------------------------------------

question1 = question(
        listvars=["NUM_POSTE", "AZIMUT", "ELEVATION"],
        table="MASQUE_NIVO",
        listorder=["num_poste", "azimut"]
        )
question1.run(outputfile='MASQUES.obs')

# Lecture du fichier extrait de la BDCLIM
list_mask_in_bdclim = []
objcsv = open("MASQUES.obs", "r")
r = csv.reader(objcsv, delimiter=";", skipinitialspace=True)
azim = {}
mask = {}
source = {}
for row in r:
    print(row)
    if re.match("^\d{7}$", row[0]):
        code = "0" + row[0]
    else:
        code = row[0]
    if code not in list(mask.keys()):
        azim[code] = []
        mask[code] = []
        list_mask_in_bdclim.append(code)
        source[code] = "BDCLIM"
    azim[code].append(row[1])
    mask[code].append(row[2])

objcsv.close()

# Lecture de la liste des postes mal géolocalisés pour ne pas les prendre en compte
# ---------------------------------------------------------------------------
list_mal_geolocalises = []
objcsv = open(os.environ["SNOWTOOLS_CEN"] + "/DATA/liste_malgeolocalises.txt", "r")
r = csv.reader(objcsv, delimiter=" ", skipinitialspace=True)
for row in r:
    if re.match("^\d{7}$", row[0]):
        code = "0" + row[0]
    else:
        code = row[0]
    list_mal_geolocalises.append(code)

# Lecture du fichier obtenu par calcul des skylinesà partir du MNT
# ---------------------------------------------------------------------------
objcsv = open(os.environ["SNOWTOOLS_CEN"] + "/DATA/sta_skylines.csv", "r")

r = csv.reader(objcsv, delimiter=" ", skipinitialspace=True)

for row in r:
    print(row)
    if re.match("^\d{7}$", row[0]):
        code = "0" + row[0]
    else:
        code = row[0]

    if code in list_mask_in_bdclim or code in list_mal_geolocalises:
        continue

    if code not in list(mask.keys()):
        azim[code] = []
        mask[code] = []
        source[code] = "IGN"

    azim[code].append(row[1])
    mask[code].append("{:.2f}".format(float(row[2])))

objcsv.close()

# Ajout des données dans fichier METADATA.xml
metadata = os.environ["SNOWTOOLS_CEN"] + "/DATA/METADATA.xml"
savefile = os.environ["SNOWTOOLS_CEN"] + "/DATA/METADATA_save.xml"
os.rename(metadata, savefile)
parser = ET.XMLParser(remove_blank_text=True)
tree = ET.parse(savefile, parser)
root = tree.getroot()

for site in root[1]:
    code = site.find("number").text.strip()
    if code in list(mask.keys()):
        attazim = ET.SubElement(site, "azimut")
        attmask = ET.SubElement(site, "mask")
        attsource = ET.SubElement(site, "source_mask")
        attazim.text = string.join(azim[code], ',')
        attmask.text = string.join(mask[code], ',')
        attsource.text = source[code]

tree.write(metadata, pretty_print=True, encoding="utf-8")
