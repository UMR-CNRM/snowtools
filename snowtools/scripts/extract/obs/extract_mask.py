#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
# Extraction des masques dans BDLIM
"""
import os
import re
import xml.etree.ElementTree as ET
import csv

from snowtools.scripts.extract.obs.bdquery import question
from snowtools.DATA import SNOWTOOLS_DIR

# Extraction des masques dans la BDCLIM
# ---------------------------------------------------------------------------

question1 = question(
        listvar=["NUM_POSTE", "AZIMUT", "ELEVATION"],
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
    if re.match('^\d{7}$', row[0]):
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
objcsv = open(SNOWTOOLS_DIR + "/DATA/liste_malgeolocalises.txt", "r")
r = csv.reader(objcsv, delimiter=" ", skipinitialspace=True)
for row in r:
    if re.match("^\d{7}$", row[0]):
        code = "0" + row[0]
    else:
        code = row[0]
    list_mal_geolocalises.append(code)

# Lecture du fichier obtenu par calcul des skylinesà partir du MNT
# ---------------------------------------------------------------------------
objcsv = open(SNOWTOOLS_DIR + "/DATA/sta_skylines.csv", "r")

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
metadata = SNOWTOOLS_DIR + "/DATA/METADATA.xml"
savefile = SNOWTOOLS_DIR + "/DATA/METADATA_save.xml"
os.rename(metadata, savefile)
# parser = ET.XMLParser(remove_blank_text=True)
# tree = ET.parse(savefile, parser)
tree = ET.parse(savefile)
root = tree.getroot()

for site in root[1]:
    code = site.find("number").text.strip()
    if code in list(mask.keys()):
        site[-1].tail = "\n      "
        attazim = ET.SubElement(site, "azimut")
        attmask = ET.SubElement(site, "mask")
        attsource = ET.SubElement(site, "source_mask")
        attazim.text = ','.join(azim[code])
        attmask.text = ','.join(mask[code])
        attsource.text = source[code]
        attazim.tail = "\n      "
        attmask.tail = "\n      "
        attsource.tail = "\n    "


tree.write(metadata, encoding="utf-8")
