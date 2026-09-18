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

masques_bdclim = os.path.join(SNOWTOOLS_DIR, 'MASQUES.obs')
question1 = question(
        listvar=["NUM_POSTE", "AZIMUT", "ELEVATION"],
        table="MASQUE_NIVO",
        listorder=["num_poste", "azimut"]
        )
question1.run(outputfile=masques_bdclim)

# Lecture du fichier extrait de la BDCLIM
list_mask_in_bdclim = []
objcsv = open(masques_bdclim, "r")
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
        source[code] = "BDCLIM-2026"
    azim[code].append(row[1])
    mask[code].append(row[2])

objcsv.close()

# Lecture de la liste des postes mal géolocalisés pour ne pas les prendre en compte
# ---------------------------------------------------------------------------
list_mal_geolocalises = []
objcsv = open(SNOWTOOLS_DIR + "/DATA/blacklist.csv", "r")
r = csv.reader(objcsv, delimiter=" ", skipinitialspace=True)
for row in r:
    if re.match("^\d{7}$", row[0]):
        code = "0" + row[0]
    else:
        code = row[0]
    list_mal_geolocalises.append(code)

# Lecture du fichier obtenu par calcul des skylines à partir du MNT
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
        source[code] = "IGN30-2026"

    azim[code].append(row[1])
    mask[code].append("{:.2f}".format(float(row[2])))

objcsv.close()

# Ajout des données dans fichier METADATA.xml
inputfile = SNOWTOOLS_DIR + "/DATA/METADATA_withoutmask.xml"
outputfile = SNOWTOOLS_DIR + "/DATA/METADATA_withmask.xml"
# parser = ET.XMLParser(remove_blank_text=True)
# tree = ET.parse(savefile, parser)
tree = ET.parse(inputfile)
root = tree.getroot()

for site in root[1]:
    code = site.find("number").text.strip()
    if code in list(mask.keys()):
        site[-1].tail = "\n\t\t"
        attazim = ET.SubElement(site, "azimut")
        attmask = ET.SubElement(site, "mask")
        attsource = ET.SubElement(site, "source_mask")
        attazim.text = "\t\t" + ','.join(azim[code])
        attmask.text = "\t\t" + ','.join(mask[code])
        attsource.text = source[code]
        attazim.tail = "\n\t\t"
        attmask.tail = "\n\t\t"
        attsource.tail = "\n\t\t"

tree.write(outputfile, encoding="utf-8")


