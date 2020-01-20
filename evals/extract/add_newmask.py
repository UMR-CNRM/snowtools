#! /usr/bin/env python
# -*- coding: utf-8 -*-

# Extraction des masques dans BDLIM

from csv import *
import csv
import re
import string
import sys, os

import xml.etree.ElementTree as ET

from .extract_obs import question


if __name__ == "__main__":

    fichier_masques = sys.argv[1]
    source_masques = sys.argv[2]

    objcsv = open(fichier_masques, "r")
    r = csv.reader(objcsv, delimiter=" ", skipinitialspace=True)
    azimSim2 = {}
    maskSim2 = {}
    for row in r:
        if re.match("^\d{7}$", row[0]):
            code = "0" + row[0]
        else:
            code = row[0]

        if not code in list(maskSim2.keys()):
            azimSim2[code] = []
            maskSim2[code] = []
        azimSim2[code].append(row[1])
        maskSim2[code].append(row[2])

    objcsv.close()

    # Ajout des données dans fichier METADATA.xml
    metadata = os.environ["SNOWTOOLS_CEN"] + "/DATA/METADATA.xml"
    savefile = os.environ["SNOWTOOLS_CEN"] + "/DATA/METADATA_save.xml"
    os.rename(metadata, savefile)
    tree = ET.parse(savefile)
    root = tree.getroot()

    for site in root[1]:
        code = site.find("number").text.strip()

        if code in list(maskSim2.keys()):
            attazim = ET.SubElement(site, "azimut")
            attmask = ET.SubElement(site, "mask")
            attsource = ET.SubElement(site, "source_mask")
            attazim.text = string.join(azimSim2[code], ',')
            attmask.text = string.join(maskSim2[code], ',')
            attsource.text = "SIM-COORD-MARIE"

    tree.write(metadata, "utf-8")
