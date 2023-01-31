#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import math
import xml.etree.cElementTree as et
import xml.etree.ElementTree as etp

import shapefile

# Command-line arguments
parser = argparse.ArgumentParser(description="Generate the METATADA_Massif.xml file from massif shapefile")
parser.add_argument("input", help="Input shapefile (shp file)")
parser.add_argument("-o", "--output", type=argparse.FileType('w', encoding='UTF-8'), default='-', dest='out',
                    help="Output filename (default to standard output)")
args = parser.parse_args()

# Creation of XML structure
root = et.Element("root")
massifs = et.SubElement(root, "Massifs")

# Open file
shp = shapefile.Reader(args.input)
lines = shp.records()
for line in lines:
    l = line.as_dict()
    altmin_real = int(l['Zstat_min'])
    altmax_real = int(l['Zstat_max'])
    altmin_safran = math.floor(altmin_real / 300.) * 300
    altmax_safran = math.ceil(altmax_real / 300.) * 300
    m = et.SubElement(massifs, "Massif")
    et.SubElement(m, "name").text = l['title']
    et.SubElement(m, "nameRed").text = l['title_shor']
    et.SubElement(m, "number").text = '{:d}'.format(l['code'])
    et.SubElement(m, "latCenter").text = '{:4.2f}'.format(l['lat_centro'])
    et.SubElement(m, "lonCenter").text = '{:4.2f}'.format(l['lon_centro'])
    et.SubElement(m, "altMin").text = '{:d}'.format(altmin_safran)
    et.SubElement(m, "altMax").text = '{:d}'.format(altmax_safran)
    et.SubElement(m, "altRealMin").text = '{:d}'.format(altmin_real)
    et.SubElement(m, "altRealMax").text = '{:d}'.format(altmax_real)


# Prettifying
# In the future, use xml.etree.ElementTree.indent(tree, space='\t', level=0)
# But only available from python 3.9
def _pretty_print(current, parent=None, index=-1, depth=0):
    for i, node in enumerate(current):
        _pretty_print(node, current, i, depth + 1)
    if parent is not None:
        if index == 0:
            parent.text = '\n' + ('\t' * depth)
        else:
            parent[index - 1].tail = '\n' + ('\t' * depth)
        if index == len(parent) - 1:
            current.tail = '\n' + ('\t' * (depth - 1))


_pretty_print(root)

args.out.write(et.tostring(root, encoding='UTF-8').decode('utf-8'))
