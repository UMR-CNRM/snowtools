#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse
import math
import xml.etree.cElementTree as et

import shapefile

# Command-line arguments
parser = argparse.ArgumentParser(description="""
Generate the Massifs section of METADATA.xml file from a shapefile describing massifs (massifs.shp).

NB: It is possible to use the same file as input and output but the Massifs tag will be overwritten
with no way of undoing changes.
""")
parser.add_argument("inputshp", help="Input shapefile (shp file)")
parser.add_argument("-i", "--input", default=None, dest='inputxml',
                    help="Input filename (to overwrite <Massifs> tag of an existing file)")
parser.add_argument("-o", "--output", default=None, dest='outputxml',
                    help="Output filename (default to standard output)")
args = parser.parse_args()

# Read or create the XML structure
if args.inputxml is None:
    root = et.Element("root")
    root.text = '\n'
else:
    tree = et.parse(args.inputxml)
    root = tree.getroot()

massifs = root.find('Massifs')
if massifs is None:
    massifs = et.SubElement(root, "Massifs")
else:
    massifs.clear()
massifs.tail = '\n'

# Open shapefile file
shp = shapefile.Reader(args.inputshp)
lines = shp.records()

# Build XML with shapefile data
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
    et.SubElement(m, "latCenter").text = '{:4.6f}'.format(l['lat_centro'])
    et.SubElement(m, "lonCenter").text = '{:4.6f}'.format(l['lon_centro'])
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


_pretty_print(massifs)
outs = et.tostring(root, encoding='UTF-8').decode('utf-8')

if args.outputxml is not None:
    with open(args.outputxml, 'w', encoding='UTF-8') as ff:
        ff.write(outs)
else:
    print(outs)
