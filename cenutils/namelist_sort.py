#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import argparse

from bronx.datagrip.namelist import NamelistParser, FIRST_ORDER_SORTING

parser = argparse.ArgumentParser(description="Parse a namelist and sort it")
parser.add_argument("namelist", default=None, help="Namelist input")
parser.add_argument("--output", "-o", required=True, help="File for sorted namelist output")
args = parser.parse_args()

n = NamelistParser()
N = n.parse(args.namelist)

with open(args.output, 'w') as ff:
    try:
        ff.write(N.dumps(sorting=FIRST_ORDER_SORTING))
    except NotImplementedError:
        ff.write(N.dumps())
