#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os
import argparse

from bronx.stdtypes.date import today
from snowtools.DATA import LUSTRE_NOSAVE_USER_DIR
from snowtools.plots.pearps2m.postprocess import Config

usage = "usage: python secure_graph_prod.py [-b YYYYMMDD] [-e YYYYMMDD] [-o diroutput]"

PARSER = argparse.ArgumentParser(description="Postprocess new snow heights: "
                                                 "1) extracts operational simulation results,"
                                                 "2) Plots maps for the Alps, the Pyrenees the Corse,"
                                                 "3) Creates spaghetti plots "
                                                 "for all massifs and stations")

PARSER.add_argument("-b", action="store", type=str, dest="datebegin", default=today().ymd,
                    help="First year of extraction")
PARSER.add_argument("-e", action="store", type=str, dest="dateend", default=today().ymd,
                    help="Last year of extraction")
PARSER.add_argument("-o", action="store", type=str, dest="diroutput",
                    default=os.path.join(LUSTRE_NOSAVE_USER_DIR, "PEARPS2M"),
                    help="Output directory")
PARSER.add_argument("--dev", action="store_true", dest="dev", default=False)
PARSER.add_argument("--reforecast", action="store_true", dest="reforecast", default=False)
PARSER.add_argument("--dble", action="store_true", dest="dble", default=False)
OPTIONS = PARSER.parse_args()  # @UnusedVariable

c = Config(OPTIONS)

print(len(os.listdir(c.diroutput_plots)))
print(len(os.listdir(c.diroutput_maps)))

if len(os.listdir(c.diroutput_maps)) > 0 and len(os.listdir(c.diroutput_plots)) > 0:
    print("output plots created. Goodbye.")
else:
    print("no output plots. retry...")
    # import postprocess as pp
    import snowtools.plots.pearps2m.postprocess as pp

    pp.main(c)
