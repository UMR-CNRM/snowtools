#!/usr/bin/env python3
# -*- coding: utf-8 -*-
import os

from snowtools.plots.pearps2m.postprocess import config

usage = "usage: python secure_graph_prod.py [-b YYYYMMDD] [-e YYYYMMDD] [-o diroutput]"

c = config()

print(len(os.listdir(c.diroutput_plots)))
print(len(os.listdir(c.diroutput_maps)))

if len(os.listdir(c.diroutput_maps)) > 0 and len(os.listdir(c.diroutput_plots)) > 0:
    print("output plots created. Goodbye.")
else:
    print("no output plots. retry...")
    # import postprocess as pp
    import snowtools.plots.pearps2m.postprocess as pp

    pp.main(c)
