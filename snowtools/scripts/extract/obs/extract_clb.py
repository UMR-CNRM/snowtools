#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# extract data from the col du lac Blanc database

import argparse
from obsdatacen import read_raw_table
import pandas as pd

parser = argparse.ArgumentParser(
    description="""
        Read temperature, wind speed, wind direction and HTN (snow height) observations from the Col du Lac Blanc
        Database.
        """
)
parser.add_argument("date_min", help="Start date")
parser.add_argument("date_max", help="End date")
parser.add_argument("-o", "--output", help="Output file. If none selected, produce CLB.obs file",
                    default='CLB_obs.csv', dest='output')
parser.add_argument("--rr", action="store_true", default=False, help="Produce RR output")
parser.add_argument("--t2m", action="store_true", default=False, help="Produce air temperature output from "
                                                                      "dome station")
parser.add_argument("--htnlb", action="store_true", default=False, help="Produce snow height output from lac blanc aws")
parser.add_argument("--htnmuz", action="store_true", default=False, help="Produce snow height output from muz aws")
parser.add_argument("--ff", action="store_true", default=False, help="Produce wind speed and direction output from"
                                                                     " dome station")
parser.add_argument("--distro", action="store_true", default=False, help="Produce distrometer output")
args = parser.parse_args()

variables = {}
if args.t2m:
    variables['dom_t_air_brut'] = dict({'MAvg': 't2m_dome'})
if args.ff:
    variables['dom_wind_brut'] = dict({'FFAvg': 'ff_dome', 'DDAvg': 'dd_dome'})
if args.rr:
    variables['lb_rain_brut'] = dict({'RR': 'rr'})
if args.htnlb:
    variables['lb_htn1_brut'] = dict({'MAvg': 'htn1_lb'})
    variables['lb_htn2_brut'] = dict({'MAvg': 'htn2_lb'})
if args.htnmuz:
    variables['muz_htn1_brut'] = dict({'MAvg': 'htn1_muz'})
    variables['muz_htn2_brut'] = dict({'MAvg': 'htn2_muz'})
if args.distro:
    variables['disd_lasera_brut'] = dict({'IAvg': 'I_distrA', 'RAvg': 'R_distrA', 'AAvg': 'A_distrA'})
    variables['disd_laserb_brut'] = dict({'IAvg': 'I_distrB', 'RAvg': 'R_distrB', 'AAvg': 'A_distrB'})
    variables['pws_brut'] = dict({'Intensity': 'I_pws', 'Accumulation': 'A_pws'})
assert len(variables.keys()) > 0, "No variables selected. Please pass at least one of the arguments --ff, --t2m, " \
                           "--htn1, --htn2, --rr, --distro"
data = None
for var, columns in variables.items():
    lb_var = read_raw_table(var, datmin=args.date_min, datmax=args.date_max, read_mode='maindb')
    lb_var = lb_var.rename(columns=columns)
    if isinstance(data, pd.DataFrame):
        data = pd.merge(data, lb_var[[k for k in columns.values()]], on="time")
    else:
        data = lb_var[[k for k in columns.values()]]
    # print(data)

data.to_csv(args.output)
