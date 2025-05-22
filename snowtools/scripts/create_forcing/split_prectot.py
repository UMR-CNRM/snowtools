#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 15 may 2025

:Authors:
    - Diego Monteiro

Create a new forcing file (or append an existing) from an initial one (containing at least
specific humidity, air temperature and total precipitation)
with recalculated solid and liquid precipitation based on Froidurot et al., (2014) method.
The method separate total precipitation, either given as one variable (PTOT) or two (Rainf and Snowf),
using a logistic function that take near surface air temperature (°C) and relative humidity (%)
as predictors.
Note : Some parameters of the logistic function (alpha, beta and gamma) can be site adjusted,
see Froidurot et al., (2014) for further details.
"""

import argparse
from typing import List

import numpy as np
import xarray as xr

from snowtools.tools.meteo import splitting_prectot
from bronx.meteo.thermo import Thermo

DEFAULT_NETCDF_FORMAT = "NETCDF4_CLASSIC"


def main_splitprectot(infile: str, prectot: List[str], outfile: str):
    """
    :param infile: path + input filename to read
    :type infile: path-like (str)
    :param outfile: path + output filename to write
    :type outfile: path-like (str)
    :param prectot: Variable name composing total precipitation (either one or multiple variables).
    :type prectot: List (str)
    """

    # units of precipitation (prectot) : units in is unit out
    # units of air temperature (Tair) : K
    # units of surface pressure (Psurf) : Pa
    # units of specific humidity (qair) : kg/kg

    ds = xr.open_dataset(infile, format=DEFAULT_NETCDF_FORMAT)

    if len(prectot) == 1:
        prectot = ds[prectot[0]].values
    elif len(prectot) > 1:
        prectot = np.array([ds[prectot[i]].values for i in range(0, len(prectot))])
        prectot = prectot.sum(axis=0)

    # Get requires variable from file
    tair = ds["Tair"].values
    qair = ds["Qair"].values
    psurf = ds["PSurf"].values

    # Compute relative humidity from specific humidity
    rh = Thermo(['v', 'c'], data=dict(P=psurf, qv=qair, qt=qair, T=tair)).get('Huw')
    rh[rh > 100] = 100
    rh[rh < 0] = 0

    # Compute new solid and liquid precipitation
    prec = splitting_prectot(prectot, tair - 273.15, rh)

    # Create new variable variable
    Rainf = xr.Variable(
        dims=ds["Tair"].dims, data=prec[0], attrs=dict(long_name="Rainfall Rate", units="kg/m2/s",
                                                       _FillValue=-9999999.0)
    )
    Snowf = xr.Variable(
        dims=ds["Tair"].dims, data=prec[1], attrs=dict(long_name="Snowfall Rate", units="kg/m2/s",
                                                       _FillValue=-9999999.0)
    )

    ds["Rainf"] = Rainf
    ds["Snowf"] = Snowf

    if infile == outfile:
        ds.to_netcdf(outfile, mode="a", format=DEFAULT_NETCDF_FORMAT, engine="netcdf4")
    else:
        ds.to_netcdf(outfile, mode="w", format=DEFAULT_NETCDF_FORMAT, engine="netcdf4")


if __name__ == "__main__":
    description = "Create a new forcing file from an initial one with recalculated solid and liquid precipitation"
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument("-i", "--infile", type=str, required=True, help="Input file path")
    parser.add_argument("-o", "--outfile", type=str, required=True, help="Output file path")
    parser.add_argument(
        "-p",
        "--prectot",
        type=int,
        default=["Rainf", "Snowf"],
        help="List of variable names (string) to be sum to get prectot",
    )
    args = parser.parse_args()

    main_splitprectot(infile=args.infile, prectot=args.prectot, outfile=args.outfile)
