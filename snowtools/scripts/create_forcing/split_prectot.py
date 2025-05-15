#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 15 may 2025

@author: Diego Monteiro

Create a new forcing file (or append an existing) from an initial one (containing at least specific humidity, air temperature and total precipitation)
with recalculated solid and liquid precipitation based on Froidurot et al., (2014) method.
The method separate total precipitation, either given as one variable (PTOT) or two (Rainf and Snowf),
using a logistic function that take near surface air temperature (°C) and relative humidity (%)
as predictors.
Note : Some parameters of the logistic function (alpha, beta and gamma) can be site adjusted,
see Froidurot et al., (2014) for further details.
"""

import argparse
from typing import List, Tuple

import numpy as np
import xarray as xr

DEFAULT_NETCDF_FORMAT = "NETCDF4_CLASSIC"


def parse_command_line():
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
    return args


def qair2rh(qair: np.array, tair: np.array, psurf: np.array = 1013.25):
    """
    Function to convert specific humidity (kg/kg) into relative humidity (%).

    :param tair: air temperature (°C)
    :type tair: np.array
    :param qair: specific humidity (kg/kg)
    :type qair: np.array
    :param psurf: atmospheric pressure at surface (mb or hPa)
    :type psurf: np.array
    :return rh: relative humidity
    :rtype rh: np.array
    """

    es = 6.112 * np.exp((17.67 * tair) / (tair + 243.5))
    e = qair * psurf / (0.378 * qair + 0.622)
    rh = e / es
    rh[rh > 1] = 1
    rh[rh < 0] = 0

    return rh * 100


def splitting_prectot(
    prectot: np.array, tair: np.array, rh: np.array, alpha: float = 22, beta: float = -2.7, gamma: float = -0.2
):
    """Function used to partition total precipitation into liquid and solid part
    using air temperature and relative humidity.
    Alpha, beta and gamma are parameters of logistic values that can be adjusted
    depending on the site considered following Froidurot et al., (2014) method
    (default values are taken from Koistinen and Saltikoff (1998))

    :param prectot: total precipitation (units in is unit out)
    :type prectot: np.array
    :param tair: air temperature (°C)
    :type tair: np.array
    :param rh:  near surface relative humidity (%)
    :type rh: np.array
    :param alpha: parameters or logistic function, defaults to 22
    :type alpha: float, optional
    :param beta: parameters or logistic function, defaults to -2.7
    :type beta: float, optional
    :param gamma: parameters or logistic function, defaults to -0.2
    :type gamma: float, optional
    :return: Tuple of precliq and precsol
    :rtype: Tuple of np.array
    """

    # Compute probability of rain and snow using air temperature and relative humidity
    prain = 1 / (1 + np.exp(alpha + beta * tair + gamma * rh))
    psnow = 1 - prain

    ## Create output liquid and solid precipitation amount
    # precliq : liquid precipitation (units in is unit out)
    # precsol : solid precipitation (units in is unit out)

    precliq = prectot * prain
    precsol = prectot * psnow

    return precliq, precsol


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
    rh = qair2rh(qair, tair - 273.15, psurf / 100)

    # Compute new solid and liquid precipitation
    prec = splitting_prectot(prectot, tair - 273.15, rh)

    # Create new variable variable
    Rainf = xr.Variable(
        dims=ds["Tair"].dims, data=prec[0], attrs=dict(long_name="Rainfall Rate", units="kg/m2/s", _FillValue=-9999999.0)
    )
    Snowf = xr.Variable(
        dims=ds["Tair"].dims, data=prec[1], attrs=dict(long_name="Snowfall Rate", units="kg/m2/s", _FillValue=-9999999.0)
    )

    ds["Rainf"] = Rainf
    ds["Snowf"] = Snowf

    if infile == outfile:
        ds.to_netcdf(outfile, mode="a", format=DEFAULT_NETCDF_FORMAT, engine="netcdf4")
    else:
        ds.to_netcdf(outfile, mode="w", format=DEFAULT_NETCDF_FORMAT, engine="netcdf4")


if __name__ == "__main__":
    args = parse_command_line()
    infile = args.infile
    outfile = args.outfile
    prectot = args.prectot

    main_splitprectot(infile=infile, prectot=prectot, outfile=outfile)
