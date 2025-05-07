#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on july 2024

@author: Vernay

'''

import xarray as xr
import glob


def execute(datebegin, dateend):
    """
    Main method
    """
    listpro = glob.glob('PRO*')
    pro     = xr.open_mfdataset(listpro, combine='nested', concat_dim='time')
    # TODO : add check on missing data / period covered ?
    outname = f'PRO_{datebegin.ymdh}_{dateend.ymdh}.nc'
    pro.to_netcdf(outname)
