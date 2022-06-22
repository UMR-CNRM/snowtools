# -*- coding: utf-8 -*-
"""
Created on Wed Oct 13 15:04:27 2021

@author: Sabine Radanovics


"""
import extracthendrix as eh
from bronx.stdtypes.date import Date
from snowtools.DATA import LUSTRE_NOSAVE_USER_DIR
import os

config_user = dict(

    #  Where you want to store the outputs
    folder=os.path.join(LUSTRE_NOSAVE_USER_DIR,'AROME'),

    # Models are defined in the models.ini file
    model_name='AROME',

    # The domain can be defined by its name or its coordinates
    # Existing domain can be found in the config_fa2nc.py file
    domain="alp",

    # Variables to extract and to store in the netcdf file
    # Variable are defined in the config_fa2nc.py file
    variables_nc=['Tair', 'Wind'],

    # "local" if the FA file are on your computer or "hendrix" otherwise
    getter="hendrix",

    # For pre-staging and sending mail during (your mail = destination) extraction
    email_address="sabine.radanovics@meteo.fr",

    # datetime(year, month, day)
    date_start=Date(2020, 1, 1),
    date_end=Date(2020, 1, 2),

    # Analysis hour
    analysis_hour=3,

    # Delta between terms
    delta_terms=1,

    # Term in hour after analysis
    start_term=1,  # Default: 6
    end_term=2,  # Default: 6+24 = 30

    # todo: concatenation mode : "timeseries" or "forecast"
    concat_mode="timeseries",

    # How to group the netcdf files: ["month", "year", "all"] for timeseries,
    # and  ["deterministic", "ensemble"] for forecasts
    final_concatenation="all"
)
e = eh.Extractor(config_user)
# ref 'first_i': np.intp(900), 'last_i': np.intp(1075), 'first_j': np.intp(525), 'last_j': np.intp(750)}
# print(e.latlon2ij(43.7, 5., 46.5, 8.12))  # (901.0, 1075.0, 510.0, 760.0)
# print(e.latlon2ij(43.8, 5., 46.4, 8.12))  # (900.0, 1076.0, 518.0, 751.0)
# print(e.latlon2ij(43.88, 5., 46.39, 8.12))  # (900.0, 1076.0, 525.0, 751.0)
# print(e.latlon2ij(43.88, 5., 46.39, 8.1))  # (900.0, 1075.0, 525.0, 750.0)
# print(e.ij2latlon(1090, 1100, 740, 750))

e.download()
