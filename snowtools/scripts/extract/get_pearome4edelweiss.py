# -*- coding: utf-8 -*-
"""
Created on Wed Oct 13 15:04:27 2021

@author: Sabine Radanovics


"""
import os
import extracthendrix.configreader as configreader
from bronx.stdtypes.date import Date
from extracthendrix.config.variables import pearome
from snowtools.DATA import LUSTRE_NOSAVE_USER_DIR

config_user = dict(

    #  Where you want to store the outputs
    folder=os.path.join(LUSTRE_NOSAVE_USER_DIR,'PEAROME'),
    native_files_folder=os.path.join(LUSTRE_NOSAVE_USER_DIR,'PEAROME','_native_files_'),
    cache_folder=os.path.join(LUSTRE_NOSAVE_USER_DIR,'PEAROME','_cache_'),


    # Models are defined in the models.ini file
    model_name='PEAROME',
    members=range(1, 17),
    # The domain can be defined by its name or its coordinates
    # Existing domain can be found in the config_fa2nc.py file
    domain="alp",

    # Variables to extract and to store in the netcdf file
    # Variable are defined in the config_fa2nc.py file
    # variables_nc=[pearome.Tair, pearome.Qair, pearome.Wind, pearome.Wind_DIR, pearome.Psurf,
    #               pearome.Rainf, pearome.Snowf, pearome.LWdown, pearome.SWdown, pearome.NEB,
    #               pearome.HUMREL, pearome.isoZeroAltitude, pearome.isowetbt0, pearome.isowetbt1,
    #               pearome.isowetbt1_5],
    variables_nc=[pearome.ZS],

    # "local" if the FA file are on your computer or "hendrix" otherwise
    getter="hendrix",

    # For pre-staging and sending mail during (your mail = destination) extraction
    email_adress="sabine.radanovics@meteo.fr",

    # datetime(year, month, day)
    date_start=Date(2020, 3, 1),
    date_end=Date(2020, 3, 1),

    # Analysis hour
    analysis_hour=3,

    # Delta between terms
    delta_terms=1,

    # Term in hour after analysis
    start_term=0,  # Default: 6
    end_term=0,  # Default: 6+24 = 30

    # todo: concatenation mode : "timeseries" or "forecast"
    concat_mode="forecast",

    # How to group the netcdf files: ["month", "year", "all"] for timeseries,
    # and  ["deterministic", "ensemble"] for forecasts
    final_concatenation="all"
)
# e = eh.Extractor(config_user)
# ref 'first_i': np.intp(900), 'last_i': np.intp(1075), 'first_j': np.intp(525), 'last_j': np.intp(750)}
# print(e.latlon2ij(43.7, 5., 46.5, 8.12))  # (901.0, 1075.0, 510.0, 760.0)
# print(e.latlon2ij(43.8, 5., 46.4, 8.12))  # (900.0, 1076.0, 518.0, 751.0)
# print(e.latlon2ij(43.88, 5., 46.39, 8.12))  # (900.0, 1076.0, 525.0, 751.0)
# print(e.latlon2ij(43.88, 5., 46.39, 8.1))  # (900.0, 1075.0, 525.0, 750.0)
# print(e.ij2latlon(1090, 1100, 740, 750))

# e.download()
e = configreader.apply_config_user(config_user)
# arome_extr = eh.readers.AromeHendrixReader(native_files_folder=config_user['folder'], model=config_user['model_name'],
#                                           runtime=config_user['analysis_hour'])
