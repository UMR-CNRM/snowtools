# -*- coding: utf-8 -*-
"""
Created on Wed Oct 13 15:04:27 2021

@author: Sabine Radanovics


"""
import os
import extracthendrix.configreader as configreader
from bronx.stdtypes.date import Date
from datetime import datetime
from extracthendrix.config.variables import pearome
from snowtools.DATA import LUSTRE_NOSAVE_USER_DIR

config_user = dict(

    #  Where you want to store the outputs
    work_folder=os.path.join(LUSTRE_NOSAVE_USER_DIR, 'AROME'),
    native_files_folder=os.path.join(LUSTRE_NOSAVE_USER_DIR, 'AROME', '_native_files_'),
    cache_folder=os.path.join(LUSTRE_NOSAVE_USER_DIR, 'AROME', '_cache_'),


    # Models are defined in the models.ini file
    # model='PEAROME',
    model='AROME_EURW1S40',
    # members=range(1, 2),
    # The domain can be defined by its name or its coordinates
    # Existing domain can be found in the config_fa2nc.py file
    domain="alp",

    # Variables to extract and to store in the netcdf file
    # Variable are defined in the config_fa2nc.py file
    # variables=['Tair', 'Qair', 'Wind', 'Wind_DIR', 'Psurf',
    #               'Rainf', 'Snowf', 'LWdown', 'SWdown', 'NEB',
    #               'HUMREL', 'isoZeroAltitude', 'isowetbt0', 'isowetbt1',
    #               'isowetbt1_5'],
    # variables=['Psurf', 'prmsl', 'P3d', 't3d', 'papt3d', 'r3d', 'clwc3d', 'ciwc10m', 'ciwc100m',
    #            'cswc10m', 'cswc100m', 'crwc10m', 'crwc100m', 't2m', 'Qair', 'u10m', 'v10m',
    #            'Rainf', 'Snowf', 'refl0m', 'reflfactorsnow', 'HUMREL', 'isoZeroAltitude',
    #            'isowetbt0', 'isowetbt1', 'isowetbt1_5', 'sd', 'avgptype', 'maxptype'],
    variables=['Rainf', 'Snowf', 'isoZeroAltitude', 'isowetbt0', 'isowetbt1', 'isowetbt1_5', 'sd',
               'avgptype', 'maxptype', 'tw3d', 'clwc3d', 't3d', 'P3d', 'r3d', 'ciwc10m', 'ciwc100m'],
    # variables=['ZS'],

    # "local" if the FA file are on your computer or "hendrix" otherwise
    getter="hendrix",

    # For pre-staging and sending mail during (your mail = destination) extraction
    email_address="sabine.radanovics@meteo.fr",

    # datetime(year, month, day)
    start_date=datetime(2022, 4, 1),
    end_date=datetime(2022, 4, 1),

    # Analysis hour
    run=3,

    # Delta between terms
    delta_t=1,

    # Term in hour after analysis
    start_term=1,  # Default: 6
    end_term=45,  # Default: 6+24 = 30
    dtype='64bits',
    make_surfex=False,

    # todo: concatenation mode : "timeseries" or "forecast"
    groupby=("forecast",),
    # groupby=("timeseries", "monthly"),
    # How to group the netcdf files: ["month", "year", "all"] for timeseries,
    # and  ["deterministic", "ensemble"] for forecasts
    # final_concatenation="all"
)
# e = eh.Extractor(config_user)
# ref 'first_i': np.intp(900), 'last_i': np.intp(1075), 'first_j': np.intp(525), 'last_j': np.intp(750)}
# print(e.latlon2ij(43.7, 5., 46.5, 8.12))  # (901.0, 1075.0, 510.0, 760.0)
# print(e.latlon2ij(43.8, 5., 46.4, 8.12))  # (900.0, 1076.0, 518.0, 751.0)
# print(e.latlon2ij(43.88, 5., 46.39, 8.12))  # (900.0, 1076.0, 525.0, 751.0)
# print(e.latlon2ij(43.88, 5., 46.39, 8.1))  # (900.0, 1075.0, 525.0, 750.0)
# print(e.ij2latlon(1090, 1100, 740, 750))

# e.download()
e = configreader.execute(config_user)
# arome_extr = eh.readers.AromeHendrixReader(native_files_folder=config_user['folder'], model=config_user['model_name'],
#                                           runtime=config_user['analysis_hour'])
