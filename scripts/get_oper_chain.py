#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 27 May. 2021

@author: Vernay
'''

import os
import sys
from optparse import OptionParser
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import Date
from utils.dates import get_list_dates_files, get_dic_dateend
import footprints

usage = "usage: python get_reanalysis.py --geometry=xxx --date=YYYYMMDDHH [--meteo] [--snow]"

toolbox.active_now = True  # permet de se passer de l'attribut now=True dans les toolbox


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("--geometry",
                      action="store", type="string", dest="geometry",
                      help="geometry")

    parser.add_option("--date",
                      action="store", type="str", dest="date",
                      help="Date of the run to extract, format YYYYMMDDHH. If you want the monthly reanalysis use DD=22 and HH=12")

    parser.add_option("--meteo",
                      action="store_true", dest="meteo", default=False,
                      help="Extract meteorological forcing files")

    parser.add_option("--snow",
                      action="store_true", dest="snow", default=False,
                      help="Extract snowpack model output files")

    parser.add_option("--previ",
                      action="store_true", dest="previ", default=False,
                      help="Extract forecast if True or assimilation if False")

    parser.add_option("--pearp",
                      action="store_true", dest="pearp", default=False,
                      help="Extract PEARP members")

    (options, args) = parser.parse_args(arguments)

    if options.geometry == None:
        print('No geometries specified, use --geometry option with a valid geometry (alp, pyr, cor or postes)')

    if options.date == None:
        print('No date specified, use --date option with a date of the form YYYYMMDDHH (HH among 03, 06, 09 and 12)')


    del args
    return options


class config(object):

    xpid = "oper"  #
    duration = "yearly"

    def __init__(self):
        options = parse_options(sys.argv)
        self.rundate = Date(options.date)
        self.geometry = options.geometry
        self.meteo = options.meteo
        self.snow = options.snow
        self.previ = options.previ
        self.pearp = options.pearp


class S2MExtractor(S2MTaskMixIn):

    def __init__(self, conf):
        self.conf = conf

    def get(self):

        datebegin, dateend = self.get_period()

        if self.conf.meteo:
            tb01 = toolbox.input(  # pylint: disable=possibly-unused-variable
                kind           = 'MeteorologicalForcing',
                source_app     = 'arpege',
                source_conf    = '4dvarfr' if not self.conf.pearp else 'pearp',
                vapp           = 's2m',
                vconf          = '[geometry:area]',
                cutoff         = 'assimilation' if not self.conf.previ else 'production',
                filename       = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc' if not self.conf.pearp else 'mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = 'oper',
                block          = 'massifs',
                member         = None if not self.conf.pearp else footprints.util.rangex(0, 34),
                geometry       = self.conf.geometry,
                nativefmt      = 'netcdf',
                model          = 's2m',
                date           = self.conf.rundate.ymdh,
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                namespace      = 'vortex.multi.fr',
            )


        if self.conf.snow:
            tb02 = toolbox.input(  # pylint: disable=possibly-unused-variable
                kind           = 'SnowpackSimulation',
                vapp           = 's2m',
                vconf          = self.conf.geometry,
                cutoff         = 'assimilation' if not self.conf.previ else 'production',
                filename       = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc' if not self.conf.pearp else 'mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = 'oper',
                member         = 35 if not self.conf.pearp else footprints.util.rangex(0, 34),
                block          = 'pro',
                geometry       = self.conf.geometry,
                date           = self.conf.rundate.ymdh,
                datebegin      = datebegin.ymd6h,
                dateend        = dateend.ymd6h,
                nativefmt      = 'netcdf',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
            )


if __name__ == "__main__":

    S2ME = S2MExtractor(conf=config())
    S2ME.get()
