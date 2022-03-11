#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 3 aug. 2018

@author: lafaysse
'''

import os
import sys
from optparse import OptionParser
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import Date
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend

usage = "usage: python get_reanalysis.py --geometry=xxx [--byear=YYYY] [--eyear=YYYY] [--meteo] [--snow] [--nativemeteo]"

toolbox.active_now = True  # permet de se passer de l'attribut now=True dans les toolbox


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("--geometry",
                      action="store", type="string", dest="geometry", default=None,
                      help="geometry")

    parser.add_option("--byear",
                      action="store", type="int", dest="byear", default=1958,
                      help="First year of extraction")

    parser.add_option("--eyear",
                      action="store", type="int", dest="eyear", default=2019,
                      help="Last year of extraction")

    parser.add_option("--meteo",
                      action="store_true", dest="meteo", default=False,
                      help="Extract meteorological forcing files")

    parser.add_option("--nativemeteo",
                      action="store_true", dest="nativemeteo", default=False,
                      help="Extract native meteorological forcing files")

    parser.add_option("--snow",
                      action="store_true", dest="snow", default=False,
                      help="Extract snowpack model output files")

    parser.add_option("--xpid",
                      action="store", dest="xpid", default=None,
                      help="Specific xpid")

    parser.add_option("--xpid_native",
                      action="store", dest="xpid_native", default=None,
                      help="Specific xpid")

    (options, args) = parser.parse_args(arguments)
    del args
    return options


class config(object):

    xpid = "reanalysis@lafaysse"  #
    xpid_native = "reanalysis@vernaym"
    duration = "yearly"

    def __init__(self):
        options = parse_options(sys.argv)
        self.datebegin = Date(options.byear, 8, 1, 6)
        self.dateend = Date(options.eyear, 8, 1, 6)
        self.geometry = options.geometry
        self.meteo = options.meteo
        self.nativemeteo = options.nativemeteo
        self.snow = options.snow
        if options.xpid:
            if '@' in options.xpid:
                self.xpid = options.xpid
            else:
                self.xpid = options.xpid + '@' + os.getlogin()
        if options.xpid_native:
            if '@' in options.xpid_native:
                self.xpid_native = options.xpid_native
            else:
                self.xpid_native = options.xpid_native + '@' + os.getlogin()


class S2MExtractor(S2MTaskMixIn):

    def __init__(self, conf):
        self.conf = conf

    def get(self):

        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
        dict_dates_end_forc = get_dic_dateend(list_dates_begin_forc, list_dates_end_forc)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)
        dict_source_app_safran, dict_source_conf_safran = self.get_safran_sources(list_dates_begin_forc)

        if self.conf.nativemeteo:

            tb01 = toolbox.input(  # pylint: disable=possibly-unused-variable
                vapp           = 'safran',
                vconf          = self.conf.geometry,
                local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid_native,
                block          = 'massifs',
                source_app     = dict_source_app_safran,
                source_conf    = dict_source_conf_safran,
                geometry       = self.conf.geometry,
                date           = '[datebegin]',
                datebegin      = list_dates_begin_forc,
                dateend        = dict_dates_end_forc,
                nativefmt      = 'netcdf',
                kind           = 'MeteorologicalForcing',
                model          = 'safran',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                # now            = True,
            )

#            for rh in tb01:
#                print(rh.quickview())
#                rh.get()

        if self.conf.meteo:
            tb01 = toolbox.input(  # pylint: disable=possibly-unused-variable
                vapp           = 's2m',
                vconf          = self.conf.geometry,
                local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                block          = 'meteo',
                geometry       = self.conf.geometry,
                date           = '[datebegin]',
                datebegin      = list_dates_begin_forc,
                dateend        = dict_dates_end_forc,
                nativefmt      = 'netcdf',
                kind           = 'MeteorologicalForcing',
                model          = 's2m',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                # now            = True,
            )

#            for rh in tb01:
#                print(rh.quickview())
#                rh.get()

        if self.conf.snow:
            tb02 = toolbox.input(  # pylint: disable=possibly-unused-variable
                vapp           = 's2m',
                vconf          = self.conf.geometry,
                local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                block          = 'pro',
                geometry       = self.conf.geometry,
                date           = '[datebegin]',
                datebegin      = list_dates_begin_pro,
                dateend        = dict_dates_end_pro,
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                # now            = True,
            )

#            for rh in tb02:
#                print(rh.quickview())
#                rh.get()


if __name__ == "__main__":

    S2ME = S2MExtractor(conf=config())
    S2ME.get()
