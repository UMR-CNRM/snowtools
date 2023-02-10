#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 3 aug. 2018

@author: lafaysse
'''

import os
import sys
import footprints

from optparse import OptionParser
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import Date

from snowtools.scripts.ESMSnowMIP.ESM_snowmip import bdate, edate
from snowtools.utils.dates import get_list_dates_files, get_dic_dateend, check_and_convert_date

usage = "usage: python get_escroc.py --site=xxx [--nmembers=xx] --escroc=xx [--byear=YYYY --eyear=YYYY] [--bdate=YYYYMMDDHH --eyear=YYYYMMDDHH]"


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("--site",
                      action="store", type="string", dest="site", default=None,
                      help="geometry")

    parser.add_option("--nmembers",
                      action="store", type="int", dest="nmembers", default=35,
                      help="nb of members")

    parser.add_option("--escroc",
                      action="store", type="string", dest="escroc", default=None,
                      help="First year of extraction")

    parser.add_option("--byear",
                      action="store", type="int", dest="byear", default=None,
                      help="First year of extraction")

    parser.add_option("--bdate",
                      action="store", type="string", dest="bdate", default=None,
                      help="First date of extraction YYYYMMDDHH")

    parser.add_option("--eyear",
                      action="store", type="int", dest="eyear", default=None,
                      help="Last year of extraction")

    parser.add_option("--edate",
                      action="store", type="string", dest="edate", default=None,
                      help="Last date of extraction  YYYYMMDDHH")

    parser.add_option("--yearly",
                      action="store_true", dest="yearly", default=False,
                      help="Extract yearly files")

    (options, args) = parser.parse_args(arguments)
    del args
    return options


class config(object):

    def __init__(self):
        options = parse_options(sys.argv)
        self.list_sites = options.site.split(",")
        self.nmembers = options.nmembers
        self.list_escroc = options.escroc.split(",")
        if options.yearly:
            self.duration = 'yearly'
        else:
            self.duration = 'full'
        if options.byear:
            bdate[options.site] = Date(options.byear, 8, 1, 6)
        if options.eyear:
            edate[options.site] = Date(options.eyear, 8, 1, 6)
        if options.bdate:
            bdate[options.site] = check_and_convert_date(options.bdate)
        if options.edate:
            edate[options.site] = check_and_convert_date(options.edate)

class S2MExtractor(S2MTaskMixIn):

    def __init__(self, conf):
        self.conf = conf

    def get(self):

        list_output = {}

        for site in self.conf.list_sites:

            list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = get_list_dates_files(
                bdate[site], edate[site], self.conf.duration)
            dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)

            for escroc in self.conf.list_escroc:
                tb = toolbox.input(
                    vapp           = 's2m',
                    vconf          = site,
                    local          = '[vconf]/[experiment]/mb[member%03d]/[block]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = escroc + '@' + os.getlogin() if '@' not in escroc else escroc,
                    block          = 'pro',
                    geometry       = site,
                    date           = '[datebegin]',
                    datebegin      = list_dates_begin_pro,
                    dateend        = dict_dates_end_pro,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                    member        = footprints.util.rangex(1, self.conf.nmembers)
                )

                for rh in tb:
                    print(rh.quickview())
                    rh.get()

                for rh in tb:
                    if rh.check():
                        if rh.resource.geometry.area not in list_output.keys():
                            list_output[rh.resource.geometry.area] = dict()

                        if escroc not in list_output[rh.resource.geometry.area].keys():
                            list_output[rh.resource.geometry.area][escroc] = []

                        list_output[rh.resource.geometry.area][escroc].append(rh.container.filename)

        return list_output


if __name__ == "__main__":

    S2ME = S2MExtractor(conf=config())
    S2ME.get()
