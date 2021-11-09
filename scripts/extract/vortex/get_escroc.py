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

from snowtols.scripts.ESMSnowMIP.ESM_snowmip import bdate, edate

usage = "usage: python get_escroc.py --site=xxx --nmembers=xx --escroc=xx"


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

    (options, args) = parser.parse_args(arguments)
    del args
    return options


class config(object):

    def __init__(self):
        options = parse_options(sys.argv)
        self.list_sites = options.site.split(",")
        self.nmembers = options.nmembers
        self.list_escroc = options.escroc.split(",")


class S2MExtractor(S2MTaskMixIn):

    def __init__(self, conf):
        self.conf = conf

    def get(self):

        list_output = {}

        for site in self.conf.list_sites:

            for escroc in self.conf.list_escroc:
                tb = toolbox.input(
                    vapp           = 's2m',
                    vconf          = site,
                    local          = '[vconf]/[experiment]/mb[member%03d]/[block]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = escroc + '@' + os.getlogin(),
                    block          = 'pro',
                    geometry       = site,
                    date           = '[datebegin]',
                    datebegin      = bdate[site],
                    dateend        = edate[site],
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
