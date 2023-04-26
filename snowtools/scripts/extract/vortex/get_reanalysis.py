#!/usr/bin/env python3
# -*- coding: utf-8 -*-

'''
Created on 3 aug. 2018
Modified on 26 april 2023 by LVG.

@author: lafaysse, viallon-galinier
'''

import os
import argparse

from snowtools.utils.dates import get_list_dates_files, get_dic_dateend, check_and_convert_date

from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
toolbox.active_now = True  # permet de se passer de l'attribut now=True dans les toolbox


def parse_args():
    parser = argparse.ArgumentParser(description="""
    Get Simulation files storend on Meteo-France archive (hendrix) through the vortex toolbox.

    Up to now, it is able to deal with PRO, PREP (final and/or yearly prep only, not daily prep),
    PGD and meteorological files.

    Do not forget to first have set your communication creedentials with hendrix (.netrc) and
    correctly installed wortex toolbox (including definition of MTOOLDIR if you are not on MF
    supercomputers).
    """)

    parser.add_argument("--geometry",
                        dest="geometry", default=None,
                        help="geometry")

    parser.add_argument("-b", "--begin", "--byear",
                        dest="begin", default=1958,
                        help="Date of begining of extraction (year of full date).")

    parser.add_argument("-e", "--end", "--eyear",
                        dest="end", default=2019,
                        help="Date of end of extraction (year or full date).")

    parser.add_argument("--meteo",
                        action="store_true", dest="meteo", default=False,
                        help="Extract meteorological forcing files")

    parser.add_argument("--prep",
                        action="store_true", dest="prep", default=False,
                        help="Extract last PREP file")

    parser.add_argument("--pgd",
                        action="store_true", dest="pgd", default=False,
                        help="Extract PGD file")

    parser.add_argument("--nativemeteo",
                        action="store_true", dest="nativemeteo", default=False,
                        help="Extract native meteorological forcing files")

    parser.add_argument("--snow",
                        action="store_true", dest="snow", default=False,
                        help="Extract snowpack model output files")

    parser.add_argument("--xpid",
                        dest="xpid", default=None,
                        help="Specific xpid (except for nativemeteo, see --xpdi_native instead). "
                        "Do not provide anything here if you want the current reanalysis.")

    parser.add_argument("--xpid_native",
                        dest="xpid_native", default=None,
                        help="Specific xpid used for nativemeteo.")

    parser.add_argument("--duration", dest="duration", default="yearly",
                        choices=['yearly', 'monthly', 'full'],
                        help="Duration of files. Default is yearly files. "
                        "Use 'monthly' for monthly files "
                        "and 'full' for one sigle file that covers the whole period between --begin and --end",
                        )

    args = parser.parse_args()
    return args


class config(object):

    xpid = "reanalysis2020.2@lafaysse"  #
    xpid_native = "reanalysis2020.2@vernaym"

    def __init__(self):
        args = parse_args()
        # Dates
        self.datebegin = check_and_convert_date(args.begin)
        self.dateend = check_and_convert_date(args.end)
        # Files to get
        self.meteo = args.meteo
        self.pgd = args.pgd
        self.prep = args.prep
        # Additional args
        self.duration = args.duration
        self.geometry = args.geometry
        self.nativemeteo = args.nativemeteo
        self.snow = args.snow
        if args.xpid:
            if '@' in args.xpid:
                self.xpid = args.xpid
            else:
                self.xpid = args.xpid + '@' + os.getlogin()
        if args.xpid_native:
            if '@' in args.xpid_native:
                self.xpid_native = args.xpid_native
            else:
                self.xpid_native = args.xpid_native + '@' + os.getlogin()


class S2MExtractor(S2MTaskMixIn):

    def __init__(self, conf):
        self.conf = conf

    def get(self):
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)
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
            )

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
            )

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

        if self.conf.prep:
            tb20 = toolbox.input(  # pylint: disable=possibly-unused-variable
                vapp           = 's2m',
                vconf          = self.conf.geometry,
                local          = 'PREP_[date:ymdh].nc',
                role           = 'SnowpackInit',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                date           = list_dates_end_pro,
                nativefmt      = 'netcdf',
                kind           = 'PREP',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'prep',
            ),

        if self.conf.pgd:
            tb21 = toolbox.input(  # pylint: disable=possibly-unused-variable
                vapp           = 's2m',
                vconf          = self.conf.geometry,
                role           = 'SurfexClim',
                kind           = 'pgdnc',
                nativefmt      = 'netcdf',
                local          = 'PGD.nc',
                experiment     = self.conf.xpid,
                geometry       = self.conf.geometry,
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                block          = 'pgd')


if __name__ == "__main__":

    S2ME = S2MExtractor(conf=config())
    S2ME.get()
