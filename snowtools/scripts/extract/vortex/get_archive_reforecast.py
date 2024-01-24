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

    This script is designed for tar archives of long S2M reforecasts

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

    parser.add_argument("--snow",
                        action="store_true", dest="snow", default=False,
                        help="Extract snowpack model output files")

    parser.add_argument("--xpid",
                        dest="xpid", default=None,
                        help="Specific xpid (except for nativemeteo, see --xpdi_native instead). "
                        "Do not provide anything here if you want the current reanalysis.")

    args = parser.parse_args()
    return args


class config(object):

    xpid = "reforecast_2023@lafaysse"  #

    def __init__(self):
        args = parse_args()
        # Dates
        self.datebegin = check_and_convert_date(args.begin)
        self.dateend = check_and_convert_date(args.end)
        # Files to get
        self.meteo = args.meteo
        # Additional args
        self.geometry = args.geometry
        self.snow = args.snow
        if args.xpid:
            if '@' in args.xpid:
                self.xpid = args.xpid
            else:
                self.xpid = args.xpid + '@' + os.getlogin()


class S2MExtractor(S2MTaskMixIn):

    def __init__(self, conf):
        self.conf = conf

    def untar_hook(self, t, rh):
        # For only one of the resources list, create the tar archive for the whole list of dates
        sh = t.sh
        sh.untar(rh.container.basename)

    def get(self):
        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = \
            get_list_dates_files(self.conf.datebegin, self.conf.dateend, 'yearly')
        dict_dates_end_forc = get_dic_dateend(list_dates_begin_forc, list_dates_end_forc)
        dict_dates_end_pro = get_dic_dateend(list_dates_begin_pro, list_dates_end_pro)
        dict_source_app_safran, dict_source_conf_safran = self.get_safran_sources(list_dates_begin_forc)

        if self.conf.meteo:
            tb01 = toolbox.input(
                vapp='s2m',
                vconf=self.conf.geometry,
                local='FORCING_[datebegin:ymdh]_[dateend:ymdh].tar',
                experiment=self.conf.xpid,
                block='meteo',
                geometry=self.conf.geometry,
                date='[datebegin]',
                datebegin=list_dates_begin_pro,
                dateend=dict_dates_end_pro,
                nativefmt='tar',
                kind='FORCING',
                model='s2m',
                namebuild='flat@cen',
                namespace='vortex.multi.fr',
                hook_autohook1=self.untar_hook,
            ),

        if self.conf.snow:
            tb02 = toolbox.input(  # pylint: disable=possibly-unused-variable
                vapp           = 's2m',
                vconf          = self.conf.geometry,
                local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].tar',
                experiment     = self.conf.xpid,
                block          = 'pro',
                geometry       = self.conf.geometry,
                date           = '[datebegin]',
                datebegin      = list_dates_begin_pro,
                dateend        = dict_dates_end_pro,
                nativefmt      = 'tar',
                kind           = 'PRO',
                model          = 'surfex',
                namespace      = 'vortex.multi.fr',
                namebuild      = 'flat@cen',
                hook_autohook1=self.untar_hook,
                # now            = True,
            )


if __name__ == "__main__":

    S2ME = S2MExtractor(conf=config())
    S2ME.get()
