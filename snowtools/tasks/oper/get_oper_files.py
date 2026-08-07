# -*- coding: utf-8 -*-
"""
Created on 3 aug. 2018

@author: lafaysse
"""

import sys
# from optparse import OptionParser
import argparse
import vortex
from collections import defaultdict
from vortex_cen.tasks.oper_research_mixin import CENTaskMixIn
from bronx.stdtypes.date import Date, daterange, tomorrow, today

from snowtools.utils.dates import check_and_convert_date
import footprints




class Configdev(object):
    rundate = Date(2018, 10, 26, 3)    # Run date can be at 3TU, 6TU, 9TU
    previ = False  # False for analysis, True for forecast
    xpid = "OPER@lafaysse"  # To be changed with IGA account when operational
    list_geometry = ['alp', 'pyr', 'cor']  # List of extracted geometries

    list_members = footprints.util.rangex(0, 36)  # 35 for determinstic member, 36 for sytron, 0-34 for PEARP members
    firstday = False


class Config(object):
    rundate = Date(2018, 10, 26, 3)    # Run date can be at 3TU, 6TU, 9TU
    previ = False  # False for analysis, True for forecast
    xpid = "oper"
    # converted later with iganame property
    list_geometry = ['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']
    list_members = footprints.util.rangex(0, 36)  # 35 for determinstic member, 36 for sytron, 0-34 for PEARP members
    firstday = False



class _Configcommand(object):

    def __init__(self, options):

        self.rundate = check_and_convert_date(options.datebegin)

        if options.deterministic:
            self.list_members = footprints.util.rangex(35, 35)

        if options.geometry:
            self.list_geometry = [options.geometry]

        if options.firstday:
            self.firstday = True

        self.meteo = options.meteo
        self.snow = options.snow
        self.hydro = options.hydro
        self.previ = options.previ
        self.pp_quantiles = options.pp_quantiles


class Configcommand(_Configcommand, Config):
    pass


class Configcommanddev(_Configcommand, Configdev):
    pass


class S2MExtractor(CENTaskMixIn):
    """
    Class to extract S2M results
    """

    def __init__(self, conf):
        """

        :param conf: configuration information for vortex toolboxes
        """
        vortex.toolbox.active_now = True
        self.conf = conf
        self.datebegin, self.dateend = self.get_period()
        if not hasattr(self.conf, 'firstday'):
            self.conf.firstday = False
        if self.conf.firstday:
            self.dateend = tomorrow(base=self.datebegin)

    def set_rundate(self, date):
        self.conf.rundate = date
        self.datebegin, self.dateend = self.get_period()
        if self.conf.firstday:
            self.dateend = tomorrow(base=self.datebegin)

    def get(self):
        if self.conf.meteo:
            meteo_outputs = self.get_meteo()
        else:
            meteo_outputs = None
        if self.conf.snow :
            snow_outputs = self.get_snow()
        else:
            snow_outputs = None

        return meteo_outputs, snow_outputs

    def get_meteo(self):

        tb01 = vortex.input(
            role           = 'Forcing',
            vapp           = 's2m',
            vconf          = '[geometry::area]',
            local          = '[geometry::iganame]/[date:ymdh]/mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            block          = 'meteo',
            geometry       = self.conf.list_geometry,
            date           = self.conf.rundate,
            datebegin      = self.datebegin,
            dateend        = self.dateend,
            member         = self.conf.list_members,
            nativefmt      = 'netcdf',
            kind           = 'MeteorologicalForcing',
            model          = 's2m',
            namespace      = 'vortex.multi.fr',
            cutoff         = 'production' if self.conf.previ else 'assimilation',
            intent         = 'in',
            fatal          = False
        )

        if hasattr(self.conf, "alternate_xpid"):
            for a, alternate_xpid in enumerate(self.conf.alternate_xpid):
                if hasattr(self.conf, "alternate_list_geometry"):
                    list_geometry = self.conf.alternate_list_geometry[a]
                else:
                    list_geometry = self.conf.list_geometry
                tb01.extend(vortex.input(
                    alternate      = 'Forcing',
                    vapp           = 's2m',
                    vconf          = '[geometry::iganame]',
                    local          = '[geometry::iganame]/[date:ymdh]/mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = alternate_xpid,
                    block          = 'meteo',
                    geometry       = list_geometry,
                    date           = self.conf.rundate,
                    datebegin      = self.datebegin,
                    dateend        = self.dateend,
                    member         = self.conf.list_members,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = 'vortex.multi.fr',
                    cutoff         = 'production' if self.conf.previ else 'assimilation',
                    intent         = 'in',
                    fatal          = False
                ))

        return self.get_std(tb01)

    def get_snow(self):
        import vortex_cen
        tb02 = vortex.input(
            role           = 'pro',
            vapp           = 's2m',
            vconf          = '[geometry::iganame]',
            local          = '[geometry::iganame]/[date:ymdh]/mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            block          = 'pro',
            geometry       = self.conf.list_geometry,
            date           = self.conf.rundate,
            datebegin      = self.datebegin if self.conf.previ else '[dateend]/-PT24H',
            dateend        = self.dateend if self.conf.previ else list(daterange(tomorrow(base=self.datebegin), self.dateend)),
            member         = self.conf.list_members,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            cutoff         = 'production' if self.conf.previ else 'assimilation',
            intent         = 'in',
            fatal          = False

        )

        if hasattr(self.conf, "alternate_xpid"):
            for a, alternate_xpid in enumerate(self.conf.alternate_xpid):
                if hasattr(self.conf, "alternate_list_geometry"):
                    list_geometry = self.conf.alternate_list_geometry[a]
                else:
                    list_geometry = self.conf.list_geometry

                tb02.extend(vortex.input(
                    alternate      = 'pro',
                    vapp           = 's2m',
                    vconf          = '[geometry::area]',
                    local          = '[geometry::iganame]/[date:ymdh]/mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = alternate_xpid,
                    block          = 'pro',
                    geometry       = list_geometry,
                    date           = self.conf.rundate,
                    datebegin      = self.datebegin if self.conf.previ else '[dateend]/-PT24H',
                    dateend        = self.dateend if self.conf.previ else list(daterange(tomorrow(base=self.datebegin), self.dateend)),
                    member         = self.conf.list_members,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    cutoff         = 'production' if self.conf.previ else 'assimilation',
                    intent         = 'in',
                    fatal          = False

                ))

        return self.get_std(tb02)

    def get_std(self, tb):

        list_output = defaultdict(list)
        list_xpid = defaultdict(list)
        for rh in tb:
            if rh.stage == 'get':
                list_output[rh.resource.geometry.area].append(rh.container.filename)
                list_xpid[rh.resource.geometry.area].append(rh.provider.experiment)

        return list_output, list_xpid


class FutureS2MExtractor(S2MExtractor):


    def get(self):
        meteo_outputs, snow_outputs = super().get()
        if self.conf.hydro:
            hydro_outputs = self.get_hydro()
        else:
            hydro_outputs = None
        if self.conf.pp_quantiles:
            pp_outputs = self.get_pp_quantiles()
        else:
            pp_outputs = None

        return meteo_outputs, snow_outputs, hydro_outputs, pp_outputs


    def get_meteo(self):

        tb01 = vortex.input(
            role           = 'Forcing',
            vapp           = 's2m',
            vconf          = '[geometry::area]',
            local          = '[geometry::area]/[date:ymdh]/mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            block          = 'meteo',
            geometry       = self.conf.list_geometry,
            date           = self.conf.rundate,
            datebegin      = self.datebegin,
            dateend        = self.dateend,
            member         = self.conf.list_members,
            nativefmt      = 'netcdf',
            kind           = 'MeteorologicalForcing',
            model          = 's2m',
            namespace      = 'vortex.multi.fr',
            cutoff         = 'production' if self.conf.previ else 'assimilation',
            intent         = 'in',
            fatal          = False,
            vortex1=True,
            namebuild='date@std'
        )

        if hasattr(self.conf, "alternate_xpid"):
            for a, alternate_xpid in enumerate(self.conf.alternate_xpid):
                if hasattr(self.conf, "alternate_list_geometry"):
                    list_geometry = self.conf.alternate_list_geometry[a]
                else:
                    list_geometry = self.conf.list_geometry
                tb01.extend(vortex.input(
                    alternate      = 'Forcing',
                    vapp           = 's2m',
                    vconf          = '[geometry::area]',
                    local          = '[geometry::area]/[date:ymdh]/mb[member]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = alternate_xpid,
                    block          = 'meteo',
                    geometry       = list_geometry,
                    date           = self.conf.rundate,
                    datebegin      = self.datebegin,
                    dateend        = self.dateend,
                    member         = self.conf.list_members,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = 'vortex.multi.fr',
                    cutoff         = 'production' if self.conf.previ else 'assimilation',
                    intent         = 'in',
                    fatal          = False,
                    vortex1=True,
                    namebuild='date@std'
                ))

        return self.get_std(tb01)

    def get_snow(self):
        import vortex_cen
        tb02 = vortex.input(
            role           = 'pro',
            vapp           = 's2m',
            vconf          = '[geometry::area]',
            local          = '[geometry::area]/[date:ymdh]/mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment     = self.conf.xpid,
            block          = 'pro',
            geometry       = self.conf.list_geometry,
            date           = self.conf.rundate,
            datebegin      = self.datebegin if self.conf.previ else '[dateend]/-PT24H',
            dateend        = self.dateend if self.conf.previ else list(daterange(tomorrow(base=self.datebegin), self.dateend)),
            member         = self.conf.list_members,
            nativefmt      = 'netcdf',
            kind           = 'SnowpackSimulation',
            model          = 'surfex',
            namespace      = 'vortex.multi.fr',
            cutoff         = 'production' if self.conf.previ else 'assimilation',
            intent         = 'in',
            fatal          = False,
            vortex1 = True,
            namebuild = 'date@std'

        )
        if hasattr(self.conf, "alternate_xpid"):
            for a, alternate_xpid in enumerate(self.conf.alternate_xpid):
                if hasattr(self.conf, "alternate_list_geometry"):
                    list_geometry = self.conf.alternate_list_geometry[a]
                else:
                    list_geometry = self.conf.list_geometry

                tb02.extend(vortex.input(
                    alternate      = 'pro',
                    vapp           = 's2m',
                    vconf          = '[geometry::area]',
                    local          = '[geometry::area]/[date:ymdh]/mb[member]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = alternate_xpid,
                    block          = 'pro',
                    geometry       = list_geometry,
                    date           = self.conf.rundate,
                    datebegin      = self.datebegin if self.conf.previ else '[dateend]/-PT24H',
                    dateend        = self.dateend if self.conf.previ else list(daterange(tomorrow(base=self.datebegin), self.dateend)),
                    member         = self.conf.list_members,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    cutoff         = 'production' if self.conf.previ else 'assimilation',
                    intent         = 'in',
                    fatal          = False,
                    vortex1=True,
                    namebuild='date@std'

                ))

        return self.get_std(tb02)

    def get_hydro(self):
        import vortex_cen

        if self.conf.previ:
            tb03 = vortex.input(
                role           = 'hydro',
                vapp           = 's2m',
                vconf          = '[geometry::area]',
                local          = '[geometry::area]/[date:ymdh]/HYDRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment     = self.conf.xpid,
                block          = 'hydro',
                geometry       = self.conf.list_geometry,
                date           = self.conf.rundate,
                datebegin      = self.datebegin if self.conf.previ else '[dateend]/-PT24H',
                dateend        = self.dateend if self.conf.previ else list(daterange(tomorrow(base=self.datebegin), self.dateend)),
                nativefmt      = 'netcdf',
                kind           = 'SnowpackSimulation',
                model          = 'postproc',
                namespace      = 'vortex.multi.fr',
                cutoff         = 'production',
                intent         = 'in',
                fatal          = False,
                vortex1=True,
                namebuild='date@std',
            )
        else:
            tb03 = vortex.input(
                role = 'hydro',
                vapp = 's2m',
                vconf = '[geometry::area]',
                local = '[geometry::area]/[date:ymdh]/HYDRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                experiment = self.conf.xpid,
                block = 'hydro',
                geometry = self.conf.list_geometry,
                date = self.conf.rundate,
                datebegin = self.datebegin if self.conf.previ else '[dateend]/-PT24H',
                dateend = self.dateend if self.conf.previ else list(daterange(tomorrow(base=self.datebegin), self.dateend)),
                nativefmt = 'netcdf',
                kind = 'SnowpackSimulation',
                model = 'postproc',
                member = 35,
                namespace = 'vortex.multi.fr',
                cutoff = 'assimilation',
                intent = 'in',
                fatal = False,
                vortex1=True,
                namebuild='date@std',
            )

        return self.get_std(tb03)


    def get_pp_quantiles(self):
        import vortex_cen

        tb_pp = vortex.input(
            role        = 'Postproc_output',
            intent      = 'in',
            vapp        = 's2m',
            vconf       = '[geometry::area]',
            local       = '[geometry::area]/[date:ymdh]/PRO_post_[datebegin:ymdh]_[dateend:ymdh].nc',
            experiment  = self.conf.xpid,
            block       = 'postproc',
            geometry    = self.conf.list_geometry,
            date        = self.conf.rundate,
            datebegin   = self.datebegin if self.conf.previ else '[dateend]/-PT24H',
            dateend     = self.dateend if self.conf.previ else list(daterange(tomorrow(base=self.datebegin),
                                                                              self.dateend)),
            nativefmt   = 'netcdf',
            kind        = 'SnowpackSimulation',
            model       = 'postproc',
            namespace   = 'vortex.multi.fr',
            cutoff      = 'production',
            fatal       = False,
            vortex1=True,
            namebuild='date@std',
        )

        return self.get_std(tb_pp)



if __name__ == "__main__":

    USAGE = "usage: get_oper_files.py [-b YYYYMMDD]  [-e YYYYMMDD] [--previ] [--dev] [--deterministic] \
            [--geometry=domain] [--firstday] [--meteo] [--snow] [--hydro] [--ppquantiles]"

    PARSER = argparse.ArgumentParser(description="get output from operational or dev simulations")
    PARSER.add_argument("-b",
                        action="store", type=str, dest="datebegin", default=today().ymd,
                        help="First day of extraction")
    PARSER.add_argument("-e",
                        action="store", type=str, dest="dateend", default=today().ymd,
                        help="Last day of extraction")
    PARSER.add_argument("--previ",
                        action="store_true", dest="previ", default=False,
                        help="Forecast instead of analysis")
    PARSER.add_argument("--dev",
                        action="store_true", dest="dev", default=False,
                        help="Dev chain instead of operational chain")
    PARSER.add_argument("--deterministic",
                        action="store_true", dest="deterministic", default=False,
                        help="Only extract member forced by deterministic ARPEGE")
    PARSER.add_argument("--geometry",
                        action="store", type=str, dest="geometry", default=None,
                        help="geometry")
    PARSER.add_argument("--firstday",
                        action="store_true", dest="firstday", default=False,
                        help="only extract first day")
    PARSER.add_argument("--meteo",
                        action="store_true", dest="meteo", default=False,
                        help="Extract meteorological forcing files")
    PARSER.add_argument("--snow",
                        action="store_true", dest="snow", default=False,
                        help="Extract snowpack model output files")
    PARSER.add_argument("--hydro",
                        action="store_true", dest="hydro", default=False,
                        help="Extract hydrological post-processing")
    PARSER.add_argument("--ppquantiles", action="store_true", dest="pp_quantiles", default=False,
                        help="Extract, potentially emos postprocessed, quantiles (fresh snow)")
    OPTIONS = PARSER.parse_args()

    config_from_command = Configcommanddev(OPTIONS) if OPTIONS.dev else Configcommand(OPTIONS)

    S2ME = FutureS2MExtractor(conf=config_from_command)

    datebegin = check_and_convert_date(OPTIONS.datebegin)
    dateend = check_and_convert_date(OPTIONS.dateend)

    currentdate = datebegin

    while currentdate <= dateend:
        S2ME.set_rundate(currentdate)

        import pprint

        pprint.pprint(S2ME.get())
        currentdate = tomorrow(currentdate)
