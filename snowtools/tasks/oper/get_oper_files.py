# -*- coding: utf-8 -*-
"""
Created on 3 aug. 2018

@author: lafaysse
"""

import sys
from optparse import OptionParser
from collections import defaultdict

from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import Date, daterange, tomorrow, today
from snowtools.utils.dates import check_and_convert_date
import footprints

usage = "usage: get_oper_files.py [-b YYYYMMDD]  [-e YYYYMMDD] [--previ] [--dev] [--deterministic] \
        [--geometry=domain] [--firstday] [--meteo] [--snow]"


class configdev(object):
    rundate = Date(2018, 10, 26, 3)    # Run date can be at 3TU, 6TU, 9TU
    previ = False  # False for analysis, True for forecast
    xpid = "OPER@lafaysse"  # To be changed with IGA account when operational
    list_geometry = ['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']  # List of extracted geometries

    list_members = footprints.util.rangex(0, 36)  # 35 for determinstic member, 36 for sytron, 0-34 for PEARP members
    firstday = False


class config(object):
    rundate = Date(2018, 10, 26, 3)    # Run date can be at 3TU, 6TU, 9TU
    previ = False  # False for analysis, True for forecast
    xpid = "oper"
    # converted later with iganame property
    list_geometry = ['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']
    list_members = footprints.util.rangex(0, 36)  # 35 for determinstic member, 36 for sytron, 0-34 for PEARP members
    firstday = False


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("-b",
                      action="store", type="string", dest="datebegin", default=today().ymd,
                      help="First day of extraction")

    parser.add_option("-e",
                      action="store", type="string", dest="dateend", default=today().ymd,
                      help="Last day of extraction")

    parser.add_option("--previ",
                      action="store_true", dest="previ", default=False,
                      help="Forecast instead of analysis")

    parser.add_option("--dev",
                      action="store_true", dest="dev", default=False,
                      help="Dev chain instead of operational chain")

    parser.add_option("--deterministic",
                      action="store_true", dest="deterministic", default=False,
                      help="Only extract member forced by deterministic ARPEGE")

    parser.add_option("--geometry",
                      action="store", type="string", dest="geometry", default=None,
                      help="geometry")

    parser.add_option("--firstday",
                      action="store_true", dest="firstday", default=False,
                      help="only extract first day")

    parser.add_option("--meteo",
                      action="store_true", dest="meteo", default=False,
                      help="Extract meteorological forcing files")

    parser.add_option("--snow",
                      action="store_true", dest="snow", default=False,
                      help="Extract snowpack model output files")

    (options, args) = parser.parse_args(arguments)  # @UnusedVariable

    return options


class configcommand(config):

    def __init__(self, options):
        if options.dev:
            for key, var in vars(configdev()).items():
                setattr(self, key, var)

        self.rundate = check_and_convert_date(options.datebegin)

        if options.deterministic:
            self.list_members = footprints.util.rangex(35, 35)

        if options.geometry:
            self.list_geometry = [options.geometry]

        if options.firstday:
            self.firstday = True

        self.meteo = options.meteo
        self.snow = options.snow


class configcommanddev(configdev):

    def __init__(self):
        self.rundate = check_and_convert_date(options.datebegin)


class S2MExtractor(S2MTaskMixIn):
    """
    Class to extract S2M results
    """

    def __init__(self, conf):
        """

        :param conf: configuration information for vortex toolboxes
        """
        toolbox.active_now = True
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

        tb01 = toolbox.input(
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
                tb01.extend(toolbox.input(
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
        import cen
        tb02 = toolbox.input(
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

                tb02.extend(toolbox.input(
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

    def get_meteo(self):

        tb01 = toolbox.input(
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
            fatal          = False
        )

        if hasattr(self.conf, "alternate_xpid"):
            for a, alternate_xpid in enumerate(self.conf.alternate_xpid):
                if hasattr(self.conf, "alternate_list_geometry"):
                    list_geometry = self.conf.alternate_list_geometry[a]
                else:
                    list_geometry = self.conf.list_geometry
                tb01.extend(toolbox.input(
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
                    fatal          = False
                ))

        return self.get_std(tb01)

    def get_snow(self):
        import cen
        tb02 = toolbox.input(
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
            fatal          = False

        )
        if hasattr(self.conf, "alternate_xpid"):
            for a, alternate_xpid in enumerate(self.conf.alternate_xpid):
                if hasattr(self.conf, "alternate_list_geometry"):
                    list_geometry = self.conf.alternate_list_geometry[a]
                else:
                    list_geometry = self.conf.list_geometry

                tb02.extend(toolbox.input(
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
                    fatal          = False

                ))

        return self.get_std(tb02)


if __name__ == "__main__":

    options = parse_options(sys.argv)

    S2ME = S2MExtractor(conf=configcommand(options))

    datebegin = check_and_convert_date(options.datebegin)
    dateend = check_and_convert_date(options.dateend)

    currentdate = datebegin

    while currentdate <= dateend:
        S2ME.set_rundate(currentdate)

        import pprint
        pprint.pprint(S2ME.get())
        currentdate = tomorrow(currentdate)
