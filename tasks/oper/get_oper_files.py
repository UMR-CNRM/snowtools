'''
Created on 3 aug. 2018

@author: lafaysse
'''

from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import Date, daterange, tomorrow, today
from optparse import OptionParser
from utils.dates import check_and_convert_date
import footprints
import sys
import six

usage = "usage: get_oper_files.py [-b YYYYMMDD] [--previ]"


class configdev(object):
    rundate = Date(2018, 10, 26, 3)    # Run date can be at 3TU, 6TU, 9TU
    previ = False  # False for analysis, True for forecast
    xpid = "OPER@lafaysse"  # To be changed with IGA account when operational
    list_geometry = ['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']  # List of extracted geometries

    list_members = footprints.util.rangex(0, 36)  # 35 for determinstic member, 36 for sytron, 0-34 for PEARP members


class config(object):
    rundate = Date(2018, 10, 26, 3)    # Run date can be at 3TU, 6TU, 9TU
    previ = False  # False for analysis, True for forecast
    xpid = "oper"
    list_geometry = ['alp', 'pyr', 'cor', 'postes']  # List of extracted geometries

    list_members = footprints.util.rangex(0, 36)  # 35 for determinstic member, 36 for sytron, 0-34 for PEARP members


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("-b",
                      action="store", type="string", dest="datebegin", default=today().ymd,
                      help="First year of extraction")

    parser.add_option("--previ",
                      action="store_true", dest="previ", default=False,
                      help="Forecast instead of analysis")

    parser.add_option("--dev",
                      action="store_true", dest="dev", default=False,
                      help="Dev chain instead of operational chain")

    parser.add_option("--deterministic",
                      action="store_true", dest="deterministic", default=False,
                      help="Dev chain instead of operational chain")

    (options, args) = parser.parse_args(arguments)  # @UnusedVariable

    return options


class configcommand(config):

    def __init__(self, options):
        if options.dev:
            for key, var in six.iteritems(vars(configdev())):
                setattr(self, key, var)

        self.rundate = check_and_convert_date(options.datebegin)

        if options.deterministic:
            self.list_members = footprints.util.rangex(35, 35)

class configcommanddev(configdev):

    def __init__(self):
        self.rundate = check_and_convert_date(options.datebegin)


class S2MExtractor(S2MTaskMixIn):

    def __init__(self, conf):
        toolbox.active_now = True
        self.conf = conf
        self.datebegin, self.dateend = self.get_period()

    def get(self):
        meteo_outputs = self.get_meteo()
        snow_outputs = self.get_snow()
        return meteo_outputs, snow_outputs

    def get_meteo(self):

        tb01 = toolbox.input(
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

        return self.get_std(tb01)

    def get_snow(self):

        tb02 = toolbox.input(
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

        return self.get_std(tb02)

    def get_std(self, tb):

        list_output = {}
        for rh in tb:
#             print(rh.quickview())
#             rh.get()
            if rh.check():
                if rh.resource.geometry.area not in list_output.keys():
                    list_output[rh.resource.geometry.area] = []
                list_output[rh.resource.geometry.area].append(rh.container.filename)

        return list_output


if __name__ == "__main__":

    options = parse_options(sys.argv)
    S2ME = S2MExtractor(conf=configcommand(options))
    S2ME.get()
