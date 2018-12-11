'''
Created on 3 aug. 2018

@author: lafaysse
'''

import sys
from optparse import OptionParser
from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import Date
from utils.dates import get_list_dates_files

usage = "usage: python get_reanalysis.py --geometry=xxx [--byear=YYYY] [--eyear=YYYY] [--meteo] [--snow]"


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("--geometry",
                      action="store", type="string", dest="geometry", default=None,
                      help="geometry")

    parser.add_option("--byear",
                      action="store", type="int", dest="byear", default=1958,
                      help="First year of extraction")

    parser.add_option("--eyear",
                      action="store", type="int", dest="eyear", default=2018,
                      help="Last year of extraction")

    parser.add_option("--meteo",
                      action="store_true", dest="meteo", default=False,
                      help="Extract meteorological forcing files")

    parser.add_option("--snow",
                      action="store_true", dest="snow", default=False,
                      help="Extract snowpack model output files")

    (options, args) = parser.parse_args(arguments)
    del args
    return options


class config(object):

    xpid = "reanalysis@lafaysse"  #
    duration = "yearly"

    def __init__(self):
        options = parse_options(sys.argv)
        self.datebegin = Date(options.byear, 8, 1, 6)
        self.dateend = Date(options.eyear, 8, 1, 6)
        self.geometry = options.geometry
        self.meteo = options.meteo
        self.snow = options.snow


class S2MExtractor(S2MTaskMixIn):

    def __init__(self, conf):
        self.conf = conf

    def get(self):

        list_dates_begin_forc, list_dates_end_forc, list_dates_begin_pro, list_dates_end_pro = get_list_dates_files(self.conf.datebegin, self.conf.dateend, self.conf.duration)

        if self.conf.meteo:
            for p, datebegin in enumerate(list_dates_begin_forc):
                dateend = list_dates_end_forc[p]
                tb01 = toolbox.input(
                    vapp           = 's2m',
                    vconf          = self.conf.geometry,
                    local          = 'FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    block          = 'meteo',
                    geometry       = self.conf.geometry,
                    date           = '[datebegin]',
                    datebegin      = datebegin,
                    dateend        = dateend,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                )

                for rh in tb01:
                    print(rh.quickview())
                    rh.get()

        if self.conf.snow:
            for p, datebegin in enumerate(list_dates_begin_pro):
                dateend = list_dates_end_pro[p]
                tb02 = toolbox.input(
                    vapp           = 's2m',
                    vconf          = self.conf.geometry,
                    local          = 'PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = self.conf.xpid,
                    block          = 'pro',
                    geometry       = self.conf.geometry,
                    date           = '[datebegin]',
                    datebegin      = datebegin,
                    dateend        = dateend,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.multi.fr',
                    namebuild      = 'flat@cen',
                )

                for rh in tb02:
                    print(rh.quickview())
                    rh.get()


if __name__ == "__main__":

    S2ME = S2MExtractor(conf=config())
    S2ME.get()
