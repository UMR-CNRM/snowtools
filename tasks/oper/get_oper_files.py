'''
Created on 3 aug. 2018

@author: lafaysse
'''

from cen.layout.nodes import S2MTaskMixIn
from vortex import toolbox
from bronx.stdtypes.date import Date, daterange, tomorrow


class config(object):
    rundate = Date(2018, 10, 26, 3)    # Run date can be at 3TU, 6TU, 9TU
    previ = False  # False for analysis, True for forecast
    xpid = "OPER@lafaysse"  # To be changed with IGA account when operational
#     list_geometry = ['alp_allslopes', 'pyr_allslopes', 'cor_allslopes', 'postes']  # List of extracted geometries
    list_geometry = ['alp_allslopes', 'cor_allslopes', 'postes']  # List of extracted geometries

    list_members = range(0, 36)  # 35 for determinstic member, 36 for sytron, 0-34 for PEARP members


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

    S2ME = S2MExtractor(conf=config())
    S2ME.get()
