#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

'''
Created on dec. 2018
@author: Vernay
'''

import vortex

import footprints
from vortex import toolbox
from vortex.layout.nodes import Task
from cen.layout.nodes import S2MTaskMixIn

from bronx.stdtypes.date import Date, Period
from utils.dates import get_list_dates_files
import os

toolbox.active_now = True


class ReforecastExtractor(Task, S2MTaskMixIn):

    def get_list_dates(self):

        list_dates = list()
#        for year in range(1994,2017):
        for year in [2017]:
            list_dates.append([Date(year, 12, 6, 6, 0, 0), Date(year+1, 4, 30, 6, 0, 0)])
        return list_dates

    def process(self):

        xpid     = 'reforecast@lafaysse'
        geometry = 'postes'
        members  = footprints.util.rangex(0,35)
        vapp     = 's2m'
        cutoff   = 'production'


        list_dates = self.get_list_dates()

        missing_files = list()
        for datebegin, dateend in list_dates:
            rundate = datebegin

            while rundate <= dateend:

                print('Running date {0:s}'.format(rundate.ymdh))

                tb01 = toolbox.input(
                    local          = 'meteo/mb[member%03d]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = 'reforecast@lafaysse',
                    block          = 'meteo',
                    geometry       = geometry,
                    date           = '[datebegin]',
                    datebegin      = rundate.ymdh,
                    dateend        = '[datebegin]/+PT96H',
                    member         = members,
                    nativefmt      = 'netcdf',
                    kind           = 'MeteorologicalForcing',
                    model          = 's2m',
                    namespace      = 'vortex.archive.fr',
                    cutoff         = cutoff,
                    vapp           = vapp,
                    vconf          = '[geometry::area]',
                    fatal          = False
                ),
#                 print('DBUG', tb01, tb01[0])
#                 for forcing in tb01[0]:
#                     if not forcing.check():
#                         missing_files.append(forcing)
#                     else:
#                          forcing.get()

#                 tb02 = toolbox.output(
#                     local          = 'mb[member%03d]/FORCING_[datebegin:ymdh]_[dateend:ymdh].nc',
#                     remote         = '/manto/nousuj/reforecast/forcing/[local]',
#                     date           = '[datebegin]',
#                     datebegin      = rundate.ymdh,
#                     dateend        = '[datebegin]/+PT96H',
#                     member         = members,
#                     geometry       = geometry,
#                     nativefmt      = 'netcdf',
#                     kind           = 'MeteorologicalForcing',
#                     model          = 's2m',
#                     cutoff         = cutoff,
#                     fatal          = False
#                 ),

#                 for forcing in tb02[0]:
#                     forcing.put()

                tb03 = toolbox.input(
                    local          = 'pro/mb[member%03d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
                    experiment     = xpid,
                    block          = 'pro',
                    geometry       = geometry,
                    date           = '[datebegin]',
                    datebegin      = rundate.ymdh,
                    dateend        = '[datebegin]/+PT96H',
                    member         = members,
                    nativefmt      = 'netcdf',
                    kind           = 'SnowpackSimulation',
                    model          = 'surfex',
                    namespace      = 'vortex.archive.fr',
                    cutoff         = cutoff,
                    fatal          = False,
                    vapp           = vapp,
                    vconf          = '[geometry::area]',
                ),
#                 for pro in tb03[0]:
#                     if not pro.check():
#                         missing_files.append(pro)
#                     else:
#                         pro.get()

#                 tb04 = toolbox.output(
#                     local          = 'mb[member%03d]/PRO_[datebegin:ymdh]_[dateend:ymdh].nc',
#                     remote         = '/manto/nousuj/reforecast/pro/[local]',
#                     date           = '[datebegin]',
#                     datebegin      = rundate.ymdh,
#                     dateend        = '[datebegin]/+PT96H',
#                     member         = members,
#                     geometry       = geometry,
#                     nativefmt      = 'netcdf',
#                     kind           = 'SnowpackSimulation',
#                     model          = 'surfex',
#                     cutoff         = cutoff,
#                     fatal          = False
#                 ),
#                 for pro in tb04[0]:
#                     pro.put()

                rundate = rundate + Period(days=1)


if __name__ == "__main__":

#    os.chdir('/home/vernaym/workdir')
    os.chdir('/manto/nousuj/reforecast')

    t = vortex.ticket()

    RFE = ReforecastExtractor(ticket=t)
    RFE.process()
