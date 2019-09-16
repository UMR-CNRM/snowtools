#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 1 oct 2014
Major update on 13 sept 2019

@author: Matthieu Lafaysse
'''


import sys
import os
import datetime
import numpy as np

import netCDF4

from scipy.stats import nanmean
from utils.FileException import FileNameException, VarNameException

# Not necessary to transfer in vortex
from utils.resources import get_file_period, get_file_date
from utils.dates import check_and_convert_date, checkdateafter
from bronx.stdtypes.date import Date, Period, Time, yesterday, tomorrow, daterange
from optparse import OptionParser


usage = "USAGE compare2versions.py [fichier1 fichier2] [DATEBEGIN DATEEND]"


class SimplifiedProsimu(netCDF4.Dataset):

    def __init__(self, filename):
        if not os.path.isfile(filename):
            raise FileNameException(filename)
        super(SimplifiedProsimu, self).__init__(filename)

    def listdim(self):
        return self.dimensions.copy()

    def listvar(self):
        return list(self.variables.keys())

    def listattr(self, varname):
        return self.variables[varname].ncattrs()

    def getattr(self, varname, attname):
        return getattr(self.variables[varname], attname)

    def readtime(self):
        # Vérification du nom de la variable
        if "time" not in list(self.variables.keys()):
            raise VarNameException("time", self.path)

        time = self.variables["time"]

        return np.array(netCDF4.num2date(time[:], time.units))

    def read(self, varname):
        if varname not in self.listvar():
            raise VarNameException(varname, self.path)

        # Sélection de la variable
        varnc = self.variables[varname]

        avail_fillvalue = "_FillValue" in varnc.ncattrs()
        if len(varnc.shape) != 0 and avail_fillvalue:
            fillvalue = varnc._FillValue
            array = np.where(varnc == fillvalue, np.nan, varnc)
        else:
            array = varnc
        return array


class ComparisonNetcdf(object):

    allowed_ga_differences = ['date_created']

    def report(self, info):
        # Define what should be done with the information message
        print (info)

    def compare2files(self, name1, name2, checktime=True):

        conform = True

        file1 = SimplifiedProsimu(name1)
        file2 = SimplifiedProsimu(name2)

        listvar1 = file1.listvar()
        listvar2 = file2.listvar()

        if checktime:
            time1 = file1.readtime()
            time2 = file2.readtime()

            difftime = time1[:] - time2[:]
            mindiff, maxdiff = np.min(difftime), np.max(difftime)

            if mindiff == datetime.timedelta(0) and maxdiff == datetime.timedelta(0):
                self.report("TIME CONFORME")
            else:
                conform = False
                self.report("TIME NON CONFORME !!  min=" + str(mindiff) + " : max=" + str(maxdiff))

        for varname in listvar2:
            if varname not in listvar1:
                self.report(varname + " is missing in first file")

        for varname in listvar1:
            if varname != "time":

                if varname in listvar2:
                    # Read and compare the values of the variable
                    var1 = file1.read(varname)
                    var2 = file2.read(varname)

#                     if len(var1.shape) != 0:
                    diff = var1[:] - var2[:]
                    mindiff, maxdiff, meandiff = np.nanmin(diff), np.nanmax(diff), nanmean(diff.flatten())
#                     else:
#                         # Specific management of 0d variables, otherwise the operation is not supported
#                         var1 = var1[...]
#                         var2 = var2[...]
#                         print var1.shape
#                         mindiff = maxdiff = meandiff = var1 - var2
#                         print type(var1)
#                         print type(mindiff)

                    if mindiff == 0 and maxdiff == 0:
                        self.report(varname + " : CONFORME")
                    else:
                        conform = False
                        self.report(varname + " : mean=" + str(meandiff) + " : min=" + str(mindiff) + " : max=" + str(maxdiff))

                    listattr1 = file1.listattr(varname)
                    listattr2 = file2.listattr(varname)

                    # Read and compare the attributes of the variable
                    for attname in listattr1:

                        if attname in listattr2:
                            attr1 = file1.getattr(varname, attname)
                            attr2 = file2.getattr(varname, attname)

                            if attr1 != attr2:
                                conform = False
                                self.report(varname + " attribute " + attname + " differs:" + attr1 + " - " + attr2)

                        else:
                            conform = False
                            self.report(varname + " attribute " + attname + " missing in second file")

                    for attname in listattr2:
                        if attname not in listattr1:
                            conform = False
                            self.report(varname + " attribute " + attname + " missing in first file")

                else:
                    conform = False
                    self.report(varname + " is missing in second file")

        # Read and compare the global attributes of the variable

        listglobattr1 = file1.ncattrs()
        listglobattr2 = file2.ncattrs()

        for globattname in listglobattr2:
            if globattname not in listglobattr1:
                conform = False
                self.report("Global attribute" + globattname + " is missing in first file")

        for globattname in listglobattr1:
            if globattname not in listglobattr2:
                conform = False
                self.report("Global attribute" + globattname + " is missing in second file")
            else:
                if globattname in self.allowed_ga_differences:
                    continue
                globattr1 = getattr(file1, globattname)
                globattr2 = getattr(file2, globattname)

                if globattr1 != globattr2:
                    conform = False
                    self.report("Global attribute " + globattname + " differs:" + globattr1 + " - " + globattr2)

        file1.close()
        file2.close()

        return conform


class ComparisonS2MIntDev(ComparisonNetcdf):

    pathint = "/chaine/mxpt001/vortex/mtool/cache/vortex/s2m"
    pathdev = "/scratch/mtool/lafaysse/cache/vortex/s2m"

    filename = dict(meteo = 'forcing', pro = 'pro', prep = 'prep')
    nmembers_snow = dict(alp_allslopes = 37, pyr_allslopes = 37, cor_allslopes = 37, postes = 35)
    nmembers_meteo = dict(alp_allslopes = 36, pyr_allslopes = 36, cor_allslopes = 36, postes = 35)
    nmembers = dict(meteo = nmembers_meteo, pro = nmembers_snow, prep = nmembers_snow)

    nightruntime = Time(hour=3, minute=0)
    firstassimruntime = Time(hour=6, minute=0)
    secondassimruntime = Time(hour=9, minute=0)
    monthly_analysis_time = Time(hour=12, minute=0)

    def dirdate(self, date, cutoff):
        return Date(date).stdvortex + cutoff

    def getpathint(self, vconf, date, cutoff, member, block):
        return "/".join([self.pathint, vconf.replace("_allslopes", ""), "OPER", self.dirdate(date, cutoff), "mb%3.3d" % member, block])

    def getpathdev(self, vconf, date, cutoff, member, block):
        return "/".join([self.pathdev, vconf, "OPER@lafaysse", self.dirdate(date, cutoff), "mb%3.3d" % member, block])

    def get_period(self, rundate, cutoff):

        previ = cutoff == 'P'

        if rundate.hour == self.nightruntime.hour:
            dateendanalysis = yesterday(rundate.replace(hour=6))
        else:
            dateendanalysis = rundate.replace(hour=6)

        if previ:
            datebegin = dateendanalysis
            if rundate.hour == self.nightruntime.hour:
                dateend = dateendanalysis + Period(days=5)
            else:
                dateend = dateendanalysis + Period(days=4)
        else:
            dateend = dateendanalysis
            if rundate.hour == self.nightruntime.hour:
                # The night run performs a 4 day analysis
                datebegin = dateend - Period(days=4)
            elif rundate.hour == self.monthly_analysis_time.hour:
                if rundate.month <= 7:
                    year = self.conf.rundate.year - 1
                else:
                    year = self.conf.rundate.year
                datebegin = Date(year, 7, 31, 6)
            else:
                # The daytime runs perform a 1 day analysis
                datebegin = dateend - Period(days=1)

        return datebegin, dateend

    def compareallmembers(self, vconf, date, cutoff):

        conform = True

        datebegin, dateend = self.get_period(date, cutoff)

        for block in ["meteo", "pro"]:

            if cutoff == 'A' and block == 'pro':
                list_datebegin = list(daterange(datebegin, yesterday(base=dateend)))
                list_dateend = list(daterange(tomorrow(base=datebegin), dateend))
            else:
                list_datebegin = [datebegin]
                list_dateend = [dateend]

            for member in range(0, self.nmembers[block][vconf]):

                for b, thisdatebegin in enumerate(list_datebegin):
                    thisdateend = list_dateend[b]
                    try:
                        get_file_period(self.filename[block], self.getpathint(vconf, date, cutoff, member, block), thisdatebegin, thisdateend)
                        os.rename(self.filename[block] + ".nc", self.filename[block] + "_int.nc")
                        get_file_period(self.filename[block], self.getpathdev(vconf, date, cutoff, member, block), thisdatebegin, thisdateend)
                        os.rename(self.filename[block] + ".nc", self.filename[block] + "_dev.nc")
                        conform = conform and self.compare2files(self.filename[block] + "_int.nc", self.filename[block] + "_dev.nc")
                    except FileNameException as FNE:
                        self.report("Missing " + self.filename[block] + " for domain " + vconf + " member " + str(member) + " date " + date.stdvortex + cutoff)
                        self.report(FNE)
                        conform = False

            try:
                get_file_date(self.filename['prep'], self.getpathint(vconf, date, cutoff, member, block), dateend, raiseexception = True)
                os.rename(self.filename['prep'] + ".nc", self.filename['prep'] + "_int.nc")
                get_file_date(self.filename['prep'], self.getpathdev(vconf, date, cutoff, member, block), dateend, raiseexception = True)
                os.rename(self.filename['prep'] + ".nc", self.filename['prep'] + "_dev.nc")
                conform = conform and self.compare2files(self.filename[block] + "_int.nc", self.filename[block] + "_dev.nc")

            except FileNameException as FNE:
                    self.report("Missing " + self.filename['prep'] + " for domain " + vconf + " member " + str(member) + " date " + date.stdvortex + cutoff)
                    self.report(FNE)
                    conform = False
        return conform

    def comparealldomains(self, date):
        conform = True
        for domain in ["alp_allslopes", "pyr_allslopes", "cor_allslopes", "postes"]:
            print (domain)
            for cutoff in ["A", "P"]:
                print (cutoff)
                conform = conform and self.compareallmembers(domain, date, cutoff)

    def compareallruns(self, date):
        conform = True
        for runtime in [self.nightruntime, self.firstassimruntime, self.secondassimruntime]:
            print (runtime)
            conform = conform and self.comparealldomains(date.replace(hour=runtime.hour))


def parse_options(arguments):
    parser = OptionParser(usage)

    parser.add_option("--old",
                      action="store", type="string", dest="old", default=None,
                      help="path of old file")

    parser.add_option("--new",
                      action="store", type="string", dest="new", default=None,
                      help="path of new file")

    parser.add_option("-b", "--begin",
                      action="store", type="string", dest="datebegin", default=None,
                      help="First date (YYYYMMDD)")

    parser.add_option("-e", "--end",
                      action="store", type="string", dest="dateend", default=None,
                      help="Last date (YYYYMMDD)")

    (options, args) = parser.parse_args(arguments)

    # Controls and type conversions of dates
    [options.datebegin, options.dateend] = list(map(check_and_convert_date, [options.datebegin, options.dateend]))
    checkdateafter(options.dateend, options.datebegin)

    del args
    return options


if __name__ == "__main__":

    options = parse_options(sys.argv)

    if options.new and options.old:
        C = ComparisonNetcdf()
        C.compare2files(options.new, options.old)
    elif options.datebegin and options.dateend:
        conform = True
        C = ComparisonS2MIntDev()
        currentdate = Date(options.datebegin)
        while currentdate < options.dateend:
            conform = conform and C.compareallruns(currentdate)
            currentdate = tomorrow(currentdate)
        if conform:
            print ("All runs are conform, well done !")
        else:
            print ("Some runs differ. Good luck !")
    else:
        print (usage)
        sys.exit(1)
