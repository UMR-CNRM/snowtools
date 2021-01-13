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

from scipy import nanmean
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

    def getlistvar(self, afile):
        return afile.listvar()

    def compare2files(self, name1, name2, checktime=True):

        conform = True

        file1 = SimplifiedProsimu(name1)
        file2 = SimplifiedProsimu(name2)

        listvar1 = self.getlistvar(file1)
        listvar2 = self.getlistvar(file2)

        if checktime:
            time1 = file1.readtime()
            time2 = file2.readtime()

            difftime = time1[:] - time2[:]
            mindiff, maxdiff = np.min(difftime), np.max(difftime)

            if mindiff == datetime.timedelta(0) and maxdiff == datetime.timedelta(0):
                # self.report("TIME CONFORME")
                pass
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
                    try:
                        var1 = file1.read(varname)
                        var2 = file2.read(varname)
                    except RuntimeError:
                        self.report(varname + " : unknown issue during file reading")
                        continue

                    # The variable should not be a masked array except if the _FillValue attribute is missing
                    if not np.ma.is_masked(var1[:]):

                        if var1.dtype == 'S1':
                            if not np.all(var1[:] == var2[:]):
                                conform = False
                                self.report(varname + " : characters differ")
                        elif np.all(np.isnan(var1[:])):
                            if not np.all(np.isnan(var2[:])):
                                conform = False
                                self.report(varname + " : only missing values in first file but defined in second file")
                        elif np.all(np.isnan(var2[:])):
                            if not np.all(np.isnan(var1[:])):
                                conform = False
                                self.report(varname + " : only missing values in second file but defined in first file")
                        else:
                            diff = var1[:] - var2[:]
                            mindiff, maxdiff, meandiff = np.nanmin(diff), np.nanmax(diff), nanmean(diff.flatten())

                            if mindiff == 0 and maxdiff == 0:
                                # self.report(varname + " : CONFORME")
                                pass
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

                try:
                    if type(globattr1) is np.ndarray:
                        if any(globattr1 != globattr2):
                            conform = False
                            self.report("Global attribute " + globattname + " differs:" + globattr1 + " - " + globattr2)

                    elif globattr1 != globattr2:
                        conform = False
                        self.report("Global attribute " + globattname + " differs:" + globattr1 + " - " + globattr2)

                except:
                    print ("BUG:")
                    print (globattname)
                    print (type(globattr1))

        file1.close()
        file2.close()

        return conform


# The following class is not useful for vortex but is temporarily used while netcdf comparison is not implemented in the diff toolbox.
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

    def __init__(self, nmembers):

        self.list_members = dict()

        for block in ["meteo", "pro", "prep"]:
            self.list_members[block] = dict()
            for conf in ["alp_allslopes", "pyr_allslopes", "cor_allslopes", "postes"]:
                if nmembers == 1:
                    self.list_members[block][conf] = [35]
                elif nmembers == 2:
                    self.list_members[block][conf] = [0, 35]
                else:
                    self.list_members[block][conf] = range(0, max(nmembers, self.nmembers[block][conf]))

        super(ComparisonS2MIntDev, self).__init__()

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

        if cutoff == 'P' and date.hour in [self.firstassimruntime.hour, self.secondassimruntime.hour]:
            block_to_test = ['pro']
        else:
            block_to_test = ["meteo", "pro"]

        for block in block_to_test:

            if cutoff == 'A' and block == 'pro':
                list_datebegin = list(daterange(datebegin, yesterday(base=dateend)))
                list_dateend = list(daterange(tomorrow(base=datebegin), dateend))
            else:
                list_datebegin = [datebegin]
                list_dateend = [dateend]

            for member in self.list_members[block][vconf]:

                for b, thisdatebegin in enumerate(list_datebegin):
                    thisdateend = list_dateend[b]
                    try:
                        get_file_period(self.filename[block], self.getpathint(vconf, date, cutoff, member, block), thisdatebegin, thisdateend)
                        os.rename(self.filename[block] + ".nc", self.filename[block] + "_int.nc")
                        get_file_period(self.filename[block], self.getpathdev(vconf, date, cutoff, member, block), thisdatebegin, thisdateend)
                        os.rename(self.filename[block] + ".nc", self.filename[block] + "_dev.nc")
                        thisconform = self.compare2files(self.filename[block] + "_int.nc", self.filename[block] + "_dev.nc")

                        if thisconform:
                            self.report("Conform output " + block + " for domain " + vconf + " member " + str(member) + " date " + date.stdvortex + cutoff)
                        else:
                            self.report("Not conform output " + block + " for domain " + vconf + " member " + str(member) + " date " + date.stdvortex + cutoff)
                        conform = conform and thisconform

                    except FileNameException as FNE:
                        self.report("Missing " + self.filename[block] + " for domain " + vconf + " member " + str(member) + " date " + date.stdvortex + cutoff)
                        self.report(FNE)
                        conform = False

        block = "prep"
        try:
            for member in self.list_members[block][vconf]:
                get_file_date(self.filename[block], self.getpathint(vconf, date, cutoff, member, block), dateend, raiseexception = True)
                os.rename(self.filename[block] + ".nc", self.filename[block] + "_int.nc")
                get_file_date(self.filename[block], self.getpathdev(vconf, date, cutoff, member, block), dateend, raiseexception = True)
                os.rename(self.filename[block] + ".nc", self.filename[block] + "_dev.nc")
                thisconform = self.compare2files(self.filename[block] + "_int.nc", self.filename[block] + "_dev.nc", checktime=False)
                if thisconform:
                    self.report("Conform output " + block + " for domain " + vconf + " member " + str(member) + " date " + date.stdvortex + cutoff)
                else:
                    self.report("Not conform output " + block + " for domain " + vconf + " member " + str(member) + " date " + date.stdvortex + cutoff)
                conform = conform and thisconform

        except FileNameException as FNE:
                self.report("Missing " + self.filename[block] + " for domain " + vconf + " member " + str(member) + " date " + date.stdvortex + cutoff)
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
                print (domain + ' ' + cutoff + ':' + str(conform))
        return conform

    def compareallruns(self, date):
        conform = True
        for runtime in [self.nightruntime, self.firstassimruntime, self.secondassimruntime]:

            conform = conform and self.comparealldomains(date.replace(hour=runtime.hour))
            print (str(runtime) + ':' + str(conform))
        return conform


class FastComparisonS2MIntDev(ComparisonS2MIntDev):

    def getlistvar(self, afile):
        vars_to_check = ['SNOWDZ', 'Tair', 'Rainf', 'TG1', 'WSN_VEG1', 'ZS']
        return list(set(vars_to_check) & set(afile.listvar()))


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

    parser.add_option("-n", "--nmembers",
                      action="store", type="int", dest="nmembers", default=1,
                      help="Last date (YYYYMMDD)")

    parser.add_option("-f", "--fast",
                      action="store_true", dest="fast", default=False,
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
        checktime = 'prep' not in options.new and 'PGD' not in options.new and 'pgd' not in options.new and 'PREP' not in options.new
        C.compare2files(options.new, options.old, checktime=checktime)
    elif options.datebegin and options.dateend:
        conform = True
        if options.fast:
            C = FastComparisonS2MIntDev(options.nmembers)
        else:
            C = ComparisonS2MIntDev(options.nmembers)
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
