#! /usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 1 oct 2014
Major update on 13 sept 2019

@author: Matthieu Lafaysse
"""

import sys
import os
import datetime

import numpy as np
import netCDF4
from scipy import nanmean

from snowtools.utils.FileException import FileNameException, VarNameException
# Not necessary to transfer in vortex
from snowtools.utils.resources import get_file_period, get_file_date
from snowtools.utils.dates import check_and_convert_date, checkdateafter
from bronx.stdtypes.date import Date, Period, Time, yesterday, tomorrow, daterange
from optparse import OptionParser

try:
    import cftime
except ImportError:
    cftime = None

usage = "USAGE compare2versions.py [fichier1 fichier2] [DATEBEGIN DATEEND]"


class SimplifiedProsimu(netCDF4.Dataset):
    """Simplified prosimu class"""

    def __init__(self, filename):
        if not os.path.isfile(filename):
            raise FileNameException(filename)
        super(SimplifiedProsimu, self).__init__(filename)

    def listdim(self):
        """
        list of dimensions

        :returns: list of dimensions
        :rtype: list
        """
        return self.dimensions.copy()

    def listvar(self):
        """
        list of variables

        :returns: list of variables
        :rtype: list
        """
        return list(self.variables.keys())

    def listattr(self, varname):
        """
        list of attributes for a variable

        :param varname: the variable name
        :type varname: str
        :returns: atributes
        """
        return self.variables[varname].ncattrs()

    def getattr(self, varname, attname):
        """
        get a specific attribute for a specific variable

        :param varname: the variable name
        :type varname: str
        :param attname: the attribute name
        :type attname: str
        :returns: the attribute value
        """
        return getattr(self.variables[varname], attname)

    def readtime(self):
        """
        Get the time dimension of the netCDF file

        :returns: time axis data
        :trype: numpy array
        """
        # Vérification du nom de la variable
        if "time" not in list(self.variables.keys()):
            raise VarNameException("time", self.path)

        time = self.variables["time"]

        if netCDF4.__version__ >= '1.4.0' and cftime is not None and cftime.__version__ >= '1.1.0':
            return np.array(netCDF4.num2date(time[:], time.units, only_use_cftime_datetimes=False,
                                             only_use_python_datetimes=True))
        else:
            return np.array(netCDF4.num2date(time[:], time.units))

    def read(self, varname):
        """
        read variable from NetCDF file.

        :param varname: variable name
        :returns: data array
        """
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
    """
    Class for Netcdf comparison
    """

    allowed_ga_differences = ['date_created'] # allowed differences in global attributes

    def report(self, info):
        """
        Define what should be done with the information message
        """
        print(info)

    def getlistvar(self, afile):
        """
        get variable list from given file

        :param afile: file object
        :type afile: :py:class:`SimplifiedProsimu`
        :returns: list of variables
        """
        return afile.listvar()

    def compare2files(self, name1, name2, checktime=True):
        """
        main comparison method

        :param name1: filename of first file
        :param name2: filename of second file
        :param checktime: check time variable
        :type checktime: bool

        :returns: conform
        :rtype: bool
        """

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
                                self.report(varname + " : mean=" + str(meandiff) + " : min=" + str(mindiff) + " : max="
                                            + str(maxdiff))

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
                globattr1 = file1.getncattr(globattname)
                globattr2 = file2.getncattr(globattname)

                try:
                    if type(globattr1) is np.ndarray:
                        if any(globattr1 != globattr2):
                            conform = False
                            self.report("Global attribute " + globattname + " differs:" + globattr1 + " - " + globattr2)

                    elif globattr1 != globattr2:
                        conform = False
                        self.report("Global attribute " + globattname + " differs:" + globattr1 + " - " + globattr2)

                except:
                    print("BUG:")
                    print(globattname)
                    print(type(globattr1))

        file1.close()
        file2.close()

        return conform


class ComparisonS2MIntDev(ComparisonNetcdf):
    """
    This class is not useful for vortex but is temporarily used while netcdf comparison is not implemented
    in the diff toolbox.
    """

    pathint = "/chaine/mxpt001/vortex/mtool/cache/vortex/s2m"  #: path to operational s2m chain
    pathdev = "/scratch/mtool/lafaysse/cache/vortex/s2m"  #: path to development chain

    xpid_int = 'OPER'  #: experiment id of operational chain
    xpid_dev = 'nouveaux_guess@lafaysse'  #: experiment id of development chain

    filename = dict(meteo='forcing', pro='pro', prep='prep')  #: filename dict for s2m files
    #: number of members for snow simulation
    nmembers_snow = dict(alp=37, pyr=37, cor=37, postes=35)
    #: number of members for safran meteo simulation
    nmembers_meteo = dict(alp=36, pyr=36, cor=36, postes=35)
    #: member number dict
    nmembers = dict(meteo=nmembers_meteo, pro=nmembers_snow, prep=nmembers_snow)

    nightruntime = Time(hour=3, minute=0)  #: runtime for first run of the day
    firstassimruntime = Time(hour=6, minute=0)  #: runtime for second run of the day
    secondassimruntime = Time(hour=9, minute=0)  #: runtime of third run of the day
    monthly_analysis_time = Time(hour=12, minute=0)  #: hour of the monthly reanalysis

    def __init__(self, nmembers):

        self.list_members = dict()

        for block in ["meteo", "pro", "prep"]:
            self.list_members[block] = dict()
            for conf in ["alp", "pyr", "cor", "postes"]:
                if nmembers == 1:
                    self.list_members[block][conf] = [35]
                elif nmembers == 2:
                    self.list_members[block][conf] = [0, 35]
                else:
                    self.list_members[block][conf] = range(0, max(nmembers, self.nmembers[block][conf]))

        super(ComparisonS2MIntDev, self).__init__()

    def dirdate(self, date, cutoff):
        """
        get date directory name

        :param date: rundate
        :param cutoff: "A" or "P" (for analysis or forecast)
        :returns: date directory name
        :rtype: str
        """
        return Date(date).stdvortex + cutoff

    def getpathint(self, vconf, date, cutoff, member, block):
        """
        get path of operational simulation

        :param vconf: vortex vconf
        :param date: rundate
        :param cutoff: "A" or "P" (for analysis or forecast)
        :param member: member number
        :param block: vortex block ("meteo", "pro", "prep")
        :returns: path
        """
        return "/".join([self.pathint, vconf.replace("_allslopes", ""), self.xpid_int, self.dirdate(date, cutoff),
                         "mb%3.3d" % member, block])

    def getpathdev(self, vconf, date, cutoff, member, block):
        """
        get path of development simulation

        :param vconf: vortex vconf
        :param date: rundate
        :param cutoff: "A" or "P" (for analysis or forecast)
        :param member: member number
        :param block: vortex block ("meteo", "pro", "prep")
        :returns: path
        """
        return "/".join([self.pathdev, vconf, self.xpid_dev, self.dirdate(date, cutoff), "mb%3.3d" % member, block])

    def get_period(self, rundate, cutoff):
        """
        get simulation start date and end date

        :param rundate: simulation reference date
        :param cutoff: "A" or "P" (for analysis or forecast)
        :returns: datebegin, dateend
        """

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
                    year = rundate.year - 1
                else:
                    year = rundate.year
                datebegin = Date(year, 7, 31, 6)
            else:
                # The daytime runs perform a 1 day analysis
                datebegin = dateend - Period(days=1)

        return datebegin, dateend

    def compareallmembers(self, vconf, date, cutoff):
        """
        Comparison loop over members

        :param vconf: vortex vconf
        :param date: run date
        :param cutoff: "A" or "P" (for analysis or forecast)
        :returns: conform
        :rtype: bool
        """

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
                        get_file_period(self.filename[block], self.getpathint(vconf, date, cutoff, member, block),
                                        thisdatebegin, thisdateend)
                        os.rename(self.filename[block] + ".nc", self.filename[block] + "_int.nc")
                        get_file_period(self.filename[block], self.getpathdev(vconf, date, cutoff, member, block),
                                        thisdatebegin, thisdateend)
                        os.rename(self.filename[block] + ".nc", self.filename[block] + "_dev.nc")
                        thisconform = self.compare2files(self.filename[block] + "_int.nc", self.filename[block]
                                                         + "_dev.nc")

                        if thisconform:
                            self.report("Conform output " + block + " for domain " + vconf + " member " + str(member)
                                        + " date " + date.stdvortex + cutoff)
                        else:
                            self.report("Not conform output " + block + " for domain " + vconf + " member "
                                        + str(member) + " date " + date.stdvortex + cutoff)
                        conform = conform and thisconform

                    except FileNameException as FNE:
                        self.report("Missing " + self.filename[block] + " for domain " + vconf + " member "
                                    + str(member) + " date " + date.stdvortex + cutoff)
                        self.report(FNE)
                        conform = False

        block = "prep"
        try:
            for member in self.list_members[block][vconf]:
                get_file_date(self.filename[block], self.getpathint(vconf, date, cutoff, member, block), dateend,
                              raiseexception=True)
                os.rename(self.filename[block] + ".nc", self.filename[block] + "_int.nc")
                get_file_date(self.filename[block], self.getpathdev(vconf, date, cutoff, member, block), dateend,
                              raiseexception=True)
                os.rename(self.filename[block] + ".nc", self.filename[block] + "_dev.nc")
                thisconform = self.compare2files(self.filename[block] + "_int.nc", self.filename[block] + "_dev.nc",
                                                 checktime=False)
                if thisconform:
                    self.report("Conform output " + block + " for domain " + vconf + " member " + str(member) + " date "
                                + date.stdvortex + cutoff)
                else:
                    self.report("Not conform output " + block + " for domain " + vconf + " member " + str(member)
                                + " date " + date.stdvortex + cutoff)
                conform = conform and thisconform

        except FileNameException as FNE:
            self.report("Missing " + self.filename[block] + " for domain " + vconf + " member " + str(member)
                        + " date " + date.stdvortex + cutoff)
            self.report(FNE)
            conform = False
        return conform

    def comparealldomains(self, date):
        """
        Comparison loop over all domains ("alp_allslopes", "pyr_allslopes", "cor_allslopes", "postes")
        and "cutoffs" ("A", "P") = analysis and forecast

        :param date: date to be compared
        :returns: conform
        :rtype: bool

        :calls: :py:meth:`compareallmembers`
        """
        conform = True
        for domain in ["alp", "pyr", "cor", "postes"]:
            print(domain)
            for cutoff in ["A", "P"]:
                print(cutoff)
                conform = conform and self.compareallmembers(domain, date, cutoff)
                print(domain + ' ' + cutoff + ':' + str(conform))
        return conform

    def compareallruns(self, date):
        """
        Comparison loop over all runs on a given day

        :param date: run date
        :returns: conform
        :rtype: bool
        :calls: :py:meth:`comparealldomains`
        """
        conform = True
        for runtime in [self.nightruntime, self.firstassimruntime, self.secondassimruntime]:

            conform = conform and self.comparealldomains(date.replace(hour=runtime.hour))
            print(str(runtime) + ':' + str(conform))
        return conform


class FastComparisonS2MIntDev(ComparisonS2MIntDev):
    """Fast comparison of operational chain with development chain """

    def getlistvar(self, afile):
        """
        get reduced list of variables to compare

        :param afile: file object
        :type afile: :py:class:`SimplifiedProsimu`
        """
        vars_to_check = ['SNOWDZ', 'Tair', 'Rainf', 'TG1', 'WSN_VEG1', 'ZS']
        return list(set(vars_to_check) & set(afile.listvar()))


class ComparisonS2MDbleDev(ComparisonS2MIntDev):
    """Class for comparing dev chain with "chaine en double". """
    xpid_int = 'DBLE'  #: experiment id


class FastComparisonS2MDbleDev(FastComparisonS2MIntDev, ComparisonS2MDbleDev):
    """Class for fast comparison dev chain with "chaine en double". """
    pass


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

    parser.add_option("--double",
                      action="store_true", dest="double", default=False,
                      help='Reference is double')

    (opts, args) = parser.parse_args(arguments)

    # Controls and type conversions of dates
    [opts.datebegin, opts.dateend] = list(map(check_and_convert_date, [opts.datebegin, opts.dateend]))
    checkdateafter(opts.dateend, opts.datebegin)

    del args
    return opts


if __name__ == "__main__":

    options = parse_options(sys.argv)

    if options.new and options.old:
        C = ComparisonNetcdf()
        checktime = 'prep' not in options.new and 'PGD' not in options.new and 'pgd' not in options.new \
                    and 'PREP' not in options.new
        C.compare2files(options.new, options.old, checktime=checktime)
    elif options.datebegin and options.dateend:
        is_conform = True
        if options.fast:
            if options.double:
                C = FastComparisonS2MDbleDev(options.nmembers)
            else:
                C = FastComparisonS2MIntDev(options.nmembers)
        else:
            if options.double:
                C = ComparisonS2MDbleDev(options.nmembers)
            else:
                C = ComparisonS2MIntDev(options.nmembers)
        currentdate = Date(options.datebegin)
        while currentdate < options.dateend:
            is_conform = is_conform and C.compareallruns(currentdate)
            currentdate = tomorrow(currentdate)
        if is_conform:
            print("All runs are conform, well done !")
        else:
            print("Some runs differ. Good luck !")
    else:
        print(usage)
        sys.exit(1)
