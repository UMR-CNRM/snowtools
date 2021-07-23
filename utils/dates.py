# -*- coding: utf-8 -*-

'''
Created on 30 Aug. 2017

@author: lafaysse
'''

from six import string_types
from bronx.stdtypes.date import Date


class TypeException(Exception):
    def __init__(self, typein, typerequired):
        self.typein = typein
        self.typerequired = typerequired
        self.message = "Type provided: " + str(typein) + " Type expected: " + str(typerequired)

    def __str__(self):
        return "Argument type exception:\n" + self.message

    def __reduce__(self):
        red = list(super(TypeException, self).__reduce__())
        red[1] = (self.typein, self.typerequired)  # Les arguments qui seront passes a __init__
        return tuple(red)


class DateException(Exception):

    def __init__(self, message=""):
        self.message = message

    def __str__(self):
        return "Dates inconsitency : " + self.message


class WallTimeException(Exception):

    def __init__(self, duration):
        self.duration = duration

    def __str__(self):
        return "Walltime is longer than 24 hours: " + str(self.duration)


class EarlyDateException(DateException):

    def __init__(self, earlydate, date):
        self.earlydate = earlydate
        self.date = date
        self.message = earlydate.strftime("%Y-%m-%d %H:%M:%S") + " is before " + date.strftime("%Y-%m-%d %H:%M:%S")

    def __reduce__(self):
        red = list(super(EarlyDateException, self).__reduce__())
        red[1] = (self.earlydate, self.date)  # Les arguments qui seront passes a __init__
        return tuple(red)


class LateDateException(DateException):

    def __init__(self, latedate, date):
        self.latedate = latedate
        self.date = date
        self.message = latedate.strftime("%Y-%m-%d %H:%M:%S") + " is after " + date.strftime("%Y-%m-%d %H:%M:%S")

    def __reduce__(self):
        red = list(super(LateDateException, self).__reduce__())
        red[1] = (self.latedate, self.date)  # Les arguments qui seront passes a __init__
        return tuple(red)


class FormatDateException(DateException):

    def __init__(self, datein):
        self.message = datein + ": Unknown format."


def checkdatebefore(date, datemax):
    if date > datemax:
        raise LateDateException(date, datemax)


def checkdateafter(date, datemin):
    if date < datemin:
        raise EarlyDateException(date, datemin)


def checkdatebetween(date, datemin, datemax):
    checkdateafter(date, datemin)
    checkdatebefore(date, datemax)


def check_and_convert_date(datearg):

    if datearg:
        if not isinstance(datearg, string_types):
            raise TypeException(type(datearg), str)

        if len(datearg) == 8:
            return Date(int(datearg[0:4]), int(datearg[4:6]), int(datearg[6:8]), 6, 0, 0)
        elif len(datearg) == 10:
            return Date(int(datearg[0:4]), int(datearg[4:6]), int(datearg[6:8]), int(datearg[8:10]), 0, 0)
        elif len(datearg) == 14:
            return Date(int(datearg[0:4]), int(datearg[4:6]), int(datearg[6:8]), int(datearg[8:10]), int(datearg[10:12]), int(datearg[12:14]))
        else:
            raise FormatDateException(datearg)
    else:
        return datearg


def pretty_date(datetimeobject):
    return datetimeobject.strftime("%A %d %B %Y à %H:%M")


def get_list_dates_files(datebegin, dateend, duration, listDateStop=None):
    list_dates_begin_forcing = []
    list_dates_end_forcing = []
    if duration == "yearly":
        if datebegin.month >= 8:
            dateforc_begin = Date(datebegin.year, 8, 1, 6, 0, 0)
        else:
            dateforc_begin = Date(datebegin.year - 1, 8, 1, 6, 0, 0)
        dateforc_end = dateforc_begin
        while dateforc_end < dateend:
            dateforc_end = dateforc_begin.replace(year= dateforc_begin.year + 1)
            list_dates_begin_forcing.append(dateforc_begin)
            list_dates_end_forcing.append(dateforc_end)
            dateforc_begin = dateforc_end
    elif duration == "monthly":
        dateforc_begin = Date(datebegin.year, datebegin.month, 1, 6, 0, 0)
        dateforc_end = dateforc_begin
        while dateforc_end < dateend:
            if dateforc_begin.month == 12:
                dateforc_end = dateforc_begin.replace(year= dateforc_begin.year + 1, month= 1)
            else:
                dateforc_end = dateforc_begin.replace(month= dateforc_begin.month + 1)
            list_dates_begin_forcing.append(dateforc_begin)
            list_dates_end_forcing.append(dateforc_end)
            dateforc_begin = dateforc_end
    elif duration == "full":
        list_dates_begin_forcing.append(datebegin)
        list_dates_end_forcing.append(dateend)
    if listDateStop is None:  # bc added for SODA stop dates
        list_dates_begin_pro = list_dates_begin_forcing[:]
        list_dates_begin_pro[0] = max(list_dates_begin_forcing[0], datebegin)
        list_dates_end_pro = list_dates_end_forcing[:]
        list_dates_end_pro[-1] = min(list_dates_end_forcing[-1], dateend)
    else:
        list_dates_begin_pro = listDateStop[:]
        list_dates_begin_pro.insert(0, max(list_dates_begin_forcing[0], datebegin))
        list_dates_end_pro = listDateStop[:]
        list_dates_end_pro.append(min(list_dates_end_forcing[-1], dateend))

    return list_dates_begin_forcing, list_dates_end_forcing, list_dates_begin_pro, list_dates_end_pro


def get_dic_dateend(list_dates_begin, list_dates_end):
    # For footprints, to not combine all datebegin values with all dateend values, dictionnaries are necessary for dateend
    return dict(datebegin= {str(k): v for k, v in zip(list_dates_begin, list_dates_end)})
