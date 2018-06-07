'''
Created on 30 Aug. 2017

@author: lafaysse
'''

import datetime


class TypeException(Exception):
    def __init__(self, typein, typerequired):
            self.message = "Type provided: " + str(typein) + " Type expected: " + str(typerequired)

    def __str__(self):
        return "Argument type exception:\n" + self.message


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
        self.message = earlydate.strftime("%Y-%m-%d %H:%M:%S") + " is before " + date.strftime("%Y-%m-%d %H:%M:%S")


class LateDateException(DateException):

    def __init__(self, latedate, date):
        self.message = latedate.strftime("%Y-%m-%d %H:%M:%S") + " is after " + date.strftime("%Y-%m-%d %H:%M:%S")


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
        if not type(datearg) is str:
            raise TypeException(type(datearg), str)

        if len(datearg) == 8:
            return datetime.datetime(int(datearg[0:4]), int(datearg[4:6]), int(datearg[6:8]), 6, 0, 0)
        elif len(datearg) == 10:
            return datetime.datetime(int(datearg[0:4]), int(datearg[4:6]), int(datearg[6:8]), int(datearg[8:10]), 0, 0)
        elif len(datearg) == 14:
            return datetime.datetime(int(datearg[0:4]), int(datearg[4:6]), int(datearg[6:8]), int(datearg[8:10]), int(datearg[10:12]), int(datearg[12:14]))
        else:
            FormatDateException(datearg)
    else:
        return datearg


def get_list_dates_files(datebegin, dateend, duration):
    list_dates_begin_forcing = []
    list_dates_end_forcing = []
    if duration == "yearly":
        if datebegin.month >= 8:
            dateforc_begin = datetime.datetime(datebegin.year, 8, 1, 6, 0, 0)
        else:
            dateforc_begin = datetime.datetime(datebegin.year - 1, 8, 1, 6, 0, 0)
        dateforc_end = dateforc_begin
        while dateforc_end < dateend:
            dateforc_end = dateforc_begin.replace(year= dateforc_begin.year + 1)
            list_dates_begin_forcing.append(dateforc_begin)
            list_dates_end_forcing.append(dateforc_end)
            dateforc_begin = dateforc_end
    elif duration == "monthly":
        dateforc_begin = datetime.datetime(datebegin.year, datebegin.month, 1, 6, 0, 0)
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

    list_dates_begin_pro = list_dates_begin_forcing[:]
    list_dates_begin_pro[0] = max(list_dates_begin_forcing[0], datebegin)
    list_dates_end_pro = list_dates_end_forcing[:]
    list_dates_end_pro[-1] = min(list_dates_end_forcing[-1], dateend)
    print("DEBUG")
    print(list_dates_begin_forcing)
    print(list_dates_end_forcing)
    return list_dates_begin_forcing, list_dates_end_forcing, list_dates_begin_pro, list_dates_end_pro
