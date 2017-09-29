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
