# -*- coding: utf-8 -*-

'''
Created on 30 Aug. 2017

@author: lafaysse
'''

import re

from bronx.stdtypes.date import Date


class TypeException(Exception):
    """
    Expetion when input type is not accepted.
    """
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
    """
    All exceptions related to date parsing
    """

    def __init__(self, message=""):
        self.message = message

    def __str__(self):
        return "Dates inconsitency : " + self.message


class WallTimeException(Exception):
    """
    Exceptions related to wall time, for walltime longer than 24h.
    """

    def __init__(self, duration):
        self.duration = duration

    def __str__(self):
        return "Walltime is longer than 24 hours: " + str(self.duration)


class EarlyDateException(DateException):
    """
    Exceptions to be raised when date need to be ordered and are not (version "too early").
    """

    def __init__(self, earlydate, date):
        self.earlydate = earlydate
        self.date = date
        self.message = earlydate.strftime("%Y-%m-%d %H:%M:%S") + " is before " + date.strftime("%Y-%m-%d %H:%M:%S")

    def __reduce__(self):
        red = list(super(EarlyDateException, self).__reduce__())
        red[1] = (self.earlydate, self.date)  # Les arguments qui seront passes a __init__
        return tuple(red)


class LateDateException(DateException):
    """
    Exceptions to be raised when date need to be ordered and are not (version "too late").
    """

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
    """
    Check that ``date < datemax`` or raises a `LateDateException`.

    :param date: Date to be tested
    :type date: datetime or Bronx Date format
    :param datemax: The maximum date accepted
    :type date: datetime or Bronx Date format
    :raises: LateDateException
    :returns: Nothing
    """
    if date > datemax:
        raise LateDateException(date, datemax)


def checkdateafter(date, datemin):
    """
    Check that ``date > datemin`` or raises a `EarlyDateException`.

    :param date: Date to be tested
    :type date: datetime or Bronx Date format
    :param datemin: The minimum date accepted
    :type date: datetime or Bronx Date format
    :raises: EarlyDateException
    :returns: Nothing
    """
    if date < datemin:
        raise EarlyDateException(date, datemin)


def checkdatebetween(date, datemin, datemax):
    """
    Uses both ``checkdateafter`` and ``checkdatebefore``.

    :param date: Date to be tested
    :type date: datetime or Bronx Date format
    :param datemin: The minimum date accepted
    :type date: datetime or Bronx Date format
    :param datemax: The maximum date accepted
    :type date: datetime or Bronx Date format
    :raises: EarlyDateException, LateDateException
    :returns: Nothing
    """
    checkdateafter(date, datemin)
    checkdatebefore(date, datemax)


def _parsematch(d, default=None):
    """
    Private function to deal with defined or undefined groups in
    regex match for the `check_and_convert_date` function.
    """
    if d is None:
        return default
    else:
        return int(d)


def check_and_convert_date(datearg):
    """
    Check an input date string and return a
    `bronx.stdtypes.date.Date` object

    Accepted date formats:

    * YYYY[MM[DD[HH[MM[SS]]]]]
    * YYYY-MM-DD[ HH:MM]
    * YYYY-MM-DDTHH:MM:SS
    * Etc (exact regex in the code).

    :param datearg: Date to be parsed
    :type datearg: str
    """

    if datearg:
        if not isinstance(datearg, str):
            raise TypeException(type(datearg), str)

        # the regex that parses date format
        f1 = re.match('([0-9]{4})[-]?([0-9]{2})?[-]?([0-9]{2})?[ T]?([0-9]{2})?[ :hH]?([0-9]{2})?[ :mM]?([0-9]{2})?',
                      datearg)
        if f1 is None:
            raise FormatDateException(datearg)
        return Date(
                _parsematch(f1.group(1)),             # Year
                _parsematch(f1.group(2), default=8),  # Month
                _parsematch(f1.group(3), default=1),  # Day
                _parsematch(f1.group(4), default=6),  # Hour
                _parsematch(f1.group(5), default=0),  # Min
                _parsematch(f1.group(6), default=0),  # Sec
                )
    else:
        return datearg


def pretty_date(datetimeobject):
    """
    Return a string with input prettfied.

    :param datetimeobject: The date object to be prettified
    :type datetimeobject: datetime or bronx Date object
    :returns: Prettified string
    :rtype: str
    """
    return datetimeobject.strftime("%A %d %B %Y à %H:%M")


def get_list_dates_files(datebegin, dateend, duration, listDateStop=None):
    """
    Get the list of begin and end dates for forcing and pro files
    according to given begin and end dates, with simulation
    possibly interrupted at given dates.

    :param datebegin: The begin date
    :type datebegin: datetime or bronx Date object
    :param dateend: The end date
    :type dateend: datetime or bronx Date object
    :param duration: One of yearly, monthly or full
    :type duration: str
    :param listDateStop: The list of dates for imposed stop of simulation (for instance for SODA assimilation)
    :type listDateStop: list
    :returns: list_dates_begin_forcing, list_dates_end_forcing, list_dates_begin_pro, list_dates_end_pro
    :rtype: list, list, list, list
    :raises: ValueError if duration is not recognized
    """
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
    else:
        raise ValueError('Value for parameter duration not accepted')
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
