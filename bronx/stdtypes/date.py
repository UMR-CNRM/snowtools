#!/usr/bin/env python
# -*- coding:Utf-8 -*-

"""
Classes and functions form this module are dedicated to the manipulation of
date and time quantities.

Obviously the main classes of this module are :class:`Date`, :class:`Period`,
:class:`Time` and :class:`Month`.

Some helper functions are also provided (to get the current date, ...).

Formats hypothesis:

1. Ideally dates and times should be represented as a valid ISO 8601 strings.
   Here are a few exemples:

    * 2016-01-01 or 20160101 (for a date)
    * 12:00, 1200, 12:00:00 or 120000 (for a time)
    * A combination of both: 2016-01-01T12:00
    * Optionally the time zone indicator: 2016-01-01T12:00Z

2. For a date, the following will also be accepted yyyymmdd[hh[mn[ss]]] with
   yyyy as the year in 4 numbers, mm as the month in 2 numbers, dd as the
   day in 2 numbers, hh as the hours (0-24), mn as the minutes and ss as the seconds.

3. For time periods, the following convention applies:

    * P starts an ISO 8601 Period definition
    * nY, the number of years (n positive integer),
    * nM, the number of months (n positive integer),
    * nD, the number of days (n positive integer),
    * T as a time separator,
    * nH, the number of hours (n positive integer),
    * nM, the number of minutes (n positive integer),
    * nS, the number of seconds (n positive integer)

   Examples:
    * P1Y <=> is a 1 year period
    * P20D <=> is a 20 days period
    * PT15H10M55S <=> is a 15 hours, 10 minutes and 55 seconds period

"""

from __future__ import print_function, absolute_import, division, unicode_literals

import six

import calendar
import datetime
import functools
import inspect
import locale
import operator
import re

from bronx.syntax.decorators import secure_getattr

#: No automatic export
__all__ = []


def today():
    """Return the date of the day, at 0 hour, 0 minute."""
    td = datetime.datetime.today()
    return Date(td.year, td.month, td.day, 0, 0)


def yesterday(base=None):
    """Return the date of yesterday (relative to today or specified ``base`` date)."""
    if not base:
        base = today()
    return base - Period(days=1)


def tomorrow(base=None):
    """Return the date of tomorrow (relative to today or specified ``base`` date)."""
    if not base:
        base = today()
    return base + Period(days=1)


def now():
    """Return the date just now, with hours, minutes, seconds and microseconds."""
    td = datetime.datetime.now()
    return Date(td.year, td.month, td.day, td.hour, td.minute, td.second, td.microsecond)


def utcnow():
    """Return the date and UTC time just now, with hours, minutes, seconds and microseconds."""
    td = datetime.datetime.utcnow()
    return Date(td.year, td.month, td.day, td.hour, td.minute, td.second, td.microsecond)


def at_second():
    """Return the date just now, with only hours, minutes and seconds."""
    td = datetime.datetime.now()
    return Date(td.year, td.month, td.day, td.hour, td.minute, td.second, 0)


def at_minute():
    """Return the date just now, with only hours and minutes."""
    td = datetime.datetime.now()
    return Date(td.year, td.month, td.day, td.hour, td.minute, 0, 0)


def at_hour():
    """Return the date just now, with only hours."""
    td = datetime.datetime.now()
    return Date(td.year, td.month, td.day, td.hour, 0, 0, 0)


def lastround(rh=1, delta=0, base=None):
    """Return the date just before ``base`` with a plain hour multiple of ``rh``."""
    if not base:
        base = now()
    if delta:
        base += Period(delta)
    return Date(base.year, base.month, base.day, base.hour - base.hour % rh, 0)


def synop(delta=0, base=None, time=None, step=6):
    """Return the date associated to the last synoptic hour."""
    synopdate = lastround(step, delta, base)
    if time is not None:
        time = Time(time)
        if time in [Time(x) for x in range(0, 24, step)]:
            dt = Period('PT{!s}H'.format(step))
            while synopdate.time() != time:
                synopdate = synopdate - dt
        else:
            raise ValueError('Not a synoptic hour: {!s}'.format(time))
    return synopdate


def stamp():
    """Return a date up to microseconds as a tuple."""
    td = datetime.datetime.now()
    return (td.year, td.month, td.day, td.hour, td.minute, td.second, td.microsecond)


def easter(year=None):
    """Return the date for easter of the given year

    >>> dates = [2013, 2014, 2015, 2016, 2017, 2018]
    >>> [easter(d).ymd for d in dates]  # doctest: +ELLIPSIS
    [...'20130331', ...'20140420', ...'20150405', ...'20160327', ...'20170416', ...'20180401']
    """
    if not year:
        year = today().year
    g = year % 19
    c = year // 100
    h = (c - c // 4 - (8 * c + 13) // 25 + 19 * g + 15) % 30
    i = h - (h // 28) * (1 - (29 // (h + 1)) * ((21 - g) // 11))
    j = (year + year // 4 + i + 2 - c + c // 4) % 7
    k = i - j
    month = 3 + (k + 40) // 44
    day = k + 28 - 31 * (month // 4)
    return Date(year, month, day)


#: The list of helper date functions
local_date_functions = dict([
    (x.__name__, x)
    for x in locals().values()
    if inspect.isfunction(x) and x.__doc__ and x.__doc__.startswith('Return the date')
])

if six.PY2:
    # noinspection PyUnboundLocalVariable
    del x


def mkisodate(datestr):
    """A crude attempt to reshape the iso8601 format."""
    ldate = list(re.sub(r' ?(UTC|GMT)$', '', datestr.strip()))
    if len(ldate) > 4 and ldate[4] != '-':
        ldate[4:4] = ['-', ]
    if len(ldate) > 7 and ldate[7] != '-':
        ldate[7:7] = ['-', ]
    if len(ldate) > 10 and ldate[10] != 'T':
        if ldate[10] in (' ', '-', 'H'):
            ldate[10] = 'T'
        else:
            ldate[10:10] = ['T', ]
    if 10 < len(ldate) <= 13:
        ldate.extend(['0', '0'])
    if len(ldate) > 13 and ldate[13] != ':':
        ldate[13:13] = [':', ]
    if len(ldate) > 16 and ldate[16] != ':':
        ldate[16:16] = [':', ]
    if len(ldate) > 13 and ldate[-1] != 'Z':
        ldate.append('Z')
    return ''.join(ldate)


def stardates():
    """Nice dump of predefined dates functions."""
    for k, v in sorted(local_date_functions.items()):
        print(k.ljust(12), v())


def guess(*args):
    """Do our best to find a :class:`Date` or :class:`Period` object compatible with ``args``."""
    for isoclass in (Date, Period):
        try:
            return isoclass(*args)
        except (ValueError, TypeError):
            continue
    raise ValueError("Cannot guess what Period or Date could be {!s}".format(args))


def daterange(start, end=None, step='P1D'):
    """Date generator.

    :param start: A :class:`Date` object or something that can be converted to one.
    :param end: A :class:`Date` object or something that can be converted to one.
    :param step: A :class:`Period` object or something that can be converted to one.

    :func:`daterange` always returns a generator object::

        >>> daterange('2017010100', '2017013100', 'P14D')  # doctest: +ELLIPSIS
        <generator object daterange at 0x...>
        >>> list(daterange('2017010100', '2017013100', 'P14D'))
        [Date(2017, 1, 1, 0, 0), Date(2017, 1, 15, 0, 0), Date(2017, 1, 29, 0, 0)]

    The *start* and *end* attributes are always sorted::

        >>> list(daterange('2017013100', '2017010100', 'P14D'))
        [Date(2017, 1, 1, 0, 0), Date(2017, 1, 15, 0, 0), Date(2017, 1, 29, 0, 0)]

    When *end* is not provided, it is assumed to be 10 days after *start*::

        >>> list(daterange('2017010100'))  # doctest: +NORMALIZE_WHITESPACE
        [Date(2017, 1, 1, 0, 0), Date(2017, 1, 2, 0, 0), Date(2017, 1, 3, 0, 0),
         Date(2017, 1, 4, 0, 0), Date(2017, 1, 5, 0, 0), Date(2017, 1, 6, 0, 0),
         Date(2017, 1, 7, 0, 0), Date(2017, 1, 8, 0, 0), Date(2017, 1, 9, 0, 0),
         Date(2017, 1, 10, 0, 0), Date(2017, 1, 11, 0, 0)]

    """
    if not isinstance(start, Date):
        start = Date(start)

    if end is None:
        end = start + Period('P10D')
    else:
        if not isinstance(end, Date):
            end = Date(end)

    start, end = sorted((start, end))

    if not isinstance(step, Period):
        step = Period(step)

    if step.total_seconds() < 0:
        step = Period(step.length)

    rollingdate = start
    while rollingdate <= end:
        yield rollingdate
        rollingdate += step


def daterangex(start, end=None, step=None, shift=None, fmt=None, prefix=None):
    """Extended date range expansion (returns a list).

    Except when ``fmt`` or ``prefix`` are specified, a list of :class:`Date`
    objects is returned.

    :func:`daterange` accepts many arguments combinations::

        >>> daterangex('2017010100', '2017013100', 'P14D')
        [Date(2017, 1, 1, 0, 0), Date(2017, 1, 15, 0, 0), Date(2017, 1, 29, 0, 0)]
        >>> daterangex('2017010100-2017013100-P14D')
        [Date(2017, 1, 1, 0, 0), Date(2017, 1, 15, 0, 0), Date(2017, 1, 29, 0, 0)]
        >>> daterangex('2017010100/2017013100/P14D')
        [Date(2017, 1, 1, 0, 0), Date(2017, 1, 15, 0, 0), Date(2017, 1, 29, 0, 0)]

    *lists*, *tuples* or comma-separated string can be provided::

        >>> daterangex(['2017010100-2017013100-P14D',
        ...             '2017060100-2017063000-P14D',
        ...             '2017122500'])  # doctest: +NORMALIZE_WHITESPACE
        [Date(2017, 1, 1, 0, 0), Date(2017, 1, 15, 0, 0), Date(2017, 1, 29, 0, 0),
         Date(2017, 6, 1, 0, 0), Date(2017, 6, 15, 0, 0), Date(2017, 6, 29, 0, 0),
         Date(2017, 12, 25, 0, 0)]
        >>> daterangex('2017010100-2017013100-P14D,' +
        ...            '2017060100-2017063000-P14D,' +
        ...            '2017122500')  # doctest: +NORMALIZE_WHITESPACE
        [Date(2017, 1, 1, 0, 0), Date(2017, 1, 15, 0, 0), Date(2017, 1, 29, 0, 0),
         Date(2017, 6, 1, 0, 0), Date(2017, 6, 15, 0, 0), Date(2017, 6, 29, 0, 0),
         Date(2017, 12, 25, 0, 0)]

    Extra features are provided such as ; applying a global *shift*::

        >>> daterangex('2017010100-2017013100-P14D,2017060100-2017063000-P14D,2017122500',
        ...            shift='P1D')  # doctest: +NORMALIZE_WHITESPACE
        [Date(2017, 1, 2, 0, 0), Date(2017, 1, 16, 0, 0), Date(2017, 1, 30, 0, 0),
         Date(2017, 6, 2, 0, 0), Date(2017, 6, 16, 0, 0), Date(2017, 6, 30, 0, 0),
         Date(2017, 12, 26, 0, 0)]

    Formatting the date as a string depending on the *fmt* function name::

        >>> daterangex('2017010100-2017013100-P14D,2017060100-2017063000-P14D,2017122500',
        ...            fmt='ymdh')  # doctest: +NORMALIZE_WHITESPACE,+ELLIPSIS
        [...'2017010100', ...'2017011500', ...'2017012900',
         ...'2017060100', ...'2017061500', ...'2017062900', ...'2017122500']

    Formatting the date as a string depending on the *fmt* format string::

        >>> daterangex('2017010100-2017013100-P14D,2017060100-2017063000-P14D,2017122500',
        ...            fmt='{0.year:04d}')  # doctest: +NORMALIZE_WHITESPACE,+ELLIPSIS
        [...'2017']

    Adding a *prefix* before the date string::

        >>> daterangex('2017010100-2017013100-P14D,2017060100-2017063000-P14D,2017122500',
        ...            fmt='ymdh', prefix='loop')  # doctest: +NORMALIZE_WHITESPACE,+ELLIPSIS
        [...'loop2017010100', ...'loop2017011500', ...'loop2017012900',
         ...'loop2017060100', ...'loop2017061500', ...'loop2017062900', ...'loop2017122500']

    """
    rangevalues = list()

    pstarts = ([six.text_type(s) for s in start]
               if isinstance(start, (list, tuple)) else six.text_type(start).split(','))

    for pstart in pstarts:
        actualrange = re.split('[-/]', pstart)
        realstart = Date(actualrange.pop(0))

        if actualrange:
            realend = Date(actualrange.pop(0))
        elif end is None:
            realend = realstart
        else:
            realend = Date(end)

        if actualrange:
            realstep = Period(actualrange.pop())
        elif step is None:
            realstep = Period('PT1H')
        else:
            realstep = Period(step)

        if shift is not None:
            realshift = Period(shift)
            realstart += realshift
            realend += realshift

        pvalues = daterange(realstart, realend, realstep)

        if pvalues:

            if fmt is not None:
                if fmt.startswith('%'):
                    fmt = '{0:' + fmt[1:] + '}'
                if '{' in fmt and '}' in fmt:
                    pvalues = [fmt.format(x, i + 1, type(x).__name__)
                               for i, x in enumerate(pvalues)]
                else:
                    pvalues = [getattr(x, fmt) for x in pvalues]
                    if callable(pvalues[0]):
                        pvalues = [x() for x in pvalues]

            if prefix is not None:
                pvalues = [prefix + six.text_type(x) for x in pvalues]

        rangevalues.extend(pvalues)

    return sorted(set(rangevalues))


def timerangex(start, end=None, step=None, shift=None, fmt=None, prefix=None):
    """Extended time range expansion (returns a list).

    Except when ``fmt`` or ``prefix`` are specified, a list of :class:`Time`
    objects is returned.

    When ``start`` is already a complex definition (as a string), ``end`` and
    ``step`` only apply as default when the sub-definition in ``start`` does not
    contain any ``end`` or ``step`` value.

    Basic examples::

        >>> timerangex(0, 12, 3)
        [Time(0, 0), Time(3, 0), Time(6, 0), Time(9, 0), Time(12, 0)]
        >>> timerangex('0-12-3')
        [Time(0, 0), Time(3, 0), Time(6, 0), Time(9, 0), Time(12, 0)]
        >>> timerangex('0-12-3', shift=24)
        [Time(24, 0), Time(27, 0), Time(30, 0), Time(33, 0), Time(36, 0)]
        >>> timerangex(0, 12, 3, shift=24)
        [Time(24, 0), Time(27, 0), Time(30, 0), Time(33, 0), Time(36, 0)]
        >>> print(', '.join(timerangex('0-12-3', shift=24, fmt='{0.hour:03d}')))
        024, 027, 030, 033, 036
        >>> print(', '.join(timerangex('0-12-3', shift=24, fmt='fmthm')))
        0024:00, 0027:00, 0030:00, 0033:00, 0036:00

    Hour/Minutes examples::

        >>> timerangex(0, 3, '0:30')
        [Time(0, 0), Time(0, 30), Time(1, 0), Time(1, 30), Time(2, 0), Time(2, 30), Time(3, 0)]
        >>> timerangex('0:00', '3:00', '0:30')
        [Time(0, 0), Time(0, 30), Time(1, 0), Time(1, 30), Time(2, 0), Time(2, 30), Time(3, 0)]
        >>> timerangex(0, 3, '0:30', shift=24)
        [Time(24, 0), Time(24, 30), Time(25, 0), Time(25, 30), Time(26, 0), Time(26, 30), Time(27, 0)]

    It also works with negative values::

        >>> timerangex(3, 0,'-0:30')
        [Time(0, 0), Time(0, 30), Time(1, 0), Time(1, 30), Time(2, 0), Time(2, 30), Time(3, 0)]
        >>> timerangex(-3, 0,'0:30')
        [Time(-3, 0), Time(-2, -30), Time(-2, 0), Time(-1, -30), Time(-1, 0), Time(0, -30), Time(0, 0)]
        >>> timerangex(-3, 0, 1)
        [Time(-3, 0), Time(-2, 0), Time(-1, 0), Time(0, 0)]

    With complex strings::

        >>> timerangex('0-12-3,18-36-6,48')  # doctest: +NORMALIZE_WHITESPACE
        [Time(0, 0), Time(3, 0), Time(6, 0), Time(9, 0), Time(12, 0), Time(18, 0),
         Time(24, 0), Time(30, 0), Time(36, 0), Time(48, 0)]
    """
    rangevalues = list()

    # Very strange case of an empty range
    if start is None:
        return list()

    pstarts = ([six.text_type(s) for s in start]
               if isinstance(start, (list, tuple)) else six.text_type(start).split(','))
    for pstart in pstarts:

        realstart = pstart
        if pstart.startswith('-'):
            realstart = '__MINUS__' + pstart[1:]

        if '--' in realstart:
            realstart = realstart.replace('--', '/__MINUS__').replace('-', '/')
        realstart = realstart.replace('-', '/')
        realstart = realstart.replace('__MINUS__', '-')
        actualrange = realstart.split('/')
        realstart = Time(actualrange[0])

        if len(actualrange) > 1:
            realend = actualrange[1]
        elif end is None:
            realend = realstart
        else:
            realend = end
        realend = Time(realend)

        if len(actualrange) > 2:
            realstep = actualrange[2]
        elif step is None:
            realstep = 1
        else:
            realstep = step
        realstep = Time(realstep)

        if shift is not None:
            realshift = Time(shift)
            realstart += realshift
            realend += realshift

        signstep = int(realstep > 0) * 2 - 1
        pvalues = [realstart, ]
        while signstep * (pvalues[-1] - realend) < 0:
            pvalues.append(pvalues[-1] + realstep)
        if signstep * (pvalues[-1] - realend) > 0:
            pvalues.pop()

        pvalues.sort()

        if fmt is not None:
            if '{' in fmt and '}' in fmt:
                pvalues = [fmt.format(x, i + 1, type(x).__name__)
                           for i, x in enumerate(pvalues)]
            else:
                pvalues = [getattr(x, fmt) for x in pvalues]
                if callable(pvalues[0]):
                    pvalues = [x() for x in pvalues]

        if prefix is not None:
            pvalues = [prefix + six.text_type(x) for x in pvalues]

        rangevalues.extend(pvalues)

    return sorted(set(rangevalues))


def timeintrangex(start, end=None, step=None, shift=None, fmt=None, prefix=None):
    """Extended range expansion  (returns a list).

    This function is built on top of :func:`timerangex`. It uses the
    :class:`TimeInt` class instead of the :class:`Time` class. Practically, if
    the 'range' is just made of basic integers (e.g. just hours), a list of
    integers is returned. If minutes are needed, a list of string formated as
    'hhhh:mm' is returned.

    When ``start`` is already a complex definition (as a string), ``end`` and
    ``step`` only apply as default when the sub-definition in ``start`` does not
    contain any ``end`` or ``step`` value.

    Basic examples::

        >>> timeintrangex(0, 12, 3)
        [0, 3, 6, 9, 12]
        >>> timeintrangex('0-12-3')
        [0, 3, 6, 9, 12]
        >>> timeintrangex('0-12-3', shift=24)
        [24, 27, 30, 33, 36]
        >>> timeintrangex(0, 12, 3, shift=24)
        [24, 27, 30, 33, 36]
        >>> print(', '.join(timeintrangex('0-12-3', shift=24, fmt='%03d')))
        024, 027, 030, 033, 036

    Hour/Minutes examples::

        >>> print(', '.join(timeintrangex(0, 3, '0:30')))
        0000:00, 0000:30, 0001:00, 0001:30, 0002:00, 0002:30, 0003:00
        >>> print(', '.join(timeintrangex('0:00', '3:00', '0:30')))
        0000:00, 0000:30, 0001:00, 0001:30, 0002:00, 0002:30, 0003:00
        >>> print(', '.join(timeintrangex(0, 3, '0:30', shift=24)))
        0024:00, 0024:30, 0025:00, 0025:30, 0026:00, 0026:30, 0027:00

    It also works with negative values::

        >>> print(', '.join(timeintrangex(3, 0,'-0:30')))
        0000:00, 0000:30, 0001:00, 0001:30, 0002:00, 0002:30, 0003:00
        >>> print(', '.join(timeintrangex(-3, 0,'0:30')))
        -0000:30, -0001:00, -0001:30, -0002:00, -0002:30, -0003:00, 0000:00
        >>> timeintrangex(-3, 0, 1)
        [-3, -2, -1, 0]

    With complex strings::

        >>> timeintrangex('0-12-3,18-36-6,48')
        [0, 3, 6, 9, 12, 18, 24, 30, 36, 48]
    """
    pstarts = ([six.text_type(s) for s in start]
               if isinstance(start, (list, tuple)) else six.text_type(start).split(','))
    auto_prefix = None
    auto_pstarts = list()
    for pstart in pstarts:
        if re.search('_', pstart):
            auto_prefix, realstart = pstart.split('_')
            auto_prefix += '_'
            auto_pstarts.append(realstart)
    if auto_prefix:
        prefix = auto_prefix
        start = auto_pstarts

    trx = timerangex(start, end=end, step=step, shift=shift)

    if all([isinstance(x, Time) for x in trx]):
        trx = [TimeInt(x) for x in trx]
        if all([x.is_int() for x in trx]):
            pvalues = [x.value for x in trx]
        else:
            pvalues = [x.str_time for x in trx]
    else:
        pvalues = trx

    if fmt is not None:
        if fmt.startswith('%'):
            fmt = '{0:' + fmt[1:] + '}'
        pvalues = [fmt.format(x, i + 1, type(x).__name__)
                   for i, x in enumerate(pvalues)]

    if prefix is not None:
        pvalues = [prefix + six.text_type(x) for x in pvalues]

    return sorted(pvalues)


class Period(datetime.timedelta):
    """
    Standard period objects, extending :class:`datetime.timedelta` features
    with iso8601 capabilities.
    """

    _my_re = re.compile(
        r'(?P<X>[+-]?P)(?P<Y>[0-9]+([,.][0-9]+)?Y)?'
        r'(?P<M>[0-9]+([,.][0-9]+)?M)?'
        r'(?P<W>[0-9]+([,.][0-9]+)?W)?'
        r'(?P<D>[0-9]+([,.][0-9]+)?D)?'
        r'((?P<T>T)(?P<h>[0-9]+([,.][0-9]+)?H)?'
        r'(?P<m>[0-9]+([,.][0-9]+)?M)?'
        r'(?P<s>[0-9]+([,.][0-9]+)?S)?)?$'
    )

    @staticmethod
    def _period_regex(s):
        return Period._my_re.match(s)

    _const_times = [
        # in a [0], there are [1] [2]
        ('m', 60, 's'),
        ('h', 60, 'm'),
        ('D', 24, 'h'),
        ('W', 7, 'D'),
        ('M', 31, 'D'),
        ('Y', 365, 'D'),
    ]

    @staticmethod
    def _adder(key, value):
        if key == 's':
            return value
        else:
            for key1, factor, key2 in Period._const_times:
                if key == key1:
                    return Period._adder(key2, factor * value)
            raise KeyError("Unknown key in Period string: %s" % key)

    @staticmethod
    def _parse(string):
        """Find out time duration that could be extracted from string argument."""
        if not isinstance(string, six.string_types):
            raise TypeError("Expected string input")
        if len(string) < 2:
            raise ValueError("Badly formed short string %s" % string)

        match = Period._period_regex(string)
        if not match:
            raise ValueError("Badly formed string %s" % string)

        values = match.groupdict()
        values.pop('T')
        sign = values.pop('X')
        if sign.startswith('-'):
            sign = -1
        else:
            sign = 1

        for k, v in values.items():
            if not v:
                values[k] = 0
            else:
                values[k] = int(v[:-1])

        secs = 0
        for k, v in values.items():
            secs += Period._adder(k, v)

        return sign * secs

    def __new__(cls, *args, **kw):
        """
        The object can be constructed from:
            * a standard :class:`datetime.timedelta` object;
            * named attributes compatible with the :class:`datetime.timedelta` class;
            * a Vortex's :class:`Time` or :class:`Period` object;
            * a string that could be reshaped as an ISO 8601 date string
              (see the description of the ISO 8601 convention at the top of
              this page);
            * one integer or float (number of seconds)
            * two integers (number of days, number of seconds)

        These four objects are identical::

            >>> Period(days=2, hours=1, seconds=30)
            Period(days=2, seconds=3630)
            >>> Period('P2DT1H30S')
            Period(days=2, seconds=3630)
            >>> Period(176430)
            Period(days=2, seconds=3630)
            >>> Period(2, 3630)
            Period(days=2, seconds=3630)

        Addition and subtraction are implemented (if the other operand is not a
        :class:`Period` object, a conversion is attempted)::

            >>> Period('PT6H') + Period('PT6H')
            Period(seconds=43200)
            >>> Period('PT6H') + 'PT6H'
            Period(seconds=43200)
            >>> Period('PT6H') + 43200
            Period(seconds=64800)

        Multiplication (by an integer) is implemented::

            >>> Period('PT6H') * 2
            Period(seconds=43200)

        Comparison operators are all available.
        """
        if kw:
            args = (datetime.timedelta(**kw),)
        if not args:
            raise ValueError("No initial value provided for Period")
        top = args[0]
        ld = list()
        if isinstance(top, datetime.timedelta):
            ld = [top.days, top.seconds, top.microseconds]
        elif isinstance(top, Time):
            ld = [0, top.hour * 3600 + top.minute * 60]
        elif len(args) < 2 and (isinstance(top, int) or isinstance(top, float)):
            ld = [0, top]
        elif isinstance(top, int) and len(args) > 1:
            ld = list(args)
        elif isinstance(top, six.string_types):
            ld = [0, Period._parse(top)]
        if not ld:
            raise ValueError("Initial Period value unknown")
        return datetime.timedelta.__new__(cls, *ld)

    def __reduce__(self):
        """Return a compatible args sequence for the Period constructor (used by :mod:`pickle`)."""
        return (self.__class__, (self.isoformat(),))

    def __deepcopy__(self, memo):
        newinstance = type(self)(self)
        memo[id(self)] = newinstance
        return newinstance

    def __len__(self):
        return self.days * 86400 + self.seconds

    def __add__(self, delta):
        """
        Add to a Period object the specified ``delta`` which could be either
        a string or a :class:`datetime.timedelta` or an ISO 6801 Period.
        """
        if not isinstance(delta, datetime.timedelta):
            delta = Period(delta)
        return Period(super(Period, self).__add__(datetime.timedelta(delta.days, delta.seconds)))

    def __sub__(self, delta):
        """
        Substract to a Period object the specified ``delta`` which could be either
        a string or a :class:`datetime.timedelta` or an ISO 6801 Period.
        """
        if not isinstance(delta, datetime.timedelta):
            delta = Period(delta)
        return Period(super(Period, self).__sub__(datetime.timedelta(delta.days, delta.seconds)))

    def __mul__(self, factor):
        """
        Add to a Period object the specified ``delta`` which could be either
        a string or a :class:`datetime.timedelta` or an ISO 6801 Period.
        """
        if not isinstance(factor, int):
            factor = int(factor)
        return Period(super(Period, self).__mul__(factor))

    def iso8601(self):
        """Plain ISO 8601 representation."""
        iso = 'P'
        sign, days, seconds = '', self.days, self.seconds
        if days < 0:
            sign = '-'
            days += 1
            seconds = 86400 - seconds
        if days:
            iso += '{!s}D'.format(abs(days))
        return sign + iso + 'T{!s}S'.format(seconds)

    def isoformat(self):
        """Return default ISO representation."""
        return self.iso8601()

    def export_dict(self):
        """Return the month and year as a tuple."""
        return (self.days, self.seconds)

    @property
    def length(self):
        """Absolute length in seconds."""
        return abs(int(self.total_seconds()))

    def time(self):
        """Return a :class:`Time` object."""
        return Time(0, int(self.total_seconds()) // 60)

    @property
    def hms(self):
        """Nicely formatted HH:MM:SS string."""
        hours, mins = divmod(self.length, 3600)
        mins, seconds = divmod(mins, 60)
        return '{0:02d}:{1:02d}:{2:02d}'.format(hours, mins, seconds)

    @property
    def hmscompact(self):
        """Compact HHMMSS string."""
        return self.hms.replace(':', '')

    def __str__(self):
        return self.isoformat()

    def __repr__(self):
        """Python changes the repr for timedelta between 3.5 and 3.7

        This forces the 3.7 way for all versions.
        """
        args = []
        if self.days:
            args.append("days=%d" % self.days)
        if self.seconds:
            args.append("seconds=%d" % self.seconds)
        if self.microseconds:
            args.append("microseconds=%d" % self.microseconds)
        if not args:
            args.append('0')
        return "%s(%s)" % (self.__class__.__name__, ', '.join(args))


class _GetattrCalculatorMixin(object):
    """This Mixin class adds the capability to do computations using fake methods.

    This can be useful during the footprint's replacement process.
    """

    _getattr_re = re.compile(r'^(?P<basics>(?:(?:add|sub)[^_]+_?)+)(?:_(?P<fmt>[^_]+))?(?<!_)$')
    _getattr_basic_re = re.compile(r'^(?P<op>add|sub)(?P<operand>[^_]+)')
    _getattr_proxyclass = None

    def _basic_calculator_proxy(self, name):
        """Return something that may (or not) create a _getattr_proxyclass object.

        The sign of the _getattr_proxyclass object depends on the chosen operation.
        If _getattr_proxyclass is None, the current object's class is used.
        """
        # The match should always succeed because _getattr_re was called before
        dmatch = self._getattr_basic_re.match(name).groupdict()
        factor = dict(add=1, sub=-1)[dmatch['op']]
        if self._getattr_proxyclass is None:
            proxyclass = self.__class__
        else:
            proxyclass = self._getattr_proxyclass
        # Determine if the operand is a valid period (like PT6H)
        try:
            p = proxyclass(dmatch['operand'])
        except ValueError:
            p = None
        # The easy case: just return the appropriate object
        if p is not None:
            return factor * p
        # Returns a function that looks up into guess and extras (given by footprints)
        else:
            def _calculator_op_proxy(guess, extra):
                t = guess.get(dmatch['operand'], extra.get(dmatch['operand'], None))
                if t is None:
                    raise KeyError("'{}' was not found in guess nor in extra.".
                                   format(dmatch['operand']))
                return factor * proxyclass(t)
            return _calculator_op_proxy

    @secure_getattr
    def __getattr__(self, name):
        """Proxy to additions and subtractions (used in footprint's replacement).

        :example:
            * self.addPT6H is equivalent to (self + 'PT6H')
            * self.addPT6H_ymdh is equivalent to (self + 'PT6H').ymdh
            * self.addterm_ymdh is equivalent to (self + [term]).ymdh
            * It is possible to combine several add and sub, like in:
              self.addterm_subPT3H_ymdh
        """
        match = self._getattr_re.match(name)
        if match is not None:
            dmatch = match.groupdict()
            basics = dmatch['basics'].rstrip('_').split('_')
            fmt = dmatch['fmt']
            proxies = [self._basic_calculator_proxy(basic) for basic in basics]
            fancy = any([callable(proxy) for proxy in proxies])
            if fancy:
                def _combi_proxy(guess, extra):
                    newobj = self
                    for proxy in proxies:
                        newobj += proxy(guess, extra) if callable(proxy) else proxy
                    return newobj if fmt is None else getattr(newobj, fmt)
                return _combi_proxy
            else:
                newobj = self + functools.reduce(operator.add, proxies)
                return newobj if fmt is None else getattr(newobj, fmt)
        else:
            raise AttributeError("'{}' object has no attribute '{}'".format(self.__class__.__name__,
                                                                            name))


class Date(datetime.datetime, _GetattrCalculatorMixin):
    """
    Standard date objects, extending :class:`datetime.datetime` features with
    iso8601 capabilities.
    """

    _origin = datetime.datetime(1970, 1, 1, 0, 0, 0)
    _getattr_proxyclass = Period

    def __new__(cls, *args, **kw):
        if kw and not args:
            args = (datetime.datetime(**kw),)
        if not args:
            raise ValueError("No initial value provided for Date")
        top = args[0]
        deltas = []
        ld = list()
        if isinstance(top, six.string_types) and top in local_date_functions:
            try:
                top = local_date_functions[top](**kw)
                kw = dict()
            except (ValueError, TypeError):
                pass
        if isinstance(top, datetime.datetime):
            ld = [top.year, top.month, top.day, top.hour, top.minute, top.second]
        elif isinstance(top, tuple) or isinstance(top, list):
            ld = list(top)
        elif isinstance(top, float):
            top = Date._origin + datetime.timedelta(0, top)
            ld = [top.year, top.month, top.day, top.hour, top.minute, top.second]
        elif isinstance(top, six.string_types):
            s_top = top.split('/')
            top = s_top[0]
            top = re.sub('^YYYY', six.text_type(max(0, int(kw.pop('year', today().year)))), top.upper())
            deltas = s_top[1:]
            ld = [int(x) for x in re.split('[-:HTZ]+', mkisodate(top)) if re.match(r'\d+$', x)]
        else:
            ld = [int(x) for x in args
                  if isinstance(x, (int, float)) or (isinstance(x, six.string_types) and re.match(r'\d+$', x))]
        if not ld:
            raise ValueError("Initial Date value unknown")
        newdate = datetime.datetime.__new__(cls, *ld)
        if deltas:
            newdate += sum([Period(d) for d in deltas], Period(0))
        return newdate

    def __init__(self, *args, **kw):  # @UnusedVariable
        """
        The object can be constructed from:
            * a standard :class:`datetime.datetime` object;
            * named attributes compatible with the :class:`datetime.datetime` class;
            * a Vortex's :class:`Date` object;
            * a tuple containing at least (year, month, day) values (optionally hours);
            * a string that could be reshaped as an ISO 8601 date string.
            * a string with one or more time deltas (e.g. 201509010600/-PT1H/-PT2H)
            * the name of one of the helper date functions (see below)
            * a float representing a number of seconds since the epoch time

        Here are a few equivalent examples::

            Date(2017, 1, 1, 12, 0)
            >>> Date(2017, 1, 1, 12)
            Date(2017, 1, 1, 12, 0)
            >>> Date([2017, 1, 1, 12])
            Date(2017, 1, 1, 12, 0)
            >>> Date('2017-01-01T12:00')
            Date(2017, 1, 1, 12, 0)
            >>> Date('2017010112')
            Date(2017, 1, 1, 12, 0)

        Helper functions can be used::

            >>> Date('now') # doctest: +SKIP
            Date(2017, 8, 21, 19, 33, 46)
            >>> Date('easter') # doctest: +SKIP
            Date(2017, 4, 16, 0, 0)

        Let's do some calculations on the fly::

            >>> Date('20170101/PT12H')
            Date(2017, 1, 1, 12, 0)
            >>> Date('2017010212/-P1D')
            Date(2017, 1, 1, 12, 0)
            >>> Date('2017010200/-P1D/PT12H')
            Date(2017, 1, 1, 12, 0)

        The addition is defined (if the operand is not a :class:`Period` object,
        a conversion is attempted)::

            >>> Date('2017010100') + Period('PT12H')
            Date(2017, 1, 1, 12, 0)
            >>> Date('2017010100') + Time(12, 00)
            Date(2017, 1, 1, 12, 0)
            >>> Date('2017010100') + 'PT12H'
            Date(2017, 1, 1, 12, 0)
            >>> Date('2017010100') + 43200
            Date(2017, 1, 1, 12, 0)

        The subtraction is also defined (the operand can be either a :class:`Period`
        object or a :class:`Date` object)::

            >>> Date('2017010100') - Period('PT12H')
            Date(2016, 12, 31, 12, 0)
            >>> Date('2017010100') - 'PT12H'
            Date(2016, 12, 31, 12, 0)
            >>> Date('2017010100') - Date('2017010212')
            Period(days=-2, seconds=43200)
            >>> Date('2017010100') - '2017010212'
            Period(days=-2, seconds=43200)

        Comparison operators are all available.

        The :class:`Date` also have the ability to generate dynamic properties
        on the fly (because it inherits the :class:`_GetattrCalculatorMixin`
        mix in). Such dynamic properties can be very useful when used with
        :mod:`footprints` package substitution mechanism. It allows to do
        calculations on the fly::

            >>> date = Date('20170416')
            >>> date
            Date(2017, 4, 16, 0, 0)
            >>> date.addPT6H
            Date(2017, 4, 16, 6, 0)
            >>> date.subP1D
            Date(2017, 4, 15, 0, 0)
            >>> date.subP1D_addPT6H
            Date(2017, 4, 15, 6, 0)
            >>> print(date.subP1D_ymdh)
            2017041500

        The following will also work::

            >>> print(date.addterm_ymdh(dict(term=Time(6, 0)), dict()))
            2017041606

        In such a documentation, this looks horrible. However, in the context of
        :mod:`footprints` substitutions, it works like a charm (to compute the
        validity date of a a resource that defines a *term*).
        """
        super(Date, self).__init__()
        delta_o = self - Date._origin
        self._epoch = delta_o.days * 86400 + delta_o.seconds

    def __reduce__(self):
        """Return a compatible args sequence for the Date constructor (used by :mod:`pickle`)."""
        return (self.__class__, (self.iso8601(),))

    def __deepcopy__(self, memo):
        newinstance = type(self)(self)
        memo[id(self)] = newinstance
        return newinstance

    def __hash__(self):
        """Force the object to be hashable (needed with Python3)."""
        return datetime.datetime.__hash__(self)

    @property
    def origin(self):
        """Origin date... far far ago at the very beginning of the 70's."""
        return Date(Date._origin)

    @property
    def epoch(self):
        """Seconds since the beginning of epoch... the first of january, 1970."""
        return self._epoch

    def iso8601(self):
        """Plain ISO 8601 representation."""
        return self.isoformat() + 'Z'

    def as_datetime(self):
        """Silly enough, but could be usefull to retrieve a raw ``datetime.datetime`` object."""
        return datetime.datetime(self.year, self.month, self.day, self.hour, self.minute, self.second, self.microsecond)

    def __str__(self):
        """Default string representation is iso8601."""
        return self.iso8601()

    def is_synoptic(self):
        """True if the current hour is a synoptic one."""
        return self.hour in (0, 6, 12, 18)

    def strftime(self, *kargs, **kwargs):
        rstr = super(Date, self).strftime(*kargs, **kwargs)
        if six.PY2:
            renc = (locale.getlocale(locale.LC_TIME)[1] or
                    locale.getlocale()[1] or 'utf-8')
            return rstr.decode(encoding=renc)
        else:
            return rstr

    @property
    def julian(self):
        """Returns Julian day."""
        return self.strftime('%j')

    @property
    def ymd(self):
        """YYYYMMDD formated string."""
        return self.strftime('%Y%m%d')

    @property
    def yymd(self):
        """YYMMDD formated string."""
        return self.strftime('%y%m%d')

    @property
    def y(self):
        """YYYYMMDDHH formated string."""
        return self.strftime('%Y')

    @property
    def ymdh(self):
        """YYYYMMDDHH formated string."""
        return self.strftime('%Y%m%d%H')

    @property
    def yymdh(self):
        """YYMMDDHH formated string."""
        return self.strftime('%y%m%d%H')

    @property
    def ymd6h(self):
        """YYMMDDHH formated string with HH=6:00."""
        return self.replace(hour=6).strftime('%Y%m%d%H')

    @property
    def ymdhm(self):
        """YYYYMMDDHHMM formated string."""
        return self.strftime('%Y%m%d%H%M')

    @property
    def ymdhms(self):
        """YYYYMMDDHHMMSS formated string."""
        return self.strftime('%Y%m%d%H%M%S')

    @property
    def mmddhh(self):
        return self.strftime('%m%d%H')

    def stamp(self):
        """Compact concatenation up to microseconds."""
        return self.ymdhms + '{0:06d}'.format(self.microsecond)

    @property
    def mm(self):
        """MM (month) formated string."""
        return self.strftime('%m')

    @property
    def hm(self):
        """HHMM formated string."""
        return self.strftime('%H%M')

    @property
    def dd(self):
        """DD formated string."""
        return self.strftime('%d')

    @property
    def hh(self):
        """HH formated string."""
        return self.strftime('%H')

    @property
    def h(self):
        """H formated string."""
        return self.strftime('%-H')

    def compact(self):
        """Compact concatenation of date values, up to the second (YYYYMMDDHHSS)."""
        return self.ymdhms

    def vortex(self, cutoff='P'):
        """Semi-compact representation for vortex paths."""
        return self.strftime('%Y%m%dT%H%M') + six.text_type(cutoff)[0].upper()

    @property
    def stdvortex(self):
        """Semi-compact representation for vortex paths (without cutoff)."""
        return self.strftime('%Y%m%dT%H%M')

    def reallynice(self):
        """Nice and verbose string representation."""
        return self.strftime("%A %d. %B %Y, at %H:%M:%S")

    def export_dict(self):
        """String representation for dict or shell variable."""
        return self.ymdhm

    def __add__(self, delta):
        """
        Add to a Date object the specified ``delta`` which could be either
        a string or a :class:`datetime.timedelta` or an ISO 6801 Period.
        """
        if not isinstance(delta, datetime.timedelta):
            delta = Period(delta)
        return Date(super(Date, self).__add__(datetime.timedelta(delta.days, delta.seconds)))

    def __radd__(self, delta):
        """Reversed add."""
        return self.__add__(delta)

    def __sub__(self, delta):
        """
        Subtract to a Date object the specified ``delta`` which could be either
        a string or a :class:`datetime.timedelta` or an ISO 6801 Period.
        """
        if not isinstance(delta, datetime.datetime) and not isinstance(delta, datetime.timedelta):
            delta = guess(delta)
        substract = super(Date, self).__sub__(delta)
        if isinstance(delta, datetime.datetime):
            return Period(substract)
        else:
            return Date(substract)

    def __rsub__(self, delta):
        """Reversed-substract (based on the __sub__ method)."""
        if not isinstance(delta, self.__class__):
            try:
                delta = self.__class__(delta)
            except (ValueError, TypeError):
                raise ValueError("'{:s} cannot be convert to a proper Date object".format(delta))
        return delta - self

    def __eq__(self, other):
        """Compare two Date values or a Date and a datetime or string value."""
        try:
            other = self.__class__(other).compact()
        except (ValueError, TypeError):
            pass
        finally:
            return self.compact() == '{0:<08s}'.format(six.text_type(other))

    def __ne__(self, other):
        return not self.__eq__(other)

    def __lt__(self, other):
        """Compare two Date values or a Date and a datetime or string value."""
        try:
            other = self.__class__(other).compact()
        except (ValueError, TypeError):
            pass
        finally:
            return self.compact() < '{0:<08s}'.format(six.text_type(other))

    def __le__(self, other):
        return self == other or self < other

    def __gt__(self, other):
        """Compare two Date values or a Date and a datetime or string value."""
        try:
            other = self.__class__(other).compact()
        except (ValueError, TypeError):
            pass
        finally:
            return self.compact() > '{0:<08s}'.format(six.text_type(other))

    def __ge__(self, other):
        return self == other or self > other

    def replace(self, **kw):
        """Possible arguments: year, month, day, hour, minute."""
        for datekey in ('year', 'month', 'day', 'hour', 'minute'):
            kw.setdefault(datekey, getattr(self, datekey))
        return Date(datetime.datetime(**kw))

    @property
    def cnes_origin(self):
        return datetime.datetime(1950, 1, 1).toordinal()

    def to_cnesjulian(self, date=None):
        """
        Convert current Date() object, or arbitrary date, to CNES julian calendar

        >>> d = Date('20111026')
        >>> d.to_cnesjulian()
        22578
        >>> d.to_cnesjulian(date=[2011, 10, 27])
        22579
        """
        if not date:
            date = datetime.datetime(self.year, self.month, self.day)
        if isinstance(date, list):
            date = datetime.datetime(*date)
        return date.toordinal() - self.cnes_origin

    def from_cnesjulian(self, jdays=None):
        """

        >>> d = Date('20111025')
        >>> d.from_cnesjulian()
        Date(2011, 10, 25, 0, 0)
        >>> d.from_cnesjulian(22578)
        Date(2011, 10, 26, 0, 0)
        """
        if jdays is None:
            jdays = self.toordinal() - self.cnes_origin
        return Date(self.fromordinal(jdays + self.cnes_origin))

    def isleap(self, year=None):
        """Return whether the current of specified year is a leap year."""
        if year is None:
            year = self.year
        return calendar.isleap(year)

    def monthrange(self, year=None, month=None):
        """Return the number of days in the current of specified year-month couple."""
        if year is None:
            year = self.year
        if month is None:
            month = self.month
        return calendar.monthrange(year, month)[1]

    def time(self):
        """Return a :class:`Time` object built from the present object hours and minutes."""
        return Time(self.hour, self.minute)

    def bounds(self):
        """Return first and last day of the current month."""
        return (
            self.replace(day=1, hour=0, minute=0),
            self.replace(day=self.monthrange(), hour=23, minute=59)
        )

    @property
    def outbound(self):
        """Return the closest day out of this month."""
        a, b = self.bounds()
        if self - a > b - self:
            out = b + 'P1D'
        else:
            out = a - 'P1D'
        return out.ymd

    @property
    def midcross(self):
        """Return the closest day out of this month."""
        a, b = self.bounds()
        if self.day > 15:
            out = b + 'P1D'
        else:
            out = a - 'P1D'
        return out.ymd

    @property
    def nivologyseason(self):
        """Return the nivology season of a current date"""
        if self.month < 8:
            season_begin = datetime.datetime(self.year - 1, 8, 1)
            season_end = datetime.datetime(self.year, 7, 31)
        else:
            season_begin = datetime.datetime(self.year, 8, 1)
            season_end = datetime.datetime(self.year + 1, 7, 31)

        return season_begin.strftime('%y') + season_end.strftime('%y')


class Time(_GetattrCalculatorMixin):
    """Basic object to handle hh:mm information.

    Extended arithmetic is supported.
    """

    def __init__(self, *args, **kw):
        """
        The object can be constructed from:
            * a standard :class:`datetime.time` object;
            * a :class:`Time` object
            * a :class:`Period` object
            * named attributes compatible with the :class:`datetime.time` class;
            * a string that could be reshaped as a :class:`Period` object.
            * a string that could be reshaped as an ISO 8601 time string.
            * two integers (hours, minutes)
            * a tuple containing (hour, minute) values;

         Here are a few equivalent examples::

            >>> Time('18:05')
            Time(18, 5)
            >>> Time('18h05')
            Time(18, 5)
            >>> Time('18-05')
            Time(18, 5)
            >>> Time('T18:05Z')
            Time(18, 5)
            >>> Time(18, 5)
            Time(18, 5)
            >>> Time((18, 5))
            Time(18, 5)
            >>> Time('PT18H05M')
            Time(18, 5)
            >>> Time(hour=18, minute=5)
            Time(18, 5)

        When constructed from strings, the :class:`Time` object can handle
        negative values::

            >>> Time('-18:05')
            Time(-18, -5)
            >>> Time('-PT18H05M')
            Time(-18, -5)

        The addition and subtraction is defined (if the operand is not a
        :class:`Time` object, a conversion is attempted)::

            >>> Time('12:00') + Time('06:30')
            Time(18, 30)
            >>> Time('12:00') + '06:30'
            Time(18, 30)
            >>> Time('12:00') + 'PT06H30M'
            Time(18, 30)
            >>> Time('12:00') - 'PT06H30M'
            Time(5, 30)

        Comparison operators are all available.

        The :class:`Time` also have the ability to generate dynamic properties
        on the fly (because it inherits the :class:`_GetattrCalculatorMixin`
        mix in). Such dynamic properties can be very useful when used with
        :mod:`footprints` package substitution mechanism.

        It allows to do calculations on the fly::

            >>> atime = Time('12:00')
            >>> atime.add06h30
            Time(18, 30)
            >>> atime.addPT6H30M
            Time(18, 30)
            >>> print(atime.addPT6H30M_fmthm)
            0018:30
            >>> print(atime.subPT18H30M_fmthm)
            -0006:30

        """
        if kw:
            kw.setdefault('hour', 0)
            kw.setdefault('minute', 0)
            args = (datetime.time(**kw),)
        if not args:
            raise ValueError("No initial value provided for Time")
        top = args[0]
        ld = list()
        self._hour, self._minute = None, None
        if isinstance(top, tuple) or isinstance(top, list):
            zz = Time(*top)
            self._hour, self._minute = zz.hour, zz.minute
        elif isinstance(top, datetime.time) or isinstance(top, Time):
            self._hour, self._minute = top.hour, top.minute
        elif isinstance(top, TimeInt):
            self._hour, self._minute = top.ti, top.tm
        elif isinstance(top, Period):
            newtime = top.time()
            self._hour, self._minute = newtime.hour, newtime.minute
        elif isinstance(top, float):
            self._hour, self._minute = int(top), int((top - int(top)) * 60)
        elif isinstance(top, six.string_types):
            if re.match(r'^[+-]?P', top):  # This looks like a Period string...
                newtime = Period(top).time()
                self._hour, self._minute = newtime.hour, newtime.minute
            else:
                top = re.sub(r'^(-?):(\d+)$', r'\g<1>0:\g<2>', top)
                thesign = -2 * int(bool(re.match(r'^-', top))) + 1
                ld = [thesign * int(x) for x in re.split('[-:hHTZ]+', top) if re.match(r'\d+$', x)]
        else:
            ld = [int(x) for x in args
                  if (type(x) in (int, float) or
                      (isinstance(x, six.string_types) and re.match(r'\d+$', x)))]
        if ld:
            if len(ld) < 2:
                ld.append(0)
            self._hour, self._minute = ld[0], ld[1]
        if self._hour is None or self._minute is None:
            raise ValueError("No way to build a Time value")
        # If minute > 60 do something...
        if abs(self._minute) >= 60:
            thesign = int(self._minute > 0) * 2 - 1
            while abs(self._minute) >= 60:
                self._hour += thesign
                self._minute -= thesign * 60

    @property
    def hour(self):
        """The number of hours"""
        return self._hour

    @property
    def minute(self):
        """The number of minutes"""
        return self._minute

    def __deepcopy__(self, memo):
        """Clone of the current :class:`Time` object."""
        newinstance = Time(self.hour, self.minute)
        memo[id(self)] = newinstance
        return newinstance

    def __repr__(self):
        """Standard hour-minute representation."""
        return 'Time({0:d}, {1:d})'.format(self.hour, self.minute)

    def export_dict(self):
        """String representation for dict or shell variable."""
        return self.__str__()

    def _formatted_str(self, fmt):
        thesign = '-' if int(self) < 0 else ''
        return fmt.format(thesign, abs(self.hour), abs(self.minute))

    def __str__(self):
        """Standard hour-minute string (like HH:MM)."""
        return self._formatted_str('{0:s}{1:02d}:{2:02d}')

    def __int__(self):
        """Convert to `int`, ie: returns hours * 60 + minutes."""
        return self._hour * 60 + self._minute

    def __hash__(self):
        """Return a hashkey."""
        return self.__int__()

    def __comparison_prepare(self, other):
        try:
            other = self.__class__(other)
        except (ValueError, TypeError):
            pass
        return other

    def __eq__(self, other):
        other = self.__comparison_prepare(other)
        try:
            return int(self) == int(other)
        except (ValueError, TypeError):
            return False

    def __ne__(self, other):
        other = self.__comparison_prepare(other)
        try:
            return int(self) != int(other)
        except (ValueError, TypeError):
            return True

    def __gt__(self, other):
        other = self.__comparison_prepare(other)
        return int(self) > int(other)

    def __ge__(self, other):
        other = self.__comparison_prepare(other)
        return int(self) >= int(other)

    def __lt__(self, other):
        other = self.__comparison_prepare(other)
        return int(self) < int(other)

    def __le__(self, other):
        other = self.__comparison_prepare(other)
        return int(self) <= int(other)

    def __add__(self, delta):
        """
        Add to a Time object the specified ``delta`` which could be either
        a string or a :class:`Period` object or an ISO 6801 Period.
        """
        delta = self.__class__(delta)
        me = int(self) + int(delta)
        return self.__class__(0, me)

    def __radd__(self, delta):
        """Reversed add."""
        return self.__add__(delta)

    def __sub__(self, delta):
        """
        Subtract to a Time object the specified ``delta`` which could be either
        a string or a :class:`Period` object or an ISO 6801 Period.
        """
        delta = self.__class__(delta)
        me = int(self) - int(delta)
        return self.__class__(0, me)

    def __rsub__(self, delta):
        """Reversed subtract."""
        delta = self.__class__(delta)
        me = int(delta) - int(self)
        return self.__class__(0, me)

    def __mul__(self, other):
        # The result might be truncated since second/microseconds are not suported
        other = self.__class__(other)
        me = (int(self) * int(other)) // 60
        return self.__class__(0, me)

    def __rmul__(self, other):
        return self.__mul__(other)

    @property
    def dhm(self):
        """Return a tuple with the number of (days, hours, minutes)."""
        totalminutes = int(self)
        sign = -1 if totalminutes < 0 else 1
        totalminutes = abs(totalminutes)
        days = totalminutes // 1440
        hours = (totalminutes % 1440) // 60
        minutes = (totalminutes % 60)
        return (sign * days, sign * hours, sign * minutes)

    @property
    def fmtdhm(self):
        """DDHHMM formated string."""
        (days, hours, minutes) = self.dhm
        sign = '-' if int(self) < 0 else ''
        return '{0:s}{1:02d}{2:02d}{3:02d}'.format(sign, abs(days), abs(hours), abs(minutes))

    @property
    def fmth(self):
        """HHHH formated string."""
        return self._formatted_str('{0:s}{1:04d}')

    @property
    def fmthour(self):
        """HHHH formated string."""
        return self.fmth

    @property
    def fmthm(self):
        """HHHH:MM formated string."""
        return self._formatted_str('{0:s}{1:04d}:{2:02d}')

    @property
    def fmthhmm(self):
        """HH:MM formated string."""
        return self._formatted_str('{0:s}{1:02d}{2:02d}')

    @property
    def fmtraw(self):
        """HHHHMM formated string."""
        return self._formatted_str('{0:s}{1:04d}{2:02d}')

    @property
    def fmtraw2(self):
        """HHHHHHHHMM formated string."""
        return self._formatted_str('{0:s}{1:08d}{2:02d}')

    @property
    def notnull(self):
        if self.hour != 0 or self.minute != 0:
            return 1
        return 0

    def isoformat(self):
        """Almost ISO representation (HH:MM)."""
        return six.text_type(self)

    def iso8601(self):
        """Plain ISO 8601 representation."""
        return 'T' + self.isoformat() + 'Z'

    def nice(self, t):
        """Kept for backward compatibility. Plesae do not use."""
        return '{0:04d}'.format(t)


def _delegate_op_to_timeobj(proxymethods):

    def delegate(cls):

        def make_proxy_mtd(real_pmethod):
            def a_pmethod(self, other):
                rv = getattr(self._timeobj, real_pmethod)(other)
                if isinstance(rv, Time):
                    rv = TimeInt(rv)
                return rv
            a_pmethod.__name__ = real_pmethod
            return a_pmethod

        for pmethod in proxymethods:
            real_pmethod = str('__' + pmethod + '__')
            setattr(cls, real_pmethod, make_proxy_mtd(real_pmethod))

        return cls

    return delegate


@_delegate_op_to_timeobj(['eq', 'ne', 'lt', 'le', 'gt', 'ge',
                          'add', 'radd', 'sub', 'rsub', 'mul', 'rmul'])
class TimeInt(int):
    """Extended integer able to handle simple integers or integer plus minutes.

    In the later case, the first integer is not limited to 24
    """

    def __new__(cls, ti, tm=None):
        if tm is None:
            if isinstance(ti, Time):
                timeobj = ti
            else:
                timeobj = Time(ti)
        else:
            timeobj = Time(ti, tm)

        obj = int.__new__(cls, timeobj.hour)
        obj._timeobj = timeobj
        obj._int = timeobj.minute == 0
        return obj

    @property
    def ti(self):
        return self._timeobj.hour

    @property
    def tm(self):
        return self._timeobj.minute

    def is_int(self):
        return self.tm == 0

    @property
    def str_time(self):
        return self._timeobj.fmthm

    def __str__(self):
        if self.is_int():
            return six.text_type(self.ti)
        else:
            return self.str_time

    def __hash__(self):
        return hash(self._timeobj)

    @property
    def realtype(self):
        return 'int' if self.is_int() else 'time'

    @property
    def value(self):
        return self.ti if self.is_int() else six.text_type(self)


@functools.total_ordering
class Month(object):
    """
    Basic class for handling a month number, according to an explicit or
    implicit year.
    """

    def __init__(self, *args, **kw):
        """
        The object can be constructed from:
            * a standard :class:`datetime.datetime` object or a :class:`Date` object;
            * a :class:`Month` object;
            * named attributes compatible with the :class:`datetime.datetime` class;
            * a unique integer representing the Month number (in such a case,
              the year is assumed to be the current year);
            * a tuple containing two integer representing (month, year) values;
            * any string supported by the :class:`Date` object;

        Here are a few equivalent examples::

            >>> Month(year=2016, month=1, day=1)
            Month(01, year=2016)
            >>> Month('2016010100')
            Month(01, year=2016)
            >>> Month(1, 2016)
            Month(01, year=2016)

        The *year* may not be specified (in such a case the current year is used)::

            >>> Month(1) # doctest: +SKIP
            Month(01, year=2017)

        When initialising the object with a string, some nice helpers are provided::

            >>> Month('2016010100:prev')
            Month(12, year=2015)
            >>> Month('2016010100:next')
            Month(02, year=2016)
            >>> Month('2016011500:closest')
            Month(12, year=2015)
            >>> Month('2016011600:closest')
            Month(02, year=2016)

        Concerted to an integer, this object returns the month number::

            >>> int(Month('2016010100'))
            1

        The addition and subtraction is defined (the operand can be an integer,
        a :class:`Period` object, or a string that can be converted to a
        :class:`Period` object)::

            >>> Month('2016010100') + 1
            Month(02, year=2016)
            >>> Month('2016010100') + Period('P2M')
            Month(03, year=2016)
            >>> Month('2016010100') + 'P2M'
            Month(03, year=2016)
            >>> Month('2016010100') - 'P2M'
            Month(10, year=2015)

        Some kind of comparisons are possible::

            >>> Month('2016010100') == 1
            True
            >>> Month('2016010100') < 2
            True
            >>> Month('2016010100') > 1
            False
            >>> Month('2016010100') > Month('2015100100')
            True

        """
        delta = kw.pop('delta', 0)
        try:
            args = (datetime.datetime(**kw),)
        except (ValueError, TypeError):
            pass
        if not args:
            raise ValueError("No initial value provided for Month")
        args = list(args)
        top = args[0]
        self._month = None
        self._year = max(0, int(kw.pop('year', today().year)))
        if isinstance(top, datetime.datetime) or isinstance(top, Month):
            self._month, self._year = top.month, top.year
        elif isinstance(top, int) and 0 < top < 13:
            self._month = top
            if len(args) == 2:
                self._year = int(args[1])
        else:
            # Try to generate a Date object
            mmod = None
            if isinstance(top, six.string_types):
                mmod = re.search(':(next|prev|closest)$', top)
                if mmod:
                    args[0] = re.sub(':(?:next|prev|closest)$', '', top)
            try:
                tmpdate = Date(*args)
            except (ValueError, TypeError):
                try:
                    self._month = int(args[0])
                    if not (1 <= self._month <= 12):
                        raise ValueError('Not a valid month: {}'.format(self._month))
                    tmpday = 1
                except (ValueError, TypeError):
                    raise ValueError('Could not create a Month from values provided {!s}'.format(args))
            else:
                self._month, self._year, tmpday = tmpdate.month, tmpdate.year, tmpdate.day
            # Process the modifiers
            if mmod:
                if mmod.group(1) == 'next':
                    delta = 1
                elif mmod.group(1) == 'prev':
                    delta = -1
                elif mmod.group(1) == 'closest':
                    if tmpday > 15:
                        delta = 1
                    else:
                        delta = -1
            # If present, the second argument is the delta (it overrides the modifiers)
            if len(args) == 2:
                delta = args.pop()

        if delta:
            mtmp = self + delta
            self._month, self._year = mtmp.month, mtmp.year

    @property
    def year(self):
        """The year number."""
        return self._year

    @property
    def month(self):
        """The month number."""
        return self._month

    @property
    def fmtym(self):
        """YYYY-MM formated string."""
        return '{0:04d}-{1:02d}'.format(self._year, self._month)

    @property
    def fmtraw(self):
        """YYYYMM formated string."""
        return '{0:04d}{1:02d}'.format(self._year, self._month)

    def export_dict(self):
        """Return the month and year as a tuple."""
        return (self.month, self.year)

    def nextmonth(self):
        """Return the month after the current one."""
        return self + 1

    def prevmonth(self):
        """Return the month before the current one."""
        return self - 1

    def __hash__(self):
        return hash((self._year, self._month))

    def __str__(self):
        """Return a two digit value of the current month int value."""
        return '{0:02d}'.format(self._month)

    def __repr__(self):
        """Return a formated id of the current month."""
        return '{0:s}({1:02d}, year={2:d})'.format(self.__class__.__name__, self._month, self._year)

    def __add__(self, delta):
        """
        Add to a Month object the specified ``delta`` which could be either
        an integer (number of months), a :class:`Period` object, or a string
        that can be converted to a :class:`Period` object.
        """
        if isinstance(delta, int):
            if delta < 0:
                incr = -1
                delta = abs(delta)
            else:
                incr = 1
            year, month = self._year, self._month
            while delta:
                month += incr
                if month > 12:
                    year += 1
                    month = 1
                if month < 1:
                    year -= 1
                    month = 12
                delta -= 1
            if self._year == 0:
                year = 0
            return Month(month, year)
        elif not isinstance(delta, datetime.timedelta):
            delta = Period(delta)
        return Month(Date(self._year, self._month, 14) + delta)

    def __radd__(self, delta):
        """Commutative add."""
        return self.__add__(delta)

    def __sub__(self, delta):
        """
        Subtract to a Month object the specified ``delta`` which could be either
        an integer (number of months), a :class:`Period` object , or a string
        that can be converted to a :class:`Period` object
        """
        if isinstance(delta, int):
            return self.__add__(-1 * delta)
        elif not isinstance(delta, datetime.timedelta):
            delta = Period(delta)
        return Month(Date(self._year, self._month, 1) - delta)

    def __int__(self):
        return self._month

    def __eq__(self, other):
        try:
            if isinstance(other, int) or (isinstance(other, six.string_types) and
                                          len(other.lstrip('0')) < 3):
                rc = self.month == Month(int(other), self.year).month
            else:
                if isinstance(other, tuple) or isinstance(other, list):
                    mtest = Month(*other)
                else:
                    mtest = Month(other)
                if self.year * mtest.year == 0:
                    rc = self.month == mtest.month
                else:
                    rc = self.fmtym == mtest.fmtym
        except (ValueError, TypeError):
            rc = False
        finally:
            return rc

    def __gt__(self, other):
        if isinstance(other, int) or (isinstance(other, six.string_types) and
                                      len(other.lstrip('0')) < 3):
            rc = self.month > Month(int(other), self.year).month
        else:
            if isinstance(other, tuple) or isinstance(other, list):
                mtest = Month(*other)
            else:
                mtest = Month(other)
            if self.year * mtest.year == 0:
                rc = self.month > mtest.month
            else:
                rc = self.fmtym > mtest.fmtym
        return rc


if __name__ == '__main__':
    import doctest
    doctest.testmod()
