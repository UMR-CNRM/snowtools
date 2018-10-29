#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""
TODO: Module documentation

TODO: Add a simple unittest

.. warning:: This module is under heavy development consequently significant
             changes will be made in future versions. DO NOT USE YET.

"""
from __future__ import absolute_import, unicode_literals, print_function, division
import six

import argparse
import re

import footprints

from bronx.stdtypes import date

logger = footprints.loggers.getLogger(__name__)


class ArgumentParser(footprints.FootprintBase):
    """
    Abstract base class for creating argument parser extensions.
    The actual job should be done by some external parser which
    class and default arguments for initialisation are given
    as footprint attributes.
    """

    _abstract  = True
    _collector = ('argparser',)
    _footprint = dict(
        info = 'Abstract Argument Parser',
        attr = dict(
            parser_kind = dict(
                info     = 'The kind of argument parser.',
                values   = ['std'],
            ),
            parser_class = dict(
                info     = 'Class of the actual parser object to create.',
                isclass  = True,
                optional = True,
                type     = argparse.ArgumentParser,
                default  = argparse.ArgumentParser,
            ),
            parser_init = dict(
                info     = 'Options to instantiate actual parser object.',
                optional = True,
                type     = footprints.stdtypes.FPDict,
                default  = footprints.stdtypes.FPDict(
                    description     = 'Inline command',
                    formatter_class = argparse.RawDescriptionHelpFormatter,
                    add_help        = False,
                ),
            ),
        )
    )

    def __init__(self, *args, **kw):
        """Preset to None or False hidden attributes ``iod``, ``iomode`` and ``filled``."""
        logger.debug('ArgumentParser %s init', self.__class__)
        super(ArgumentParser, self).__init__(*args, **kw)
        self._actual_parser = None
        self.opt_counter = 0

    @property
    def realkind(self):
        return 'argparser'

    @property
    def actual_parser(self):
        if self._actual_parser is None:
            self._actual_parser = self.parser_class(**self.parser_init)
        return self._actual_parser

    def _get_description(self):
        return self.actual_parser.description

    def _set_description(self, value):
        self.actual_parser.description = value
    description = property(_get_description, _set_description)

    def _get_usage(self):
        return self.actual_parser.usage

    def _set_usage(self, value):
        self.actual_parser.usage = value
    usage = property(_get_usage, _set_usage)

    def _get_prog(self):
        return self.actual_parser.prog

    def _set_prog(self, value):
        self.actual_parser.prog = value
    prog = property(_get_prog, _set_prog)

    def __getattr__(self, attr):
        if attr.startswith('add_'):
            raise AttributeError(self.__class__.__name__ + " object has no attribute '" + str(attr) + "'")
        else:
            return getattr(self.actual_parser, attr)


class DefinedArgumentParser(ArgumentParser):
    """
    A standard ArgumentParser with some pre-defined arguments.
    """

    _footprint = dict(
        info = 'Defined Argument Parser',
        attr = dict(
            parser_kind = dict(
                values  = ['defined'],
            ),
            safeopts = dict(
                type     = bool,
                optional = True,
                default  = False,
            ),
        ),
    )

    def keys(self):
        """Return all available key arguments defined."""
        return [x.replace('add_defined_', '') for x in dir(self) if x.startswith('add_defined_')]

    def add_argument(self, *opts, **kw):
        callback = kw.pop('callback', lambda x: x)
        optname = kw.pop('optname', 'unknown')
        if self.safeopts:
            self.opt_counter = self.opt_counter + 1
            opts = ['--{1:s}_X{0:03d}'.format(self.opt_counter, x[2:]) for x in opts if x.startswith('--')]
        storeaction = self.actual_parser.add_argument(*opts, **kw)
        storeaction.callback = callback
        storeaction.optname = optname
        storeaction.optclean = re.sub('_X\d+$', '', storeaction.dest)
        return storeaction

    def refine_argument(self):
        return dict()

    def store_defined_argument(self, item, required=set(), optional=set()):
        kw = getattr(self, 'add_defined_' + item)()
        kw.update(getattr(self, 'refine_defined_' + item, self.refine_argument)())
        kw.update(optname=item)
        if item in required:
            kw.update(required=True)
        if item in optional:
            kw.update(required=False)
        options = self.mktuple(kw.pop('options'))
        return self.add_argument(*options, **kw)

    def add_defined_report(self):
        return dict(
            options  = '--report',
            help     = 'Report output of the command to the logfile',
            action   = 'store_true',
        )

    def add_defined_grep(self):
        return dict(
            options  = '--grep',
            help     = 'Grep list items',
            metavar  = 'regex',
            nargs    = '+',
            type     = str,
            default  = '.',
            callback = self.mkregex,
        )

    def add_defined_discard(self):
        return dict(
            options  = '--discard',
            help     = 'Black list items',
            metavar  = 'item',
            nargs    = '+',
            type     = str,
            default  = None,
            callback = self.mktuple,
        )

    def add_defined_only(self):
        return dict(
            options  = '--only',
            help     = 'White list items',
            metavar  = 'item',
            nargs    = '+',
            type     = str,
            default  = None,
            callback = self.mktuple,
        )

    def add_defined_resize(self):
        return dict(
            options  = '--resize',
            help     = 'Resize history logfile.',
            metavar  = 'length',
            type     = int,
            default  = 999,
        )

    def add_defined_loglevel(self):
        return dict(
            options  = '--loglevel',
            help     = 'Logging level',
            choices  = ('debug', 'info', 'warning', 'error', 'critical'),
            default  = 'warning',
            callback = footprints.loggers.setGlobalLevel,
        )

    def add_defined_focus(self):
        return dict(
            options  = '--focus',
            help     = 'Focus history on a date value',
            metavar  = 'datestr',
            type     = date.Date,
            default  = None,
        )

    def add_defined_delta(self):
        return dict(
            options  = '--delta',
            help     = 'A delta value given as an integer (mostly seconds).',
            metavar  = 'int',
            type     = int,
            default  = None,
        )

    @classmethod
    def mktuple(cls, value):
        """Do our best to build a tuple from a given command line value."""
        if isinstance(value, six.string_types):
            # just split a comma sparated string
            value = tuple([x.strip() for x in value.split(',')])
        elif isinstance(value, list) and len(value) == 1 and isinstance(value[0], six.string_types):
            # because of the special nargs case we must check again comma separated lists
            value = tuple([x.strip() for x in value[0].split(',')])
        else:
            # well, let's try to build a proper tuple or create one
            try:
                value = tuple(value)
            except TypeError:
                value = (value,)
        return value

    @classmethod
    def mkregex(cls, value):
        """Return a compiled regular expression."""
        return re.compile('(?:' + '|'.join(cls.mktuple(value)) + ')', re.IGNORECASE)


class CfgExtendedArguments(object):
    """Add some cfg* defined arguments."""

    def add_defined_cfgname(self):
        return dict(
            options  = '--cfgname',
            help     = 'Cfg files radical',
            metavar  = 'name',
            default  = None,
        )

    def add_defined_cfgtag(self):
        return dict(
            options  = '--cfgtag',
            help     = 'Cfg files tag',
            metavar  = 'tag',
            default  = 'default',
        )

    def add_defined_cfgdir(self):
        return dict(
            options  = '--cfgdir',
            help     = 'Cfg files directory name',
            metavar  = 'path',
            default  = 'conf',
        )

    def add_defined_cfgroot(self):
        return dict(
            options  = '--cfgroot',
            help     = 'Cfg files root path',
            metavar  = 'path',
            default  = None,
        )

    def add_defined_cfgext(self):
        return dict(
            options  = '--cfgext',
            help     = 'Cfg extension filename',
            metavar  = 'str',
            default  = 'yaml',
        )

    def add_defined_cfgfile(self):
        return dict(
            options  = '--cfgfile',
            help     = 'Cfg full file path',
            metavar  = 'path',
            default  = None,
        )


class MeteoExtendedArguments(object):
    """Add some meteo pre-defined arguments."""

    _footprint = dict(
        info = 'Defined Meteo Argument Parser',
        attr = dict(
            parser_kind = dict(
                values = ['meteo'],
            ),
        ),
    )

    def add_defined_date(self):
        return dict(
            options  = ('-d', '--date'),
            help     = 'Date value',
            metavar  = 'datestr',
            type     = date.Date,
            default  = None,
        )

    def add_defined_xdate(self):
        return dict(
            options  = ('-d', '--date'),
            help     = 'Extended date range',
            metavar  = 'datestr',
            type     = str,
            default  = date.synop().ymdhm,
            callback = date.daterangex,
        )

    def add_defined_time(self):
        return dict(
            options  = ('-t', '--time'),
            help     = 'Time value',
            metavar  = 'timestr',
            type     = date.Time,
            default  = date.Time(1),
        )

    def add_defined_xtime(self):
        return dict(
            options  = ('-t', '--time'),
            help     = 'Extended time range',
            metavar  = 'timestr',
            type     = str,
            default  = date.Time(1),
            callback = self.mktimelist,
        )

    def add_defined_period(self):
        return dict(
            options  = ('-p', '--period'),
            help     = 'Period value',
            metavar  = 'periodstr',
            type     = date.Period,
            default  = date.Period('PT1H'),
        )

    def add_defined_xperiod(self):
        return dict(
            options  = ('-p', '--period'),
            help     = 'Extended period range (in minutes)',
            metavar  = 'periodstr',
            type     = str,
            default  = '60',
            callback = self.mkperiodlist,
        )

    def add_defined_suite(self):
        return dict(
            options  = ('-s', '--suite'),
            help     = 'Op suite',
            metavar  = 'suite',
            choices  = ('oper', 'dble', 'test'),
            default  = 'oper',
        )

    def add_defined_model(self):
        return dict(
            options  = ('-m', '--model', '--vapp'),
            help     = 'Vortex application name',
            metavar  = 'vapp[-vconf]',
            nargs    = '+',
            default  = 'arome',
            callback = self.mktuple,
        )

    def add_defined_cutoff(self):
        return dict(
            options  = ('-c', '--cutoff'),
            help     = 'Cutoff',
            metavar  = 'cutoff-name',
            nargs    = '+',
            choices  = ('production', 'assim'),
            default  = 'production',
            callback = self.mktuple,
        )

    def add_defined_term(self):
        return dict(
            options  = ('-t', '--term'),
            help     = 'Term',
            metavar  = 'term',
            type     = date.Time,
            default  = None,
        )

    def add_defined_xterm(self):
        return dict(
            options  = ('-t', '--term'),
            help     = 'Extended terms range',
            metavar  = 'termstr',
            default  = date.Time(1),
            callback = self.mktimelist,
        )

    def add_defined_step(self):
        return dict(
            options  = '--step',
            help     = 'Time step',
            metavar  = 'time',
            type     = date.Time,
            default  = date.Time('01:00'),
        )

    def add_defined_kind(self):
        return dict(
            options  = '--kind',
            help     = 'Resource kind',
            metavar  = 'kind',
            default  = None,
        )

    def add_defined_namespace(self):
        return dict(
            options  = '--namespace',
            help     = 'Resource namespace',
            metavar  = 'name',
            default  = None,
        )

    def add_defined_datacheck(self):
        return dict(
            options  = '--datacheck',
            help     = 'Activate inline check of metadata',
            action   = 'store_true',
        )

    def add_defined_filename(self):
        return dict(
            options  = '--filename',
            help     = 'Container filename',
            metavar  = 'name',
            default  = None,
        )

    def add_defined_remote(self):
        return dict(
            options  = '--remote',
            help     = 'Remote resource filename',
            metavar  = 'name',
            default  = None,
        )

    def add_defined_incore(self):
        return dict(
            options  = '--incore',
            help     = 'Select an incore memory container',
            action   = 'store_true',
        )

    @classmethod
    def mktimelist(cls, value):
        """Return a list of date.Time values from an extended time range value."""
        return [date.Time(x) for x in footprints.util.rangex(value)]

    @classmethod
    def mkperiodlist(cls, value):
        """Return a list of date.Period values from an extended period range value in minutes."""
        return [date.Period('PT' + six.text_type(x ) + 'M') for x in footprints.util.rangex(value)]


class CfgMeteoArgumentParser(DefinedArgumentParser, CfgExtendedArguments, MeteoExtendedArguments):
    """A standard ArgumentParser with some pre-defined arguments."""

    _footprint = dict(
        info = 'Defined Meteo Argument Parser',
        attr = dict(
            parser_kind = dict(
                values = ['meteo', 'full'],
            ),
        ),
    )

    def add_defined_mine(self):
        return dict(
            options  = '--mine',
            help     = 'Give a name to the mine to digg in.',
            default  = 'op',
        )
