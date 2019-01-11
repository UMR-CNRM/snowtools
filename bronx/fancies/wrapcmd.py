#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""
TODO: Module documentation
TODO: More documentation (incl. examples)
TODO: unittest

.. warning:: This module is under heavy development consequently significant
             changes will be made in future versions. DO NOT USE YET.

"""

from __future__ import absolute_import, unicode_literals, print_function, division
import six

import re
import os
import shlex
import textwrap

import footprints

from bronx.fancies import loggers
import bronx.fancies.arguments  # @UnusedImport

logger = loggers.getLogger(__name__)

DEFAULT_PARSER_KIND = 'full'


class WrapCmdLineArgs(object):
    """Decorator for ``do_`` functions of cmd.Cmd derivated classes."""

    _re_doc_defaults = re.compile(r'^\s*[Dd]efaul?ts?\s+(\w+)\s*:\s*(\S.*)\s*$', re.MULTILINE)

    def __init__(self, *items, **kw):
        """Instantiate a default argument parser and setup the decorator."""
        self.items = set(['report'])
        self.discarded = set()
        self.required = set()
        self.optional = set()
        expanded_items = list()
        for item in items:
            if hasattr(item, '__iter__') and not isinstance(item, six.string_types):
                expanded_items.extend(list(item))
            else:
                expanded_items.append(item)
        for item in expanded_items:
            if item.startswith('-'):
                self.discarded.add(item[1:])
            else:
                if item.startswith('+'):
                    item = item.replace('+', '')
                    self.required.add(item)
                if item.startswith('*'):
                    item = item.replace('*', '')
                    if item in self.required:
                        self.required.remove(item)
                    self.optional.add(item)
                self.items.add(item)
        self.items = self.items - self.discarded
        self.defaults = kw.get('defaults', True)
        self.addhelp = kw.get('addhelp', False)
        self.rstdoc = kw.get('rstdoc', bool(os.environ.get('BRONX_FANCIES_RSTDOC', 0)))
        self.strict = kw.get('strict', False)
        self.kind = kw.get('kind', footprints.setup.defaults.get('parser_kind', DEFAULT_PARSER_KIND))
        self.loadparser()

    def loadparser(self):
        """Load the actual parser according to some defined kind through footprint proxy."""
        logger.debug('Load parser <kind:{0}'.format(self.kind))
        protect_opts = 'True' if '_all' in self.items else False
        self.parser = footprints.proxy.argparser(parser_kind=self.kind, safeopts=protect_opts)
        self.parser.parser_init.update(add_help=self.addhelp)
        if '_all' in self.items:
            self.items = set(self.parser.keys())
        self.opts = {opt: self.parser.store_defined_argument(opt, required=self.required, optional=self.optional)
                     for opt in self.items}

    def setprog(self, f):
        """Top level documentation of the decorated function: prog and description."""
        self.parser.prog = f.__name__.replace('do_', '')
        doclines = f.__doc__.split('\n')
        finaldoc = ''
        if doclines:
            if doclines[0]:
                finaldoc += doclines[0] + '\n'
            if len(doclines) > 1:
                finaldoc += textwrap.dedent('\n'.join(doclines[1:]))
        self.parser.description = finaldoc

    def setdefaults(self):
        """
        Try to refine the default value of some options
        according to information found in the current function description.
        """
        if self.defaults:
            for m in self._re_doc_defaults.findall(self.parser.description):
                if m[0] in self.opts:
                    if m[1].startswith('`'):
                        self.opts[m[0]].default = eval(m[1].replace('`', ''))
                    elif m[1] == 'None':
                        self.opts[m[0]].default = None
                    elif re.match('false', m[1], re.IGNORECASE):
                        self.opts[m[0]].default = False
                    elif re.match('true', m[1], re.IGNORECASE):
                        self.opts[m[0]].default = True
                    else:
                        self.opts[m[0]].default = m[1]

    def __call__(self, f):
        """Decorate the do_ function in order to get a dict with parsed options as sole argument."""
        self.setprog(f)
        self.setdefaults()

        def parsed_f(objcmd, *args, **kw):
            objcmd.defined_opts = self.opts
            line = args[0] if args else ''
            try:
                if self.strict:
                    options = dict(**vars(self.parser.parse_args(shlex.split(line))))
                else:
                    options = dict(**vars(self.parser.parse_known_args(shlex.split(line))[0]))
            except SystemExit:
                return
            options.update(kw)
            objcmd.logflag = options.get('report')
            for item, pstore in self.opts.items():  # @UnusedVariable
                options[pstore.dest] = pstore.callback(options[pstore.dest])
            return f(objcmd, **options)

        parsed_f.__name__ = f.__name__

        actual_help = self.parser.format_help()
        if self.rstdoc:
            parsed_f.__doc__ = ("Documentation auto-generated by argparse.\n\n" +
                                ".. code-block:: none\n\n" +
                                '\n'.join(['   ' + s for s in actual_help.split('\n')]) +
                                "\n")
        else:
            parsed_f.__doc__ = actual_help

        return parsed_f
