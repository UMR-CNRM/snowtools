#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""
A very crude action dispatcher mostly for interactive usage.

The object should be created before parsing of arguments.
The first argument is the so-called action to perform.
All other command-line arguments are supposed to be options
given to action-methods of the dispatcher.

.. warning:: This module is under heavy development consequently significant
             will be made in future versions. DO NOT USE YET.

"""

from __future__ import absolute_import, unicode_literals

import os
import sys
import io
import yaml


def _construct_yaml_str(self, node):
    # Override the default string handling function
    # to always return unicode objects
    return self.construct_scalar(node)


yaml.Loader.add_constructor(u'tag:yaml.org,2002:str', _construct_yaml_str)
yaml.SafeLoader.add_constructor(u'tag:yaml.org,2002:str', _construct_yaml_str)


def upfirst(subpath='work', thispath=None):
    """Return first directory matching subpath in specified or current path."""
    if thispath is None:
        thispath = os.getcwd()
    found = None
    while thispath and not found:
        if os.path.isdir(os.path.join(thispath, subpath)):
            found = os.path.join(thispath, subpath)
        else:
            if thispath == '/':
                break
            thispath = os.path.dirname(thispath)
    return found


class InteractiveDispatcher(object):

    def __init__(self, prefix='do', name='dispatch',
                 cfgpath='conf', cfgroot=None, cfgtag='default', cfgfile=None, cfgload=True):
        self._name = name
        self._prefix = prefix + '_'
        if cfgload:
            if cfgfile is None:
                self._cfgfile = os.path.join(upfirst(cfgpath, thispath=cfgroot), name + '-' + cfgtag + '.yaml')
            else:
                self._cfgfile = cfgfile
            with io.open(self._cfgfile, 'rb') as fd:
                self._cfginfo = yaml.load(fd)
                fd.seek(0, os.SEEK_SET)
                self._cfgraw = fd.read()
        else:
            self._cfgfile = self.cfgraw = None
            self._cfginfo = dict()
        self._excluded = self._cfginfo.get('excluded', list())
        self._excluded.append('excluded')
        self.cfgsetup()
        for action in ('commands', 'stderror', 'echo', 'cat', 'cfg', 'excluded'):
            setattr(self, self.prefix + action, getattr(self, '_x_' + action))

    def _x_commands(self, **kw):
        return sorted([x.split('_', 1)[-1] for x in dir(self) if x.startswith(self.prefix)])

    def _x_stderror(self):
        raise StandardError('No action provided')

    def _x_echo(self, **kw):
        return kw

    def _x_cat(self, **kw):
        return self.cfgraw

    def _x_cfg(self, **kw):
        return self.cfginfo

    def _x_excluded(self, **kw):
        return self.excluded

    def cfgsetup(self):
        pass

    @property
    def name(self):
        return self._name

    @property
    def prefix(self):
        return self._prefix

    @property
    def cfgfile(self):
        return self._cfgfile

    @property
    def cfginfo(self):
        return self._cfginfo

    @property
    def cfgraw(self):
        return self._cfgraw

    @property
    def excluded(self):
        return self._excluded

    def notexcluded(self, data):
        return [x for x in data if x not in self.excluded]

    @classmethod
    def getaction(cls):
        return 'stderror' if len(sys.argv) < 2 else sys.argv.pop(1)

    def execute(self, action, options):
        return getattr(self, self.prefix + action)(**options)
