#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""
YAML Configuration file with possible multiple versions.

A basic mechanism is implemented for the selection of the current version.
It may be extended to support various criteria.

TODO: More documentation (incl. examples)
TODO: unittest

.. warning:: This module is under heavy development consequently significant
             changes will be made in future versions. DO NOT USE YET.

"""

from __future__ import absolute_import, unicode_literals, print_function, division

import os
import glob
import io
import yaml

import footprints

from bronx.fancies import loggers, fscrawler
from bronx.stdtypes import date

logger = loggers.getLogger(__name__)


def _construct_yaml_str(self, node):
    # Override the default string handling function
    # to always return unicode objects
    return self.construct_scalar(node)


yaml.Loader.add_constructor(u'tag:yaml.org,2002:str', _construct_yaml_str)
yaml.SafeLoader.add_constructor(u'tag:yaml.org,2002:str', _construct_yaml_str)


class MultiFileCfg(footprints.FootprintBase):
    """
    Handler for multi-files YAML configuration files.
    Filenames should be ``cfgname-cfgtag-*.cfgext``
    """

    _abstract  = True
    _collector = ('ymlconf',)
    _footprint = dict(
        info = 'Default Multi YAML files configuration manager.',
        attr = dict(
            cfgname = dict(),
            cfgtag = dict(
                optional = True,
                default  = 'oper',
            ),
            cfgdir = dict(
                optional = True,
                default  = 'conf',
            ),
            cfgroot = dict(
                optional = True,
                default  = None,
            ),
            cfgext = dict(
                optional = True,
                default  = 'yaml',
            ),
        )
    )

    def __init__(self, *args, **kw):
        logger.debug('Abstract multicfg init %s', self.__class__)
        super(MultiFileCfg, self).__init__(*args, **kw)
        self._cfgtmp  = dict()
        self._cfgload = dict()
        self._cfgpath = fscrawler.upfirst(self.cfgdir, thispath=self.cfgroot)
        self._cfgstack = glob.glob(self.cfgfullpath())
        self._cfgstack.sort()
        self.select()

    @property
    def cfgfile(self):
        return self._cfgfile

    @property
    def cfgpath(self):
        return self._cfgpath

    @property
    def info(self):
        return self._cfginfo

    @property
    def raw(self):
        return self._cfgraw

    @property
    def tmp(self):
        return self._cfgtmp

    @property
    def defaults(self):
        return self._cfgdefaults

    @property
    def excluded(self):
        return self._cfgexcluded

    def cfgfullpath(self, extra='*'):
        """Build the actual path of potential configuration files."""
        return os.path.join(self.cfgpath, self.cfgname + '-' + self.cfgtag + '-' + extra + '.' + self.cfgext)

    def select(self, **kw):
        """Switch to a valid configuration file, according to date."""
        self._cfgfile = self.cfgfullpath('default')
        if 'extra' in kw:
            self._cfgfile = self.cfgfullpath(kw['extra'])
        elif 'date' in kw:
            cfgdate = date.Date(kw['date'])
            cfgtest = self.cfgfullpath(cfgdate.ymdh)
            for cfgfile in reversed(self._cfgstack):
                if cfgtest >= cfgfile:
                    self._cfgfile = cfgfile
                    break
        else:
            self._cfgfile = self._cfgstack[-1]
        if self._cfgfile not in self._cfgstack:
            raise ValueError(self._cfgfile + ' not in actual stack of configuration files')
        if self._cfgfile in self._cfgload:
            self._cfginfo, self._cfgraw, self._cfgexcluded, self._cfgdefaults = self._cfgload[self._cfgfile]
        else:
            with io.open(self._cfgfile, 'rb') as fd:
                self._cfginfo = self.cfgclean(yaml.load(fd))
                fd.seek(0, os.SEEK_SET)
                self._cfgraw = fd.read()
            self._cfgexcluded = set(self._cfginfo.pop('excluded', list()))
            self._cfgdefaults = self._cfginfo.pop('defaults', dict())
            self._cfgexcluded.add('excluded')
            self.cfgprocess()
            self._cfgload[self._cfgfile] = (self._cfginfo, self._cfgraw, self._cfgexcluded, self._cfgdefaults)

    def cfgclean(self, data):
        return data

    def cfgprocess(self):
        pass

    def notexcluded(self, data):
        return [x for x in data if x not in self.excluded]


class OpTableCfg(MultiFileCfg):
    """
    Extend multi file configurations for pre-processing some kind of data
    such as dates, times, etc.
    """

    _footprint = dict(
        info = 'Default Multi YAML optables files.',
        attr = dict(
            cfgname = dict(
                values = ['optables', 'opnam'],
            ),
            cfgtag = dict(
                values = ['oper', 'dble', 'test'],
            ),
        )
    )

    def cfgclean(self, data):
        self.tmp.update(maxterm=0)
        self.expandall(data)
        return data

    def expandall(self, data):
        for k, v in data.items():
            if k.endswith('_period'):
                data[k] = date.Period(v)
            elif k.endswith('_term') or k == 'terms':
                data[k] = footprints.util.rangex(v)
                self.tmp['maxterm'] = max(self.tmp['maxterm'], data[k][-1])
            elif k.startswith('t') and type(v) is list:
                data[k] = [date.Time(x) for x in v]
            elif type(v) is dict:
                self.expandall(v)

    def cfgprocess(self):
        """Upgrade run hours to `date.Time` objects."""
        for vapp in self.models():
            for vconf in self.notexcluded(self.info[vapp]):
                for cutoff in self.notexcluded(self.info[vapp][vconf].keys()):
                    for run in self.info[vapp][vconf][cutoff] or tuple():
                        self.info[vapp][vconf][cutoff][date.Time(run).fmthm] = self.info[vapp][vconf][cutoff].pop(run)

    @property
    def maxterm(self):
        return self.tmp.get('maxterm')

    def models(self, **kw):
        return self.notexcluded(self.info.keys())

    def vapps(self, **kw):
        return [vapp + '-' + vconf
                for vapp in self.models(**kw)
                for vconf in self.notexcluded(self.info[vapp])]

    def get_vapp_vconf(self, model):
        if '-' in model:
            vapp, vconf = model.split('-')
        else:
            vapp = model
            vconf = self.info[model]['default']
        if 'remap' in self.info[vapp]:
            vconf = self.info[vapp]['remap'].get(vconf, vconf)
        return (vapp, vconf)

    def cutoffs(self, **kw):
        """Set of defined cutoff for all models."""
        c = set()
        for vapp in self.models():
            for vconf in self.notexcluded(self.info[vapp]):
                c.update(self.notexcluded(self.info[vapp][vconf].keys()))
        return list(c)

    def locations(self, **kw):
        """List of storage location (probably: disk and arch)."""
        return self.defaults['locations'].keys()
