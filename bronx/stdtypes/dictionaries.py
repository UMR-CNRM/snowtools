#!/usr/bin/env python
# -*- coding:Utf-8 -*-

"""
Structs or dictionary like classes for miscellaneous usage.

TODO: Example
TODO: unittest
"""

from __future__ import print_function, absolute_import, division, unicode_literals

import collections


class Foo(object):
    """
    Protected C-struct like class... for gathering anything.
    Internal dict methods could be called through i_*methodname* protection.
    """
    def __init__(self, **kw):
        self.__dict__.update(kw)

    def as_dict(self):
        return self.__dict__

    def __getattr__(self, attr):
        if attr.startswith('i_'):
            return getattr(self.__dict__, attr.split('_', 1)[1])
        else:
            raise AttributeError

    def __str__(self):
        return str(self.__dict__)


class ReadOnlyDict(collections.Mapping):
    """A type of read-only dictionary."""

    def __init__(self, data=dict()):
        self._data = data

    def __getitem__(self, key):
        return self._data[key]

    def __len__(self):
        return len(self._data)

    def __iter__(self):
        return iter(self._data)

    def __repr__(self):
        return repr(self._data)

    def __str__(self):
        return str(self._data)
