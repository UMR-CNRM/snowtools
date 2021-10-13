# -*- coding: utf-8 -*-

"""
Syntax useful tools.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import six
import copy

#: No automatic export
__all__ = []


def dictmerge(d1, d2):
    """Merge two dictionaries *d1* and *d2* with a recursive function.

    * *d1* and *d2* can be dictionaries of dictionaries;
    * The result is in *d1*. If keys exist in *d1* and *d2*, *d1* keys are
      replaced by *d2* keys.

    Examples::

        >>> a = {'name':'clim','attr':{'model':{'values':('arpege','arome')}}}
        >>> b = {'name':'clim model','attr':{'truncation':{'type':'int','optional':'False'}}}
        >>> (dictmerge(a, b) ==
        ...  {'name': 'clim model', 'attr': {'model': {'values': ('arpege', 'arome')},
        ...                                  'truncation': {'type': 'int', 'optional': 'False'}}})
        True

        >>> (dictmerge({'a':'1'}, {'b':'2'}) ==
        ...  {'a': '1', 'b': '2'})
        True

        >>> (dictmerge({'a':'1','c':{'d':'3','e':'4'},'i':{'b':'2','f':{'g':'5'}}}, {'c':{'h':'6', 'e':'7'}}) ==
        ...  {'a': '1', 'i': {'b': '2', 'f': {'g': '5'}}, 'c': {'h': '6', 'e': '7', 'd': '3'}})
        True

    """
    for key, value in six.iteritems(d2):
        if isinstance(value, dict) and not value.__class__.__name__.startswith('FP'):
            if key in d1 and isinstance(d1[key], dict) and not value.__class__.__name__.startswith('FP'):
                dictmerge(d1[key], d2[key])
            else:
                d1[key] = copy.deepcopy(value)
        else:
            d1[key] = value

    return d1


def mktuple(obj):
    """Make a tuple from any kind of object."""
    if isinstance(obj, list) or isinstance(obj, set) or isinstance(obj, tuple):
        return tuple(obj)
    else:
        return obj,
