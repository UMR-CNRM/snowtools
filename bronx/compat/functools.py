# -*- coding: utf-8 -*-

"""
Compatibility for some of the features of the functools modules.

The :class:`cached_property` decorator was introduced with Python3.8, this
module provides a less sophisticated backport. Note: If :mod:`bronx` is running
on Python 3.8+ the :class:`cached_property` decorator from the standard library
is used.
"""

from __future__ import absolute_import, division, print_function, unicode_literals

import sys
from threading import RLock

import six

if six.PY2 or (sys.version_info.major == 3 and sys.version_info.major < 8):

    _NOT_FOUND = object()

    class cached_property(object):
        """Call the function only once and cache its result."""

        def __init__(self, func):
            self.func = func
            self.__name__ = func.__name__
            self.__doc__ = func.__doc__
            self.lock = RLock()

        def __get__(self, instance, owner=None):
            if instance is None:
                return self
            try:
                cache = instance.__dict__
            except AttributeError:  # not all objects have __dict__ (e.g. class defines slots)
                raise TypeError("No '__dict__' attribute on {!r} instance to cache {!r} property.".format(
                                type(instance).__name__, self.__name__))
            val = cache.get('_autocache_' + self.__name__, _NOT_FOUND)
            if val is _NOT_FOUND:
                with self.lock:
                    # check if another thread filled cache while we awaited lock
                    val = cache.get('_autocache_' + self.__name__, _NOT_FOUND)
                    if val is _NOT_FOUND:
                        val = self.func(instance)
                        try:
                            cache['_autocache_' + self.__name__] = val
                        except TypeError:
                            msg = (
                                "The '__dict__' attribute on {!r} instance " +
                                "does not support item assignment for caching {!r} property."
                            ).format(type(instance).__name__, self.__name__)
                            raise TypeError(msg)
            return val

else:

    import functools
    cached_property = functools.cached_property
