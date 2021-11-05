# -*- coding: utf-8 -*-

"""
Compatibility for modules or attributes that move or change their name across versions of Python
"""

from __future__ import absolute_import, division, print_function, unicode_literals

import abc
import re
import sys

import six


def _require_version(major, minor=0):
    """Check if the running python version is at least Python **major**.**minor**."""
    return (sys.version_info.major >= major and
            sys.version_info.minor >= minor)


# ABCs are moved from "collections" to "collections.abc" in 3.8
six.add_move(six.MovedModule("collections_abc", "collections", "collections.abc"))
collections_abc = six.moves.collections_abc
collections_abc.__doc__ = "Compatibility module for abstract classes that move from 'collections' " \
                          "in Python 2.7 to 'collections.abc' in Python 3.x (will break in 3.8)"


# re._pattern_type is removed in python3.7
@six.add_metaclass(abc.ABCMeta)
class re_Pattern(object):
    """Mimics Python3.7 re.Pattern behaviour."""

    def __new__(self, *args, **kwargs):
        """This is an abstract class."""
        raise TypeError("cannot create 're.Pattern' instances.")


if _require_version(3, 7):
    re_Pattern.register(re.Pattern)
else:
    re_Pattern.register(re.compile('^').__class__)
