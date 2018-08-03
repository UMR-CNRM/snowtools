#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Making things pretty.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import pprint


#: No automatic export
__all__ = []


def smooth_string(s, escaped_characters={' ': '_',
                                         '{': '', '}': '',
                                         '(': '', ')': '',
                                         '[': '', ']': '',
                                         '*': '', '?': ''}):
    """
    Returns str(*s*) escaping special characters that may
    be forbidden in filenames.

    :param escaped_characters: special characters to escape,
                               and their replacement in case.
    """
    result = str(s).strip()
    for repl in escaped_characters.items():
        result = result.replace(*repl)
    return result


class Utf8PrettyPrinter(pprint.PrettyPrinter, object):
    """
    An utf-8 friendly version of the standard pprint.

    This class may be used like the original, e.g.::

       pf = Utf8PrettyPrinter().pformat
       print 'an_object:', pf(vars(an_object))
    """
    def __init__(self, *args, **kw):
        super(Utf8PrettyPrinter, self).__init__(*args, **kw)

    def format(self, obj, context, maxlevels, level):
        """Use readable representations for str and unicode, instead of repr."""
        if isinstance(obj, str):
            return obj, True, False
        if isinstance(obj, unicode):
            return obj.encode('utf8'), True, False
        return pprint.PrettyPrinter.format(self, obj, context, maxlevels, level)
