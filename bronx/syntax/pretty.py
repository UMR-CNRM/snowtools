# -*- coding: utf-8 -*-

"""
Making things pretty.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import pprint
import six

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


if six.PY3:

    EncodedPrettyPrinter = pprint.PrettyPrinter

else:

    class EncodedPrettyPrinter(pprint.PrettyPrinter, object):
        """
        An encoding friendly version of the standard pprint.

        The pformat method returns unicode instead of an
        encoded string, like in Python3.

        This class may be used like the original, e.g.:

           pf = EncodedPrettyPrinter().pformat
           print('an_object:', pf(vars(an_object)))
        """

        def __init__(self, encoding='utf-8', *args, **kw):
            super(EncodedPrettyPrinter, self).__init__(*args, **kw)
            self.encoding = encoding

        def format(self, obj, context, maxlevels, level):
            """Use readable representations for str and unicode, instead of repr."""
            if isinstance(obj, str):
                return obj, True, False
            if isinstance(obj, six.text_type):
                return obj.encode(self.encoding), True, False
            return pprint.PrettyPrinter.format(self, obj, context, maxlevels, level)

        def pformat(self, obj):
            encoded = super(EncodedPrettyPrinter, self).pformat(obj)
            return encoded.decode(self.encoding)
