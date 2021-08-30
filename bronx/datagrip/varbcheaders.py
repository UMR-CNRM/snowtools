# -*- coding: utf-8 -*-

"""Utility class to read the headers of a VarBC files."""

from __future__ import print_function, absolute_import, unicode_literals, division

import re

from bronx.compat.moves import collections_abc
from bronx.stdtypes.date import Date


#: No automatic export
__all__ = []


class VarbcHeadersFile(collections_abc.Mapping):
    r"""Class to handle the headers of a VarBC file.

    By headers, we mean the first 3 lines that contain informations about the
    file's version, date, experiment number (expver) and size (e.g. the number
    of entries in the file).

    This object behaves like a ``Mapping``::

        >>> print(VarbcHeadersFile.DOCTEST_DATA)
        VARBC_cycle.version006
        MINI  20200101         0
                11     10980
        other stuff...
        >>> vbch = VarbcHeadersFile(VarbcHeadersFile.DOCTEST_DATA.split("\n"))
        >>> len(vbch)
        4
        >>> set(vbch) == set(['version', 'date', 'nentries', 'expver'])
        True
        >>> vbch['version']
        6
        >>> vbch['nentries']
        11

    """

    DOCTEST_DATA = """VARBC_cycle.version006
MINI  20200101         0
        11     10980
other stuff..."""

    _VBC_VERSION_RE = re.compile(r'\w+\.version(\d+)')
    _VBC_XPDATE_RE = re.compile(r'\s*(\w+)\s+(\d{8})\s+(\d+)')
    _VBC_LEN_RE = re.compile(r'\s*(\d+)')

    @staticmethod
    def _serious_match(regex, line):
        mobj = regex.match(line)
        if not mobj:
            raise ValueError('Unparsable line in the VarBC headers: {:s}'
                             .format(line))
        return mobj

    def __init__(self, varbclines):
        """
        :param varbclines: Iterable over lines read from a VarBC file. Note:
                           Only the first 3 lines of the VarBC file will be used.
        """
        self._metadata = {}
        l_iter = iter(varbclines)
        mobj = self._serious_match(self._VBC_VERSION_RE, next(l_iter))
        self._metadata['version'] = int(mobj.group(1))
        mobj = self._serious_match(self._VBC_XPDATE_RE, next(l_iter))
        self._metadata['expver'] = mobj.group(1)
        self._metadata['date'] = Date('{:s}{:06d}'.format(mobj.group(2),
                                                          int(mobj.group(3))))
        mobj = self._serious_match(self._VBC_LEN_RE, next(l_iter))
        self._metadata['nentries'] = int(mobj.group(1))

    def __getitem__(self, item):
        return self._metadata[item]

    def __iter__(self):
        for k in self._metadata.keys():
            yield k

    def __len__(self):
        return len(self._metadata)


if __name__ == '__main__':
    import doctest
    doctest.testmod()
