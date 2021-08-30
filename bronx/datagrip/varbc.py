# -*- coding: utf-8 -*-

"""Utility class to read VarBC files.

The :class:`VarbcFile` should be used to read VarBC files (see its documentation
below).
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import six

from collections import namedtuple, OrderedDict
from contextlib import contextmanager
import numpy as np
import re

from bronx.compat.moves import collections_abc
from bronx.datagrip import varbcheaders


#: No automatic export
__all__ = []


class _VarbcEntryTypeDescriptor(object):
    """Handle to an object's data of a specific type."""

    def __init__(self, attr, objtype, doc='Undocumented footprint attribute'):
        """
        Ensures a proper conversion to the **objtype** type for the _**attr**
        attribute.
        """
        self._attr = attr
        self._objtype = objtype
        self.__doc__ = doc

    def __get__(self, obj, objtype=None):  # @UnusedVariable
        return getattr(obj, '_' + self._attr)

    def __set__(self, obj, value):
        setattr(obj, '_' + self._attr, self._objtype(value))


class _VarbcEntryNumpyDescriptor(_VarbcEntryTypeDescriptor):
    """Handle access to an object's data contained in NumPy arrays."""

    def __set__(self, obj, value):
        if isinstance(value, six.string_types):
            value = [self._objtype(st) for st in value.split()]
        setattr(obj, '_' + self._attr, np.array(value, dtype=self._objtype))


class VarbcEntry(object):
    """One entry of a VarBC file.

    The comparison operator ``==`` is available between objects of this class.
    """

    def __init__(self):
        """No arguments have to be provided at creation time."""
        self._type = ''
        self._key = ''
        self._ix = None
        self._ndata = -9999
        self._npred = 0
        self._predcs = np.array((), dtype=np.uint8)
        self._params = np.array((), dtype=np.float32)

    type = _VarbcEntryTypeDescriptor("type", str, "The observation type.")

    key = _VarbcEntryTypeDescriptor("key", str, "The entry key.")

    ix = _VarbcEntryTypeDescriptor("ix", int, "The entry identifier.")

    ndata = _VarbcEntryTypeDescriptor("ndata", int, "Number of data.")

    npred = _VarbcEntryTypeDescriptor("npred", int, "Number of predictors.")

    predcs = _VarbcEntryNumpyDescriptor("predcs", np.uint8, "Predictors NumPy array.")

    params = _VarbcEntryNumpyDescriptor("params", np.float32, "Coefficients NumPy array.")

    def __repr__(self):
        return ('{0:s}(type: {1.type:s}, ix={1.ix:d}, key={1.key:s}, ndata={1.ndata:d}, npred={1.npred:d})'
                .format(self.__class__.__name__, self))

    def __str__(self):
        return ('{!r}\n  preds = {:s}\n  params= {:s}'
                .format(self,
                        ' '.join(['{:7d}'.format(n) for n in self.predcs]),
                        ' '.join(['{:7.3f}'.format(x) for x in self.params])))

    def __eq__(self, other):
        if not isinstance(other, VarbcEntry):
            return False
        return (self.key == other.key and self.type == other.type and
                self.ndata == other.ndata and self.npred == other.npred and
                np.alltrue(self.predcs == other.predcs) and
                np.alltrue(self.params == other.params))

    def __ne__(self, other):
        return not self == other

    def valid(self):
        """Check if all the mandatory fields are properly set up."""
        return (len(self.predcs) == self.npred and
                len(self.params) == self.npred and
                self.key and self.type)


#: Holds the **regex** associated with a given **element** attribute of a
#: :class:`VarbcEntry` object.
_VarbcMatchElement = namedtuple('_VarbcMatchElement', ('element', 'regex'))


class _VarbcMatchTool(object):
    """Object that uses regular expressions to parse varbc entry."""

    def __init__(self, matches, stack):
        """
        :param matches: The list of :class:`_VarbcMatchElement` to look for
        :param stack: The list where new :class:`ObsVarbcEntry` will be appended
        """
        self._matches = matches
        self._stack = stack
        self._cur_entry = None
        self._imatch = 0

    def save_entry(self):
        """Save the current entry into the **stack**."""
        if self._cur_entry:
            if self._cur_entry.valid():
                if self._cur_entry.ix != len(self._stack) + 1:
                    raise ValueError("Entry numbering inconsistency: {!s}"
                                     .format(self._cur_entry))
                self._stack.append(self._cur_entry)
                self._cur_entry = None
            else:
                raise ValueError("Incomplete entry encountered: {!s}"
                                 .format(self._cur_entry))

    def __call__(self, line):
        """Process a single VarBC line."""
        # New entry starting ?
        firstmatch = self._matches[0].regex.match(line)
        if firstmatch:
            self.save_entry()  # Save a previous one
            self._cur_entry = VarbcEntry()  # Create a new entry for the new run
            setattr(self._cur_entry, self._matches[0].element, firstmatch.group(1))
            self._imatch = 1
        elif self._imatch > 0:
            # It's not a first line, look for the next expected regex
            curmatch = self._matches[self._imatch].regex.match(line)
            if curmatch:
                setattr(self._cur_entry, self._matches[self._imatch].element, curmatch.group(1))
                self._imatch = (self._imatch + 1) % len(self._matches)


class _VarbcMatchList(object):
    """Object that uses regular expressions to parse varbc entry."""

    def __init__(self, matches):
        """
        :param matches: The list of :class:`_VarbcMatchElement` to look for
        """
        self._matches = matches

    @contextmanager
    def autorecord(self, entrystatck):
        """Return a :class:`_VarbcMatchTool` that will be able to parse VarBC lines."""
        mt = _VarbcMatchTool(self._matches, entrystatck)
        yield mt
        mt.save_entry()


class VarbcFile(collections_abc.Mapping):
    """Class to handle a full VarBC file.

    It provides then two simple methods to access to elements :class:`ObsVarbcEntry`,
    one with ix (:meth:`getix`), the other with varbc 'key' (:meth:`getkey`).

    It also behaves like a *Mapping* since the :meth:`__getitem__`,
    :meth:`__iter__`, :meth:`keys`, :meth:`values` and :meth:`items` methods
    are defined. With all of these methods, the values are returned in the same
    order than originaly read in thh VarBC file.
    """

    _VBC_MATCH_ELEMENTS = [
        _VarbcMatchElement('ix', re.compile(r'^ix=0*(\d+)$')),
        _VarbcMatchElement('type', re.compile(r'^class=(\w+)$')),
        _VarbcMatchElement('key', re.compile(r'^key=\s*([^=]+)\n$')),
        _VarbcMatchElement('ndata', re.compile(r'^ndata=(\d+)$')),
        _VarbcMatchElement('npred', re.compile(r'^npred=(\d+)$')),
        _VarbcMatchElement('predcs', re.compile(r'^predcs=([\d ]+)$')),
        _VarbcMatchElement('params', re.compile(r'^params=([\dEe+-. ]+)$')),
    ]

    def __init__(self, asciidatas):
        """
        :param asciidatas: Any iterable over lines from a VarBC file.
        """
        self._datalist = []

        self._metadata = varbcheaders.VarbcHeadersFile(asciidatas)

        mymatchlist = _VarbcMatchList(self._VBC_MATCH_ELEMENTS)
        with mymatchlist.autorecord(self._datalist) as mymatchtool:
            for a_line in asciidatas:
                mymatchtool(a_line)

        self._key2entry = OrderedDict()
        for entry in self._datalist:
            self._key2entry[entry.key] = entry

    @property
    def metadata(self):
        """The metadata associated to the varbc file.

        :rtype: :class:`bronx.datagrip.varbcheaders.VarbcHeadersFile`
        """
        return self._metadata

    def __len__(self):
        """The number of entries in the VarBC file."""
        return len(self._datalist)

    def __getitem__(self, item):
        """Return the entry associated with **entry**.

        If **entry** is an integer, this is equivalent to :meth:`getix`. If
        **entry** is a string, this is equivalent to :meth:`getkey`.
        """
        if isinstance(item, six.string_types):
            return self.getkey(item)
        elif isinstance(item, int):
            return self.getix(item)
        else:
            raise KeyError('{!s} is not a valid key for a VarbcFile object'.format(item))

    def keys(self):
        """Iterate over all the keys available in the VarBC file."""
        for k in self._key2entry.keys():
            yield k

    def __iter__(self):
        """Iterate over all the keys available in the VarBC file."""
        return self.keys()

    def values(self):
        """Iterate over all the :class:`ObsVarbcEntry` objects read from file."""
        for entry in self._datalist:
            yield entry

    def items(self):
        """Iterate over all the (key, entry) pairs available in the VarBC file."""
        for k, e in self._key2entry.items():
            yield (k, e)

    def getix(self, ix):
        """Gives the **ix** th entry of the VarBC file

        :rtype: :class:`VarbcEntry`
        """
        if ix < 1:
            raise KeyError("The serie of ix numbers starts with 1")
        return self._datalist[ix - 1]

    def getkey(self, key):
        """Returns a VarBC entry given its **key**

        :rtype: :class:`VarbcEntry`
        :example: ``myobj.getkey('4 3 7')``
        """
        return self._key2entry[key]
