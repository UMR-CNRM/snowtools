# -*- coding: utf-8 -*-

"""
A simple class for managing a collection of *items*.
"""

from __future__ import print_function, absolute_import, division, unicode_literals

import six

from weakref import WeakSet

from bronx.fancies import loggers

logger = loggers.getLogger(__name__)


class Catalog(object):
    """A simple class for managing a collection of *items*.

    The interface is very light : :meth:`add`, :meth:`discard` and
    :meth:`clear` are the more heavily used

    Of course a catalog is an iterable object. It is also callable, and then
    returns a copy of the list of its items. It can be deep-copied or pickled.

    :note: It looks like the Python's set builtin type but does not behave the
        same way. A set builtin, provides a rich set of comparison and logical
        operators whereas this class has none of them. On the contrary, the set
        builtin is not hashable (since set is mutable, it would result in
        inconsistencies between __hash__ and __eq__ which is prohibited) whereas
        the present class is hashable (it just identifies the object).

    """

    def __init__(self, items=tuple(), weak=False, **kw):
        """
        :param items: Any kind of iterable object that will be used to populate
            the catalog
        :param weak: If ``True``, weak references to catalog's items are kept.
        :param kw: Any other named parameters will be added to the catalog's
            attributes
        """
        logger.debug('Abstract %s init', self.__class__)
        self._items = items
        self.weak = weak
        self.__dict__.update(kw)

    @classmethod
    def _from_iterable(cls, iterable):
        return cls(items=iterable)

    @classmethod
    def fullname(cls):
        """Returns a nicely formatted name of the current class (dump usage)."""
        return '{0:s}.{1:s}'.format(cls.__module__, cls.__name__)

    @property
    def filled(self):
        """Boolean value, true if there is at least one item in the catalog."""
        return bool(self._items)

    def _get_weak(self):
        """Boolean value, true if the catalog is built with weak references."""
        return self._weak

    def _set_weak(self, switch):
        """Set boolean value, true if the catalog should be made of weak references."""
        self._weak = bool(switch)
        if self._weak:
            self._items = WeakSet(self._items)
        else:
            self._items = set(self._items)

    weak = property(_get_weak, _set_weak)

    def items(self):
        """Catalog items.

        In python2, a list that contains a copy of the catalog items. In python3;
        an iterator over catalog items.
        """
        return list(self) if six.PY2 else iter(self)

    def __iter__(self):
        """Iterate over catalog items"""
        for c in self._items:
            yield c

    def __call__(self):
        """Return a list that contains a copy of the catalog items."""
        return list(self.items())

    def __len__(self):
        return len(self._items)

    def __contains__(self, item):
        return item in self._items

    def __getstate__(self):
        d = self.__dict__.copy()
        d['_items'] = list(self._items)
        return d

    def __setstate__(self, state):
        self.__dict__.update(state)
        self._items = WeakSet(self._items) if self._weak else set(self._items)

    def add(self, *items):
        """Add the ``item`` entry in the current catalog."""
        for item in items:
            self._items.add(item)

    def pop(self):
        """Remove and return an arbitrary element from the catalog"""
        return self._items.pop()

    def discard(self, bye):
        """Remove the ``bye`` entry from current catalog."""
        self._items.discard(bye)

    def clear(self):
        """Completely clear the list of items previously recorded in this catalog."""
        self._items = WeakSet() if self._weak else set()
