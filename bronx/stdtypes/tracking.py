#!/usr/bin/env python
# -*- coding:Utf-8 -*-

"""
Tools to handle changes in some context.

Changes could be creation, deletion, modification.
"""
from __future__ import absolute_import, division, print_function, unicode_literals

from bronx.compat.moves import collections_abc


class Tracker(object):
    """Handling of simple state status through ``deleted``, ``created`` or ``updated`` items.

    :param collections.abc.Iterable before: The reference list of items
    :param collections.abc.Iterable after: The possibly modified list of items
    :param collections.abc.Iterable deleted: The list of deleted items
    :param collections.abc.Iterable created: The list of created items
    :param collections.abc.Iterable updated: The list of updated items
    :param collections.abc.Iterable unchanged: The list of unchanged items

    There are two way to initialise such an object:

    * Using the *after* and *before* attributes. Doing so, the *deleted*, *created* and
      *unchanged* sets are automatically computed;
    * Using the *deleted*, *created* and *unchanged* attributes in order to setup
      manually those items.

    Example using *after* and *before*::

        >>> a=[1,2,3,4,5,6]
        >>> b=[1,2,4,6,7]
        >>> tracker=Tracker(before=a, after=b)
        >>> tracker.dump()   # doctest: +NORMALIZE_WHITESPACE
        Section deleted: 3, 5
        Section created: 7
        Section updated:
        Section unchanged: 1, 2, 4, 6
        >>> tracker.differences()   # doctest: +NORMALIZE_WHITESPACE
        Section deleted: 3, 5
        Section created: 7
        Section updated:
        >>> list(tracker)
        [1, 2, 3, 4, 5, 6, 7]
        >>> tracker.updated = [2, 4]
        >>> tracker.dump()
        Section deleted: 3, 5
        Section created: 7
        Section updated: 2, 4
        Section unchanged: 1, 6

    Example using *deleted*, *created* and *unchanged*::

        >>> trackbis=Tracker(deleted=[1, 2], created=[3, 4], unchanged=[5, 6], updated=[7, 8])
        >>> trackbis.dump()
        Section deleted: 1, 2
        Section created: 3, 4
        Section updated: 8, 7
        Section unchanged: 5, 6
        >>> trackbis.unchanged = [7, ]
        >>> trackbis.dump()
        Section deleted: 1, 2
        Section created: 3, 4
        Section updated: 8
        Section unchanged: 7

    """

    def __init__(self, before=None, after=None, deleted=None, created=None, updated=None, unchanged=None):
        for args in (before, after, deleted, created, updated, unchanged):
            if args is not None:
                if not (isinstance(args, collections_abc.Iterable) and
                        all([isinstance(item, collections_abc.Hashable) for item in args])):
                    raise ValueError("Whenever provided, arguments must consists of hashable items.")
        if before is not None and after is not None:
            before = frozenset(before)
            after = frozenset(after)
            self._deleted = before - after
            self._created = after - before
            self._unchanged = before & after
        else:
            if deleted is None or created is None:
                raise ValueError("None of the deleted and created attributes should be omitted")
            self._unchanged = frozenset()
            self._updated = frozenset()
            self._set_deleted(deleted)
            self._set_created(created)
            self._set_unchanged(unchanged)
        self._updated = frozenset()
        self._set_updated(updated)

    def __str__(self):
        return '{0:s} | deleted={1:d} created={2:d} updated={3:d} unchanged={4:d}>'.format(
            repr(self).rstrip('>'),
            len(self.deleted), len(self.created), len(self.updated), len(self.unchanged)
        )

    def _get_deleted(self):
        return self._deleted

    def _set_deleted(self, value):
        if value is not None:
            try:
                self._deleted = frozenset(value)
                self._unchanged = self._unchanged - self._deleted
            except TypeError:
                self._deleted = frozenset()

    deleted = property(_get_deleted, _set_deleted, None, "The set of deleted items.")

    def _get_created(self):
        return self._created

    def _set_created(self, value):
        if value is not None:
            try:
                self._created = frozenset(value)
                self._unchanged = self._unchanged - self._created
            except TypeError:
                self._created = frozenset()

    created = property(_get_created, _set_created, None, "The set of created items.")

    def _get_updated(self):
        return self._updated

    def _set_updated(self, value):
        if value is not None:
            try:
                self._updated = frozenset(value)
                self._unchanged = self._unchanged - self._updated
            except TypeError:
                self._updated = frozenset()

    updated = property(_get_updated, _set_updated, None, "The set of updated items.")

    def _get_unchanged(self):
        return self._unchanged

    def _set_unchanged(self, value):
        if value is not None:
            try:
                self._unchanged = frozenset(value)
                self._updated = self._updated - self._unchanged
            except TypeError:
                self._unchanged = frozenset()

    unchanged = property(_get_unchanged, _set_unchanged, None, "The set of unchanged items.")

    def __contains__(self, item):
        return item in self.deleted or item in self.created or item in self.updated or item in self.unchanged

    def __iter__(self):
        for item in self.deleted | self.created | self.updated | self.unchanged:
            yield item

    def __len__(self):
        return len(self.deleted | self.created | self.updated)

    def dump(self, *args):
        """Produce a simple dump report."""
        if not args:
            args = ('deleted', 'created', 'updated', 'unchanged')
        for section in args:
            sectionset = getattr(self, section)
            print('Section {0:s}: {1:s}'.format(section, ', '.join([str(x) for x in sectionset])))

    def differences(self):
        """Dump only created, deleted and updated items."""
        return self.dump('deleted', 'created', 'updated')


class MappingTracker(Tracker):
    """A tracker that compute the differences between two mappings (e.g. dicitonaries).

    :param collections.abc.Mapping before: The reference mapping
    :param collections.abc.Mapping after: The (possibly) modified mapping

    On the contrary to the :class:`Tracker` class, the :data:`deleted`, :data:`created`,
    :data:`updated` and :data:`unchanged` properties are read-only.

    Example::

        >>> a=dict(a=1, b=2, c=3)
        >>> b=dict(b=9, c=3, d=4)
        >>> mtracker=MappingTracker(a, b)
        >>> mtracker.dump()
        Section deleted: a
        Section created: d
        Section updated: b
        Section unchanged: c
        >>> mtracker.differences()
        Section deleted: a
        Section created: d
        Section updated: b
        >>> len(mtracker)
        3
        >>> sorted(mtracker)
        ['a', 'b', 'c', 'd']
        >>> 'c' in mtracker
        True

    """

    def __init__(self, before, after):
        if not isinstance(before, collections_abc.Mapping):
            raise ValueError('The before argument must be some kind of mapping.')
        if not isinstance(after, collections_abc.Mapping):
            raise ValueError('The after argument must be some kind of mapping.')
        super(MappingTracker, self).__init__(before, after)
        super(MappingTracker, self)._set_updated([k for k in self.unchanged if after[k] != before[k]])

    deleted = property(Tracker._get_deleted, None, None, "The set of deleted items.")

    created = property(Tracker._get_created, None, None, "The set of created items.")

    updated = property(Tracker._get_updated, None, None, "The set of updated items.")

    unchanged = property(Tracker._get_unchanged, None, None, "The set of unchanged items.")


if __name__ == '__main__':
    import doctest

    doctest.testmod()
