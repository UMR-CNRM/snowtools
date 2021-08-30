# -*- coding: utf-8 -*-

"""
Tools to handle changes in some context.

Changes could be creation, deletion, modification.
"""
from __future__ import absolute_import, division, print_function, unicode_literals

import collections

from bronx.compat.moves import collections_abc
from bronx.fancies.dump import OneLineTxtDumper


class Tracker(object):
    """Handling of simple state status through ``deleted``, ``created`` or ``updated`` items.

    :param collections.abc.Iterable before: The reference list of items
    :param collections.abc.Iterable after: The possibly modified list of items
    :param collections.abc.Iterable deleted: The list of deleted items
    :param collections.abc.Iterable created: The list of created items
    :param collections.abc.Iterable updated: The list of updated items
    :param collections.abc.Iterable unchanged: The list of unchanged items
    :param sectionlabel: The display name of items

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

    def __init__(self, before=None, after=None, deleted=None, created=None, updated=None, unchanged=None,
                 sectionlabel='Section'):
        self._sectionlabel = sectionlabel
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

    def dump_str(self, *args, **kwargs):
        """Produce a simple report string."""
        result = list()
        with_empty = kwargs.pop('with_empty', True)
        if not args:
            args = ('deleted', 'created', 'updated', 'unchanged')
        for section in args:
            sectionset = getattr(self, section)
            if with_empty or sectionset:
                result.append('{0:s} {1:s}: {2:s}'.format(self._sectionlabel,
                                                          section,
                                                          ', '.join([str(x) for x in sectionset])))

        return '\n'.join(result)

    def dump(self, *args):
        """Produce a simple dump report."""
        print(self.dump_str(*args))

    def differences(self):
        """Dump only created, deleted and updated items."""
        result = self.dump_str('deleted', 'created', 'updated', with_empty=False)
        print(result if result else 'No differences')


class MappingTracker(Tracker):
    """A tracker that compute the differences between two mappings (e.g. dictionaries).

    :param collections.abc.Mapping before: The reference mapping
    :param collections.abc.Mapping after: The (possibly) modified mapping

    On the contrary to the :class:`Tracker` class, the :data:`deleted`, :data:`created`,
    :data:`updated` and :data:`unchanged` properties are read-only.

    Example::

        >>> a=dict(a=1, b=2, c=3)
        >>> b=dict(b=9, c=3, d=4)
        >>> mtracker=MappingTracker(a, b)
        >>> mtracker.dump()
        Item deleted: a
        Item created: d
        Item updated: b
        Item unchanged: c
        >>> mtracker.differences()
        Item deleted: a
        Item created: d
        Item updated: b
        >>> len(mtracker)
        3
        >>> sorted(mtracker)
        ['a', 'b', 'c', 'd']
        >>> 'c' in mtracker
        True

    """

    def __init__(self, before, after):
        if not isinstance(before, collections_abc.Mapping):
            raise ValueError('The before argument must be some kind of mapping (got {!s}).'
                             .format(type(before)))
        if not isinstance(after, collections_abc.Mapping):
            raise ValueError('The after argument must be some kind of mapping (got {!s}).'
                             .format(type(before)))
        super(MappingTracker, self).__init__(before, after, sectionlabel='Item')
        super(MappingTracker, self)._set_updated([k for k in self.unchanged if after[k] != before[k]])

    deleted = property(Tracker._get_deleted, None, None, "The set of deleted items.")

    created = property(Tracker._get_created, None, None, "The set of created items.")

    updated = property(Tracker._get_updated, None, None, "The set of updated items.")

    unchanged = property(Tracker._get_unchanged, None, None, "The set of unchanged items.")


SimpleDifference = collections.namedtuple('SimpleDiffernce', ('before', 'after'))


class RecursiveMappingTracker(MappingTracker):
    """
    A tracker that compute that recursively compute differences between
    two mappings (e.g. dictionaries).

    :param collections.abc.Mapping before: The reference mapping
    :param collections.abc.Mapping after: The (possibly) modified mapping

    On the contrary to the :class:`Tracker` class, the :data:`deleted`, :data:`created`,
    :data:`updated` and :data:`unchanged` properties are read-only.

    Example::

        >>> a = dict(a=1, b=dict(b1=1, b2=2, b3={1, 2}), c=0)
        >>> b = dict(a=1, b=dict(b1=1, b2=3, b3={1, 3}, b4='x'), d=0)
        >>> mtracker=RecursiveMappingTracker(a, b)
        >>> mtracker.dump_legend()
        (legend: created: "+"  deleted: "-"  unchanged: "="  updated: "?")
        >>> mtracker.differences()
        - c: 0
        + d: 0
        ? b:
          | + b4: 'x'
          | ? b2: before=2 after=3
          | ? b3:
          |   | Set's item deleted: 2
          |   | Set's item created: 3
        >>> mtracker.deleted_data == {'c': 0}
        True
        >>> mtracker.created_data == {'d': 0}
        True
        >>> mtracker.unchanged_data == {'a': 1}
        True
        >>> set(mtracker.updated_data.keys()) == {'b'}
        True

    The `updated_data` one and only item (`b`) is itself a
    :class:`RecursiveMappingTracker` object:

        >>> mtracker.updated_data['b'].dump()
        + b4: 'x'
        = b1: 1
        ? b2: before=2 after=3
        ? b3:
          | Set's item deleted: 2
          | Set's item created: 3

    """

    _level_indent = '  | '
    _default_dumper = OneLineTxtDumper(tag='bronx_recursive_tracking')
    _dump_tr = dict(deleted='-', created='+', updated='?', unchanged='=')

    def __init__(self, before, after):
        super(RecursiveMappingTracker, self).__init__(before, after)
        self._created_data = {k: after[k] for k in self.created}
        self._deleted_data = {k: before[k] for k in self.deleted}
        self._unchanged_data = {k: after[k] for k in self.unchanged}
        self._updated_data = dict()
        for k_changed in self.updated:
            k_before = before[k_changed]
            k_after = after[k_changed]
            if isinstance(k_before, set) and isinstance(k_after, set):
                self._updated_data[k_changed] = Tracker(k_before, k_after, sectionlabel="Set's item")
            elif (isinstance(k_before, collections_abc.Mapping) and
                  isinstance(k_after, collections_abc.Mapping)):
                self._updated_data[k_changed] = RecursiveMappingTracker(k_before, k_after)
            else:
                self._updated_data[k_changed] = SimpleDifference(k_before, k_after)

    @property
    def created_data(self):
        """A mapping of created items (present in **after** but not in **before**)."""
        return self._created_data

    @property
    def deleted_data(self):
        """A mapping of deleteted items (present in **before** but not in **after**)."""
        return self._deleted_data

    @property
    def unchanged_data(self):
        """A mapping of items available and identical in both **before** and **after**."""
        return self._unchanged_data

    @property
    def updated_data(self):
        """
        A mapping of items available in both **before** and **after** but with
        different values.
        """
        return self._updated_data

    def dump_str(self, *args, **kwargs):
        """Produce a simple report string."""
        result = list()
        with_empty = kwargs.pop('with_empty', True)
        if not args:
            args = sorted(self._dump_tr.keys())
        for section in args:
            sectionset = getattr(self, section)
            if with_empty or sectionset:
                sectiondata = getattr(self, section + '_data')
                if section == 'updated':
                    for k, v in sorted(sectiondata.items()):
                        if isinstance(v, SimpleDifference):
                            result.append('{0:s} {1:s}: before={2:s} after={3:s}'.format(
                                self._dump_tr.get(section, ' '),
                                k,
                                self._default_dumper.dump(v.before),
                                self._default_dumper.dump(v.after),
                            ))
                        else:
                            xresult = v.dump_str('deleted', 'created', 'updated', with_empty=False)
                            xresult = '\n'.join([self._level_indent + l for l in xresult.split('\n')])
                            result.append('{0:s} {1:s}:\n{2:s}'.format(
                                self._dump_tr.get(section, ' '), k, xresult
                            ))
                else:
                    for k, v in sorted(sectiondata.items()):
                        result.append('{0:s} {1:s}: {2:s}'.format(
                            self._dump_tr.get(section, ' '), k, self._default_dumper.dump(v)
                        ))
        return '\n'.join(result)

    def dump_legend(self):
        """The "legend" of the result of a :meth:`dump` call."""
        print('(legend: ' +
              '  '.join(['{:s}: "{:s}"'.format(k, v)
                         for k, v in sorted(self._dump_tr.items())]) +
              ')')

    def dump(self, *args):
        """Produce a simple dump report."""
        super(RecursiveMappingTracker, self).dump(*args)
        self._default_dumper.reset()


if __name__ == '__main__':
    import doctest

    doctest.testmod()
