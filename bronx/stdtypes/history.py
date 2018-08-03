#!/usr/bin/env python
# -*- coding:Utf-8 -*-

"""
Some types to manage history of commands, actions, etc.

TODO: Module documentation + example
TODO: Add a simple unittest
TODO: Fix undocumented properties
"""

from __future__ import print_function, absolute_import, division, unicode_literals
import six

import collections
import datetime


class PrivateHistory(object):
    """Multi-purpose history like object."""

    def __init__(self, queue=list(), maxlen=9999, timer=False):
        self._history = collections.deque(queue, maxlen=maxlen)
        self._count = 0
        self._timer = timer

    @property
    def count(self):
        return self._count

    def _get_timer(self):
        if not hasattr(self, '_timer'):
            self._timer = False
        return self._timer

    def _set_timer(self, value):
        self._timer = bool(value)

    timer = property(_get_timer, _set_timer)

    @property
    def size(self):
        return self._history.maxlen

    @property
    def sizefmt(self):
        return len(str(self.size))

    @property
    def lower_bound(self):
        return self._history[0][0]

    @property
    def upper_bound(self):
        return self._history[-1][0]

    def resize(self, maxlen=None):
        """Resize the internal history log to the specified length."""
        self._history = collections.deque(self._history, maxlen=maxlen)
        return self._history.maxlen

    def nice(self, item):
        """Try to build some nice string of the item."""
        if type(item) is list or type(item) is tuple:
            niceitem = ' '.join([six.text_type(x) for x in item])
        else:
            niceitem = item
        return niceitem

    def __iter__(self):
        for item in self._history:
            yield item

    def __len__(self):
        return len(self._history)

    def __getitem__(self, key):
        if key < self.lower_bound or key > self.upper_bound:
            raise KeyError('Key value out of real history bounds.')
            return None
        else:
            return self._history[int(key) - self.lower_bound]

    def __setitem__(self, key, value):
        raise KeyError('Could not set a value to a history item.')

    def __delitem__(self, key):
        raise KeyError('Could not delete a value of a history item.')

    def grep(self, key):
        """Match the ``key`` in the string representation of history items."""
        return [(count, stamp, item) for count, stamp, item in self._history if key in self.nice(item)]

    def match(self, regex):
        """Match the ``regex`` in a forced string representation of history items."""
        return [(count, stamp, item) for count, stamp, item in self._history if regex.search(self.nice(item))]

    def __contains__(self, key):
        return bool(self.grep(key))

    def stamp(self):
        """Return a time stamp."""
        return datetime.datetime.now()

    def append(self, *items):
        """Add the specified ``items`` as a new history entry."""
        stamp = self.stamp()
        if items:
            self._count += 1
            self._history.append((self._count, stamp, items))
        return (self._count, stamp)

    def get(self, start=1, end=None):
        """
        Extract history entries with a count value contained
        in the inclusive interval ``start`` - ``end``.
        """
        if end is None:
            end = self._count
        return [ (c, t, i) for c, t, i in self._history if start <= c <= end ]

    def fmt_timer(self, stamp, force=False):
        """Return a formatted string of the time stamp suitable for history log."""
        if self.timer or force:
            return stamp.strftime('[%Y-%m-%dT%H:%M:%S]')
        else:
            return ''

    def show(self, start=1, end=None):
        """
        Display a numbered list of history items with a count value contained
        in the inclusive interval ``start`` - ``end``.
        """
        for c, t, i in self.get(start, end):
            print('[{0:{size}d}]{1:s} : {2:s}'.format(c, self.fmt_timer(t), self.nice(i), size=self.sizefmt))

    def showmatch(self, regex):
        """
        Display a selection of history items matching argument ``regex``.
        """
        for c, t, i in self.match(regex):
            print('[{0:{size}d}]{1:s} : {2:s}'.format(c, self.fmt_timer(t), self.nice(i), size=self.sizefmt))

    def showlast(self):
        """Display the last entry of the current history."""
        return self.show(start=self._count)

    def getaround(self, focus, delta=60):
        """
        Extract history entries with a stamp value contained
        in the exclusive interval [``focus`` - ``delta``, ``focus`` + ``delta``].
        """
        delta = datetime.timedelta(0, delta)
        return [(c, t, i) for c, t, i in self._history if abs(t - focus) < delta]

    def around(self, focus=None, delta=60):
        """
        Display a numbered list of history items with a stamp value contained
        in the exclusive interval [``focus`` - ``delta``, ``focus`` + ``delta``].
        """
        if focus is None:
            focus = self.stamp()
        for c, t, i in self.getaround(focus, delta):
            print('[{0:{size}d}]{1:s} : {2:s}'.format(c, self.fmt_timer(t, force=True), self.nice(i), size=self.sizefmt))

    def __call__(self):
        return self.show()

    @property
    def last(self):
        return self._history[-1][-1] if self.count else None

    def getbynumber(self, num):
        return self[num][-1]

    def reset(self):
        """Clear the current history."""
        self._history.clear()
        self._count = 0

    def merge(self, *others):
        """Merge current history with other history objects."""
        pass
