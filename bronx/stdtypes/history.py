# -*- coding: utf-8 -*-

"""
Some types to manage history of commands, actions, etc.
"""

from __future__ import print_function, absolute_import, division, unicode_literals
import six

import collections
import datetime
import re

from bronx.patterns import getbytag


class PrivateHistory(object):
    r"""Multi-purpose history like object.

    Items added to an History object are recorded along with a number (starting
    from 1 and incremented each time an item is added) and a timestamp.

    Available history items can be listed searched and printed.

    Thanks to the ``maxlen`` parameter, the history object retains only a limited
    number of items (only the ``maxlen`` latest ones). This mechanism prevent from
    an excessive memory consumption.

    Example::

        # Initalise an History object that will retain at most 3 entries
        # (if not specified, default for maxlen is 9999)
        >>> hst = PrivateHistory(maxlen=3)
        >>> hst.size
        3

        # Simply add four entries...
        >>> hst.append("Entry 1") # doctest: +ELLIPSIS
        (1, datetime.datetime(...))
        >>> len(hst)
        1
        >>> hst.append("Entry 2") # doctest: +ELLIPSIS
        (2, datetime.datetime(...))
        >>> hst.append("Entry 3") # doctest: +ELLIPSIS
        (3, datetime.datetime(...))
        >>> hst.append("Entry 4", "as a tuple") # doctest: +ELLIPSIS
        (4, datetime.datetime(...))

        # This objects, auto-limits itself to self.size/maxlen
        >>> len(hst)
        3
        >>> hst.lower_bound
        2
        >>> hst.upper_bound
        4

        # It is iterable and individual elements can be accessed
        >>> list(hst) # doctest: +ELLIPSIS +NORMALIZE_WHITESPACE
        [(2, datetime.datetime(...), (...'Entry 2',)),
         (3, datetime.datetime(...), (...'Entry 3',)),
         (4, datetime.datetime(...), (...'Entry 4', ...'as a tuple'))]
        >>> hst[3] # doctest: +ELLIPSIS
        (3, datetime.datetime(...), (...'Entry 3',))

        # Simple searches are possible
        >>> hst.grep('a tup') # doctest: +ELLIPSIS
        [(4, datetime.datetime(...), (...'Entry 4', ...'as a tuple'))]
        >>> hst.match(r'En\w+\s+[24]') # doctest: +ELLIPSIS +NORMALIZE_WHITESPACE
        [(2, datetime.datetime(...), (...'Entry 2',)),
         (4, datetime.datetime(...), (...'Entry 4', ...'as a tuple'))]

        # Formatted output are do-able
        >>> hst.show()
        [2] : Entry 2
        [3] : Entry 3
        [4] : Entry 4 as a tuple
        >>> hst.show(2, 3)
        [2] : Entry 2
        [3] : Entry 3
        >>> hst.showgrep('a tup')
        [4] : Entry 4 as a tuple

        # With the timestamp
        >>> hst.timer = True
        >>> hst.showlast() # doctest: +ELLIPSIS
        [4][...] : Entry 4 as a tuple

    """

    def __init__(self, queue=tuple(), maxlen=9999, timer=False):
        """
        :param queue: Pre-existing history items
        :param maxlen: The maximum size of the history object (see the comment above)
        :param timer: Add a timestamp when showing history entries
        """
        self._history = collections.deque(queue, maxlen=maxlen)
        self._count = 0
        self._timer = timer

    @property
    def count(self):
        """The current value of the history entry counter."""
        return self._count

    def _get_timer(self):
        return self._timer

    def _set_timer(self, value):
        self._timer = bool(value)

    timer = property(_get_timer, _set_timer,
                     doc="Add a timestamp when showing history entries.")

    @property
    def size(self):
        """The maximum number of entries retained in the preent object."""
        return self._history.maxlen

    @property
    def _sizefmt(self):
        return len(str(self.size))

    @property
    def lower_bound(self):
        """The number of oldest history entry retained in the object"""
        return self._history[0][0]

    @property
    def upper_bound(self):
        """The number of latest history entry in the object"""
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
        """Iterate over history entries."""
        for item in self._history:
            yield item

    def __len__(self):
        """The the amount of history entries currently retained in this object."""
        return len(self._history)

    def __getitem__(self, key):
        """Return an history entry given its number"""
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
        """Lookup for ``key`` in the string representation of history items."""
        return [(count, stamp, item) for count, stamp, item in self._history if key in self.nice(item)]

    def match(self, regex):
        """Match the ``regex`` in the string representation of history items."""
        regex = re.compile(regex)
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
        return [(c, t, i) for c, t, i in self._history if start <= c <= end]

    def _fmt_timer(self, stamp, force=False):
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
            print('[{0:{size}d}]{1:s} : {2:s}'.format(c, self._fmt_timer(t), self.nice(i), size=self._sizefmt))

    def showgrep(self, key):
        """
        Display a selection of history items containing the ``key``.
        """
        for c, t, i in self.grep(key):
            print('[{0:{size}d}]{1:s} : {2:s}'.format(c, self._fmt_timer(t), self.nice(i), size=self._sizefmt))

    def showmatch(self, regex):
        """
        Display a selection of history items matching argument ``regex``.
        """
        for c, t, i in self.match(regex):
            print('[{0:{size}d}]{1:s} : {2:s}'.format(c, self._fmt_timer(t), self.nice(i), size=self._sizefmt))

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
            print('[{0:{size}d}]{1:s} : {2:s}'.format(c,
                                                      self._fmt_timer(t, force=True),
                                                      self.nice(i),
                                                      size=self._sizefmt))

    def __call__(self):
        """Display the whole history as a numbered list."""
        return self.show()

    @property
    def last(self):
        """Return the last history entry."""
        return self._history[-1][-1] if self.count else None

    def getbynumber(self, num):
        """Get an history entry knowing its internal number."""
        return self[num][-1]

    def reset(self):
        """Clear the current history."""
        self._history.clear()
        self._count = 0

    def merge(self, *others):
        """Merge current history with other history objects."""
        pass


class History(PrivateHistory, getbytag.GetByTag):
    """Shared Multi-purpose history like object.

    It is managed by :class:`bronx.patterns.getbytag.GetByTag` consequently
    a `tag` parameter needs to be added to the constructor.
    """
    pass


if __name__ == '__main__':
    import doctest
    doctest.testmod()
