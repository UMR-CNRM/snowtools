# -*- coding: utf-8 -*-

"""
Useful iterators and associated tools..
"""

from __future__ import print_function, absolute_import, unicode_literals, division

from collections import deque


def pcn(iterable, fillvalue=None):
    """Iterate over **iterable** but also return the previous and next values.

    Example::

        >>> for p, c, n in pcn([]):
        ...     print(p, c, n)
        >>> for p, c, n in pcn([1, ]):
        ...     print(p, c, n)
        None 1 None
        >>> for p, c, n in pcn([1, 2, 3, 4, 5]):
        ...     print(p, c, n)
        None 1 2
        1 2 3
        2 3 4
        3 4 5
        4 5 None
        >>> for p, c, n in pcn([1, 2, None, 4, 5], fillvalue='foo'):
        ...     print(p, c, n)
        foo 1 2
        1 2 None
        2 None 4
        None 4 5
        4 5 foo

    """
    iterator = iter(iterable)
    sentinel = object()

    def _stransform(result):
        return fillvalue if result is sentinel else result

    prev = deque([sentinel, sentinel], maxlen=2)
    try:
        prev.append(next(iterator))
        while True:
            cur = next(iterator)
            yield _stransform(prev[0]), prev[-1], cur
            prev.append(cur)
    except StopIteration:
        if prev[-1] is not sentinel:
            yield _stransform(prev[0]), prev[-1], fillvalue
        return


def izip_pcn(* iterables):
    """Like izip but also returns the Previous, Current and Next values.

    Example::

        >>> for p, c, n in izip_pcn([], []):
        ...     print(p, c, n)
        >>> for p, c, n in izip_pcn([1, ], [10, ]):
        ...     print(p, c, n)
        (None, None) (1, 10) (None, None)
        >>> for p, c, n in izip_pcn([1, 2], [10, 11]):
        ...     print(p, c, n)
        (None, None) (1, 10) (2, 11)
        (1, 10) (2, 11) (None, None)
        >>> for p, c, n in izip_pcn([1, 2, 3, 4], [10, 11, None, 13]):
        ...     print(p, c, n)
        (None, None) (1, 10) (2, 11)
        (1, 10) (2, 11) (3, None)
        (2, 11) (3, None) (4, 13)
        (3, None) (4, 13) (None, None)

    """
    iterators = [pcn(i) for i in iterables]
    try:
        while iterators:
            currents = [next(i) for i in iterators]
            yield [tuple([c[i] for c in currents])
                   for i in (0, 1, 2)]
    except StopIteration:
        return


def interleave(* iterables):
    """Take several iterable objects and iterate in a roundrobin manner.

    Example::

        >>> print(','.join(
        ...     interleave('abcd', '1234', 'ABCD')
        ... ))
        a,1,A,b,2,B,c,3,C,d,4,D
        >>> print(','.join(
        ...     interleave('abcd', '12', 'ABCD')
        ... ))
        a,1,A,b,2,B,c,C,d,D
        >>> print(','.join(
        ...     interleave('ab', '1234', 'ABCD')
        ... ))
        a,1,A,b,2,B,3,C,4,D

    """
    my_iterators = [iter(it) for it in iterables]
    my_index = len(iterables) - 1
    while True:
        found = None
        my_index += 1
        while my_iterators and found is None:
            my_index %= len(my_iterators)
            try:
                found = next(my_iterators[my_index])
            except StopIteration:
                del my_iterators[my_index]
        if found is None:
            return
        else:
            yield found


if __name__ == '__main__':
    import doctest
    doctest.testmod()
