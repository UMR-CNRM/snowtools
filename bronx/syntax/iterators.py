#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Useful iterators and associated tools..
"""



from collections import deque


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
        >>> for p, c, n in izip_pcn([1, 2, 3, 4], [10, 11, 12, 13]):
        ...     print(p, c, n)
        (None, None) (1, 10) (2, 11)
        (1, 10) (2, 11) (3, 12)
        (2, 11) (3, 12) (4, 13)
        (3, 12) (4, 13) (None, None)

    """
    # iterators = map(iter, iterables)
    iterators = [iter(i) for i in iterables]
    void = tuple([None, ] * len(iterables))
    prev = deque([void, void], maxlen=2)
    try:
        while iterators:
            cur = tuple([next(i) for i in iterators])
            if prev[-1] is not void:
                yield prev[0], prev[-1], cur
            prev.append(cur)
    except StopIteration:
        if prev[-1] is not void:
            yield prev[0], prev[-1], void
        return


if __name__ == '__main__':
    import doctest
    doctest.testmod()
