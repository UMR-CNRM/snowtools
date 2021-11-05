# -*- coding: utf-8 -*-

"""
The :class:`Random` class from this module will produce results that are
identical to the Python2 version.

If you do not care about random number generation reproducibility, please use
the bare :class:`random.Random` class.
"""

from __future__ import absolute_import, division, print_function, unicode_literals

import random as _barerandom
from math import ceil as _ceil, log as _log

import six

if six.PY3:

    class Random(_barerandom.Random):
        """The Python3 version of the Python's 2.7 Random class.

        This is mostly a cut&paste of Python2.7' code. Sorry for that.
        """

        def seed(self, a=None):
            """Call seed() with the version=1 option.

            :note: This will only be reproducible if a is an int or a string.
            """
            super().seed(a=a, version=1)

        # -------------------- integer methods  -------------------

        def randrange(self, start, stop=None, step=1, _int=int, _maxwidth=1 << _barerandom.BPF):
            """Choose a random item from range(start, stop[, step]).

            This fixes the problem with randint() which includes the
            endpoint; in Python this is usually not what you want.

            :note: This is a cut&paste of the old Python2 code !
            """
            # This code is a bit messy to make it fast for the
            # common case while still doing adequate error checking.
            istart = _int(start)
            if istart != start:
                raise ValueError("non-integer arg 1 for randrange()")
            if stop is None:
                if istart > 0:
                    if istart >= _maxwidth:
                        return self._randbelow(istart)
                    return _int(self.random() * istart)
                raise ValueError("empty range for randrange()")

            # stop argument supplied.
            istop = _int(stop)
            if istop != stop:
                raise ValueError("non-integer stop for randrange()")
            width = istop - istart
            if step == 1 and width > 0:
                # Note that
                #     int(istart + self.random()*width)
                # instead would be incorrect.  For example, consider istart
                # = -2 and istop = 0.  Then the guts would be in
                # -2.0 to 0.0 exclusive on both ends (ignoring that random()
                # might return 0.0), and because int() truncates toward 0, the
                # final result would be -1 or 0 (instead of -2 or -1).
                #     istart + int(self.random()*width)
                # would also be incorrect, for a subtler reason:  the RHS
                # can return a long, and then randrange() would also return
                # a long, but we're supposed to return an int (for backward
                # compatibility).

                if width >= _maxwidth:
                    return _int(istart + self._randbelow(width))
                return _int(istart + _int(self.random() * width))
            if step == 1:
                raise ValueError("empty range for randrange(): {:d}, {:d}, {:d}".format(istart, istop, width))

            # Non-unit step argument supplied.
            istep = _int(step)
            if istep != step:
                raise ValueError("non-integer step for randrange()")
            if istep > 0:
                n = (width + istep - 1) // istep
            elif istep < 0:
                n = (width + istep + 1) // istep
            else:
                raise ValueError("zero step for randrange()")

            if n <= 0:
                raise ValueError("empty range for randrange()")

            if n >= _maxwidth:
                return istart + istep * self._randbelow(n)
            return istart + istep * _int(self.random() * n)

        # -------------------- sequence methods  -------------------

        def choice(self, seq):
            """Choose a random element from a non-empty sequence."""
            return seq[int(self.random() * len(seq))]  # raises IndexError if seq is empty

        def shuffle(self, x, random=None):
            """x, random=random.random -> shuffle list x in place; return None.

            Optional arg random is a 0-argument function returning a random
            float in [0.0, 1.0); by default, the standard random.random.

            """
            if random is None:
                random = self.random
            _int = int
            for i in reversed(range(1, len(x))):
                # pick an element in x[:i+1] with which to exchange x[i]
                j = _int(random() * (i + 1))
                x[i], x[j] = x[j], x[i]

        def sample(self, population, k):
            """Chooses k unique random elements from a population sequence.

            Returns a new list containing elements from the population while
            leaving the original population unchanged.  The resulting list is
            in selection order so that all sub-slices will also be valid random
            samples.  This allows raffle winners (the sample) to be partitioned
            into grand prize and second place winners (the subslices).

            Members of the population need not be hashable or unique.  If the
            population contains repeats, then each occurrence is a possible
            selection in the sample.

            To choose a sample in a range of integers, use xrange as an argument.
            This is especially fast and space efficient for sampling from a
            large population:   sample(xrange(10000000), 60)
            """
            # Sampling without replacement entails tracking either potential
            # selections (the pool) in a list or previous selections in a set.

            # When the number of selections is small compared to the
            # population, then tracking selections is efficient, requiring
            # only a small set and an occasional reselection.  For
            # a larger number of selections, the pool tracking method is
            # preferred since the list takes less space than the
            # set and it doesn't suffer from frequent reselections.
            n = len(population)
            if not 0 <= k <= n:
                raise ValueError("sample larger than population")
            random = self.random
            _int = int
            result = [None] * k
            setsize = 21  # size of a small set minus size of an empty list
            if k > 5:
                setsize += 4 ** _ceil(_log(k * 3, 4))  # table size for big sets
            if n <= setsize or hasattr(population, "keys"):
                # An n-length list is smaller than a k-length set, or this is a
                # mapping type so the other algorithm wouldn't work.
                pool = list(population)
                for i in range(k):  # invariant:  non-selected at [0,n-i)
                    j = _int(random() * (n - i))
                    result[i] = pool[j]
                    pool[j] = pool[n - i - 1]  # move non-selected item into vacancy
            else:
                try:
                    selected = set()
                    selected_add = selected.add
                    for i in range(k):
                        j = _int(random() * n)
                        while j in selected:
                            j = _int(random() * n)
                        selected_add(j)
                        result[i] = population[j]
                except (TypeError, KeyError):  # handle (at least) sets
                    if isinstance(population, list):
                        raise
                    return self.sample(tuple(population), k)
            return result

else:

    class Random(_barerandom.Random):
        """This is just the bare :class:random.Random` class."""

        pass
