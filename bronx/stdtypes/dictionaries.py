# -*- coding: utf-8 -*-

"""
Structure or dictionary like classes for miscellaneous usage.
"""

from __future__ import print_function, absolute_import, division, unicode_literals

import six

from bronx.compat.moves import collections_abc


class Foo(object):
    """
    Protected C-struct like class... for gathering anything.
    Internal dict methods could be called through i_*methodname* protection.
    """

    def __init__(self, **kw):
        self.__dict__.update(kw)

    def as_dict(self):
        """Returns the object's content as a dictionary."""
        return self.__dict__

    def __getattr__(self, attr):
        if attr.startswith('i_'):
            return getattr(self.__dict__, attr.split('_', 1)[1])
        else:
            raise AttributeError

    def __str__(self):
        return str(self.__dict__)


class ReadOnlyDict(collections_abc.Mapping):
    """A type of read-only dictionary.

    Example::

        >>> rodict = ReadOnlyDict()
        >>> len(rodict)
        0
        >>> rodict['a'] = 1
        Traceback (most recent call last):
          File "<stdin>", line 1, in <module>
        TypeError: 'ReadOnlyDict' object does not support item assignment

        >>> rodict = ReadOnlyDict(dict(a=1, b=2))
        >>> len(rodict)
        2
        >>> rodict['a']
        1
        >>> print(','.join(rodict))
        a,b
        >>> print(','.join([str(x) for k, x in sorted(rodict.items())]))
        1,2

    """

    def __init__(self, data=None):
        if data is None:
            self._data = dict()
        else:
            self._data = dict(data)

    def __getitem__(self, key):
        return self._data[key]

    def __len__(self):
        return len(self._data)

    def __iter__(self):
        return iter(self._data)

    def __repr__(self):
        return repr(self._data)

    def __str__(self):
        return str(self._data)


class SpecialDict(dict):
    """
    Add some special features to std dict especially the ability to remap
    keys on the fly.

    This class behaves like a usual dictionary, to be useful, it should be
    subclassed and the method :meth:`remap` should be redefined in order to
    implement a customised key remaping
    """

    def __init__(self, *kargs, **kwargs):
        tmpdict = dict(*kargs, **kwargs)
        # Check the dictionnary keys. If necessary change them
        for k, v in [(k, v) for k, v in six.iteritems(tmpdict) if k != self.remap(k)]:
            del tmpdict[k]
            tmpdict[self.remap(k)] = v
        super(SpecialDict, self).__init__(tmpdict)

    def show(self, ljust=24):
        """Print the actual values of the dictionary."""
        for k in sorted(self.keys()):
            print('+', k.ljust(ljust), '=', self.get(k))

    def update(self, *args, **kw):
        """Extended dictionary update with args as dict and extra keywords."""
        args = list(args)
        args.append(kw)
        for objiter in args:
            for k, v in objiter.items():
                self.__setitem__(k, v)

    def __call__(self, **kw):
        """Calling a special dict is equivalent to updating."""
        self.update(**kw)

    def remap(self, key):
        """Return a new value for the actual ``key``. Default is identity."""
        return key

    def __getitem__(self, key):
        """Force remapped key retrieval."""
        return dict.__getitem__(self, self.remap(key))

    def __setitem__(self, key, value):
        """Force remapped key setting."""
        dict.__setitem__(self, self.remap(key), value)

    def __delitem__(self, key):
        """Force remapped key deletion."""
        dict.__delitem__(self, self.remap(key))

    def __contains__(self, key):
        """Force remapped key ``in`` checking."""
        return (dict.__contains__(self, key) or  # Try with out a remap first... just in case
                dict.__contains__(self, self.remap(key)))


class LowerCaseDict(SpecialDict):
    """A dictionary with only lower case keys.

    Example::

        >>> lcdict = LowerCaseDict()
        >>> len(lcdict)
        0
        >>> lcdict['Ab'] = 1
        >>> lcdict['cD'] = 2
        >>> 'ab' in lcdict
        True
        >>> 'AB' in lcdict
        True
        >>> 'Ab' in lcdict
        True
        >>> 'abc' in lcdict
        False
        >>> print(','.join(sorted(lcdict.keys())))
        ab,cd
        >>> lcdict.show()
        + ab                       = 1
        + cd                       = 2

    """

    def remap(self, key):
        """Return a lower case value of the actual key."""
        return key.lower()


class UpperCaseDict(SpecialDict):
    """A dictionary with only upper case keys."""

    def remap(self, key):
        """Return a upper case value of the actual key."""
        return key.upper()


if __name__ == '__main__':
    import doctest

    doctest.testmod()
