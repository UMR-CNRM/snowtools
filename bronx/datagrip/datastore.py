# -*- coding: utf-8 -*-

"""
A simplified key/value embedded-database.

See the :class:`DataStore` class docstring for an example.
"""

from __future__ import print_function, absolute_import, unicode_literals, division
import six

import collections
import functools
import io
import pickle

#: No automatic export
__all__ = []


class _DataStoreEntryKey(object):
    """The key of any element stored in a DataStore class."""

    def __init__(self, kind, **kwargs):
        """
        :param object kind: The `kind` of data (must be hashable)
        :param dict kwargs: Any key/value pairs that describe the data (values
            must be hashable)
        """
        self._kind = kind
        self._extras = kwargs
        try:
            self._hash = self._compute_hash()
        except TypeError:
            raise TypeError('kind and extra arguments values must be hashable.')

    def _compute_hash(self):
        """Get the hash values (it's precomputed)."""
        return hash(tuple([self._kind, ] +
                          [(k, v) for k, v in sorted(self._extras.items())]))

    def __getstate__(self):
        return dict(_kind=self._kind, _extras=self._extras)

    def __setstate__(self, state):
        self._kind = state['_kind']
        self._extras = state['_extras']
        self._hash = self._compute_hash()

    def __repr__(self):
        """Return a string representation of the present object."""
        return '<{:s} object | {!s}>'.format(self.__class__.__name__, self)

    def __str__(self):
        """Return a string representation of the present object."""
        extras_str = (' ' + ' '.join(['{!s}={!r}'.format(k, v) for k, v in self])).rstrip()
        return 'kind={!r}{:s}'.format(self.kind, extras_str)

    @property
    def kind(self):
        """The kind of the data."""
        return self._kind

    @property
    def extras(self):
        """Dictionary of key/value pairs that describe the data."""
        return self._extras.copy()  # This way, the user won't mess up things

    def __iter__(self):
        """Iterates through *extras*."""
        for k, v in six.iteritems(self._extras):
            yield (k, v)

    def __eq__(self, other):
        """Compare to keys."""
        return self.kind == other.kind and self._extras == other.extras

    def __hash__(self):
        """Return the hash value of the present object."""
        return self._hash

    def __getattr__(self, key):
        """Elements of extra are directly accessible."""
        if key.startswith('_'):
            raise AttributeError('Attribute not found')
        if key in self._extras:
            return self._extras[key]
        else:
            raise AttributeError('Attribute not found')


class DataStore(object):
    """An object that can store any pickable data. It acts like a small
    key/value database.

    * Keys are of :class:`_DataStoreEntryKey` class. They contain a
      mandatory `kind` attribute plus key/value pairs that describe the stored
      data more precisely.
    * Various methods are provided to access the entries.
    * Keys are indexed in order to perform fast searches (see the grep method).

    Data should always be pickalable so that the DataStore could be dumped to
    disk using the :meth:`pickle_dump` method.

    :example: Data should be inserted this way::

            ds = DataStore()
            ds.insert('kind_of_data', dict(key1='meaningful'),
                      'The data themselves...', readonly=True)
            ds.insert('kind_of_data', dict(key1='meaningful', key2='breathtaking'),
                      'More date...', readonly=True)
            ds.insert('kind_of_data', dict(), 'Another One', readonly=True)

        It could later be accessed::

            data = ds.get('kind_of_data', dict(key1='meaningful', key2='breathtaking'))
            print data
            More date...

        A search can be performed::

            dict_of_results = ds.grep('kind_of_data', dict(key1='meaningful'))
            print dict_of_results
            {<_DataStoreEntryKey object | kind='kind_of_data' key1='meaningful' key2='breathtaking'>: 'More date...',
             <_DataStoreEntryKey object | kind='kind_of_data' key1='meaningful'>: 'The data themselves...'}

        Finally the DataStore can be dumped/loaded to/from disk::

            ds.pickle_dump()
            another_ds = DataStore()
            another_ds.pickle_load()

    """

    _PICKLE_PROTOCOL = pickle.HIGHEST_PROTOCOL

    def __init__(self, default_picklefile='datastore.pickled'):
        """
        :param str default_picklefile: default name for the pickle dump file
        """
        self._pickle_dumpfile = default_picklefile
        self._reset_internal_state()

    def _reset_internal_state(self):
        self._store = dict()
        self._lock = dict()
        self._index = collections.defaultdict(functools.partial(collections.defaultdict,
                                                                set))

    def _index_update(self, key):
        self._index['kind'][key.kind].add(key)
        for k, v in key:
            self._index[k][v].add(key)

    def _index_remove(self, key):
        self._index['kind'][key.kind].remove(key)
        for k, v in key:
            self._index[k][v].remove(key)

    def _build_key(self, kind, extras):
        if not isinstance(extras, dict):
            raise ValueError("The 'extras' needs to be dictionary of hashables.")
        return _DataStoreEntryKey(kind, **extras)

    def insert(self, kind, extras, payload, readonly=True):
        """Insert a new ``payload`` data in the current DataStore.

        :param object kind: The kind of the ``payload`` data
        :param dict extras: Any key/value pairs that describe the ``payload`` data
        :param object payload: The data that will be stored
        :param bool readonly: Is the data readonly ?
        """
        key = self._build_key(kind, extras)
        if key in self._store and self._lock[key]:
            raise RuntimeError("This entry already exists and is read-only.")
        self._index_update(key)
        self._store[key] = payload
        self._lock[key] = readonly
        return payload

    def check(self, kind, extras):
        """Check if a data described by ``kind`` and ``extras`` exists in this DataStore.

        :param object kind: The kind of the expected data
        :param dict extras: Any key/value pairs that describe the expected data
        """
        key = self._build_key(kind, extras)
        return key in self._store

    def get(self, kind, extras, default_payload=None, readonly=True):
        """Retrieve data from the current DataStore.

        if the desired data is missing and ``default_payload`` is not `None`, a
        new entry is added to the DataStore using the ``default_payload`` and
        ``readonly`` arguments.

        :param object kind: The kind of the expected data
        :param dict extras: Any key/value pairs that describe the expected data
        :param object default_payload: Default data that may be stored and returned
        :param bool readonly: Is the default data readonly ?
        """
        key = self._build_key(kind, extras)
        try:
            return self._store[key]
        except KeyError:
            if default_payload is None:
                raise KeyError("No corresponding entry was found in the DataStore for {!r}".
                               format(key))
            else:
                self.insert(kind, extras, default_payload, readonly=readonly)
                return self._store[key]

    def delete(self, kind, extras, force=False):
        """Delete data from the current DataStore.

        :param object kind: The kind of the expected data
        :param dict extras: Any key/value pairs that describe the expected data
        """
        key = self._build_key(kind, extras)
        if not self._lock[key] or force:
            self._index_remove(key)
            del self._store[key]
            del self._lock[key]
        else:
            raise RuntimeError("This entry already exists and is read-only.")

    def grep(self, kind, extras):
        """Search for items that matches both ``kind`` and ``extras``.

        :note: When matching ``extras``, supernumerary attributes are ignored
            (e.g. ``extras=dict(a=1)`` will match ``dict(a=1, b=2)``)

        :param object kind: The kind of the expected data
        :param dict extras: Any key/value pairs that describe the expected data
        """
        if not isinstance(extras, dict):
            raise ValueError("The 'extras' needs to be dictionary of hashables.")
        result = self._index['kind'][kind].copy()
        for k, v in six.iteritems(extras):
            result &= self._index[k][v]
        return {k: self._store[k] for k in result}

    def grep_delete(self, kind, extras, force=False):
        """Search for items that matches both ``kind`` and ``extras`` and delete them.

        The dictionary of the removed key/data is returned.

        :note: When matching ``extras``, supernumerary attributes are ignored
            (e.g. ``extras=dict(a=1)`` will match ``dict(a=1, b=2)``)

        :param object kind: The kind of the expected data
        :param dict extras: Any key/value pairs that describe the expected data
        """
        grep = self.grep(kind, extras)
        for k in grep.keys():
            if not self._lock[k] or force:
                self._index_remove(k)
                del self._store[k]
                del self._lock[k]
            else:
                raise RuntimeError("This entry already exists and is read-only.")
        return grep

    def pickle_dump(self, dumpfile=None):
        """Pickle the content of the current DataStore and write it to disk.

        :param str dumpfile: Path to the dump file (if `None`, the default provided
            at the object creation time is used).
        """
        thefile = dumpfile or self._pickle_dumpfile
        with io.open(thefile, 'wb') as pfh:
            pickle.dump((self._store, self._lock), pfh,
                        protocol=self._PICKLE_PROTOCOL)

    def pickle_load(self, dumpfile=None):
        """Read a pickle dump file from disk and refill the current DataStore.

        :param str dumpfile: Path to the dump file (if `None`, the default provided
            at the object creation time is used).
        """
        # Get the pickle file contents
        thefile = dumpfile or self._pickle_dumpfile
        with io.open(thefile, 'rb') as pfh:
            unpickled = pickle.load(pfh)
        # Build the new store dictionary
        newstore = dict()
        for k, v in six.iteritems(unpickled[0]):
            if k in self._store and hasattr(self._store[k], 'datastore_inplace_overwrite'):
                # In some particular cases, we want the an existing object to
                # reset itself. I guess we could call that an inplace overwrite
                self._store[k].datastore_inplace_overwrite(v)
                newstore[k] = self._store[k]
            else:
                newstore[k] = v
        # Update internals and rebuild the index
        self._reset_internal_state()
        self._store = newstore
        self._lock = unpickled[1]
        for k in six.iterkeys(self._store):
            self._index_update(k)

    def keys(self):
        """Return the list of available keys in this DataStore."""
        return self._store.keys()

    def __iter__(self):
        """Iterate over the DataStore's items."""
        for k, v in six.iteritems(self._store):
            # Return copies of keys so that the _index WeakSet remain unperturbed
            yield (k, v)

    def __len__(self):
        """The number of entries in the present DataStore."""
        return len(self._store)

    def __repr__(self):
        """A condensed string representation of the present DataStore."""
        return '<{:s} object at {!s} | {:d} items>'.format(self.__class__.__name__,
                                                           hex(id(self)),
                                                           len(self))

    def __str__(self):
        """A condensed string representation of the present DataStore."""
        outstr = ''
        for k, v in self:
            outstr += '{:10s} key  : {!s}\n'.format('read-only' if self._lock[k] else 'read-write', k)
            outstr += '{:10s} value: {!r}\n'.format('', v)
        return outstr
