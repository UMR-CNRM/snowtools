# -*- coding:Utf-8 -*-

r"""
A personal implementation of the Observer design pattern.

Using the factory :func:`get` should provide a convenient way to register
to an undetermined number of items hold by :class:`ObserverBoard` objects.

Example::

    # Let's create an observable object
    >>> class A(object):
    ...     def __init__(self, id, status=0):
    ...         self.id = id
    ...         self._status = status
    ...         # Note: the Interface method ``get`` could also be used:
    ...         self._obs = ObserverBoard(tag='DocTestObsBoard')
    ...         self._obs.notify_new(self, {'status': self._status})
    ...     def quit(self):
    ...         self._obs.notify_del(self, {'status': self._status})
    ...     def _get_status(self):
    ...         return self._status
    ...     def _set_status(self, value):
    ...         self._status = value
    ...         self._obs.notify_upd(self, {'status': self._status})
    ...     status = property(_get_status, _set_status)
    ...

    # Let's create an observer
    >>> class MyObserver(Observer):
    ...     def __init__(self):
    ...         self.log = list()
    ...     def newobsitem(self, item, info):
    ...         self.log.append(('NEW', item.id, info['status']))
    ...     def updobsitem(self, item, info):
    ...         self.log.append(('UPD', item.id, info['status']))
    ...     def delobsitem(self, item, info):
    ...         self.log.append(('DEL', item.id, info['status']))
    ...     def __str__(self):
    ...         return '\n'.join(['{:s} {:6s} status={!s}'.format(*s) for s in self.log])
    ...

    # Create an observer object and register it to the observer board
    >>> obs1 = MyObserver()
    >>> obs_board = ObserverBoard(tag='DocTestObsBoard')
    >>> obs_board.register(obs1)

    # Let's create some Observee
    >>> a1 = A('First')
    >>> a2 = A('Second', status=5)
    >>> a2.status = 6
    >>> a1.status = 1
    >>> a1.quit()
    >>> del a1

    >>> a2.status = 'z'

    # obs1 stops listening
    >>> obs_board.unregister(obs1)

    # Create a new observer... it will only record new events...
    >>> obs2 = MyObserver()
    >>> obs_board.register(obs2)

    >>> a3 = A('Third')

    # List the currently Observed objects
    >>> len(obs_board.observed())
    2

    # List the current Observers
    >>> obs_board.observers() # doctest: +ELLIPSIS
    [<...MyObserver object at 0x...>]

    # What did the observers do ?
    >>> print(obs1)
    NEW First  status=0
    NEW Second status=5
    UPD Second status=6
    UPD First  status=1
    DEL First  status=1
    UPD Second status=z
    >>> print(obs2)
    NEW Third  status=0

"""

from __future__ import print_function, absolute_import, division, unicode_literals

import copy

from bronx.fancies import loggers
from bronx.stdtypes import catalog
from bronx.patterns import getbytag

#: No automatic export
__all__ = []

logger = loggers.getLogger(__name__)


def get(**kw):
    """
    Return an :class:`ObserverBoard` objects for the specified tag name
    (a class name for example).
    """
    return ObserverBoard(**kw)


def keys():
    """Return actual tags names of the instantiated :class:`ObserverBoard` objects."""
    return ObserverBoard.tag_keys()


def values():
    """Return actual values of the instantiated :class:`ObserverBoard` objects."""
    return ObserverBoard.tag_values()


def items():
    """Return the items of the :class:`ObserverBoard` objects collection."""
    return ObserverBoard.tag_items()


class Observer(object):
    """
    Pseudo-Interface class.
    The three public methods should be implemented by any Observer object.
    """

    def _debuglogging(self, msg, *kargs):
        logger.debug('Notified %s ' + msg, self, *kargs)

    def newobsitem(self, item, info):
        """A new ``item`` has been created. Some information is provided through the dict ``info``."""
        self._debuglogging('new item %s info %s', item, info)

    def delobsitem(self, item, info):
        """The ``item`` has been deleted. Some information is provided through the dict ``info``."""
        self._debuglogging('manual del item %s info %s', item, info)

    def updobsitem(self, item, info):
        """The ``item`` has been updated. Some information is provided through the dict ``info``."""
        self._debuglogging('upd item %s info %s', item, info)


class ParrotObserver(Observer):
    """Like :class:`Observer` but boosts the verbosity (useful for tests)."""

    def _debuglogging(self, msg, *kargs):
        logger.info('Notified %s ' + msg, self, *kargs)


class SecludedObserverBoard(object):
    """A SecludedObserverBoard provides an indirection for the observing pattern.

    It holds two lists: one list of objects that are observed and
    another list of observers, listening to any creation, deletion
    or update of the observed objects.
    """

    def __init__(self):
        self._listen = catalog.Catalog(weak=True)
        self._items = catalog.Catalog(weak=True)

    def __deepcopy__(self, memo):
        """No deepcopy expected, so ``self`` is returned."""
        return self

    def register(self, remote):
        """
        Push the ``remote`` object to the list of listening objects.
        A listening object should implement the :class:`Observer` interface.
        """
        self._listen.add(remote)

    def observers(self):
        """List of observing objects."""
        return list(self._listen)

    def observed(self):
        """List of observed objects."""
        return list(self._items)

    def unregister(self, remote):
        """Remove the ``remote`` object from the list of listening objects."""
        self._listen.discard(remote)

    def _extended_info(self, info):
        return info

    def notify_new(self, item, info):
        """Notify the listening objects that a new observed object is born."""
        logger.debug('Notify new %s info %s', repr(item), info)
        self._items.add(item)
        for remote in list(self._listen):
            remote.newobsitem(item, self._extended_info(info))

    def notify_del(self, item, info):
        """
        Notify the listening objects that an observed object does not want to be
        observed anymore.

        :note: It is useless to call notify_del from within an observed object
            __del__ method. Indeed, When __del__ is called by the garbage collector,
            the reference count of the observed object is already set to 0 which
            causes the observed object to disappear from self._items (since self._items
            is a WeakSet). In turns, since the observed object is not anymore in
            self._items, this method has no effect.
        """
        if item in self._items:
            logger.debug('Notify del %s info %s', repr(item), info)
            for remote in list(self._listen):
                remote.delobsitem(item, self._extended_info(info))
            self._items.discard(item)

    def notify_upd(self, item, info):
        """Notify the listening objects that an observed object has been updated."""
        if item in self._items:
            logger.debug('Notify upd %s info %s', repr(item), info)
            for remote in list(self._listen):
                remote.updobsitem(item, self._extended_info(info))


class ObserverBoard(SecludedObserverBoard, getbytag.GetByTag):
    """
    Like a :class:`SecludedObserverBoard` but using the :class:`footprints.util.GetByTag`
    class to provide an easy access to existing boards.
    """

    def _extended_info(self, info):
        fullinfo = copy.copy(info)  # This is only a shallow copy...
        fullinfo['observerboard'] = self.tag
        return fullinfo


if __name__ == '__main__':
    import doctest
    doctest.testmod()
