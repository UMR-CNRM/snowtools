# -*- coding: utf-8 -*-

"""
This package defines some useful Design Patterns.

Implementations may be not the most efficient or
thread-safe proof ones.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import bronx.fancies.loggers

__all__ = []

logger = bronx.fancies.loggers.getLogger(__name__)


class Borg(object):
    """A base class for sharing a common state by different objects."""
    __state = {}

    def __new__(cls, *args, **kw):
        logger.debug('Request a borg %s', cls)
        self = object.__new__(cls)
        self.__dict__ = cls.__state
        logger.debug('New borg %s', self)
        return self


class Singleton(object):
    """Obviously a base class for any *real* singleton."""

    def __new__(cls, *args, **kw):
        logger.debug('Request a singleton %s', cls)
        if '_instance' not in cls.__dict__:
            cls._instance = object.__new__(cls)
            logger.debug('Building a brand new singleton %s', cls._instance)
        logger.debug('New singleton %s', cls._instance)
        return cls._instance
