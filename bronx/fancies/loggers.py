#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This module provides a few functions on top of the standard logging module in
order to easily create new loggers (including root ones) and control their
verbosity level.

The interface to this module is made of two functions: :func:`getLogger` and
:func:`setGlobalLevel`.

Example::

    # Create a main logger (a :class:`logging.Logger` object is returned)
    >>> totolog = getLogger('toto')

    # From now and on, the root logger 'toto' is created and properly initialised
    # (i.e. a console handler is added to it and a :class:`LoggingFilter` instance
    # is set as filter.

    # its name has been recording in the 'roots' module level variable
    # as well as in the 'lognames' module level variable
    >>> 'toto' in roots
    True
    >>> 'toto' in lognames
    True

    # It can be fetched a second time (for example in another module): the same
    # object will be returned
    >>> totolog2 = getLogger('toto')
    >>> totolog2 is totolog
    True

    >>> totolog.debug('By default, only info messages are shown')
    >>> totolog.info('This will show up !')

    # The logging level for all root loggers can easily be changed (at once)
    >>> newlevel = setGlobalLevel('warning')
    >>> newlevel == logging.WARNING
    True

    # One can still set the verbosity level per logger
    >>> getLogger('toto').setLevel(logging.ERROR)

    # Sub-loggers can easily be created
    >>> totosublog = getLogger('toto.sub')

     # its name has been recording in the 'lognames' module level variable
    >>> 'toto.sub' in lognames
    True

"""



import six

import contextlib
import logging

from bronx.syntax.decorators import nicedeco

#: No automatic export
__all__ = []

#: The actual set of pseudo-root loggers created
roots = set()
lognames = set()

#: Default formatters
formats = dict(
    default = logging.Formatter(
        fmt = '# [%(asctime)s][%(name)s][%(funcName)s:%(lineno)04d][%(levelname)s]: %(message)s',
        datefmt = '%Y/%m/%d-%H:%M:%S',
    ),
    fixsize = logging.Formatter(
        fmt = '# [%(asctime)s][%(name)-24s][%(funcName)16s:%(lineno)04d][%(levelname)9s]: %(message)s',
        datefmt = '%Y/%m/%d-%H:%M:%S',
    ),
)

#: Console handler
console = logging.StreamHandler()
console.setLevel(logging.DEBUG)
console.setFormatter(formats['default'])


# MAIN INTERFACES TO THIS MODULE

def getLogger(modname):
    """Return a standard logger in the scope of an appropriate root logger."""
    rootname = modname.split('.')[0]
    rootlogger = logging.getLogger(rootname)
    if rootname not in roots:
        setRootLogger(rootlogger)
    lognames.add(modname)
    if rootname == modname:
        return rootlogger
    else:
        return logging.getLogger(modname)


def setGlobalLevel(level):
    """
    Explicitly sets the logging level to the ``level`` value for all roots items.
    """
    thislevel = getActualLevel(level)
    if thislevel is None:
        print('ERROR!!! Try to set an unknown log level {:s}'.format(level))
    else:
        for rootname in roots:
            r_logger = logging.getLogger(rootname)
            r_logger.setLevel(thislevel)
    return thislevel


@contextlib.contextmanager
def contextboundGlobalLevel(level):
    """
    Within this context manager, explicitly sets the logging level to the
    ``level`` value for all roots items.

    When the context exists, logging levels are restored to their previous values.
    """
    thislevel = getActualLevel(level)
    if thislevel is None:
        print('ERROR!!! Try to set an unknown log level {:s}'.format(level))
        yield
    else:
        known_roots = [logging.getLogger(l) for l in roots]
        known_levels = [l.level for l in known_roots]
        for a_logger in known_roots:
            a_logger.setLevel(thislevel)
        try:
            yield
        finally:
            for a_logger, level in zip(known_roots, known_levels):
                a_logger.setLevel(level)


def fdecoGlobalLevel(level):
    """Function decorator that set loglevels to ``level``.

    When the function exits, loglevels are restored to their previous values.
    """
    @nicedeco
    def deco_f(f):
        def wrapped_f(*kargs, **kwargs):
            with contextboundGlobalLevel(level):
                return f(*kargs, **kwargs)
        return wrapped_f
    return deco_f


def unittestGlobalLevel(level):
    """
    Function decorator that set loglevels to ``level`` during unit tests
    execution.
    """
    def deco_cls(cls):
        orig_setUp = getattr(cls, 'setUp', None)
        orig_tearDown = getattr(cls, 'tearDown', None)

        thislevel = getActualLevel(level)
        if thislevel is None:
            print('ERROR!!! Try to set an unknown log level {:s}'.format(level))

        else:
            def setUp(self):
                self._log_known_roots = [logging.getLogger(l) for l in roots]
                self._log_known_levels = [l.level for l in self._log_known_roots]
                for a_logger in self._log_known_roots:
                    a_logger.setLevel(thislevel)
                if orig_setUp is not None:
                    orig_setUp(self)
            if orig_setUp:
                setUp.__doc__ = orig_setUp.__doc__

            def tearDown(self):
                for a_logger, a_level in zip(self._log_known_roots, self._log_known_levels):
                    a_logger.setLevel(a_level)
                if orig_tearDown is not None:
                    orig_tearDown(self)
            if orig_tearDown:
                tearDown.__doc__ = orig_tearDown.__doc__

            setattr(cls, 'setUp', setUp)
            setattr(cls, 'tearDown', tearDown)

        return cls
    return deco_cls


# OTHER UTILITY METHODS

def setRootLogger(logger, level=logging.INFO):
    """Set appropriate Handler and Console to a top level logger."""
    logger.setLevel(level)
    logger.addHandler(console)
    logger.addFilter(LoggingFilter(name=logger.name))
    logger.propagate = False
    roots.add(logger.name)
    return logger


def setLogMethods(logger, methods=('debug', 'info', 'warning', 'error', 'critical')):
    """Reset some loggers methods with methods from an external logger."""
    for modname in lognames:
        thislog = logging.getLogger(modname)
        for logmethod in methods:
            setattr(thislog, logmethod, getattr(logger, logmethod))


def getActualLevel(level):
    """Return the actual level value as long as the argument is valid.

    ``level`` can be a verbosity level name (e.g debug, info, ...) or the
    number associated with it.
    """
    lnames = logging._levelNames if six.PY2 else logging._nameToLevel
    if type(level) is not int:
        level = lnames.get(level.upper(), None)
    return level


# OTHER UTILITY CLASSES

# A hook filter (optional)
class LoggingFilter(logging.Filter):
    """Add module name to record."""

    def filter(self, record):
        """Remap top interactive module to ``prompt``."""
        if record.funcName == '<module>':
            record.funcName = 'prompt'
        return True


class SlurpHandler(logging.Handler):
    """A strange Handler that accumulates the log-records in a list.

    We try to make sure that each individual record is pickable.
    """

    def __init__(self, records_stack):
        super(SlurpHandler, self).__init__()
        self._stack = records_stack

    def prepare(self, record):
        """
        Prepares a record for queuing.

        The base implementation formats the record to merge the message
        and arguments, and removes unpickleable items from the record
        in-place.

        :param record: The record to prepare.
        """
        self.format(record)
        record.msg = record.message
        record.args = None
        record.exc_info = None
        return record

    def emit(self, record):
        """
        Emit a record.

        Adds the LogRecord to the stack, preparing it for pickling first.

        :param record: The record to emit.
        """
        try:
            self._stack.append(self.prepare(record))
        except (KeyboardInterrupt, SystemExit):
            raise
        except Exception:
            self.handleError(record)


# Initialise the bronx logger (for future uses)
getLogger('bronx')


if __name__ == '__main__':
    import doctest
    doctest.testmod()
