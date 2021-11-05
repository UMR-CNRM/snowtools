# -*- coding: utf-8 -*-

"""
This module handles advanced signal catching.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import logging
import signal

from bronx.fancies import loggers

logger = loggers.getLogger(__name__)


class SignalInterruptError(BaseException):
    """Exception raised when a system signal is caught."""
    pass


class SignalInterruptHandler(object):
    """Handler class to deal with system signals."""

    def __init__(self, signals=(signal.SIGHUP, signal.SIGINT, signal.SIGQUIT,
                                signal.SIGTRAP, signal.SIGABRT, signal.SIGFPE,
                                signal.SIGUSR1, signal.SIGUSR2, signal.SIGTERM),
                 emitlogs=True):
        """

        :param signals: list/tuple of signals that will be caught
        :param emitlogs: emit log messages

        For each of the signals specified to the class constructor, this
        signal handler is able to switch on and off a customised signal
        handler that will raise a :class:`SignalInterruptError` exception when
        the signal is received by the python shell.

        An exception is made with SIGINT that will trigger the usual
        Python's :class:`KeyboardInterrupt` exception.

        :example: a simple way of activating/deactivating the signal handlers:

        .. code-block:: python

            shandler = SignalInterruptHandler()
            shandler.activate()

            # In this portion of the script an exception is raised when a signal is
            # sent to the python shell

            print 'Is the signal handler active?', shandler.active

            shandler.deactivate()

            # In this portion of the script the python shell will abruptly stop if
            # a signal is received

        :example: the same thing but using a context:

        .. code-block:: python

            with SignalInterruptHandler() as shandler:

                # In this portion of the script an exception is raised when a signal is
                # sent to the python shell

                print 'Is the signal handler active?', shandler.active

            # In this portion of the script the python shell will abruptly stop if
            # a signal is received
        """
        self._signals = signals
        self._original_handlers = {}
        self._active = False
        self._emitlogs = emitlogs

    def __enter__(self):
        self.activate()
        return self

    def __exit__(self, exctype, excvalue, exctb):
        self.deactivate()

    @property
    def signals(self):
        """List of the signals catched by the signal handlers."""
        return list(self._signals)

    @property
    def active(self):
        """Are the singal handlers active ?"""
        return self._active

    def _logstuff(self, level, message, *kargs):
        """Emit a log emssage if need be."""
        if self._emitlogs:
            logger.log(level, message, *kargs)

    def activate(self):
        """Activate the signal handlers."""
        if not self._active:
            def handler(signum, frame):
                self.deactivate()
                self._logstuff(logging.ERROR,
                               'Signal %d was caught. All original signal handler are restored.',
                               signum)
                if signum == signal.SIGINT:
                    raise KeyboardInterrupt()
                else:
                    raise SignalInterruptError('Signal {:d} was caught.'.format(signum))
            for sig in self.signals:
                self._original_handlers[sig] = signal.signal(sig, handler)
                self._logstuff(logging.INFO, 'Customised signal handler installed for signal %d', sig)
        self._active = True

    def deactivate(self):
        """Deactivate the signal handlers and restore the previous ones."""
        if self._active:
            for sig in self.signals:
                signal.signal(sig, self._original_handlers[sig] or signal.SIG_DFL)
                self._logstuff(logging.INFO, 'Original signal handler restored for signal %d', sig)
        self._active = False
