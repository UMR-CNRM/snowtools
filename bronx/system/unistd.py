# -*- coding: utf-8 -*-

"""
Utilities to work on file descriptors through the standard Linux C layer.

The name of this module is directly inspired from the eponym C library.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import io
from contextlib import contextmanager
import os
import sys

#: No automatic export
__all__ = []


@contextmanager
def stdout_redirected(to=os.devnull):
    """
    Redirect *sys.stdout* to **to**.

    Usage::

        with stdout_redirected(to=filename):
            print("from Python")
            import os
            os.system("echo non-Python applications are also supported")
    """
    return redirected_stdio(module=sys, stdio='stdout', to=to)


@contextmanager
def stderr_redirected(to=os.devnull):
    """
    Redirect *sys.stderr* to **to**.

    Usage::

        with stdout_redirected(to=filename):
            # there, an error message won't be printed
    """
    return redirected_stdio(module=sys, stdio='stderr', to=to)


def redirected_stdio(module=sys, stdio='stdout', to=os.devnull):
    """
    Redirect **module.stdio** to **to**,
    e.g. (default): *sys.stdout* to *os.devnull*

    Usage::

        with redirected_stdio(sys, out='stdout', to=filename):
            print("from Python")
            import os
            os.system("echo non-Python applications are also supported")

    Warning: Use with care. It will crash if **module.stdio** is already redirected
    to a File like object that is not a "real" physical file (e.g. a StringIO object).
    Because of that, it can not be tested with the unitest module since usual
    test runner redirect stdout/stderr to memory.

    Inspired from:
    http://stackoverflow.com/questions/5081657/how-do-i-prevent-a-c-shared-library-to-print-on-stdout-in-python
    """
    fd = getattr(module, stdio).fileno()

    def _redirect_stdout(to):
        getattr(module, stdio).close()  # + implicit flush()
        os.dup2(to.fileno(), fd)  # fd writes to 'to' file
        setattr(module, stdio, os.fdopen(fd, 'w'))  # Python writes to fd
    with os.fdopen(os.dup(fd), 'w') as old_stdio:
        with io.open(to, 'w') as f:
            _redirect_stdout(f)
        try:
            yield  # allow code to be run with the redirected stdio
        finally:
            _redirect_stdout(to=old_stdio)  # restore stdio.
