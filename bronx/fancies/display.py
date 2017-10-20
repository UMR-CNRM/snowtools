#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
Various tools designed for interactive scripts.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import six
import sys

#: No automatic export
__all__ = []


def printstatus(step, end, refresh_freq=1):
    """
    Print percentage of the loop it is in.

    :param step: the current loop step
    :param end: the final loop step
    :param refresh_freq: the frequency in % at which reprinting status.
    """
    status = step * 100. / end
    if status % refresh_freq == 0:
        sys.stdout.write('{:>{width}}%'.format(int(status), width=3))
        sys.stdout.flush()
        if step < end:
            sys.stdout.write('\b' * 4)
        else:
            sys.stdout.write('\n')


def query_yes_no_quit(question, default="yes"):
    """Ask a yes/no/quit question via raw_input() and return their answer.

    :param str question: String that is presented to the user.
    :param str default: The presumed answer if the user just hits <Enter>.
                        It must be "yes" (the default), "no", "quit" or None
                        (meaning an answer is required).
    :return: 'yes', 'no' or 'quit' depending on the user's answer.
    :rtype: A unicode string.

    :example: Ask something to someone...

        >>> answer = query_yes_no_quit('Do you want to continue ?', default='yes')
        Do you want to continue ? [Y/n/q] Y
        >>> answer
        u'yes'

    from: http://code.activestate.com/recipes/577097/
    """
    valid = {"yes": "yes", "y": "yes", "ye": "yes",
             "no": "no", "n": "no",
             "quit": "quit", "qui": "quit", "qu": "quit", "q": "quit"}
    if six.PY2 and isinstance(default, str):
        default = six.u(default)  # We always return an unicode string
    if default is None:
        prompt = " [y/n/q] "
    elif default == "yes":
        prompt = " [Y/n/q] "
    elif default == "no":
        prompt = " [y/N/q] "
    elif default == "quit":
        prompt = " [y/n/Q] "
    else:
        raise ValueError("invalid default answer: '%s'" % default)

    while 1:
        sys.stdout.write(question + prompt)
        choice = six.moves.input().lower()
        if default is not None and choice == '':
            return default
        elif choice in valid.keys():
            return valid[choice]
        else:
            sys.stdout.write("Please respond with 'yes', 'no' or 'quit'.\n")
