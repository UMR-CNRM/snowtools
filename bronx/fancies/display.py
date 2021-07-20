# -*- coding: utf-8 -*-

"""
Various tools designed for interactive scripts.
"""

from __future__ import print_function, absolute_import, unicode_literals, division
import six

from bronx.compat.moves import collections_abc
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
    status = int(step * 100. / end)
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

        >>> answer = query_yes_no_quit('Do you want to continue ?', default='yes') # doctest: +SKIP
        Do you want to continue ? [Y/n/q] Y
        >>> answer # doctest: +SKIP
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


def print_tablelike(fmt, *args, **kwargs):
    """Left align all strings in order to have a well aligned output.

    :param str fmt: The format string used for each of the output lines
    :param str output_callback: The function to call for each output line (default: ``print``)
    :param str preserve_last: If *True* and if the last column is of string type,
                              leave it untouched (i.e. unaligned) (default: ``True``)

    Simple examples::

        >>> print_tablelike('{:s} = {:d}', ['a', 'bcde', 'f'], [1, 2, 3])
        a    = 1
        bcde = 2
        f    = 3

    If one wants to write somewhere else::

        >>> outlist = list()
        >>> print_tablelike('{:s} = {:d}', ['a', 'bcde', 'f'], [1, 2, 3], output_callback=outlist.append)
        >>> for l in outlist:
        ...     print(l)
        a    = 1
        bcde = 2
        f    = 3

    By default, if the last column consists of string, it is un-aligned::

        >>> print_tablelike('{:s} = << {:s} >>', ['a', 'bcde', 'f'], ['a', 'bcde', 'f'])
        a    = << a >>
        bcde = << bcde >>
        f    = << f >>

    If one wants to align it::

        >>> print_tablelike('{:s} = << {:s} >>', ['a', 'bcde', 'f'], ['a', 'bcde', 'f'], preserve_last=False)
        a    = << a    >>
        bcde = << bcde >>
        f    = << f    >>
    """
    cb = kwargs.pop('output_callback', print)
    plast = kwargs.pop('preserve_last', True)
    datasize = None
    newargs = list()
    for i, arg in enumerate(args):
        assert issubclass(type(arg), collections_abc.Iterable), \
            "Each of the positional arguments must be iterable."
        assert issubclass(type(arg), collections_abc.Sized), \
            "Each of the positional arguments must have a query-able size."
        if datasize is None:
            datasize = len(arg)
        else:
            assert len(arg) == datasize, "Size inconsistency between arguments"
        if all([isinstance(s, six.string_types) for s in arg]) and (not plast or i < len(args) - 1):
            maxlen = max([len(s) for s in arg])
            newargs.append([("{:<" + str(maxlen) + "s}").format(s) for s in arg])
        else:
            newargs.append(arg)
    for args in zip(* newargs):
        cb(fmt.format(* args))


def join_list_in_proper_english(a_list, l_fmt='{!s}'):
    """Join a list using commas + a final 'and' word if needed.

    :param a_list: any iterable object to be concatenated
    :param str a_fmt: The Python's format applied to each **a_list** item

    :example: Ask something to someone...

        >>> print(join_list_in_proper_english(['a', 'b', 'c']))
        a, b and c
        >>> print(join_list_in_proper_english('abc'))  # Any iterable can be used
        a, b and c
        >>> print(join_list_in_proper_english(['a', 'b']))
        a and b
        >>> print(join_list_in_proper_english(['a', ]))
        a
        >>> print(join_list_in_proper_english([]))
        <BLANKLINE>

    """
    a_list = [l_fmt.format(i) for i in a_list]
    if len(a_list) >= 2:
        outstr = ', '.join(a_list[:-1]) + ' and ' + a_list[-1]
    elif a_list:
        outstr = a_list[-1]
    else:
        outstr = ''
    return outstr


if __name__ == '__main__':
    import doctest
    doctest.testmod()
