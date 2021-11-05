# -*- coding: utf-8 -*-

"""
Useful decorators.
"""

from __future__ import print_function, absolute_import, unicode_literals, division
import six

import time


#: No automatic export
__all__ = []


def nicedeco(decorator):
    """
    A decorator of decorator, this decorator enforces that the resulting
    decorated functions looks like the original one.
    """
    def new_decorator(f):
        g = decorator(f)
        g.__name__ = f.__name__
        g.__doc__ = f.__doc__
        g.__dict__.update(f.__dict__)
        return g
    return new_decorator


def nicedeco_plusdoc(doc_bonus):
    """
    A decorator of decorator, this decorator enforces that the resulting
    decorated functions looks like the original one but an extra bit of
    documentation is added.
    """
    def nicedeco_doc(decorator):
        def new_decorator(f):
            g = decorator(f)
            g.__name__ = f.__name__
            g.__doc__ = (f.__doc__ +
                         doc_bonus.format(name=f.__name__))
            g.__dict__.update(f.__dict__)
            return g
        return new_decorator
    return nicedeco_doc


@nicedeco
def disabled(func):  # @UnusedVariable
    """This decorator disables the provided function, and does nothing."""
    def empty_func(*args, **kw):
        pass
    return empty_func


@nicedeco
def printargs(func):
    """This decorator prints out the arguments passed to a function before calling it."""
    argnames = func.func_code.co_varnames[:func.func_code.co_argcount]
    fname = func.__name__

    def echo_func_args(*args, **kw):
        print('> > >', fname, '(', ', '.join(
            '%s=%r' % entry
            for entry in zip(argnames, args) + kw.items()), ')')
        return func(*args, **kw)
    return echo_func_args


@nicedeco
def unicode_filter(func):
    """This decorator force unicode when a string is returned."""
    def unicode_func(*args, **kw):
        out = func(*args, **kw)
        if isinstance(out, str):
            out = six.text_type(out)
        return out
    return unicode_func


def timelimit(logger, nbsec):
    """This decorator warn if the function is more than ``nbsec`` seconds long."""
    @nicedeco
    def internal_decorator(func):
        def timed_func(*args, **kw):
            t0 = time.time()
            results = func(*args, **kw)
            tt = time.time() - t0
            if tt >= nbsec:
                logger.warn('Function %s took %f seconds', func.__name__, tt)
            return results
        return timed_func
    return internal_decorator


@nicedeco
def secure_getattr(func):
    """
    This decorator is to be used on __getattr__ methods to ensure that essential
    method such as __getstate__/__setstate__ are not looked for.
    """
    def secured_getattr(self, key):
        # Avoid nasty interactions when copying/pickling
        if key in ('__bases__',
                   '__deepcopy__', '__copy__',
                   '__reduce__', '__reduce_ex__',
                   '__getinitargs__', '__getnewargs__', '__getnewargs_ex__',
                   '__getstate__', '__setstate__'):
            raise AttributeError(key)
        else:
            return func(self, key)
    return secured_getattr
