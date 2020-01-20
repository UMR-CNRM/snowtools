#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-

"""
Various tools to crawl the file system in order to find some config files

TODO: More documentation (incl. examples)
TODO: unittest

.. warning:: This module is under heavy development consequently significant
             changes will be made in future versions. DO NOT USE YET.

"""



import os

from bronx.fancies import loggers

logger = loggers.getLogger(__name__)


def upfirst(subpath='work', thispath=None):
    """Return first directory matching subpath in specified or current path."""
    if thispath is None:
        try:
            thispath = os.getcwd()
        except OSError as e:
            logger.error('getcwd failed: %s.', str(e))
            thispath = ''
    found = None
    while thispath and not found:
        if os.path.isdir(os.path.join(thispath, subpath)):
            found = os.path.join(thispath, subpath)
        else:
            if thispath == '/':
                break
            thispath = os.path.dirname(thispath)
    return found
