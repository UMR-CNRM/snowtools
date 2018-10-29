#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This module is in charge of getting informations on Memory.

Various concrete implementations may be provided since the mechanism to retrieve
information on Memory may not be portable across platforms.
At the present time, the only concrete implementation is
the :class:`LinuxMemInfo`.
"""

from __future__ import print_function, absolute_import, unicode_literals, division
import six

import abc
import os
import resource


DEFAULT_MEM_UNIT = 'MiB'


class MemToolUnavailableError(Exception):
    """Raised whenever the necessary commands and/or system files are missing."""
    pass


def convert_bytes_in_unit(mem_b, unit):
    """Convert bytes to **unit** (among KB, MB, GB, ... or KiB, MiB, GiB).

    Note: KB, MB, ... are powers of 1000 whereas KiB, MiB, ... are powers of 1024
    (KiB are often mistaken of KB).
    """
    unit_power = {'B': 0,
                  'KB': 1, 'MB': 2, 'GB': 3, 'TB': 4, 'PB': 5, 'EB': 6,
                  'KiB': 1, 'MiB': 2, 'GiB': 3, 'TiB': 4, 'PiB': 5, 'EiB': 6, }
    if unit not in unit_power:
        raise ValueError('Unknown unit {!s}'.format(unit))
    if unit != 'B':
        mem_b = float(mem_b)
    return mem_b / ((1024 if 'i' in unit else 1000) ** unit_power[unit])


@six.add_metaclass(abc.ABCMeta)
class MemInfo(object):
    """Provide various informations about Memory (abstract class)."""

    @abc.abstractmethod
    def __init__(self):
        self._system_RAM = None

    def system_RAM(self, unit=DEFAULT_MEM_UNIT):
        """Get total RAM memory available in the system."""
        return convert_bytes_in_unit(self._system_RAM, unit)

    @abc.abstractmethod
    def process_maxRSS(self, unit=DEFAULT_MEM_UNIT):
        pass

    @abc.abstractmethod
    def children_maxRSS(self, unit=DEFAULT_MEM_UNIT):
        pass


class LinuxMemInfo(MemInfo):
    """Provide various informations about Memory."""

    def __init__(self):
        # The RAM size in bytes
        self._system_RAM = os.sysconf(str('SC_PAGE_SIZE')) * os.sysconf(str('SC_PHYS_PAGES'))

    def process_maxRSS(self, unit=DEFAULT_MEM_UNIT):
        """
        Get Maximum Resident Set Size (i.e. maximum memory used at one moment)
        used by of the process.
        """
        maxrss = resource.getrusage(resource.RUSAGE_SELF).ru_maxrss * 1024
        return convert_bytes_in_unit(maxrss, unit)

    def children_maxRSS(self, unit=DEFAULT_MEM_UNIT):
        """
        Get Maximum Resident Set Size (i.e. maximum memory used at one moment)
        of the process children.
        """
        maxrss = resource.getrusage(resource.RUSAGE_CHILDREN).ru_maxrss * 1024
        return convert_bytes_in_unit(maxrss, unit)
