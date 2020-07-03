#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This module is in charge of getting informations on CPUs.

Various concrete implementations may be provided since the mechanism to retrieve
information on CPUs is not portable across platforms. At the present time, the
only concrete implementation is the :class:`LinuxCpusInfo` class that relies on
the /proc/cpuinfo virtual file.

On Epona (2x AMD Rome socket with 64 cores each)::

    >>> cpu_i = LinuxCpusInfo()
    >>> cpu_i.nphysical_cores
    128
    >>> cpu_i.nvirtual_cores
    128
    >>> cpu_i.nsockets
    2
    >>> cpu_i.nphysical_cores_per_socket
    64
    >>> cpu_i.smt_threads
    1
    >>> import pprint
    >>> pprint.pprint(cpu_i.cpus_hierarchy)
    defaultdict(functools.partial(<class 'collections.defaultdict'>, <class 'list'>),
                {0: defaultdict(<class 'list'>,
                                {0: [0],
                                 1: [1],
                                 2: [2],
                                 3: [3],
                                 4: [4],
                                 5: [5],
                                 6: [6],
                                 7: [7],
                                 8: [8],
                                 9: [9],
                                 10: [10],
                                 11: [11],
                                 12: [12],
                                 13: [13],
                                 14: [14],
                                 15: [15],
                                 16: [16],
                                 17: [17],
                                 18: [18],
                                 19: [19],
                                 20: [20],
                                 21: [21],
                                 22: [22],
                                 23: [23],
                                 24: [24],
                                 25: [25],
                                 26: [26],
                                 27: [27],
                                 28: [28],
                                 29: [29],
                                 30: [30],
                                 31: [31],
                                 32: [32],
                                 33: [33],
                                 34: [34],
                                 35: [35],
                                 36: [36],
                                 37: [37],
                                 38: [38],
                                 39: [39],
                                 40: [40],
                                 41: [41],
                                 42: [42],
                                 43: [43],
                                 44: [44],
                                 45: [45],
                                 46: [46],
                                 47: [47],
                                 48: [48],
                                 49: [49],
                                 50: [50],
                                 51: [51],
                                 52: [52],
                                 53: [53],
                                 54: [54],
                                 55: [55],
                                 56: [56],
                                 57: [57],
                                 58: [58],
                                 59: [59],
                                 60: [60],
                                 61: [61],
                                 62: [62],
                                 63: [63]}),
                 1: defaultdict(<class 'list'>,
                                {0: [64],
                                 1: [65],
                                 2: [66],
                                 3: [67],
                                 4: [68],
                                 5: [69],
                                 6: [70],
                                 7: [71],
                                 8: [72],
                                 9: [73],
                                 10: [74],
                                 11: [75],
                                 12: [76],
                                 13: [77],
                                 14: [78],
                                 15: [79],
                                 16: [80],
                                 17: [81],
                                 18: [82],
                                 19: [83],
                                 20: [84],
                                 21: [85],
                                 22: [86],
                                 23: [87],
                                 24: [88],
                                 25: [89],
                                 26: [90],
                                 27: [91],
                                 28: [92],
                                 29: [93],
                                 30: [94],
                                 31: [95],
                                 32: [96],
                                 33: [97],
                                 34: [98],
                                 35: [99],
                                 36: [100],
                                 37: [101],
                                 38: [102],
                                 39: [103],
                                 40: [104],
                                 41: [105],
                                 42: [106],
                                 43: [107],
                                 44: [108],
                                 45: [109],
                                 46: [110],
                                 47: [111],
                                 48: [112],
                                 49: [113],
                                 50: [114],
                                 51: [115],
                                 52: [116],
                                 53: [117],
                                 54: [118],
                                 55: [119],
                                 56: [120],
                                 57: [121],
                                 58: [122],
                                 59: [123],
                                 60: [124],
                                 61: [125],
                                 62: [126],
                                 63: [127]})})

"""

from __future__ import print_function, absolute_import, unicode_literals, division

import six

import abc
from collections import namedtuple, defaultdict
from functools import partial
import io
import locale
import re

import os
import subprocess

from bronx.fancies import loggers

logger = loggers.getLogger(__name__)

#: Data about a given CPU
CpuInfo = namedtuple('CpuInfo', ('socket_id', 'core_id'))

AFFINITY_CMD = 'taskset'


class CpusToolUnavailableError(Exception):
    """Raised whenever the necessary commands and/or system files are missing."""
    pass


@six.add_metaclass(abc.ABCMeta)
class CpusInfo(object):
    """Provide various informations about CPUs (abstract class)."""

    def __init__(self):
        self._cpus = None
        self._cpus_hierarchy = None
        self._physical_cores_smtthreads = None

    @property
    @abc.abstractmethod
    def cpus(self):
        """The raw dictionary of the system's CPUs."""
        pass

    @property
    def cpus_hierarchy(self):
        """A hierarchical view of CPUs.

        This returns a dictionary with the following structure::

            dict(socket_id1:dict(core_id1:list(cpuid1, cpuid2, ...), ...), ...)

        For a given socket_id and core_id, the list of cpuIDs shows all of the
        virtual CPUs associated to the physical core.
        """
        if self._cpus_hierarchy is None:
            hierarchy = defaultdict(partial(defaultdict, list))
            for icpu, cpu in sorted(self.cpus.items()):
                hierarchy[cpu.socket_id][cpu.core_id].append(icpu)
            self._cpus_hierarchy = hierarchy
        return self._cpus_hierarchy

    @property
    def nphysical_cores(self):
        """The total number of physical cores on this system."""
        return len(set([(c.socket_id, c.core_id) for c in self.cpus.values()]))

    @property
    def nvirtual_cores(self):
        """The total number of virtual cores on this system."""
        return len(self.cpus)

    @property
    def nsockets(self):
        """The number of sockets on this system."""
        return len(self.cpus_hierarchy)

    @property
    def nphysical_cores_per_socket(self):
        """The number of physical cores per socket."""
        ncores = set([len(socket) for socket in self.cpus_hierarchy.values()])
        assert len(ncores) == 1
        return ncores.pop()

    @property
    def smt_threads(self):
        """The Simultaneous MultiThreading threads count."""
        nsmt = set([len(core)
                    for socket in self.cpus_hierarchy.values()
                    for core in socket.values()])
        assert len(nsmt) == 1
        return nsmt.pop()

    @property
    def physical_cores_smtthreads(self):
        """For each physical core, associate the first CPUid and its siblings"""
        if self._physical_cores_smtthreads is None:
            self._physical_cores_smtthreads = dict()
            for socket in self.cpus_hierarchy.values():
                for core in socket.values():
                    self._physical_cores_smtthreads[core[0]] = core[1:]
        return self._physical_cores_smtthreads

    def raw_cpulist(self, bsize=1):
        """Re-arrange the list of CPUs in a very simple way.

        :param int bsize: unused
        :rtype: generator (of int)
        """
        for ismt in range(self.smt_threads):
            for isocket in sorted(self.cpus_hierarchy.keys()):
                for icore in sorted(self.cpus_hierarchy[isocket].keys()):
                    yield self.cpus_hierarchy[isocket][icore][ismt]

    def socketpacked_cpulist(self, bsize=1):
        """Re-arrange the list of CPUs in a round-robin manner across sockets.

        :param int bsize: The number of threads used by a task (the CPU will be
            arranged in a way that all of one task's threads are located on the
            same socket).
        :rtype: generator (of int)

        For example, on a two sockets system, with *bsize=1*, the first element
        will be the first physical core of socket#0, the second element will be
        the first physical core of socket#1, the third element will be
        the second physical core of socket#0, ...

        With *bsize=2* and the same example, the generator would produce
        something like::

            list(Socket0-Core0, Socket0-Core1, Socket1-Core0, Socket1-Core1,
                 Socket0-Core2, Socket0-Core3, ...)

        """
        sockets = sorted(self.cpus_hierarchy.keys())
        # The easiest case: A given block fits on the physical cores of one socket
        if bsize <= self.nphysical_cores_per_socket:
            cores = [sorted(self.cpus_hierarchy[isocket].keys())
                     for isocket in sockets]
            loc_nphysical_cores = self.nphysical_cores
            if self.nphysical_cores_per_socket % bsize:
                loc_nphysical_cores = (self.nphysical_cores_per_socket // bsize) * bsize
                loc_nphysical_cores *= self.nsockets
                logger.warning("Some of the physical cores won't be used (%d instead of %d)",
                               loc_nphysical_cores, self.nphysical_cores)
            for ismt in range(self.smt_threads):
                for globalcoreidx in range(loc_nphysical_cores // bsize):
                    socketidx = globalcoreidx % self.nsockets
                    isocket = sockets[socketidx]
                    for bidx in range(bsize):
                        coreidx = (globalcoreidx // self.nsockets) * bsize + bidx
                        icore = cores[socketidx][coreidx]
                        yield self.cpus_hierarchy[isocket][icore][ismt]
        # The block is spanned between physical and virtul cores
        elif (bsize > self.nphysical_cores_per_socket and
              bsize <= self.nphysical_cores_per_socket * self.smt_threads):
            flatcores = dict()
            for isocket in sockets:
                flatcores[isocket] = [self.cpus_hierarchy[isocket][icore][ismt]
                                      for ismt in range(self.smt_threads)
                                      for icore in sorted(self.cpus_hierarchy[isocket].keys())]
            for isocket in sockets:
                for i in range(bsize):
                    yield flatcores[isocket][i]
        # Impossible... switch back to blocksize = 1 (round-robin over socket)
        else:
            for cpu in list(self.socketpacked_cpulist(bsize=1)):
                yield cpu


class LinuxCpusInfo(CpusInfo):
    """Provide various informations about CPUs based on the /proc/cpuinfo file."""

    _INFOFILE_CHECK = True
    _INFOFILE = '/proc/cpuinfo'
    _CPU_RE = re.compile(r'^processor\s*:\s*(\d+)\b')
    _PHYSID_RE = re.compile(r'^physical id\s*:\s*(\d+)\b')
    _COREID_RE = re.compile(r'^core id\s*:\s*(\d+)\b')

    def __new__(cls):
        """Check the the /proc/cpuinfo file exists before going on."""
        if cls._INFOFILE_CHECK and not os.path.exists(cls._INFOFILE):
            raise CpusToolUnavailableError('The {:s} file was not found'.format(cls._INFOFILE))
        return super(LinuxCpusInfo, cls).__new__(cls)

    @property
    def cpus(self):
        """The dictionary of the system's CPUs.

        If needed, process /proc/cpuinfo to get all the necessary data.
        """
        if self._cpus is None:
            self._cpus = dict()
            cpu_n = None
            cpu = None
            with io.open(self._INFOFILE, 'r') as infofd:
                for line in infofd:
                    # Detect the begining of a new CPU description
                    cpumatch = self._CPU_RE.match(line)
                    if cpumatch:
                        if cpu_n is not None:
                            self._cpus[cpu_n] = CpuInfo(**cpu)
                        cpu_n = int(cpumatch.group(1))
                        cpu = dict()
                        continue
                    if cpu_n is None:
                        continue
                    # Physical ID (i.e socket number)
                    cpuphysmatch = self._PHYSID_RE.match(line)
                    if cpuphysmatch:
                        cpu['socket_id'] = int(cpuphysmatch.group(1))
                        continue
                    # Physical core ID
                    cpucorematch = self._COREID_RE.match(line)
                    if cpucorematch:
                        cpu['core_id'] = int(cpucorematch.group(1))
                        continue
            if cpu_n is not None:
                self._cpus[cpu_n] = CpuInfo(**cpu)
        return self._cpus


if six.PY2:

    def get_affinity(pid=None):
        """Get the cpu affinity of a process. Returns None if no affinity is set."""
        if pid is None:
            pid = os.getpid()
        _re_get_out = re.compile(r'.*:\s*(?P<binproc>[0-9a-f]+)\s*$')
        try:
            t_out = subprocess.check_output([AFFINITY_CMD, '-p', str(pid)])
        except OSError:
            raise CpusToolUnavailableError('No {:s} command on this system.'.format(AFFINITY_CMD))
        locencode = locale.getdefaultlocale()[1] or 'ascii'
        uni_out = t_out.decode(locencode, 'replace')  # Unicode stuff...
        binproc = int(_re_get_out.match(uni_out).group('binproc'), 16)  # It's hexadecimal
        binlist = list()
        while binproc:
            binlist.append(binproc % 2)
            binproc = binproc >> 1
        list_of_cpus = [i for i, v in enumerate(binlist) if v]
        return set(list_of_cpus)

    def set_affinity(cpus, pid=None):
        """Set the cpu affinity of a process."""
        if pid is None:
            pid = os.getpid()
        if isinstance(cpus, int):
            cpus = [cpus]
        cpus = ','.join([str(c) for c in cpus])
        try:
            subprocess.check_output([AFFINITY_CMD, '-p', '--cpu-list', cpus, str(pid)], stderr=subprocess.STDOUT)
        except OSError:
            raise CpusToolUnavailableError('No {:s} command on this system.'.format(AFFINITY_CMD))
        except subprocess.CalledProcessError as e:
            logger.error(str(e))
            logger.error("stdout/stderr: %s", e.output)
            raise

else:

    def get_affinity(pid=None):
        """Get the cpu affinity of a process. Returns None if no affinity is set."""
        if pid is None:
            pid = 0
        return os.sched_getaffinity(pid)

    def set_affinity(cpus, pid=None):
        """Set the cpu affinity of a process."""
        if pid is None:
            pid = 0
        if isinstance(cpus, int):
            cpus = [cpus]
        os.sched_setaffinity(pid, cpus)
