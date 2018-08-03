#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This module is in charge of getting informations on CPUs.

Various concrete implementations may be provided since the mechanism to retrieve
information on CPUs is not portable across platforms. At the present time, the
only concrete implementation is the :class:`LinuxCpusInfo` class that relies on
the /proc/cpuinfo virtual file.
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

import footprints

logger = footprints.loggers.getLogger(__name__)


#: Data about a given CPU
CpuInfo = namedtuple('CpuInfo', ('socket_id', 'core_id'))

AFFINITY_CMD = 'taskset'


class CpusToolUnavailableError(Exception):
    """Raised whenever the necessary commands and/or system files are missing."""
    pass


@six.add_metaclass(abc.ABCMeta)
class CpusInfo(object):
    '''Provide various informations about CPUs (abstract class).'''

    def __init__(self):
        self._cpus = None
        self._cpus_hierarchy = None

    @abc.abstractproperty
    def cpus(self):
        '''The raw dictionary of the system's CPUs.'''
        pass

    @property
    def cpus_hierarchy(self):
        '''A hierarchical view of CPUs.

        This returns a dictionary with the following structure::

            dict(socket_id1:dict(core_id1:list(cpuid1, cpuid2, ...), ...), ...)

        For a given socket_id and core_id, the list of cpuIDs shows all of the
        virtual CPUs associated to the physical core.
        '''
        if self._cpus_hierarchy is None:
            hierarchy = defaultdict(partial(defaultdict, list))
            for icpu, cpu in self.cpus.items():
                hierarchy[cpu.socket_id][cpu.core_id].append(icpu)
            self._cpus_hierarchy = hierarchy
        return self._cpus_hierarchy

    @property
    def nphysical_cores(self):
        '''The total number of physical cores on this system.'''
        return len(set([(c.socket_id, c.core_id) for c in self.cpus.values()]))

    @property
    def nvirtual_cores(self):
        '''The total number of virtual cores on this system.'''
        return len(self.cpus)

    @property
    def nsockets(self):
        '''The number of sockets on this system.'''
        return len(self.cpus_hierarchy)

    @property
    def nphysical_cores_per_socket(self):
        '''The number of physical cores per socket.'''
        ncores = set([len(socket) for socket in self.cpus_hierarchy.values()])
        assert len(ncores) == 1
        return ncores.pop()

    @property
    def smt_threads(self):
        '''The Simultaneous MultiThreading threads count.'''
        nsmt = set([len(core)
                    for socket in self.cpus_hierarchy.values()
                    for core in socket.values()])
        assert len(nsmt) == 1
        return nsmt.pop()

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
    '''Provide various informations about CPUs based on the /proc/cpuinfo file.'''

    _INFOFILE_CHECK = True
    _INFOFILE = '/proc/cpuinfo'
    _CPU_RE = re.compile(r'^processor\s*:\s*(\d+)\b')
    _PHYSID_RE = re.compile(r'^physical id\s*:\s*(\d+)\b')
    _COREID_RE = re.compile(r'^core id\s*:\s*(\d+)\b')

    def __new__(cls):
        if cls._INFOFILE_CHECK and not os.path.exists(cls._INFOFILE):
            raise CpusToolUnavailableError('The {:s} file was not found'.format(cls._INFOFILE))
        return super(LinuxCpusInfo, cls).__new__(cls)

    @property
    def cpus(self):
        '''The dictionary of the system's CPUs.

        If needed, process /proc/cpuinfo to get all the necessary data.
        '''
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
                            self._cpus[cpu_n] = CpuInfo(** cpu)
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
                            self._cpus[cpu_n] = CpuInfo(** cpu)
        return self._cpus


def get_affinity(pid=None):
    """Get the cpu affinity of a process. Returns None if no affinity is set."""
    if pid is None:
        pid = os.getpid()
    _re_get_out = re.compile(r'.*:\s*(?P<binproc>[0-9a-f]+)\s*$')
    try:
        t_out = subprocess.check_output([AFFINITY_CMD, '-p', str(pid)])
    except OSError:
        raise CpusToolUnavailableError('No {:s} command on this system.'.format(AFFINITY_CMD))
    loc_cmd = locale.getdefaultlocale() or ('unknown', 'ascii')
    uni_out = t_out.decode(loc_cmd[1], 'replace')  # Unicode stuff...
    binproc = int(_re_get_out.match(uni_out).group('binproc'), 16)  # It's hexadecimal
    binlist = list()
    while binproc:
        binlist.append(binproc % 2)
        binproc = binproc >> 1
    list_of_cpus = [i for i, v in enumerate(binlist) if v]
    return list_of_cpus


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
