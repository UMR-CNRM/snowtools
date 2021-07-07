#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
This module provides informations on the NUMA partitioning of the CPUs.

On Belenos (2x AMD Rome socket with 64 cores each)::

    >>> numa_i = numa_nodes_info()
    >>> print(numa_i)
    There are 8 nodes.
    Node 0 description:
    - cpus: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
    - totalsize: 34234114048 bytes
    Node 1 description:
    - cpus: 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31
    - totalsize: 34358689792 bytes
    Node 2 description:
    - cpus: 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47
    - totalsize: 34358689792 bytes
    Node 3 description:
    - cpus: 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63
    - totalsize: 34346106880 bytes
    Node 4 description:
    - cpus: 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79
    - totalsize: 34358689792 bytes
    Node 5 description:
    - cpus: 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95
    - totalsize: 34358689792 bytes
    Node 6 description:
    - cpus: 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111
    - totalsize: 34358689792 bytes
    Node 7 description:
    - cpus: 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
    - totalsize: 34358689792 bytes
    Distances matrix:
              0    1    2    3    4    5    6    7
       0:    10   12   12   12   32   32   32   32
       1:    12   10   12   12   32   32   32   32
       2:    12   12   10   12   32   32   32   32
       3:    12   12   12   10   32   32   32   32
       4:    32   32   32   32   10   12   12   12
       5:    32   32   32   32   12   10   12   12
       6:    32   32   32   32   12   12   10   12
       7:    32   32   32   32   12   12   12   10
    >>> import pprint
    # To obtain a partitioning in blocks of 4 cores that takes NUMA nodes
    # into account
    >>> pprint.pprint(numa_i.numapacked_cpulist(blocksize=4))
    [[0, 1, 2, 3],
     [64, 65, 66, 67],
     [16, 17, 18, 19],
     [80, 81, 82, 83],
     [32, 33, 34, 35],
     [96, 97, 98, 99],
     [48, 49, 50, 51],
     [112, 113, 114, 115],
     [4, 5, 6, 7],
     [68, 69, 70, 71],
     [20, 21, 22, 23],
     [84, 85, 86, 87],
     [36, 37, 38, 39],
     [100, 101, 102, 103],
     [52, 53, 54, 55],
     [116, 117, 118, 119],
     [8, 9, 10, 11],
     [72, 73, 74, 75],
     [24, 25, 26, 27],
     [88, 89, 90, 91],
     [40, 41, 42, 43],
     [104, 105, 106, 107],
     [56, 57, 58, 59],
     [120, 121, 122, 123],
     [12, 13, 14, 15],
     [76, 77, 78, 79],
     [28, 29, 30, 31],
     [92, 93, 94, 95],
     [44, 45, 46, 47],
     [108, 109, 110, 111],
     [60, 61, 62, 63],
     [124, 125, 126, 127]]

"""

from __future__ import print_function, absolute_import, unicode_literals, division

import six

import abc
from collections import defaultdict, deque
import copy
from ctypes import CDLL, byref, c_longlong
from ctypes.util import find_library
from itertools import chain, cycle, combinations

from bronx.compat.moves import collections_abc
from bronx.patterns import Singleton


def numa_nodes_info():
    """Pick a subclass of :class:`NumaNodesInfo` and instantiate it.

    Depending on the operating system, the approriate class will be picked. If
    no :class:`NumaNodesInfo` is available, the :class:`NotImplementedError`
    exception will be raised.
    """
    numalib_ok = False
    try:
        numalib_ok = bool(LibnumaGateway().lib)
    except OSError:
        # OSError: The library is not instaled or cannot be loaded
        # NotImplementedError: The libnuma is here but is not functional
        pass
    if numalib_ok:
        return LibNumaNodesInfo()
    else:
        raise NotImplementedError('No suitable NumaNodesInfo implementation could be found')


class NumaNodeInfo(object):
    """Hold information on a single Numa node."""

    def __init__(self, cpus, distances, totalsize):
        """
        :param set cpus: The CPU numbers associated with this NUMA node
        :param dict distances: The distance to other NUMA nodes
        :param totalsize: The NUMA node memory size (in bytes)
        """
        self._cpus = cpus
        self._distances = distances
        self._totalsize = totalsize

    @property
    def cpus(self):
        """The set of CPU numbers associated with this NUMA node."""
        return self._cpus

    @property
    def distances(self):
        """The distance to other NUMA nodes."""
        return self._distances

    @property
    def totalsize(self):
        """The NUMA node memory size (in bytes)."""
        return self._totalsize

    def __str__(self):
        s_out = '- cpus: {:s}\n'.format(' '.join(['{:d}'.format(c) for c in sorted(self.cpus)]))
        s_out += '- totalsize: {:d} bytes'.format(self._totalsize)
        return s_out


@six.add_metaclass(abc.ABCMeta)
class _NumaAbstractCpuIdDispencer(object):
    """Dispenser object return blocks of CPUs of a given size.

    It can be used as an iterator (in this case, the block's size needs to be
    fixed and specified at creation time using the *default_bsize* attribute) or
    by simply calling the object (:meth:`__call__`) with a single argument that is
    the desired blokc size.
    """

    def __init__(self, default_bsize=None):
        """
        :param int default_bsize: The block size used when iterating
        """
        self._default_bsize = default_bsize
        self._iterlock = False

    def __iter__(self):
        if not self._default_bsize:
            raise ValueError('Cannot iterate because "default_bsize" was not given when building the object')
        if self._iterlock:
            raise RuntimeError('You can only iterate once...')
        self._iterlock = True
        return self

    def __next__(self):
        return self(self._default_bsize)

    if six.PY2:
        next = __next__

    @abc.abstractmethod
    def __call__(self, blocksize):
        pass


class _NumaPackedCpuIdDispenser(_NumaAbstractCpuIdDispencer):
    """
    Return blocks of CPUs that are chosen in such a way that the NUMA distance
    between CPUs is minimun.
    """

    def __init__(self, total_avcpus, xnodesclust, default_bsize=None):
        super(_NumaPackedCpuIdDispenser, self).__init__(default_bsize=default_bsize)
        self._total_avcpus = total_avcpus
        self._last_blocksize = None
        self._xnodesclust = xnodesclust
        self._xnodesclust_its_cache = dict()

    def _xnodesclust_its(self, maxdist, blocksize):
        """For a given *maxdist*, return the iterators through clusters and NUMA nodes."""
        if ((maxdist not in self._xnodesclust_its_cache) or
                (self._last_blocksize and self._last_blocksize != blocksize)):
            xnodeclust = self._xnodesclust[maxdist]
            xnodeclust_list = list()
            for cluster in xnodeclust:
                # Always start with the NUMA node that has the buggest number of
                # available CPUs
                nodes_av_cpus = [len(node) for node in cluster]
                nodes_maxidx_av_cpus = nodes_av_cpus.index(max(nodes_av_cpus))
                nodes_its = cycle(cluster)
                for _ in range(nodes_maxidx_av_cpus):
                    next(nodes_its)
                xnodeclust_list.append((cluster, nodes_its))
            # Always start with the cluster that has the buggest number of available CPUs
            clust_av_cpus = [sum([len(d) for d in clust]) for clust in xnodeclust]
            clust_maxidx_av_cpus = clust_av_cpus.index(max(clust_av_cpus))
            xnodeclust_its = cycle(xnodeclust_list)
            for _ in range(clust_maxidx_av_cpus):
                next(xnodeclust_its)
            self._xnodesclust_its_cache[maxdist] = xnodeclust_its
        return self._xnodesclust_its_cache[maxdist]

    def __call__(self, blocksize):
        if blocksize > self._total_avcpus:
            # If the number of available CPUs is to small, stop right away
            raise StopIteration()
        # Start to distibute the block within cluster if NUMA nodes that have
        # the smallest NUMA distance...
        for maxdist in sorted(self._xnodesclust.keys()):
            # Go through the various clusters
            xnodeclust_its = self._xnodesclust_its(maxdist, blocksize)
            for _ in range(len(self._xnodesclust[maxdist])):
                # We use persistent iterator (from one call to another) in order
                # To evenly distribute the CPUs
                cluster, nodes_it = next(xnodeclust_its)
                # Try to create block of blocksize size in a round-robin manner
                if sum([len(nodes) for nodes in cluster]) >= blocksize:
                    block = list()
                    while len(block) < blocksize:
                        nodes = next(nodes_it)
                        if nodes:
                            block.append(nodes.popleft())
                    self._total_avcpus -= blocksize
                    # Great we found a match !
                    return sorted(block)

        raise RuntimeError('We should never end up here...')


class _NumaBalancedCpuIdDispenser(_NumaAbstractCpuIdDispencer):
    """
    Return blocks of CPUs that are chosen in such a way that they spread over
    all available NUMA zones (regardless of the NUMA distance between CPUs.
    """

    def __init__(self, cpuiterator, default_bsize=None):
        super(_NumaBalancedCpuIdDispenser, self).__init__(default_bsize=default_bsize)
        self._cpuiter = cpuiterator

    def __call__(self, blocksize):
        res = []
        while len(res) < blocksize:
            # Stop Iteration may be raised here... that's fine with us !
            res.append(next(self._cpuiter)[0])
        return sorted(res)


class _MetaCpuIdDispenser(object):
    """Group several :class:`_NumaAbstractCpuIdDispencer` into one."""

    def __init__(self, *dispensers):
        """
        :param dispensers: The list of dispensers that will be grouped by this object.
        """
        self._dispensers = deque(dispensers)

    def __iter__(self):
        return chain(* self._dispensers)

    def __call__(self, blocksize):
        while self._dispensers:
            try:
                return self._dispensers[0](blocksize)
            except StopIteration:
                self._dispensers.popleft()
        raise StopIteration()


@six.add_metaclass(abc.ABCMeta)
class NumaNodesInfo(collections_abc.Mapping):
    """Hold information on the system's NUMA nodes.

    Abstract class.
    """

    def __init__(self):
        self._nodes = dict()
        self._clustering_cache = None
        self._cyclic_cpus_cache = dict()
        self._fill_nodes()

    @abc.abstractmethod
    def _fill_nodes(self):
        """Fill the self._nodes dictionary with :class:`NumaNodeInfo` objects."""
        pass

    @abc.abstractmethod
    def _get_freesize(self, nodeid):
        """Return the amount of free memory for the **nodeid** NUMA node."""
        pass

    def __getitem__(self, nodeid):
        """Return the :class:`NumaNodeInfo` corresponding to the **nodeid** NUMA node."""
        return self._nodes[nodeid]

    def __len__(self):
        """Return the number of NUMA nodes on this system."""
        return len(self._nodes)

    def __iter__(self):
        """Iterate through NUMA nodes identifiers."""
        return iter(self._nodes)

    def freesize(self, nodeid):
        """Return the amount of free memory for the **nodeid** NUMA node."""
        if nodeid not in self:
            raise KeyError("The requested node does not exists")
        return self._get_freesize(nodeid)

    def __str__(self):
        s_out = 'There are {:d} nodes.\n'.format(len(self))
        for n, ni in self.items():
            s_out += 'Node {:d} description:\n{!s}\n'.format(n, ni)
        s_out += 'Distances matrix:\n'
        s_out += '       {:s}\n'.format(' '.join(['{:4d}'.format(i)
                                                  for i in sorted(self)]))
        s_out += '\n'.join(['{:-4d}:  {:s}'
                            .format(i, ' '.join(['{:4d}'.format(d)
                                                 for _, d in sorted(node.distances.items())]))
                            for i, node in sorted(self.items())])
        return s_out

    def numapacked_cpu_dispenser(self, smtlayout=None, default_bsize=None):
        """Return a new :class:`_NumaPackedCpuIdDispenser` object.

        :param smtlayout: A dictionary that associates a physical CPU ids to its
                          virtual CPUs
        :param default_bsize: The default blocksize
        """
        total_ncpus = sum([len(zi.cpus) for zi in self.values()])
        smt_replication = smtlayout and len(smtlayout) < total_ncpus
        if smt_replication:
            # SMT layout provided: Consider only the physical CPUs when
            # calculating the cpculist (SMT threads are added at the end...)
            nsmt_threads = total_ncpus // len(smtlayout)
            if total_ncpus % len(smtlayout) != 0:
                raise ValueError('Strange number of SMT threads !')
            physicalcpus = set(smtlayout.keys())
            numa_nodes_iterator = dict()
            for nnode, ninfo in self.items():
                linfo = NumaNodeInfo(set([c for c in ninfo.cpus if c in physicalcpus]),
                                     ninfo.distances, ninfo.totalsize)
                numa_nodes_iterator[nnode] = linfo
        else:
            # The SMT is not activated or the requested blocksize is too big
            numa_nodes_iterator = self

        # Obtain a hierarchical clustering of the nodes
        if len(self) == 1:
            nodesclust = dict()
        elif len(self) == 2:
            # Easy...
            nodesclust = {self[0].distances[1]: [set(self.keys()), ]}
        else:
            nodesclust = copy.copy(self.nodes_clustering)
        nodesclust[0] = [set([n, ]) for n in sorted(self.keys())]

        # Re-order things in order to spread the blocks all over the NUMA nodes
        distances = sorted(nodesclust.keys())
        for i, d in enumerate(distances[:-1]):
            mynodesclust = list(nodesclust[d])
            nextnodesclust = nodesclust[distances[i + 1]]
            mynewnodesclust = list()
            nextclusters_it = cycle(nextnodesclust)
            while mynodesclust:
                parentclust = next(nextclusters_it)
                itodo = None
                for iclust, c in enumerate(mynodesclust):
                    if c <= parentclust:
                        itodo = iclust
                        break
                if itodo is not None:
                    mynewnodesclust.append(mynodesclust[itodo])
                    del mynodesclust[itodo]
            nodesclust[d] = mynewnodesclust

        # A dictionary of available CPUS...
        nodes_avcpus = dict()
        for n, ninfo in numa_nodes_iterator.items():
            nodes_avcpus[n] = deque(sorted(ninfo.cpus))
        total_avcpus = sum([len(avcpus) for avcpus in nodes_avcpus.values()])

        xnodesclust = {d: [[nodes_avcpus[n] for n in c] for c in clusters]
                       for d, clusters in nodesclust.items()}

        dispensers = [_NumaPackedCpuIdDispenser(total_avcpus, xnodesclust,
                                                default_bsize=default_bsize), ]

        if smt_replication:
            # Add extra entries for SMT threads (if needed)
            for ismt in range(0, nsmt_threads - 1):
                nodes_avcpus = dict()
                for n, ninfo in numa_nodes_iterator.items():
                    nodes_avcpus[n] = deque([smtlayout[c][ismt]
                                             for c in sorted(ninfo.cpus)])

                xnodesclust = {d: [[nodes_avcpus[n] for n in c] for c in clusters]
                               for d, clusters in nodesclust.items()}

                dispensers.append(_NumaPackedCpuIdDispenser(total_avcpus, xnodesclust,
                                                            default_bsize=default_bsize))

            return _MetaCpuIdDispenser(* dispensers)
        else:
            return dispensers[0]

    def numabalanced_cpu_dispenser(self, smtlayout=None, default_bsize=None):
        """Return a new :class:`_NumaBalancedCpuIdDispenser` object.

        :param smtlayout: A dictionary that associates a physical CPU ids to its
                          virtual CPUs
        :param default_bsize: The default blocksize
        """
        return _NumaBalancedCpuIdDispenser(
            iter(self.numapacked_cpu_dispenser(smtlayout, default_bsize=1)),
            default_bsize=default_bsize
        )

    def numapacked_cpulist(self, blocksize, smtlayout=None):
        """Return a list of consecutive **blocksize** CPU ids.

        :param blocksize: The size of each block of CPUs
        :param smtlayout: A dictionary that associates a physical CPU ids to its
                          virtual CPUs

        Such CPU ids should be evenly distributed across NUMA zones while trying
        to minimise the NUMA distance between CPUs.
        """
        total_ncpus = sum([len(zi.cpus) for zi in self.values()])
        if not (0 < blocksize <= total_ncpus):
            raise ValueError('blocksize must be between 1 and the number of CPUs.')

        # Check for a pre-computed result
        cache_key = (blocksize, bool(smtlayout))
        if cache_key not in self._cyclic_cpus_cache:
            dispenser = self.numapacked_cpu_dispenser(smtlayout=smtlayout,
                                                      default_bsize=blocksize)
            self._cyclic_cpus_cache[cache_key] = [block for block in dispenser]

        return self._cyclic_cpus_cache[cache_key]

    def numabalanced_cpulist(self, blocksize, smtlayout=None):
        """Return a list of consecutive **blocksize** CPU ids.

        :param blocksize: The size of each block of CPUs
        :param smtlayout: A dictionary that associates a physical CPU ids to its
                          virtual CPUs

        Such CPU ids should be evenly distributed across NUMA zones in order to
        spread as much as possible (regardless of the NUMA distance between CPUs).
        """
        cyclic_singleblocks = self.numapacked_cpulist(1, smtlayout=smtlayout)
        return [[c[0] for c in cyclic_singleblocks[i * blocksize:(i + 1) * blocksize]]
                for i in range(len(cyclic_singleblocks) // blocksize)]

    @property
    def nodes_clustering(self):
        """
        Perform a hierarchical clustering over the NUMA nodes based on the
        inter-node distance (using a 'Farthest Point Algorithm' method)

        An harcoded version of the clustering algorithm is available, but a
        version based on scipy is also included (depending on the number of
        nodes on or another is chosen).
        """
        if self._clustering_cache is None:
            if len(self) <= 16:
                # If the number of nodes is small enough our hard-coded clustering
                # seems to be fast enough...
                self._clustering_cache = self._inhouse_nodes_clustering()
            else:
                try:
                    self._clustering_cache = self._scipy_nodes_clustering()
                except ImportError:
                    self._clustering_cache = self._inhouse_nodes_clustering()
        return self._clustering_cache

    def _scipy_nodes_clustering(self):
        import numpy as np
        import scipy.cluster.hierarchy as hie
        nz = len(self)
        allnodes = sorted(self.keys())
        # Compute the flat distance matrix
        flatmat = np.empty((nz * (nz - 1) // 2, ), dtype=np.int)
        i = 0
        for iz in allnodes:
            for jz in allnodes[iz + 1:]:
                flatmat[i] = self[iz].distances[jz]
                i += 1
        # Compute the linkage matrix
        lm = hie.linkage(flatmat, method='complete')
        # generate clusters
        mdistances = np.unique(hie.maxdists(lm))
        raw_clust_results = {d: hie.fcluster(lm, t=d, criterion='distance')
                             for d in mdistances}
        clust_results = dict()
        nodes_mapping = {i: n for i, n in enumerate(allnodes)}
        for d, res in raw_clust_results.items():
            dset = defaultdict(set)
            for i, c in enumerate(res):
                dset[c].add(nodes_mapping[i])
            clust_results[d] = [v for v in sorted(dset.values(), key=lambda x: min(x))]
        return clust_results

    def _inhouse_nodes_clustering(self):
        clust_results = dict()
        latest_clust_list = [[n, ] for n in sorted(self.keys())]
        for _ in range(len(self) - 1):
            d_ijs = defaultdict(list)
            for combi in combinations(range(len(latest_clust_list)), 2):
                distances = list()
                for i0 in latest_clust_list[combi[0]]:
                    for i1 in latest_clust_list[combi[1]]:
                        distances.append(self[i0].distances[i1])
                d_ijs[max(distances)].append(combi)
            best_d_ijs = min(d_ijs.keys())
            best_ij = d_ijs[best_d_ijs][0]
            combi = latest_clust_list[best_ij[0]] + latest_clust_list[best_ij[1]]
            latest_clust_list[best_ij[0]] = combi
            del latest_clust_list[best_ij[1]]
            clust_results[best_d_ijs] = copy.copy(latest_clust_list)
        clust_results = {float(d): [set(c) for c in sorted(clusters, key=lambda x: min(x))]
                         for d, clusters in clust_results.items()}
        return clust_results


class LibnumaGateway(Singleton):
    """Interface to the libnuma DLL."""

    def __init__(self):
        """
        The DLL is initialised on the first call.
        """
        self._lib = None

    @property
    def lib(self):
        """Return the libnuma DLL (provided by the ctypes package)."""
        if self._lib is None:
            libname = find_library("numa")
            if libname:
                self._lib = CDLL(libname)  # May raise OSError if the library is missing
            else:
                raise OSError('The libnuma library is not available on this system.')
            self._lib.numa_node_size64.restype = c_longlong
            if self._lib.numa_available() == -1:
                raise NotImplementedError('The NUMA library is instaled but cannot be used.')
        return self._lib

    def numa_node_size64(self, node):
        """Return the memory associated with a given **node**.

        :rtype: tuple
        :return: (total_memory, free_memory)
        """
        free = c_longlong()
        total = self.lib.numa_node_size64(node, byref(free))
        return (total, free.value)

    def __getattr__(self, name):
        """Re-route all the calls to the libnuma's DLL."""
        return getattr(self.lib, name)


class LibNumaNodesInfo(NumaNodesInfo):
    """Hold information on the system's NUMA nodes.

    The information about NUMA nodes is retrieved using the standard libnuma.
    """

    _gateway_class = LibnumaGateway

    def __init__(self, **kwargs):
        self._gateway = self._gateway_class(** kwargs)
        super(LibNumaNodesInfo, self).__init__()

    def _fill_nodes(self):
        """Fill the self._nodes dictionary with :class:`NumaNodeInfo` objects."""
        nodes_cpus = defaultdict(set)
        for c in range(0, self._gateway.numa_num_configured_cpus()):
            nodes_cpus[self._gateway.numa_node_of_cpu(c)].add(c)
        for i in range(0, self._gateway.numa_max_node() + 1):
            totalsize, _ = self._gateway.numa_node_size64(i)
            self._nodes[i] = NumaNodeInfo(nodes_cpus[i],
                                          {n: self._gateway.numa_distance(i, n)
                                           for n in range(0, self._gateway.numa_max_node() + 1)},
                                          totalsize)

    def _get_freesize(self, node):
        _, freesize = self._gateway.numa_node_size64(node)
        """Return the amount of free memory for the **nodeid** NUMA node."""
        return freesize
