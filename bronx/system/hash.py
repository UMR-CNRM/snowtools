# -*- coding: utf-8 -*-

"""
Interface to the Python's :mod:`hashlib` module that generates hashes on files, strings, ...
"""

from __future__ import print_function, division, absolute_import, unicode_literals
import six

import hashlib
import io
import os


class HashAdapter(object):
    """
    This class provides convenient methods to generate and check hash sums on
    files, File objects or strings.
    """

    _PREFERRED_BLOCKSIZE = 8 * 1024 * 1024  # 8 Mb

    def __init__(self, algorithm):
        """
        :param str algorithm: The name of an available hash algorithm (see the
            :meth:`algorithms` static method)
        """
        if algorithm not in self.algorithms():
            raise ValueError('The "{:s}" hash algorithm is not available.'.format(algorithm))
        self._h_algo = algorithm

    @property
    def algorithm(self):
        """Hash algorithm in use within this instance."""
        return self._h_algo

    @staticmethod
    def algorithms():
        """List of available hash algorithms."""
        if hasattr(hashlib, 'algorithms_guaranteed'):
            return hashlib.algorithms_guaranteed
        else:
            return hashlib.algorithms  # For old Python interpreters

    def _hinstance(self):
        """Return a new Hash object (see hashlib documentation)."""
        return hashlib.new(self._h_algo)

    def _hinstance_from_fh(self, i_fh):
        """Return a new Hash object computed from the *i_fh* File object."""
        h = self._hinstance()
        actual_bsize = max(self._PREFERRED_BLOCKSIZE // h.block_size, 1) * h.block_size
        block = i_fh.read(actual_bsize)
        while block:
            h.update(block)
            block = i_fh.read(actual_bsize)
        return h

    def _read_hashline(self, i_fh):
        """Read a one line hashfile."""
        rawstr = i_fh.readline()
        if isinstance(rawstr, bytes):
            rawstr = rawstr.decode(encoding='ascii', errors='ignore')
        return rawstr.split(' ')[0]

    def file2hash(self, input_file):
        """Returns a hash string.

        :param input_file: Path to a file or opened File-like object
        """
        if isinstance(input_file, six.string_types):
            # input_file should be a file path
            with io.open(input_file, 'rb') as i_fh:
                h = self._hinstance_from_fh(i_fh)
        elif hasattr(input_file, 'seek') and hasattr(input_file, 'read'):
            # input_file should be a file like object
            input_file.seek(0)
            h = self._hinstance_from_fh(input_file)
        return six.text_type(h.hexdigest())

    def file2hash_fh(self, input_file):
        """Returns a File-like object that contains a hash string

        :param input_file: Path to a file or opened File-like object
        """
        output = six.BytesIO()
        output.write(self.file2hash(input_file).encode(encoding='utf-8'))
        output.seek(0)
        return output

    def file2hash_file(self, input_file, output_file):
        """Create a plain file that contains a hash string

        :param input_file: Path to a file or opened File-like object
        :param output_file: Path to the output file
        """
        with io.open(output_file, 'w', encoding='utf-8') as o_fh:
            o_fh.write(self.file2hash(input_file))
        return output_file

    def string2hash(self, input_data):
        """Return a hash string.

        :param input_data: Data on which the hash is computed
        """
        h = self._hinstance()
        h.update(str(input_data).encode(encoding='utf-8'))
        return h.hexdigest()

    def filecheck(self, input_file, reference):
        """Check if *input_file* checks out with the hash given in *reference*

        :param input_file: Path to a file or opened File-like object
        :param reference: Reference hash data (path to a file, opened File-like object
            or string that contains the reference hash sum)
        """
        # Get the reference hash string
        if isinstance(reference, six.string_types):
            if os.path.isfile(reference):
                with io.open(reference, 'r', encoding='utf-8') as i_fh:
                    hashref = self._read_hashline(i_fh)
            else:
                hashref = reference
        elif hasattr(reference, 'seek') and hasattr(reference, 'read'):
            reference.seek(0)
            hashref = self._read_hashline(reference)
        # Compute input file's hash
        myhash = self.file2hash(input_file)
        return myhash == hashref
