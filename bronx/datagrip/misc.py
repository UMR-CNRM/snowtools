# -*- coding: utf-8 -*-

"""
Miscellaneous I/O tools.
"""

from __future__ import print_function, absolute_import, unicode_literals, division

import csv
import io
import yaml
from collections import OrderedDict

#: No automatic export
__all__ = []


def read_dict_in_CSV(filename):
    """
    Reads a .csv file formatted as follow:

        * on the first line is described the delimiter
        * on the second line is described the 'priority' of the dict.
        * all subsequent lines contain equivalent of a dict, with key/value duets
          separated by the delimiter

    """
    field_dict = []
    with io.open(filename, 'r') as f:
        delimiter = str(f.readline()[0])
        file_priority = str(f.readline()[0:-1])
        field_table = csv.reader(f, delimiter=delimiter)
        for field in field_table:
            # syntax example of field description:
            # name:FIELDNAME;param:value;...
            if len(field) > 1 and field[0][0] != '#':
                fd = {}
                for kv in field:
                    k, v = kv.split(':')
                    try:
                        fd[k] = int(v)
                    except ValueError:
                        fd[k] = v
                field_dict.append(fd)

    return field_dict, file_priority


class OrderedYAMLLoader(yaml.SafeLoader):
    """Loader for YAML that returns an OrderedDict."""
    def __init__(self, *args, **kwargs):
        def _dict_constructor(loader, node):
            return OrderedDict(loader.construct_pairs(node))
        self.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG,
                             _dict_constructor)
        super(OrderedYAMLLoader, self).__init__(*args, **kwargs)


def load_ordered_yaml(filename):
    """Proxy to load with OrderedYAMLLoader."""
    with io.open(filename, 'r') as yamlfh:
        odict = yaml.load(yamlfh, Loader=OrderedYAMLLoader)
    return odict
