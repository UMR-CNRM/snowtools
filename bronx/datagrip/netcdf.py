# -*- coding: utf-8 -*-

"""
Utility class to deal with netcdf files.

It allows to generate a simplified representation of a NetCDF Dataset structure
(see the :meth:`netcdf_dataset_structure` method). Additionaly, to NetCDF
datasets can be compared: both the structure of the file and the data will be
compared (see the :meth:`netcdf_dataset_diff` and :meth:`netcdf_file_diff`
methods).

Here is a demo:

First we need a helper function that creates a NetCDF dataset for demonstration
purposes:

    >>> def create_demo_netcdf4(nc_filename):
    ...     demoset = netCDF4.Dataset(nc_filename, mode='w')
    ...     demoset.setncattr('title', 'NetCDF Demo Data')
    ...     x_u = demoset.createDimension('X', 5)
    ...     y_u = demoset.createDimension('Y', 2)
    ...     g1 = demoset.createGroup('group1')
    ...     g2 = demoset.createGroup('group2')
    ...     vx = demoset.createVariable('x', np.float64, ('X', ), zlib=True)
    ...     vx[:] = np.arange(1, 7, 1.4)
    ...     vy = demoset.createVariable('y', np.float64, ('Y', ), zlib=True)
    ...     vy[:] = [0, 1]
    ...     T = g1.createVariable('T', np.float32, ('X', 'Y'))
    ...     T.setncattr('unit', 'Kelvin')
    ...     T[...] = np.reshape(np.arange(270, 280, 1), (5, 2))
    ...     T[1, 0] = np.ma.masked
    ...     z_u = g2.createDimension('Z', size=None)
    ...     T = g2.createVariable('T', np.float32, ('X', 'Y', 'Z'))
    ...     T.setncattr('unit', 'Kelvin')
    ...     T[..., 0] = np.reshape(np.arange(270, 280, 1), (5, 2))
    ...     T[..., 1] = np.reshape(np.arange(300, 310, 1), (5, 2))
    ...     T[0, 0, 0] = np.nan
    ...     return demoset

Create a NetCDF dataset and display its structure:

    >>> import tempfile
    >>> demofile1 = tempfile.NamedTemporaryFile(mode='wb', delete=True)
    >>> demoset1 = create_demo_netcdf4(demofile1.name)
    >>> demodesc1 = netcdf_dataset_structure(demoset1)
    >>> demodesc1 == {
    ...     'dimensions': {'X': {'size': 5, 'unlimited': False},
    ...                    'Y': {'size': 2, 'unlimited': False}},
    ...     'groups': {'group1': {'dimensions': {},
    ...                           'groups': {},
    ...                           'ncattrs': {},
    ...                           'variables': {'T': {'datatype': np.float32,
    ...                                               'dimensions': ('X', 'Y'),
    ...                                               'filters': {'complevel': 0,
    ...                                                           'fletcher32': False,
    ...                                                           'shuffle': False,
    ...                                                           'zlib': False},
    ...                                               'ncattrs': {'unit': 'Kelvin'},
    ...                                               'shape': (5, 2)}}},
    ...                'group2': {'dimensions': {'Z': {'size': 2, 'unlimited': True}},
    ...                           'groups': {},
    ...                           'ncattrs': {},
    ...                           'variables': {'T': {'datatype': np.float32,
    ...                                               'dimensions': ('X', 'Y', 'Z'),
    ...                                               'filters': {'complevel': 0,
    ...                                                           'fletcher32': False,
    ...                                                           'shuffle': False,
    ...                                                           'zlib': False},
    ...                                               'ncattrs': {'unit': 'Kelvin'},
    ...                                               'shape': (5, 2, 2)}}}},
    ...     'ncattrs': {'title': 'NetCDF Demo Data'},
    ...     'variables': {'x': {'datatype': np.float64,
    ...                         'dimensions': ('X',),
    ...                         'filters': {'complevel': 4,
    ...                                     'fletcher32': False,
    ...                                     'shuffle': True,
    ...                                     'zlib': True},
    ...                         'ncattrs': {},
    ...                         'shape': (5,)},
    ...                   'y': {'datatype': np.float64,
    ...                         'dimensions': ('Y',),
    ...                         'filters': {'complevel': 4,
    ...                                     'fletcher32': False,
    ...                                     'shuffle': True,
    ...                                     'zlib': True},
    ...                         'ncattrs': {},
    ...                         'shape': (2,)}}}
    True

First comparison attempt (compare a dataset to itself):

    >>> netcdf_dataset_diff(demoset1, demoset1)
    == Comparison of the two netcdf structures: ==
    (legend: created: "+"  deleted: "-"  unchanged: "="  updated: "?")
    No differences
    == Comparison of data available in both netcdf datasets: ==
    4 data arrays out of 4 are identical
    True

Create a different dataset and perform a comparison:

    >>> demofile2 = tempfile.NamedTemporaryFile(mode='wb', delete=True)
    >>> demoset2 = create_demo_netcdf4(demofile2.name)
    >>> demoset2.delncattr('title')
    >>> demoset2['group1']['T'][1, 0] = 276.
    >>> demoset2['group1']['T'][1, 1] = np.ma.masked
    >>> demoset2['group2']['T'].unit = 'Celsius'
    >>> demoset2['group2']['T'][1, 1, 1] = 0
    >>> s2_g3 = demoset2.createGroup('group3')
    >>> s2_T = s2_g3.createVariable('T', np.float32, ('X', 'Y'))
    >>> s2_T[...] = 0
    >>> netcdf_dataset_diff(demoset1, demoset2)  # doctest: +ELLIPSIS
    == Comparison of the two netcdf structures: ==
    (legend: created: "+"  deleted: "-"  unchanged: "="  updated: "?")
    ? groups:
      | + group3: NetCDF4ParentStructure::<<as_dict:: ...>>
      | ? group2:
      |   | ? variables:
      |   |   | ? T:
      |   |   |   | ? ncattrs:
      |   |   |   |   | ? unit: before='Kelvin' after='Celsius'
    ? ncattrs:
      | - title: 'NetCDF Demo Data'
    == Comparison of data available in both netcdf datasets: ==
    2 data arrays out of 4 are identical
    /group1/T differs
    /group2/T differs
    False

Clean things up...

    >>> demofile1.close()
    >>> demofile2.close()

"""

from __future__ import print_function, absolute_import, unicode_literals, division

import netCDF4
import numpy as np

from bronx.stdtypes.tracking import RecursiveMappingTracker


class NetCDF4ParentStructure(dict):
    """
    Represents the structure of a :class:`NetCDF4.Dataset` or
    :class:`NetCDF4.Group` object.
    """

    def __init__(self, netcdf4_obj):
        """
        :param netcdf4_obj: The Dataset or Group object to process.
        """
        # Sanity checks
        if not isinstance(netcdf4_obj, (netCDF4.Dataset, netCDF4.Group)):
            raise ValueError("'{!r}' is not an appropriate netCDF4 object"
                             .format(netcdf4_obj))
        super(NetCDF4ParentStructure, self).__init__()
        # Read the object's properties
        self['ncattrs'] = {k: netcdf4_obj.getncattr(k) for k in netcdf4_obj.ncattrs()}
        self['groups'] = {k: NetCDF4ParentStructure(g)
                          for k, g in netcdf4_obj.groups.items()}
        self['dimensions'] = {k: NetCDF4DimensionStructure(d)
                              for k, d in netcdf4_obj.dimensions.items()}
        self['variables'] = {k: NetCDF4VariableStructure(v)
                             for k, v in netcdf4_obj.variables.items()}


class NetCDF4DimensionStructure(dict):
    """Represents the structure of a :class:`NetCDF4.Dimension` object."""

    def __init__(self, netcdf4_obj):
        """
        :param netcdf4_obj: The Dimension object to process.
        """
        # Sanity checks
        if not isinstance(netcdf4_obj, netCDF4.Dimension):
            raise ValueError("'{!r}' is not an appropriate netCDF4 object"
                             .format(netcdf4_obj))
        super(NetCDF4DimensionStructure, self).__init__()
        # Read the object's properties
        self['size'] = netcdf4_obj.size
        self['unlimited'] = netcdf4_obj.isunlimited()


class NetCDF4VariableStructure(dict):
    """Represents the structure of a :class:`NetCDF4.Variable` object."""

    def __init__(self, netcdf4_obj):
        """
        :param netcdf4_obj: The Variable object to process.
        """
        # Sanity checks
        if not isinstance(netcdf4_obj, netCDF4.Variable):
            raise ValueError("'{!r}' is not an appropriate netCDF4 object"
                             .format(netcdf4_obj))
        super(NetCDF4VariableStructure, self).__init__()
        # Read the object's properties
        self['datatype'] = netcdf4_obj.datatype
        self['dimensions'] = netcdf4_obj.dimensions
        self['shape'] = netcdf4_obj.shape
        self['ncattrs'] = {k: netcdf4_obj.getncattr(k) for k in netcdf4_obj.ncattrs()}
        self['filters'] = netcdf4_obj.filters()


def netcdf_dataset_structure(netcdf4_obj):
    """
    Generate a representation of the structure of a
    :class:`NetCDF4.Dataset` object.
    """
    return NetCDF4ParentStructure(netcdf4_obj)


def netcdf_dataset_diff(netcdf4_ref, netcdf4_new, verbose=True):
    """Compare two :class:`NetCDF4.Dataset` objects.

    Both the structure of the NetCDF Dataset and the data will be compared.
    """
    # Compare the netcdf structures
    rmt = RecursiveMappingTracker(
        netcdf_dataset_structure(netcdf4_ref),
        netcdf_dataset_structure(netcdf4_new)
    )
    rc = not (len(rmt))
    if verbose or not rc:
        print('== Comparison of the two netcdf structures: ==')
        rmt.dump_legend()
        rmt.differences()

    def all_variables(netcdf4_obj):
        """Helper fonction to recursively list the NetCDF variables."""
        variables = dict()
        for nc_var in netcdf4_obj.variables.values():
            where = nc_var.group().path.rstrip('/') + '/' + nc_var.name
            variables[where] = nc_var
        for nc_group in netcdf4_obj.groups.values():
            variables.update(all_variables(nc_group))
        return variables

    # Generate the list of variables
    ref_vars = all_variables(netcdf4_ref)
    new_vars = all_variables(netcdf4_new)
    # Compare the data for common items
    common_vars = set(ref_vars.keys()) & set(new_vars.keys())
    updated_vars = set()
    for path_to_var in common_vars:
        rc = True
        ref_values = ref_vars.pop(path_to_var)[...]
        new_values = new_vars.pop(path_to_var)[...]
        # Specifically look for NaNs
        try:
            ref_nans = np.isnan(ref_values).filled(False)
            new_nans = np.isnan(new_values).filled(False)
        except TypeError:
            # a TypeError is raised if ref_values or new_values are not
            # numerical data (e.g strings)
            numeric_type = False
        else:
            numeric_type = True
        if numeric_type and (np.any(ref_nans) or np.any(new_nans)):
            if not np.array_equal(ref_nans, new_nans):
                updated_vars.add(path_to_var)
                continue
            ref_values[ref_nans] = np.ma.masked
            new_values[new_nans] = np.ma.masked
        if np.any(ref_values.mask) or np.any(new_values.mask):
            if not np.array_equal(new_values.mask, ref_values.mask):
                updated_vars.add(path_to_var)
                continue
            ref_values = ref_values.compressed()
            new_values = new_values.compressed()
        if not np.array_equal(new_values, ref_values):
            updated_vars.add(path_to_var)
    # Summary
    if verbose or updated_vars:
        print('== Comparison of data available in both netcdf datasets: ==')
        print('{:d} data arrays out of {:d} are identical'
              .format(len(common_vars) - len(updated_vars), len(common_vars)))
        if updated_vars:
            rc = False
            for path_to_var in sorted(updated_vars):
                print('{:s} differs'.format(path_to_var))
    return rc


def netcdf_file_diff(netcdf_file_ref, netcdf_file_new):
    """Compare two NetCDF files."""
    return netcdf_dataset_diff(netCDF4.Dataset(netcdf_file_ref, mode='r'),
                               netCDF4.Dataset(netcdf_file_new, mode='r'))


if __name__ == '__main__':
    import doctest
    doctest.testmod()
