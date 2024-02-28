# -*- coding: utf-8 -*-

import abc
import glob
import os
import os.path
import re
import logging
import numpy as np
import typing
from itertools import repeat
from multiprocessing import Pool, cpu_count

logger = logging.getLogger()


def read_file(filename, *args, **kwargs):
    """
    Instantiate a reader-like class for the given file.
    Guess the correct reader depending on the file type and content.

    :param filename: the file path to be read. May be a list of files to concatenate.
    :type filename: str (path-like) or list
    :returns: The corresponding reader-like object
    :rtype: object inheriting from reader
    """
    # TODO: Do the correct guess between available readers <07-09-21, Léo Viallon-Galinier> #
    return proreader(filename, *args, **kwargs)


class reader(abc.ABC):
    """
    Abstract class defining the API for all reader objects.
    """

    @abc.abstractmethod
    def __init__(self, filename):
        """
        Instantiate a reader object from a file identified by its path

        :param filename: file path to open, or  a list of files to concatenate
        :type filename: str (path-like) or list
        """

    @property
    @abc.abstractmethod
    def _association_names(self):
        """
        An associative dict relating usual names
        (altitude, aspect, slope, dz, t, etc.)
        to actual variable names. For internal use.

        :meta: private
        """

    @abc.abstractmethod
    def _get_varname(self, varname):
        """
        Get the canonical name (as stored in the data)
        from varname which can be a canonical name or an
        usual name (e.g. altitude, density, dz, t...)
        :param varname: Variable name
        :type varname: str
        :returns: Canonical name as stored in the data file
        :rtype: str

        :meta: private
        """

    def get_filename(self, convert_str=False):
        """
        Get the file name corresponding to the reader object

        :param convert_str: If true, always return a string instead of a list. Useful for display purpose
                            buy may return a path that would not be recognized by the system.
        :type convert_str: bool
        :returns: File name (or list)
        :rtype: str or list
        """

    @property
    def variables(self):
        """
        The list of all available variables
        :rtype: list of strings
        """
        return self.variables_desc.keys()

    @property
    @abc.abstractmethod
    def variables_desc(self):
        """
        The dict associating each variable to its metadata (dimensions,
        rank (1 or 2), full_name, dtype)
        :rtype: dict
        """

    def variable_desc(self, varname):
        """
        Get information on a specific variable, given its name on any form (short, long).
        See `variable_desc` for more details on reported information.

        :param varname: variable name (or alias)
        :type varname: str
        :returns: Dictionary of information on variable
        :rtype: dict
        """
        v = self._get_varname(varname)
        return self.variables_desc[v] if v in self.variables_desc else None

    @property
    @abc.abstractmethod
    def variables_t(self):
        """
        The list of all available variables with time dimension
        :rtype: list of strings
        """

    @property
    @abc.abstractmethod
    def variable_dz(self):
        """
        Get the variable for thicknesses of layers (dz)
        :returns: Variable name
        :rtype: str
        """

    @property
    def variable_ram(self):
        """
        The variable containing penetration resistance (RAM).
        Used for profile plotting.
        :returns: variable name
        :rtype: str
        """

    @property
    @abc.abstractmethod
    def variable_default(self):
        """
        The default variable to plot
        :returns: variable name
        :rtype: str
        """

    @property
    @abc.abstractmethod
    def variable_grain(self):
        """
        The variable containing grain shape
        :returns: variable name
        :rtype: str
        """

    @abc.abstractmethod
    def colorbar_variable(self, varname: str):
        """
        The colorbar name to be associated to the variable
        or None if no colorbar is associated to considered variable

        :param varname: the variable name
        :returns: colorbar name
        :rtype: str
        """

    @abc.abstractmethod
    def limits_variable(self, varname: str):
        """
        The usual limits for the considered variable

        :param varname: the variable name
        :returns: range as (min, max)
        :rtype: tuple
        """

    @property
    @abc.abstractmethod
    def variables_selection_point(self):
        """
        Get the list of variables usable to select a point, with its type and
        possible choices (when relevant). Each variable is represented by a
        python dictionary containing keys:
         * name: variable name
         * full_name: the variable full name/. this name is recognized as varname
                      for other method calls
         * type: may be choices or float or int
         * choices: the array of possible values
         * limits: if type is float or int, a tuple of size 2 describing the
           range of the data (min, max)

        :returns: List of variables to be used to select a point
        :rtype: list of dict
        """

    @abc.abstractmethod
    def get_points(self, selector: dict = {}) -> list:
        """
        Get all the points corresponding to the selection made by the
        selector dictionary.

        :param selector: Dict which keys must be variable names from
        `~proreader.self.variables_selection_point` and values the
        selected value.

        :param selector: dict of selected values for each variable
        :returns: list of `Point`
        """

    def get_point(self, selector: dict = {}) -> list:
        """
        Same as get_points but raise a ValueError if there is not exactly one point
        corresponding to the selection.

        :returns: Point corresponding to the selection
        :rtype: `Point` identifier
        :raises: ValueError if not exactly one point in selection
        """
        points = self.get_points(selector=selector)
        if len(points) > 1 or len(points) == 0:
            raise ValueError('{} points found'.format(len(points)))
        return points[0]

    @abc.abstractmethod
    def variables_choices(self, selector: dict = {}) -> dict:
        """
        Get the possible choices remaining after selecting some values
        with the selector dict. For ``selector`` description, please
        refer to `~reader.get_points` documentation.

        :returns: Possible choices for each variable (keys are variable
                  names and values are list of remaining choices).
        :rtype: dict
        """

    @property
    def additional_choices(self) -> dict:
        """
        Additional variables to be chosen to select data properly.
        Depends on the type of file used.

         * name: name of the choice
         * type: may be choices or float or int
         * choices: the array of possible values
         * limits: if type is float or int, a tuple of size 2 describing the
                   range of the data (min, max)
         * default_value: If provided, replace none value by this default value.
        """
        return {}

    @abc.abstractmethod
    def get_time(self, begin=None, end=None):
        """
        Get the time dimension
        :param begin: A begin time/date (remove data before this date)
        :param end: A end time/date (remove data after this date)
        :returns: the time dimension axis values
        :rtype: numpy.array
        """

    @abc.abstractmethod
    def get_data(self, varname: str, point, additional_options: dict = None, fillnan=False, members=False,
                 begin=None, end=None):
        """
        Get the data for selected variable and point

        :param varname: The variable name
        :param point: The point identifier
        :param additional_options: A set of additional options to select the data. Keys from additional_choices.
        :param fillnan: Whether or not to fill the nan values. Defaults to False (do not fill nan values).
                        All other value will be used to fill the data.
        :param members: If not false, use `~reader.get_data_members` instead. See corresponding documentation.
        :param begin: A begin date (remove data before this date)
        :param end: A end date (remove data after this date)
        :returns: The selected data
        :rtype: numpy.array
        """

    @abc.abstractmethod
    def get_data_members(self, varname, point, additional_options: dict = None, fillnan=False, members='all',
                         begin=None, end=None):
        """
        Get a list of data for selected variable and point, for all members or
        a subset of all available.

        :param varname: The variable name
        :param point: The point identifier
        :param additional_options: A set of additional options to select the data. Keys from additional_choices.
        :param fillnan: Whether or not to fill the nan values. Defaults to False (do not fill nan values).
                        All other value will be used to fill the data.
        :param members: 'all' to get all members or a list of members numbers
        :param begin: A begin date (remove data before this date)
        :param end: A end date (remove data after this date)
        :returns: List of the selected data for all members and list of members names
        :rtype: list of numpy.array, list of member numbers
        """


class proreader(reader):
    """
    Class designed to read data as provided by SURFEX-Crocus snow model
    (``PRO.nc`` files).
    """

    _association_names_defaut = {
            'ZS': ['altitude', 'alt'],
            'time': ['t'],
            'RSN_VEG': ['density', 'rho', 'snowrho'],
            'SNOWDZ': ['dz'],
            }

    _range_defaults = {
            'WSN_VEG': (0, 40),
            'SNOWRO': (0, 500),
            'SNOWTEMP': (220, 274),
            'tsnl': (220, 274),
            'SNOWLIQ': (0, 35),
            'SNOWDZ': (0, 0.2),
            'SNOWDEND': (-0.1, 1),
            'SNOWSPHER': (-0.1, 1),
            'SNOWSIZE': (0, 0.01),
            'SNOWSSA': (-1, 60),
            'SNOWSHEAR': (0, 30),
            'RSN_VEG': (0, 600),
            'ASN_VEG': (0, 300),
            'ACC_RAT': (0, 20),
            'NAT_RAT': (0, 20),
            'SNOWIMP1': (-10, 0),
            'SNOWIMP2': (-10, 0),
            }

    _colorbar_defaults = {
            'SNOWTYPE': 'grains',
            'SNOWTEMP': 'tempK',
            'tsnl': 'tempK',
            'SNOWLIQ': 'lwc',
            'SNOWIMP1': 'echelle_log',
            'SNOWIMP2': 'echelle_log_sahara',
            'NAT_RAT': 'ratio_cisaillement',
            'ACC_RAT': 'ratio_cisaillement',
            }
    _default_colorbar = 'viridis'

    _selection_point_defaults = {
            'xx': {'type': 'float'},
            'yy': {'type': 'float'},
            }

    _variables_default = ['SNOWTYPE', 'SNOWSSA']
    _variables_dz = ['SNOWDZ', 'Dsnw']
    _variables_grain = ['SNOWTYPE']
    _variables_ram = ['SNOWRAM']
    _variables_log = ['SNOWIMP1', 'SNOWIMP2']

    _name_variable_dz = ['']
    _name_variable_t = ['time']
    _name_attribute_full_name = 'long_name'

    def __init__(self, filename):
        """
        Instantiate a reader object from a file identified by its path

        :param filename: file path to open
        :type filename: str (path-like) or list
        """
        self._is_list_filename = isinstance(filename, list) or '*' in filename
        if isinstance(filename, list):
            self._mainfilename = filename[0]
        elif '*' in filename:
            filename = glob.glob(filename)
            self._mainfilename = filename[0]
        else:
            self._mainfilename = filename
        self._filename = filename

        self._variables = {}    # Plottable variables with description
        self._variables_t = []  # Variables with time dimension
        self._variables_p = {}  # Variables for point selection
        self._variables_p_values = {}
        self._npoints = 0
        # Also defined below:
        # - self._time
        # - self._association_names

        # Read file
        from snowtools.utils.prosimu import prosimu_auto

        with prosimu_auto(self._mainfilename) as ff:
            _variables_raw_list = ff.listvar()
            self._npoints = 1
            self._ntile = ff.gettiledim()
            for dimname in ff.Points_dimensions:
                self._npoints = self._npoints * ff.getlendim(dimname)
            for v in _variables_raw_list:
                dimensions = list(ff.getdimvar(v))
                if v in list(self._selection_point_defaults.keys()):
                    long_x = len(ff.read(ff.Points_dimensions[0]))
                    long_y = len(ff.read(ff.Points_dimensions[1]))
                    values = np.zeros((long_x, long_y))
                    if v == ff.Points_dimensions[0]:
                        for i in range(long_y):
                            values[:, i] = ff.read(v)
                        self._variables_p_values[v] = values.flatten()
                        vardesc = {'name': v, **self._infer_point_selection_type(v, ff)}
                        self._variables_p[v] = vardesc
                    if v == ff.Points_dimensions[1]:
                        for i in range(long_x):
                            values[i, :] = ff.read(v)
                        self._variables_p_values[v] = values.flatten()
                        vardesc = {'name': v, **self._infer_point_selection_type(v, ff)}
                        self._variables_p[v] = vardesc
                if ff.Number_of_Patches in dimensions:
                    dimensions.remove(ff.Number_of_Patches)
                for t_name in ff.Number_of_Tiles:
                    if t_name in dimensions:
                        dimensions.remove(t_name)
                has_point_dim = True
                for i in range(len(ff.Points_dimensions)):
                    dimname = ff.Points_dimensions[i]
                    if dimname in dimensions:
                        dimensions.remove(dimname)
                    else:
                        has_point_dim = False

                # Identify variables for point selection
                if has_point_dim and len(dimensions) == 0:
                    self._variables_p_values[v] = ff.read(v)
                    vardesc = {'name': v, **self._infer_point_selection_type(v, ff)}
                    self._variables_p[v] = vardesc

                # Select plotable variables and get information on it
                if len(dimensions) > 0 and len(dimensions) < 3:
                    if 'snow_layer' in dimensions:
                        vardesc = {
                            'name': v, 'full_name': self._get_full_name(v, ff),
                            'rank': len(dimensions), 'dtype': ff.gettypevar(v),
                            'dimensions': dimensions, 'has_snl': True}
                    else:
                        vardesc = {
                            'name': v, 'full_name': self._get_full_name(v, ff),
                            'rank': len(dimensions), 'dtype': ff.gettypevar(v),
                            'dimensions': dimensions, 'has_snl': False}
                    self._variables[v] = vardesc

                    # Identify variables with time dimension
                    if 'time' in dimensions:
                        self._variables_t.append(v)

            # Sorting variables by name
            self._variables = {key: self._variables[key] for key in sorted(self._variables.keys())}
            self._variables_p = {key: self._variables_p[key] for key in sorted(self._variables_p.keys())}
            self._variables_t.sort()

            # Add the point dimension for selection
            self._variables_p_values['point'] = np.arange(self._npoints)
            self._variables_p['point'] = {'name': 'point',
                                          'full_name': 'Point nr',
                                          'rank': 1,
                                          'dtype': np.int_,
                                          'type': 'int',
                                          'choices': self._variables_p_values['point'],
                                          }

            # Get and store time dimension
            if not self._is_list_filename:
                self._time = ff.readtime()

        if self._is_list_filename:
            with prosimu_auto(self._filename) as ff:
                self._time = ff.readtime()

            pass

        # Create association names array
        self._association_names_ = self._association_name_generator()

    def additional_choices(self) -> dict:
        """
        Additional variables to be chosen to select data properly.
        Depends on the type of file used.

         * name: name of the choice
         * type: may be choices or float or int
         * choices: the array of possible values
         * limits: if type is float or int, a tuple of size 2 describing the
                   range of the data (min, max)
         * default_value: If provided, replace none value by this default value.
        """
        if self._ntile is None or self._ntile <= 1:
            return {}
        return {'tile': {'name': 'Patch (tile)',
                         'type': 'int',
                         'limits': [0, self._ntile - 1],
                         'default_value': 0,
                         }
               }

    def _association_name_generator(self):
        _association_names = {}
        for v in self._association_names_defaut:
            if v in self._variables:
                for alias in self._association_names_defaut[v]:
                    _association_names[alias] = v
        for v in self._variables:
            if 'full_name' in self._variables[v]:
                _association_names[self._variables[v]['full_name']] = v
        return _association_names

    @property
    def _association_names(self):
        return self._association_names_

    def _get_full_name(self, v, ff):
        """
        Get the full name in a netCDF file from variable name and
        open prosimu object

        :param v: variable name
        :type v: str
        :param ff: prosimu object
        :type ff: `~snowtools.utils.prosimu.prosimu`
        :returns: The long name
        :rtype: str
        """
        if self._name_attribute_full_name in ff.listattr(v) and \
                len(ff.getattr(v, self._name_attribute_full_name)) > 0:
            full_name = ff.getattr(v, self._name_attribute_full_name) + ' ({})'.format(v)
        elif v in self._association_names_defaut:
            full_name = self._association_names_defaut[v][0] + ' ({})'.format(v)
        else:
            full_name = v
        return full_name

    def get_filename(self, convert_str=False):
        """
        Get the file name corresponding to the reader object

        :param convert_str: If true, always return a string instead of a list. Useful for display purpose
                            buy may return a path that would not be recognized by the system.
        :type convert_str: bool
        :returns: File name (or list)
        :rtype: str or list
        """
        if convert_str and self._is_list_filename:
            try:
                cp = os.path.commonpath(self._filename)
                rp = [p[len(cp):] for p in self._filename]
            except ValueError:
                return ', '.join(self._filename)
            s = cp + '[{}]'.format(', '.join(rp))
            return s
        else:
            return self._filename

    def _infer_point_selection_type(self, v, ff):
        """
        Infer selection type to apply for variable v.
        Note that ``self._variables_p_values[v]`` should be defined.

        Get the following information into a dict:
         * full_name
         * type: may be choices or float or int
         * choices: if type is choices, the list of possible values
         * limits: if type is float or int, a tuple of size 2 describing the
                   range of the data (min, max)

        :param v: variable name
        :type v: str
        :param ff: prosimu object
        :type ff: prosimu.prosimu
        :returns: The long name
        :rtype: str
        """
        full_name = self._get_full_name(v, ff)
        typ = 'choices'
        limits = None
        choices = np.sort(np.unique(self._variables_p_values[v]))
        if v in self._selection_point_defaults:
            _defaults = self._selection_point_defaults[v]
            if 'type' in _defaults:
                typ = _defaults['type']
            else:
                typ = None
            if 'full_name' in _defaults:
                full_name = _defaults['full_name']
            if 'limits' in _defaults:
                limits = _defaults['full_name']
        if typ == 'choices':
            return {
                    'full_name': full_name,
                    'type': 'choices',
                    'choices': choices,
                    }
        else:
            if typ is None:
                if np.issubdtype(self._variables_p_values[v], np.integer):
                    typ = 'int'
                elif np.issubdtype(self._variables_p_values[v], np.inexact):
                    typ = 'float'
                elif np.issubdtype(self._variables_p_values[v], np.bool_):
                    typ = 'int'
                else:
                    raise ValueError('Unknown data type')
            if limits is None:
                limits = (np.min(self._variables_p_values[v]), np.max(self._variables_p_values[v]))
            return {
                    'full_name': full_name,
                    'type': typ,
                    'limits': limits,
                    'choices': choices,
                    }

    def _get_varname(self, varname):
        """
        Get the canonical name (as stored in the data)
        from varname which can be a canonical name or an
        usual name (e.g. altitude, density, dz, t...)
        :param varname: Variable name
        :type varname: str
        :returns: Canonical name as stored in the data file
        :rtype: str

        :meta: private
        """
        if varname in self._association_names:
            return self._association_names[varname]
        else:
            return varname

    @property
    def variables_desc(self):
        """
        The dict associating each variable to its metadata (dimensions,
        rank (1 or 2), full_name, dtype)
        """
        return self._variables

    @property
    def variables_t(self):
        """
        The list of all available variables with time dimension
        :rtype: list of strings
        """
        return self._variables_t

    @property
    def variables_log(self):
        """
        The list of all variables that should be plotted with log scale
        :rtype: list of strings
        """
        return self._variables_log

    @property
    def variable_dz(self):
        """
        Get the variable for thicknesses of layers (dz)
        :returns: Variable name
        :rtype: str
        """
        for v in self._variables_dz:
            if v in self._variables:
                return v
        return None

    @property
    def variable_default(self):
        """
        The default variable to plot
        :returns: variable name
        :rtype: str
        """
        for v in self._variables_default:
            if v in self._variables:
                return v
        if len(self._variables) > 0:
            return self._variables[0]
        return None

    @property
    def variable_grain(self):
        """
        The variable containing grain shape
        :returns: variable name
        :rtype: str
        """
        for v in self._variables_grain:
            if v in self._variables:
                return v
        return None

    @property
    def variable_ram(self):
        """
        The variable containing RAM
        :returns: variable name
        :rtype: str
        """
        for v in self._variables_ram:
            if v in self._variables:
                return v
        return None

    def colorbar_variable(self, varname: str):
        """
        The colorbar name to be associated to the variable
        or None if no colorbar is associated to considered variable

        :param varname: the variable name
        :returns: colorbar name
        :rtype: str
        """
        v = self._get_varname(varname)
        if v in self._colorbar_defaults:
            return self._colorbar_defaults[v]
        else:
            return self._default_colorbar

    def limits_variable(self, varname: str):
        """
        The usual limits for the considered variable

        :param varname: the variable name
        :returns: range as (min, max)
        :rtype: tuple
        """
        v = self._get_varname(varname)
        if v in self._range_defaults:
            return self._range_defaults[v]
        else:
            return (None, None)

    @property
    def variables_selection_point(self):
        """
        Get the list of variables usable to select a point, with its type and
        possible choices (when relevant). Each variable is represented by a
        python dictionary containing keys:
         * name: variable name
         * full_name: the variable full name
         * type: may be choices or float or int
         * choices: if type is choices, the list of possible values
         * limits: if type is float or int, a tuple of size 2 describing the
           range of the data (min, max)

        :returns: List of variables to be used to select a point
        :rtype: list of dict
        """
        return self._variables_p

    def get_points(self, selector: dict = {}, _raw=False) -> list:
        """
        Get all the points corresponding to the selection made by the
        selector dictionary.

        :param selector: Dict which keys must be variable names from
        `~proreader.self.variables_selection_point` and values the
        selected value.

        :param selector: dict of selected values for each variable
        :param _raw: Return a boolean array instead of points number. For internal use only.
        :type _raw: bool
        :returns: list of `Point`
        """
        if len(selector) == 0:
            allpoints = np.arange(self._npoints)
            return list(allpoints)

        # Special case of point key in selector
        if 'point' in selector:
            return [selector['point']]

        # Check varnames in selector
        lkey = []
        selection = None
        for key, value in selector.items():
            v = self._get_varname(key)
            if v in lkey:
                raise ValueError('Selector passed twice for variable {}, {}'.format(key, v))
            if v not in self._variables_p:
                raise ValueError('Selector variable {} unknown'.format(key))
            if self._variables_p[v]['type'] == 'choices':
                corrected_value = value
            else:
                idx_nearest = (np.abs(self._variables_p_values[v] - value)).argmin()
                corrected_value = self._variables_p_values[v][idx_nearest]
            if selection is None:
                selection = np.full_like(self._variables_p_values[v], True, dtype=bool)
            selection = selection & (self._variables_p_values[v] == corrected_value)
        if selection is None:
            selection = np.full((len(selector), self._npoints), True, dtype=bool)
        return np.where(selection.flatten())[0]

    def variables_choices(self, selector: dict = {}) -> dict:
        """
        Get the possible choices remaining after selecting some values
        with the selector dict. For ``selector`` description, please
        refer to `~reader.get_points` documentation.

        :returns: Possible choices for each variable (keys are variable
                  names and values are list of remaining choices).
        :rtype: dict
        """
        selectedpoints = self.get_points(selector=selector, _raw=True)
        r = {}
        for v in self._variables_p:
            r[v] = np.sort(np.unique(self._variables_p_values[v][selectedpoints]))
        return r

    def get_time(self, begin=None, end=None):
        """
        Get the time dimension
        :param begin: A begin time/date (remove data before this date)
        :param end: A end time/date (remove data after this date)
        :returns: the time dimension axis values
        :rtype: numpy.array
        """
        return self._time

    def _get_data(self, filename, varname: str, point: typing.Union[int, list],
                  additional_options: dict = None, fillnan=False, begin=None, end=None):
        """
        Get data for a given filename (to be used by both get_data with
        the default filename and by get_data_members with each member
        name).

        See documentation of get_data for arguments description.
        """
        if type(begin) == int:
            begin = self.get_time()[begin]
        if type(end) == int:
            end = self.get_time()[end]

        v = self._get_varname(varname)
        
        # Tile management
        if additional_options is not None and 'tile' in additional_options:
            tile = additional_options['tile']
        else:
            tile = None

        from snowtools.utils.prosimu import prosimu_auto
        with prosimu_auto(filename) as ff:
            data = ff.read(v, selectpoint=point, tile=tile)
            # TODO: the order of dimension is not ensured by prosimu ! Check order of time and layer dimensions here !

        # Date filtering
        if begin is not None and end is not None:
            select = (self._time >= begin) * (self._time <= end)
        elif begin is not None:
            select = self._time >= begin
        elif end is not None:
            select = self._time <= end
        else:
            select = ...
        data = data[select]

        # Filling nan
        if fillnan is not False and np.issubdtype(data.dtype, np.inexact):
            data[np.isnan(data)] = fillnan

        return data

    def get_data(self, varname: str, point: typing.Union[int, list], additional_options: dict = None,
                 fillnan=False, members=False, begin=None, end=None):
        """
        Get the data for selected variable and point

        :param varname: The variable name
        :param point: The point identifier or the list of point identifier
        :param additional_options: A set of additional options to select the data. Keys from additional_choices.
        :param fillnan: Whether or not to fill the nan values. Defaults to False (do not fill nan values).
                        All other value will be used to fill the data.
        :param members: If not false, use `~reader.get_data_members` instead. See corresponding documentation.
        :param begin: A begin date (remove data before this date) or an integer
        :param end: A end date (remove data after this date) or an integer
        :returns: The selected data. Dimensions are [members], time, layers, [points].
        :rtype: numpy.array
        """
        if members is not False:
            return self.get_data_members(varname, point, additional_options=additional_options,
                                         fillnan=fillnan, members=members, begin=begin, end=end)

        return self._get_data(self._filename, varname, point, additional_options=additional_options,
                              fillnan=fillnan, begin=begin, end=end)

    @staticmethod
    def _get_member_filenames_one(filename):
        m = re.search(r'^(?P<prefix>.*)[/\\](?P<mb>mb[0-9]*)[/\\](?P<suffix>.*)$', filename)
        if m is None:
            raise ValueError('Could not find members list from file path {}'.format(filename))
        lf = []  # List of filenames
        ln = []  # List of numbers
        for elem in os.listdir(m.group('prefix')):
            m2 = re.match('mb(?P<nr>[0-9]*)', elem)
            if m2 is None:
                continue
            fn = os.path.join(m.group('prefix'), elem, m.group('suffix'))
            nr = int(m2.group('nr'))
            if not os.path.isfile(fn):
                continue
            lf.append(fn)
            ln.append(nr)
        return lf, ln

    def _get_member_filenames(self):
        """
        Return a list of filenames containing all filenames for all members and a list of
        members numbers.

        :returns: List of members filenames, List of members numbers
        :rtype: List of str or list of list of str, list of int
        """
        if self._is_list_filename:
            llf = []
            lln = []
            for fn in self._filename:
                fn_abs = os.path.abspath(fn)
                lf, ln = self._get_member_filenames_one(fn_abs)
                llf.append(lf)
                lln.append(ln)

            # check for consistency between length and generate full list
            # in the right order (list of members)
            lmb = []  # The final member list (list of list)
            ln = list(lln[0])

            # Get the correct list number
            for elem in lln:
                toremove = []
                for n in ln:
                    if n not in elem:
                        toremove.append(n)
                for n in toremove:
                    ln.remove(n)

            if len(ln) < 1:
                raise ValueError('Could not find a subset of members that have all files together')

            # Fill lmb
            for nr in ln:
                lmbi = []
                for i in range(len(lln)):
                    j = lln[i].index(nr)
                    lmbi.append(llf[i][j])
                lmb.append(lmbi)

            return lmb, ln

        else:
            filename = os.path.abspath(self._filename)
            lf, ln = self._get_member_filenames_one(filename)

            return lf, ln

    def get_data_members(self, varname, point, additional_options: dict = None, fillnan=False, members='all',
                         begin=None, end=None):
        """
        Get a list of data for selected variable and point, for all members or
        a subset of all available.

        :param varname: The variable name
        :param point: The point identifier
        :param additional_options: A set of additional options to select the data. Keys from additional_choices.
        :param fillnan: Whether or not to fill the nan values. Defaults to False (do not fill nan values).
                        All other value will be used to fill the data.
        :param members: 'all' to get all members or a list of members numbers
        :param begin: A begin date (remove data before this date)
        :param end: A end date (remove data after this date)
        :returns: List of the selected data for all members and list of members names
        :rtype: list of numpy.array, list of member numbers
        """
        lf, ln = self._get_member_filenames()
        if members == "all":
            lf2 = lf
            lnr = ln
        else:
            lf2 = [f for num in members for i, f in zip(ln, lf) if num == i]
            lnr = [i for num in members for i, f in zip(ln, lf) if num == i]

        with Pool(cpu_count()) as p:
            ldata = p.starmap(self._get_data,
                              zip(lf2, repeat(varname), repeat(point), repeat(additional_options),
                                  repeat(fillnan), repeat(begin), repeat(end)))
        return ldata, lnr
