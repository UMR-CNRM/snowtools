# -*- coding: utf-8 -*-

import abc
import logging
import typing

import epygram

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
    return proreader(filename, *args, **kwargs)


class reader(abc.ABC):
    """
    Abstract class defining the API for all reader objects.
    """

    _variables_default = {}
    _colorbar_defaults = {}
    _default_colorbar = 'viridis'
    _range_defaults = {}
    _association_names = {}

    @abc.abstractmethod
    def __init__(self, filename):
        """
        Instantiate a reader object from a file identified by its path

        :param filename: file path to open, or  a list of files to concatenate
        :type filename: str (path-like) or list
        """

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
    def variables_desc(self):
        """
        The dict associating each variable to its metadata (dimensions,
        rank (1 or 2), full_name, dtype)
        :rtype: dict
        """
        return self._variables

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
    @abc.abstractmethod
    def additional_choices(self) -> dict:
        """
        Additional variables to be chosen to select data properly.
        Depends on the type of file used.

         * name: name of the choice
         * len: length
        """

    @abc.abstractmethod
    def get_time(self, begin=None, end=None):
        """
        Get the time dimension
        :param begin: A begin time/date (remove data before this date)
        :param end: A end time/date (remove data after this date)
        :returns: the time dimension axis values
        :rtype: list
        """

    @abc.abstractmethod
    def get_data(self, varname: str, time, additional_options: dict = None, fillnan=False):
        """
        Get the data for selected variable and point

        :param varname: The variable name
        :param time: The time identifier (int or datetime.datetime)
        :param additional_options: A set of additional options to select the data. Keys from additional_choices.
        :param fillnan: Whether or not to fill the nan values. Defaults to False (do not fill nan values).
                        All other value will be used to fill the data.
        :returns: The selected data
        :rtype: epygram 2D Field
        """


class proreader(reader):

    """
    Object for reading 2D files through epygram
    to be used in 2D plots.
    """
    _association_names_defaut = {
        'ZS': ['altitude', 'alt'],
        'time': ['t'],
        'DSN_T_ISBA': ['height', 'depth', 'htn'],
        'WSN_T_ISBA': ['swe', ],
    }

    _range_defaults = {
        'DSN_T_ISBA': (0, 5),
        'WSN_T_ISBA': (0, 5),
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

    _variables_log = ['SNOWIMP1', 'SNOWIMP2']

    _name_attribute_full_name = 'long_name'


    # TODO: Add special case for wind  <04-12-25, Léo Viallon-Galinier> #

    def __init__(self, filename):
        """TODO: to be defined. """
        self.resource = epygram.formats.resource(filename, openmode='r')
        self._additional_choices = {}
        self.filename = filename
        self._variables = {}
        self.t_name = 'time'

        # Identifying relevant variables
        for v in self.resource.listfields():
            v_i = self.resource.ncinfo_field(v)
            dimensions = {x: n for x, n in v_i['dimensions'].items()}
            attributes = v_i['metadata']
            if len(dimensions) < 2:
                pass
            t_dims = set(dimensions).intersection(
                set(epygram.config.netCDF_usualnames_for_standard_dimensions['T_dimension']))
            if len(t_dims) > 0:
                self.t_name = list(t_dims)[0]
            x_dims = set(dimensions).intersection(
                set(epygram.config.netCDF_usualnames_for_standard_dimensions['X_dimension']))
            y_dims = set(dimensions).intersection(
                set(epygram.config.netCDF_usualnames_for_standard_dimensions['Y_dimension']))
            if y_dims == 0 or x_dims == 0:
                continue
            for add_dim in set(dimensions).difference(x_dims, y_dims, t_dims):
                if add_dim not in self._additional_choices:
                    self._additional_choices[add_dim] = {'len': dimensions[add_dim]}

            fullname = attributes[self._name_attribute_full_name] if self._name_attribute_full_name in attributes else v
            self._association_names[fullname] = v
            self._variables[v] = {
                'full_name': fullname,
                'name': v,
                'dimensions': set(dimensions)}

        # Sorting variables by name
        self._variables = {key: self._variables[key] for key in sorted(self._variables.keys())}

        # Getting time dimension
        for v in self.resource.listfields():
            if v in epygram.config.netCDF_usualnames_for_standard_dimensions['T_dimension']:
                t = self.resource.readfield(v)
                break
        else:
            # TODO: Crash  <04-12-25, Léo Viallon-Galinier> #
            pass
        self._time = [d.get() for d in t.validity]
        self._timebasis = t.validity[0].getbasis()

    def get_filename(self, convert_str=False):
        return self.filename

    def get_time(self, begin=None, end=None):
        """
        Get the time dimension
        :param begin: A begin time/date (remove data before this date)
        :param end: A end time/date (remove data after this date)
        :returns: the time dimension axis values
        :rtype: list
        """
        return self._time

    def get_data(self, varname: str, time, additional_options: dict = None, fillnan=False):
        """
        Get the data for selected variable and point

        :param varname: The variable name
        :param time: The time identifier (int or datetime.datetime)
        :param additional_options: A set of additional options to select the data. Keys from additional_choices.
        :param fillnan: Whether or not to fill the nan values. Defaults to False (do not fill nan values).
                        All other value will be used to fill the data.
        :returns: The selected data
        :rtype: epygram 2D Field
        """
        return

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
        return {k: {'name': k,
                    'type': 'int',
                    'limits': [0, v['len']],
                    'default_value': 0}
                for k, v in self._additional_choices.items()}
        # return {'tile': {'name': 'Patch (tile)',
        #                  'type': 'int',
        #                  'limits': [0, self._ntile - 1],
        #                  'default_value': 0,
        #                  }
        # }
