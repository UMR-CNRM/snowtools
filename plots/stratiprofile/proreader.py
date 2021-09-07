# -*- coding: utf-8 -*-

import abc
import logging

logger = logging.getLogger()


def read_file(filename):
    """
    Instantiate a reader-like class for the given file.
    Guess the correct reader depending on the file type and content.

    :param filename: the file path to be read
    :type filename: str (path-like)
    :returns: The corresponding reader-like object
    :rtype: object inheriting from reader
    """
    # TODO: Do the correct guess between available readers <07-09-21, Léo Viallon-Galinier> #
    return proreader(filename)


class reader(abc.ABC):
    """
    Abstract class defining the API for all reader objects.
    """

    @property
    @abc.abstractmethod
    def variables(self):
        """
        The list of all available variables
        :rtype: list of strings
        """

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

    @abc.abstractmethod
    def variables_selection_point(self):
        """
        Get the list of variables usable to select a point, with its type and
        possible choices (when relevant). Each variable is represented by a
        python dictionary containing keys:
         * name: variable name
         * full_name: the variable full name
         * typer: may be choices or float or int
         * choices: if type is choices, the list of possible values
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

        :param selector: Dict whick keys must be variable names from
        `~proreader.self.variables_selection_point` and values the
        selected value.

        :param selector: dict of selected values for each variable
        :returns: list of `Point`
        """

    def get_point(self, *args, **kwargs):
        """
        Same as get_points but raise a ValueError if there is not exactly one point
        corresponding to the selection.

        :returns: Point corresponding to the selection
        :rtype: `Point` identifier
        """
        points = self.get_points()
        if len(points) > 1 or len(points) == 0:
            raise ValueError('{} points found'.format(len(points)))
        return points[0]

    @abc.abstractmethod
    def variables_choices(self, selector: dict = {}) -> dict:
        """
        Get the possible choices ramining after selecting some values
        with the selector dict. For ``selector`` description, please
        refer to `~reader.get_points` documentation.

        :returns: Posisble choices for each variable (keys are variable
                  names and values are list of remaining choices).
        :rtype: dict
        """

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
    def get_data(self, varname: str, point, fillnan=False, members=False,
                 begin=None, end=None):
        """
        Get the data for selected variable and point

        :param varname: The variable name
        :param point: The point identifier
        :param fillnan: Whether or not to fill the nan values. Defaults to False (do not fill nan values).
                        All other value will be used to fill the data.
        :param members: If not false, use `~reader.get_data_members` instead. See corresponding documentation.
        :param begin: A begin date (remove data before this date)
        :param end: A end date (remove data after this date)
        :returns: The selected data
        :rtype: numpy.array
        """

    @abc.abstractmethod
    def get_data_members(self, varname, point, fillnan=False, members='all',
                         begin=None, end=None):
        """
        Get a list of data for selected variable and point, for all members or
        a subset of all available.

        :param varname: The variable name
        :param point: The point identifier
        :param fillnan: Whether or not to fill the nan values. Defaults to False (do not fill nan values).
                        All other value will be used to fill the data.
        :param members: 'all' to get all members or a list of members numbers
        :param begin: A begin date (remove data before this date)
        :param end: A end date (remove data after this date)
        :returns: List of the selected data for all members
        :rtype: list of numpy.array
        """


class proreader(reader):
    """
    Class designed to read data as provided by SURFEX-Crocus snow model
    (``PRO.nc`` files).
    """
    pass
