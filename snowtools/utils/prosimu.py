# -*- coding: utf-8 -*-

"""
Created on 4 oct. 2012

:Authors:
    - lafaysse
    - LVG

This module contains the ``prosimu`` class used to read simulation files
(netCDF format) as produced by SURFEX/Crocus for instance.

More in details, ``prosimu1d`` and ``prosimu2d`` classes are provided to
read 1D files (ony one spatial dimension, Number_of_points) or 2D files
(gridded simulations with x and y axes). Unless your application is
specific to 1D or 2D files, please use ``prosimu_auto`` that will
instantiate the appropriate class (``prosimu1d`` or ``prosimu2d``).

A short example of how to use this module and read time dimension and
snow density variables (``RSN_VEG``) for all layers at point in massif
11, 2700m, northern aspect on 40 degree slope:

.. code-block:: python

   from snowtools.utils.prosimu import prosimu_auto

   with prosimu_auto('/path/to/my/PRO_XXX.nc') as ff:
           time= ff.readtime()
           point = ff.get_point(ZS=2700, aspect=0, slope=40, massif=11)
           density = ff.read('RSN_VEG', point=point)

``time`` and ``density`` now contains the data in the form of numpy arrays
for the selected point.


Example to get total snow water equivalent of the snowpack (variable
``WSN_T_ISBA``) across time for all points:

.. code-block:: python

   from snowtools.utils.prosimu import prosimu_auto

   with prosimu_auto('/path/to/my/PRO_XXX.nc') as ff:
           time= ff.readtime()
           swe_total = ff.read('WSN_T_ISBA')


Most useful methods are:

* :meth:`prosimuAbstract.gettime` to read tim dimension
* :meth:`prosimuAbstract.listvar` to have a list of available variables
* :meth:`prosimuAbstract.read` to read data from one variable
* :meth:`prosimuAbstract.get_point` to help in selecting a point across spatial dimensions
"""

import os
import abc
import logging

import netCDF4
import numpy as np
import glob

from snowtools.utils.FileException import FileNameException, DirNameException, FileOpenException, VarNameException
from snowtools.utils.FileException import TimeException, MultipleValueException
from snowtools.utils.S2M_standard_file import StandardCROCUS

try:
    import cftime
except ImportError:
    cftime = None

DEFAULT_NETCDF_FORMAT = 'NETCDF3_CLASSIC'


def prosimu_auto(path, ncformat=DEFAULT_NETCDF_FORMAT, openmode='r', **kwargs):
    """
    Factory function that guess the correct class to return among
    :class:`prosimu1d` or :class:`prosimu2d`.

    :param path: file path or file path list
    :type path: str or path-like or list
    :ncformat: netCDF format only in case of creation
    :type ncformat: str
    :param openmode: Open mode (r, r+ or w generally)
    :type openmode: str
    :param kwargs: Other arguments passed to the instantiated class
    """
    if isinstance(path, list) and os.path.isfile(path[0]):
        toanalyzepath = path[0]
    elif os.path.isfile(path):
        toanalyzepath = path
    else:
        toanalyzepath = None

    targetclass = prosimu1d

    if toanalyzepath is not None:
        with netCDF4.Dataset(toanalyzepath, 'r') as ncdf:
            if 'Number_of_points' not in ncdf.dimensions:
                if 'xx' in ncdf.dimensions and 'yy' in ncdf.dimensions:
                    targetclass = prosimu2d
                elif 'location' in ncdf.dimensions:
                    targetclass = prosimu_old

    return targetclass(path, ncformat=ncformat, openmode=openmode, **kwargs)


class prosimuAbstract(abc.ABC):
    """
    Abstract class designed to read simulations files
    Describe the interface provided to access simulations.

    :param path: path of the file to read
    :type path: path-like or list
    :param ncformat: NetCDF format to use.
    :type ncformat: str
    :param openmode: open mode (mainly ``r``, ``w`` or ``r+``)
    :type openmode: str

    Do not forget to close the file at the end or use a context manager:

    .. code-block:: python

       with prosimu(filename) as ff:
           time= ff.readtime()
           var = ff.read(varname)
           # Do your stuff

    To read data (in variables), see :meth:`read` or :meth:`read_var`

    Note that the recommended file format is NETCDF4_CLASSIC. Prosimu also allow for using NETCDF3_CLASSIC.
    However, NETCDF4 is discouraged because this format is not supported by netCDF4.MFDataset function that
    is used to open several files at one time.

    ``path`` could be a list of files to concatenate along an undefined length dimension
    (should be unique, e.g. time dimension). Note that in this case, NETCDF4 format is highly discouraged as
    it implies a full type conversion and copy of the data.

    ``path`` could also be a string with a glob sign ``*`` to automatically fill a list of files to concatenate.

    Please note that in case of opening simultaneously several files, only the ``'r'`` open mode is allowed.
    """

    @property
    @abc.abstractmethod
    def Points_dimensions(self):
        pass

    Number_of_Patches = 'Number_of_Patches'
    Number_of_Tiles = ['tile', 'Number_of_Tile', 'Number_of_Patches']

    _rewrite_dims = {}
    _rewrite_vars = {}

    def __init__(self, path, ncformat='NETCDF3_CLASSIC', openmode='r'):
        """
        forceread : si True force lors de la lecture d'une variable par la
        méthode read_var à mettre l'ensemble des données de cette variable en
        cache - utile lorsque de grands nombre d'accès à la même variable sont
        nécessaires
        """
        self.dataset = None

        # BC add the possibility to give wildcards to prosimu
        if type(path) is str:
            if openmode == 'w':
                glob_path = [path]
            else:
                glob_path = glob.glob(path)
            if len(glob_path) == 0:
                raise FileNameException(path)
            path = sorted(glob_path)

        # At this point path is supposed to be a list of at least 1 element.

        # Reading of several files: use netCDF4.MFDataset
        # And do not allow r+ or w open modes.
        if len(path) > 1:
            if openmode == 'r':
                for fichier in path:
                    if not os.path.isfile(fichier):
                        raise FileNameException(fichier)
                try:
                    self.dataset = netCDF4.MFDataset(path)
                    self.path = path
                    self.mfile = 1

                except ValueError:
                    # Conversion in netcdf4_classic
                    import xarray
                    classic_path = list()
                    for fichier in path:
                        newname = 'CLASSIC_' + fichier
                        ds = xarray.open_dataset(fichier)
                        ds.to_netcdf(path=newname, format='NETCDF4_CLASSIC')
                        ds.close()
                        classic_path.append(newname)
                    self.dataset = netCDF4.MFDataset(classic_path)
                    self.path = classic_path
                    self.mfile = 1
            else:
                logging.error(('prosimu - Open several NetCDF dataset with openmode \'{}\''
                               ' is not supported. '
                               'Only \'r\' mode is supported.').format(openmode))
                raise FileNameException('+'.join(path))

        # Reading/Writting only one file: use StandardCROCUS (class deriving of netCDF4.Dataset)
        elif os.path.isfile(path[0]):
            self.path = path[0]
            self.mfile = 0
            try:
                if openmode == "w":
                    self.dataset = StandardCROCUS(self.path, openmode, format=ncformat)
                else:
                    self.dataset = StandardCROCUS(self.path, openmode)
            except Exception:
                raise FileOpenException(self.path)

        # Create the file if openmode is w and one file only
        elif openmode == "w":
            dirname = os.path.dirname(path[0])
            if len(dirname) > 0:
                if not os.path.isdir(dirname):
                    raise DirNameException(path[0])

            self.dataset = StandardCROCUS(path[0], openmode, format=ncformat)
            self.path = path[0]
            self.mfile = 0

        # Else, we could not either read or create the file -> crash
        else:
            logging.error("I am going to crash because there is a filename exception: \
                        file {} not found and openmode is not 'w'".format(path))
            raise FileNameException(path[0])

        self.varcache = {}

        if "time" in self.dataset.dimensions:
            self.timedim = range(len(self.dataset.dimensions['time']))
        else:
            self.timedim = None

        # Get the dimension of the dataset
        # self.pointsdim is a list of indices available
        #                  (list of integers)
        # self.pointsdim_n give its length
        #                  (integer)
        # self.pointsdim_l give the dimension of each point dimension of the dataset
        #                  (list of integers)
        self.pointsdim_l = self._get_pointsdim()
        if self.pointsdim_l is None:
            self.pointsdim = None
            self.pointsdim_n = None
        else:
            self.pointsdim_n = 1
            for n in self.pointsdim_l:
                self.pointsdim_n *= n
            self.pointsdim = range(self.pointsdim_n)

    def _get_pointsdim(self):
        """
        Return a list of dimensions for Points_dimensions
        """
        if isinstance(self.Points_dimensions, str):
            points_dimname = [self.Points_dimensions]
        else:
            # Else assume it is a list or similar iterable
            points_dimname = self.Points_dimensions

        r = []
        for dimname in points_dimname:
            if dimname in self.dataset.dimensions:
                r.append(len(self.dataset.dimensions[dimname]))
            else:
                return None
        return r

    def _check_varname(self, varname):
        """
        chack the variable name exists or raise a VarNameException
        and if necessary perform a rewritting of variable

        :param varname: Variable name to test
        :type varname: str
        :returns: Actual variable name (possibly substituted from varname
        :rtype: str
        :raises: VarNameException
        """
        listvar = self.listvar()
        if varname in listvar:
            return varname
        else:
            if varname in self._rewrite_vars and self._rewrite_vars[varname] in listvar:
                return self._rewrite_vars[varname]
            else:
                raise VarNameException(varname, self.path)

    def _check_dimname(self, dimname):
        """
        chack the variable name exists or raise a dimnameException
        and if necessary perform a rewritting of variable

        :param dimname: Variable name to test
        :type dimname: str
        :returns: Actual variable name (possibly substituted from dimname
        :rtype: str
        :raises: dimnameException
        """
        listdim = self.listdim()
        if dimname in listdim:
            return dimname
        else:
            if dimname in self._rewrite_dims and self._rewrite_dims[dimname] in listdim:
                return self._rewrite_dims[dimname]
            else:
                raise VarNameException(dimname, self.path)

    def gettiledim(self):
        """
        Return the number of tile in the file or None if no tile dimension is found.

        :returns: Number of tile
        :rtype: int or None
        """
        dimensions = self.listdim()
        for key in self.Number_of_Tiles:
            if key in dimensions:
                dimension = dimensions[key]
                return dimension.size
        return None

    def force_read_in_cache(self):
        """
        Force la lecture des variables du netcdf sous forme de tableau numpy.
        Ces tableaux sont stockés dans l'attribut varcache de la classe

        Le cache est utilisé par la méthode read_var, son utilisation n'est pas
        implémentée pour les autres méthodes

        Utile lorsque de nombreuses lectures de la même variable sont requises
        """
        self.varcache = {}
        for varname, var in self.dataset.variables.items():
            self.varcache[varname] = var[Ellipsis]

    def format(self):
        """
        Get the format of the undelying netCDF file (e.g. NETCDF3_CLASSIC)

        :returns: The format of NetCDF file
        :rtype: str
        """
        try:
            return self.dataset.data_model
        except AttributeError:
            logging.warning("WARNING : old version of netCDF4 module. We cannot check the format of your forcing file. \
                             We assume that you provide a NETCDF3_CLASSIC file.")
            return 'NETCDF3_CLASSIC'

    def listdim(self):
        """
        Return a copy of the list of dimensions present in the netCDF file
        """
        return self.dataset.dimensions.copy()

    def listvar(self):
        """
        Return the list of variables present in the netCDF file

        :returns: list of variables
        :rtype: list
        """
        return list(self.dataset.variables.keys())

    def getlendim(self, dimname):
        """
        Return the number of dimensions

        :returns: number of dimensions
        :rtype: int
        """
        return len(self.dataset.dimensions[dimname])

    def getdimvar(self, varname):
        """
        The dimensions of a specific variable

        :param varname: the variable name
        :type varname: str
        :returns: dimensions of varname
        :rtype: numpy array
        """
        varname = self._check_varname(varname)
        return np.array(self.dataset.variables[varname].dimensions)

    def getrankvar(self, varname):
        """
        Get the rank (number of dimensions) of a variable

        :param varname: the variable name
        :type varname: str
        :returns: rank
        :rtype: int
        """
        varname = self._check_varname(varname)
        return len(self.dataset.variables[varname].shape)

    def listattr(self, varname):
        """
        Get the list of attributtes attached to a variable

        :param varname: the variable name
        :type varname: str
        :returns: atributes
        """
        varname = self._check_varname(varname)
        return self.dataset.variables[varname].ncattrs()

    def getattr(self, varname, attname):
        """
        Get the value of an attribute of a variable

        :param varname: the variable name
        :type varname: str
        :param attname: the attribute name
        :type attname: str
        :returns: the attribute value
        """
        varname = self._check_varname(varname)
        return getattr(self.dataset.variables[varname], attname)

    def gettypevar(self, varname):
        """
        Return the dtype of a variable

        :param varname: the variable name
        :type varname: str
        :returns: the corresponding dtype
        """
        varname = self._check_varname(varname)
        return self.dataset.variables[varname].dtype

    def getfillvalue(self, varname):
        """
        Get the fill value for a variable

        :param varname: Variable name
        :type varname: str
        :returns: the fill value
        """
        varname = self._check_varname(varname)
        if hasattr(self.dataset.variables[varname], "_FillValue"):
            return self.dataset.variables[varname]._FillValue
        else:
            return None

    def infovar(self, varname):
        # Vérification du nom de la variable
        varname = self._check_varname(varname)
        return self.gettypevar(varname), self.getrankvar(varname), self.getdimvar(varname), self.getfillvalue(varname), self.listattr(varname)

    def readtime_for_copy(self):
        """
        Get the time raw variable and units

        :returns: time variable, time units
        """
        time = self.dataset.variables["time"]
        return time, time.units

    def readtime(self):
        """
        Get the time dimension of the netCDF file

        :returns: time axis data
        :rtype: numpy array
        """
        # Vérification du nom de la variable
        if "time" not in list(self.dataset.variables.keys()):
            raise VarNameException("time", self.path)

        if self.mfile == 1:
            time_base = self.dataset.variables["time"]
            time = netCDF4.MFTime(time_base, calendar='standard')
        else:
            time = self.dataset.variables["time"]
        if netCDF4.__version__ >= '1.4.0' and cftime is not None and cftime.__version__ >= '1.1.0':
            return np.array(netCDF4.num2date(time[:], time.units, only_use_cftime_datetimes=False,
                                             only_use_python_datetimes=True))
        else:
            return np.array(netCDF4.num2date(time[:], time.units))

    def get_time(self, time_asdatetime):
        """
        Renvoie l'indice de la dimension time correspondant au datetime donné en
        argument
        """
        return np.where(self.readtime() == time_asdatetime)[0][0]

    def read_var(self, variable_name, **kwargs):
        """
        Read a variable from netCDF file. Note that this function does not support patches.

        :param variable_name: nom de la variable
        :param kwargs: spécifier la sous-sélection sous la forme  dimname = value
                         ou dimname est le nom de la dimension d'intéret, value est une valeur
                         numérique ou un objet slice python pour récupérer une plage de valeurs
        :returns: un tableau numpy.ma.MaskedArray (on peut toujours remplacer les éléments masqués
                  par un indicateur de valeur manquante  pas implémenté)

        Exemples:

        .. code-block:: python

           snowemp = prosimu.read_var('SNOWTEMP', time=0, Number_of_points = slice(100,125))
           snowtemp = prosimu.read_var('SNOWTEMP', time=slice(0,10,2), Number_of_points=1, snow_layer=slice(0,10))

        peut-être utilisé en combinaison avec les méthodes get_point et get_time
        pour récupérer un point / un instant donné :

        .. code-block:: python

           snowtemp = prosimu.read_var('SNOWTEMP', time=self.get_time(datetime(2018,3,1,9)),
                       Number_of_points = self.get_point(massif_num=3,slope=20,ZS=4500,aspect=0))
        """
        # Sanitize dimensions (kwargs)
        args = {}
        for arg, val in kwargs.items():
            arg2 = self._check_dimname(arg)
            args[arg2] = val

        # Sanitize input variable name
        variable_name = self._check_varname(variable_name)

        # Default value : Remove patches
        if self.Number_of_Patches not in args.keys():
            args[self.Number_of_Patches] = 0

        # Check cache
        ncvariable = self.dataset.variables[variable_name]
        if variable_name in self.varcache:
            ncvariable_data = self.varcache[variable_name]
        else:
            ncvariable_data = self.dataset.variables[variable_name]

        # Extraction
        dims = ncvariable.dimensions
        slices = []
        for dimname in dims:
            slices.append(args.get(dimname, slice(None)))
        slices = tuple(slices)
        result = ncvariable_data[slices]
        return result

    @abc.abstractmethod
    def get_points(self, **kwargs):
        """
        Return the values of dimension :data:`number of points<Number_of_points>` correpsonding to
        a subset defined by variables such as aspect, elevation (ZS), massif_num, slope, latitude, longitude
        according to named arguments passed to the function.

        :returns: List of points
        :rtype: list
        """
        pass

    def get_point(self, **kwargs):
        """
        Same as :meth:`get_points` but raise an exception if there is not exactly one point.
        """
        point_list = self.get_points(**kwargs)
        if len(point_list) > 1:
            raise MultipleValueException()
        elif len(point_list) == 0:
            raise IndexError('No point matching the selection')
        return point_list[0]

    @abc.abstractmethod
    def read(self, varname, fill2zero=False, selectpoint=None, keepfillvalue=False, tile=None, removetile=True,
             needmodif=False, hasDecile=False):
        """
        Read data from a variable in the netCDF file.

        :param varname: the variable name
        :type varname: str
        :param fill2zero: if True, will replace undefined data with the value of 0. Otherwise, ``np.nan`` is used
                          except if ``keepfillvalue`` is set.
        :type fill2zero: bool
        :param selectpoint: Select a point. If -1 or None, select all points.
        :type selectpoint: integer or list of integers
        :param tile: Select a tile.
        :type tile: int
        :param keepfillvalue: Do not replace the undefined data with ``np.nan`` and keep the fill value used in the
                              netCDF file.
        :type keepfillvalue: bool
        :param needmodif: If True, return also the variable object
        :type needmodif: bool
        :param hasDecile: Deprecated, please do not use.
        :param removetile: Deprecated, see tile instead.

        :returns: Numpy data array containing the data of the selected variable.
                  Note that order of dimensions is the same as in in file except if selectpoint is a list.
                  In this case, the resulting point dimension is appended as the last dimension.
        :rtype: numpy array
        """
        pass

    # Pour compatibilité anciens codes, on conserve les routines obsolètes read1d et read2d

    def read1d(self, varname, fill2zero=False, indpoint=0):
        """
        .. warning:: Obsolete method

        :meta private:
        """
        varname = self._check_varname(varname)
        return self.read(varname, fill2zero=fill2zero, selectpoint=indpoint)

    def read2d(self, varname, fill2zero=False):
        """
        .. warning:: Obsolete method

        :meta private:
        """
        varname = self._check_varname(varname)
        return self.read(varname, fill2zero=fill2zero)

    def checktime(self, nametime, timeref):

        newtime = self.read(nametime)

        #  Provisoirement on compare brutalement.
        #  A terme il faudra faire en fonction du units ou sélectionner une période commune.
        if (newtime[:] != timeref[:]).all():
            raise TimeException(self.path)

    def close(self):
        """
        Close the netCDF dataset.
        """
        if self.dataset is not None:
            self.dataset.close()
            self.dataset = None

    def integration(self, variable, nstep, start=0):
        """
        Renvoie les valeurs cumulées tous les nstep pas de temps

        .. todo::
           Documenter cette fonction
        """
        cumsum = np.cumsum(np.insert(variable, 0, 0, axis=0), axis=0)
        if len(variable.shape) == 1:
            temp = cumsum[nstep:] - cumsum[:-nstep]
            return temp[np.arange(0, len(variable) - nstep, nstep)]
        elif len(variable.shape) == 2:
            temp = cumsum[nstep:, :] - cumsum[:-nstep, :]
            return temp[np.arange(start, len(variable) - nstep, nstep), :]
        else:
            raise NotImplementedError("Integration of 3D variables not implemented")

    def moytempo(self, precip, nstep, start=0):
        """
        Même chose que integration mais renvoie une moyenne

        .. todo::
           Documenter cette fonction
        """
        return self.integration(precip, nstep, start=start) / nstep

    def __enter__(self):
        return self

    def __exit__(self, e_type, e_value, e_traceback):
        self.close()


class _prosimu1d2d():
    """ Common definitions to :class:`prosimu1d` and :class:`prosimu2d` """

    def get_points(self, **kwargs):
        """
        Return the values of dimension :data:`number of points<Number_of_points>` correpsonding to
        a subset defined by variables such as aspect, elevation (ZS), massif_num, slope, latitude, longitude
        according to named arguments passed to the function.

        :rtype: list of integers
        """
        for varname in kwargs.keys():
            varname = self._check_varname(varname)
            if not set(self.dataset.variables[varname].dimensions).issubset(set(self.Points_dimensions)):
                raise TypeError("""Le filtrage ne peut se faire que sur des variables géographiques
                    (such as ZS, slope, aspect, lat, lon). Variable {} not compatible.""".format(varname))

        locations_bool = np.ones(tuple(self.pointsdim_l),
                                 dtype=bool)

        for varname, values in kwargs.items():
            varname = self._check_varname(varname)
            vardims = self.dataset.variables[varname].dimensions

            data = self.dataset.variables[varname][Ellipsis]
            if list(self.dataset.variables[varname].dimensions) == self.Points_dimensions:
                slices = Ellipsis
            else:
                # Swap axes if necessary
                index = []
                for axis in self.dataset.variables[varname].dimensions:
                    index.append(self.Points_dimensions.index(axis))

                index = np.argsort(np.array(index))
                if np.all(np.diff(index) > 0):
                    data = np.moveaxis(data, np.arange(len(index)), index)
                slices = tuple([None if axis not in vardims else slice(None) for axis in self.Points_dimensions])

            locations_bool = np.logical_and(locations_bool, np.isin(data[slices], values))

        return np.where(locations_bool.flatten())[0]

    def read(self, varname, fill2zero=False, selectpoint=-1, keepfillvalue=False, tile=None, removetile=True,
             needmodif=False, hasDecile=False):
        """
        Read data from a variable in the netCDF file.

        :param varname: the variable name
        :type varname: str
        :param fill2zero: if True, will replace undefined data with the value of 0. Otherwise, ``np.nan`` is used
                          except if ``keepfillvalue`` is set.
        :type fill2zero: bool
        :param selectpoint: Select a point. If -1 or None, select all points.
        :type selectpoint: int or tuple or list of int or tuple
        :param tile: Select a specific tile.
        :type removetile: int
        :param keepfillvalue: Do not replace the undefined data with ``np.nan`` and keep the fill value used in the
                              netCDF file.
        :type keepfillvalue: bool
        :param needmodif: If True, return also the variable object
        :type needmodif: bool
        :param hasdecile: Deprecated.
        :param removetile: Deprecated. See tile instead.
        """
        varname = self._check_varname(varname)

        # Vérification du nom de la variable
        if varname not in self.listvar():
            raise VarNameException(varname, self.path)

        # Sélection de la variable
        varnc = self.dataset.variables[varname]

        avail_fillvalue = "_FillValue" in varnc.ncattrs()
        if avail_fillvalue:
            fillvalue = varnc._FillValue

        # Sélection d'un point si demandé
        # Suppression dimension tile si nécessaire
        var = self.extract(varname, varnc, selectpoint=selectpoint, tile=tile, removetile=removetile)

        # Remplissage des valeurs manquantes si nécessaire
        if (len(var.shape) > 1 or (len(var.shape) == 1 and var.shape[0] > 1)) and not keepfillvalue:
            try:
                if fill2zero:
                    array = var.filled(fill_value=0)
                    logging.debug("Fill missing data with 0 for variable " + varname)
                else:
                    array = var.filled(fill_value=np.nan)
                    logging.debug("Fill missing data with np.nan for variable " + varname)
            except Exception:
                if avail_fillvalue:
                    if fill2zero:
                        array = np.where(var == fillvalue, 0, var)
                        logging.debug("Fill missing data with 0 for variable " + varname + " (old method)")
                    else:
                        array = np.where(var == fillvalue, np.nan, var)
                        logging.debug("Fill missing data with np.nan for variable " + varname + " (old method)")
                else:
                    array = var
                    logging.debug("Unable to fill data with 0 or np.nan for variable " + varname)

        else:
            array = var

        if needmodif:
            return array, varnc
        else:
            return array

    def _extract(self, varname, var, selectpoint=None, removetile=True, tile=None):
        """
        Extract data as a numpy array, possibly selecting point, and removing useless dimensions

        :meta private:

        :param selectpoint: List of points id to select along dimensions of Points_dimensions.
                            Length of the list is the same as len(Points_dimensions).
        :type selectpoint: list or None
        :param removetile: Boolean, if true, select the tile 0. Deprecated, use tile instead.
        :type removetile: bool
        :param tile: The tile to select.
        :type tile: int
        """
        varname = self._check_varname(varname)

        vardims = self.dataset.variables[varname].dimensions
        allpointstest = selectpoint is None

        # Special case
        if len(var.shape) == 0:
            if allpointstest:
                return var
            else:
                raise ValueError('Could not extract a point. The {} dimension was not found for '
                                 'variable {}'.format(', '.join(self.Points_dimensions), varname))

        selector = [slice(None, None, None)] * len(vardims)

        # Point extraction
        if not allpointstest:
            if not set(self.Points_dimensions).issubset(set(vardims)):
                raise ValueError('Could not extract a point. The {} dimension(s) was not found '
                                 'for variable {}'.format(', '.join(self.Points_dimensions), varname))
            i = 0
            for dimpoint in self.Points_dimensions:
                axispoint = vardims.index(dimpoint)
                selector[axispoint] = selectpoint[i]
                i += 1

        if removetile:
            for key in self.Number_of_Tiles:
                if key in vardims:
                    tileindex = vardims.index(key)
                    selector[tileindex] = 0

        if tile is not None and tile != -1:
            for key in self.Number_of_Tiles:
                if key in vardims:
                    tileindex = vardims.index(key)
                    selector[tileindex] = tile

        return var[tuple(selector)]


class prosimu_base(_prosimu1d2d, prosimuAbstract):
    """
    Common class that is allowed to read both 1d and 2d simualtions. The selection of points is
    not allowed in this class.
    """

    Points_dimensions = []

    def get_points(*args, **kwargs):
        """
        :meta: private
        """
        raise NotImplementedError('get_points is not implemented in prosimu_base. Please use prosimu1d or prosimu2d classes')

    def extract(self, varname, var, selectpoint=-1, tile=None, removetile=True, hasTime=True, hasDecile=False):
        """
        Extract data as a numpy array, possibly removing useless dimensions. In prosimu_base class,
        does not allow to select a point !

        :meta private:
        """

        if isinstance(selectpoint, int) and selectpoint == -1:
            selectpoint = None
        else:
            raise ValueError('To use the selectpoint option, please use prosimu_auto, prosimu1d or prosimu2d. '
                             'prosimu_base does not allow for point selection.')

        return self._extract(varname, var, selectpoint=selectpoint, tile=tile, removetile=removetile)


class prosimu1d(_prosimu1d2d, prosimuAbstract):
    """
    Class to read simulations where simulation points are aggregated along one dimension.
    This is commonly used for semi-distributed simulations.
    """
    Number_of_points = 'Number_of_points'
    Points_dimensions = [Number_of_points]

    def extract(self, varname, var, selectpoint=-1, tile=None, removetile=True, hasTime=True, hasDecile=False):
        """
        Extract data as a numpy array, possibly selecting point, and removing useless dimensions

        :meta private:
        """

        if isinstance(selectpoint, int) and selectpoint == -1:
            selectpoint = None
        if selectpoint is not None:
            selectpoint = [selectpoint]

        return self._extract(varname, var, selectpoint=selectpoint, tile=tile, removetile=removetile)


class prosimu2d(_prosimu1d2d, prosimuAbstract):
    """
    Class to read simulations where simulation points are aggregated along one dimension.
    This is commonly used for semi-distributed simulations.
    """
    Points_dimensions = ['xx', 'yy']

    def extract(self, varname, var, selectpoint=None, tile=None, removetile=True):
        """
        Extract data as a numpy array, possibly selecting point, and removing useless dimensions

        :meta private:
        """
        if isinstance(selectpoint, int) and selectpoint == -1:
            selectpoint = None
        elif selectpoint is None:
            pass
        elif np.issubdtype(type(selectpoint), np.integer):
            # We have one point, identified with unique integer
            selectpoint = self._translate_pointnr_to_varind(selectpoint)
            onepoint = True
        elif isinstance(selectpoint, list):
            # We have a list of points
            onepoint = False
            if np.issubdtype(type(selectpoint[0]), np.integer):
                # Translate point number in dimensions indexes
                selectpoint = [self._translate_pointnr_to_varind(p) for p in selectpoint]
            elif isinstance(selectpoint[0], tuple):
                pass
            else:
                raise ValueError('selectpoint must be list of integer or tuple')
        elif isinstance(selectpoint, tuple):
            selectpoint = selectpoint
            onepoint = True

        varname = self._check_varname(varname)
        vardims = self.dataset.variables[varname].dimensions

        allpointstest = selectpoint is None

        # Special case
        if len(var.shape) == 0:
            if allpointstest:
                return var
            else:
                raise ValueError('Could not extract a point. The point dimension(s) were not found for '
                                 'variable {}'.format(varname))

        selector = [slice(None, None, None)] * len(vardims)
        if removetile:
            for key in self.Number_of_Tiles:
                if key in vardims:
                    tileindex = vardims.index(key)
                    selector[tileindex] = 0

        if tile is not None and tile != -1:
            for key in self.Number_of_Tiles:
                if key in vardims:
                    tileindex = vardims.index(key)
                    selector[tileindex] = tile

        if allpointstest:
            return var[tuple(selector)]
        else:
            # Point extraction
            if not set(self.Points_dimensions).issubset(set(vardims)):
                raise ValueError('Could not extract a point. The {} dimension(s) was not found '
                                 'for variable {}'.format(', '.join(self.Points_dimensions), varname))

            if onepoint:
                for i, dimpoint in enumerate(self.Points_dimensions):
                    axispoint = vardims.index(dimpoint)
                    selector[axispoint] = selectpoint[i]
                return var[tuple(selector)]
            else:
                outputdata = []

                for point in selectpoint:
                    selectorp = selector.copy()
                    for i, dimpoint in enumerate(self.Points_dimensions):
                        axispoint = vardims.index(dimpoint)
                        selectorp[axispoint] = point[i]
                    outputdata.append(var[tuple(selectorp)])

            # Do a numpy array, ensure Point dimension is the last one and return
            return np.moveaxis(np.array(outputdata), 0, -1)

    def _translate_pointnr_to_varind(self, nr):
        """
        Return the tuple of indices along ``Points_dimensions`` dimensions
        for an integer
        """
        l = []
        pd = self.pointsdim_n
        reste = nr
        for i in range(len(self.pointsdim_l[::-1])):
            pd = pd // self.pointsdim_l[i]
            l.append(reste // pd)
            reste = reste % pd
        return tuple(l)

    def _translate_listtuple_to_listsidx(self, listtuple):
        """
        Translate a list of tuple, each one describing a point for its dimensions to
        a list of indices by dimension.
        """
        r = []
        for i in range(len(self.pointsdim_l)):
            r1 = []
            for point in listtuple:
                r1.append(point[i])
            r.append(r1)
        return r


class prosimu_old(prosimu1d):
    """
    In the old operationnal format (before 2018), some dimensions have different
    names, this class allows to deal with them easily

    .. warning::
       Should not be used nowadays
    """

    Number_of_points = 'location'
    Points_dimensions = [Number_of_points]
    Number_of_Patches = 'tile'

    _rewrite_dims = {'Number_of_points': 'location', 'Number_of_patches': 'tile'}


prosimu = prosimu1d
