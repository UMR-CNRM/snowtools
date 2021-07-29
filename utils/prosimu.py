# -*- coding: utf-8 -*-

'''
Created on 4 oct. 2012

:Authors:
    lafaysse

This module contains the ``prosimu`` class used to read simulation files
(netCDF format) as produced by SURFEX/Crocus for instance.
'''

import os
import sys

import six
import netCDF4
import numpy as np

from snowtools.utils.FileException import FileNameException, DirNameException, FileOpenException, VarNameException, \
        TimeException, MultipleValueException
from snowtools.utils.S2M_standard_file import StandardCROCUS

# Fichier PRO.nc issu d'une simulation SURFEX post-traitée


class prosimu():
    """
    Class designed to read simulations files

    :param path: path of the file to read
    :type path: path-like
    :param ncformat: NetCDF format to use
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
    """

    Number_of_points = 'Number_of_points'
    Number_of_Patches = 'Number_of_Patches'
    Number_of_Tiles = ['tile', 'Number_of_Tile', 'Number_of_Patches']

    def __init__(self, path, ncformat='NETCDF3_CLASSIC', openmode='r'):
        """
        forceread : si True force lors de la lecture d'une variable par la
        méthode read_var à mettre l'ensemble des données de cette variable en
        cache - utile lorsque de grands nombre d'accès à la même variable sont
        nécessaires
        """
        if type(path) is list:
            for fichier in path:
                if not os.path.isfile(fichier):
                    raise FileNameException(fichier)

                else:
                    tempdataset = netCDF4.Dataset(fichier, "a")
                    if "time" in list(tempdataset.variables.keys()):
                        tempdataset.variables["time"].calendar = "standard"
                    tempdataset.close()

            self.dataset = netCDF4.MFDataset(path, "r")
            self.path = path[0]
            self.mfile = 1

        # Vérification du nom du fichier
        elif os.path.isfile(path):
            self.path = path
            self.mfile = 0
            try:
                if openmode == "w":
                    self.dataset = StandardCROCUS(path, openmode, format=ncformat)
                else:
                    self.dataset = StandardCROCUS(path, openmode)
            except Exception:
                raise FileOpenException(path)
        else:
            if openmode == "w":
                dirname = os.path.dirname(path)

                if len(dirname) > 0:
                    if not os.path.isdir(dirname):
                        raise DirNameException(path)

                self.dataset = StandardCROCUS(path, openmode, format=ncformat)
                self.path = path
                self.mfile = 0
            else:
                print("I am going to crash because there is a filename exception")
                print(path)
                print(type(path))
                raise FileNameException(path)

        self.varcache = {}

        if "time" in self.dataset.dimensions:
            self.timedim = range(len(self.dataset.dimensions['time']))
        else:
            self.timedim = None
        if self.Number_of_points in self.dataset.dimensions:
            self.pointsdim = range(len(self.dataset.dimensions[self.Number_of_points]))
        else:
            self.pointsdim = None

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
            slices = []
            dims = var.dimensions
            for dimname in dims:
                slices.append(slice(None))
            slices = tuple(slices)
            self.varcache[varname] = var[slices]

    def format(self):
        """
        Get the format of the undelying netCDF file (e.g. NETCDF3_CLASSIC)
        """
        try:
            return self.dataset.data_model
        except AttributeError:
            print("WARNING : old version of netCDF4 module. We cannot check the format of your forcing file. We assume that you provide a NETCDF3_CLASSIC file.")
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
        return np.array(self.dataset.variables[varname].dimensions)

    def getrankvar(self, varname):
        """
        Get the rank (number of dimensions) of a variable

        :param varname: the variable name
        :type varname: str
        :returns: rank
        :rtype: int
        """
        return len(self.dataset.variables[varname].shape)

    def listattr(self, varname):
        """
        Get the list of attributtes attached to a variable

        :param varname: the variable name
        :type varname: str
        :returns: atributes
        """
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
        return getattr(self.dataset.variables[varname], attname)

    def gettypevar(self, varname):
        """
        Return the dtype of a variable

        :param varname: the variable name
        :type varname: str
        :returns: the corresponding dtype
        """
        return self.dataset.variables[varname].dtype

    def getfillvalue(self, varname):
        """
        Get the fill value for a variable

        :param varname: Variable name
        :type varname: str
        :returns: the fill value
        """
        if hasattr(self.dataset.variables[varname], "_FillValue"):
            return self.dataset.variables[varname]._FillValue
        else:
            return None

    def infovar(self, varname):
        # Vérification du nom de la variable
        if varname not in self.listvar():
            raise VarNameException(varname, self.path)

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
        :trype: numpy array
        """
        # Vérification du nom de la variable
        if "time" not in list(self.dataset.variables.keys()):
            raise VarNameException("time", self.path)

        if(self.mfile == 1):
            time_base = self.dataset.variables["time"]
            time_base.calendar = 'standard'
            time = netCDF4.MFTime(time_base)
            # time=time_base
        else:
            time = self.dataset.variables["time"]
        if netCDF4.__version__ >= '1.5.4':
            return np.array(netCDF4.num2date(time[:], time.units, only_use_cftime_datetimes=False, only_use_python_datetimes=True))
        else:
            return np.array(netCDF4.num2date(time[:], time.units))

    def get_time(self, time_asdatetime):
        """
        Renvoie l'indice de la dimension time correspondant au datetime donné en
        argument
        """
        return np.where( self.readtime() == time_asdatetime )[0][0]

    def read_var(self, variable_name, **kwargs):
        """
        Read a variable from netCDF file

        :param variable_name: nom de la variable
        :param kwargs: spécifier la sous-sélection sous la forme  dimname = value
                         ou dimname est le nom de la dimension d'intéret, value est une valeur
                         numérique ou un objet slice python pour récupérer une plage de valeurs
        :returns: un tableau numpy.ma.MaskedArray (on peut toujours remplacer les éléments masqués
                  par un indicateur de valeur manquante  pas implémenté)

        Exemples:

        .. code-block:: python

           snowemp = prosimu.read_var('SNOWTEMP',time=0,Number_of_points = slice(100,125))
           snowtemp = prosimu.read_var('SNOWTEMP',time= slice(0,10,2), Number_of_points=1,snow_layer=slice(0,10))

        peut-être utilisé en combinaison avec les méthodes get_point et get_time
        pour récupérer un point / un instant donné :

        .. code-block:: python

           snowtemp = prosimu.read_var('SNOWTEMP',time=self.get_time(datetime(2018,3,1,9)),
                       Number_of_points = self.get_point(massif_num=3,slope=20,ZS=4500,aspect=0))
        """
        # Gestion des noms de dimensions différents entre ancien et nouveau
        # format
        condition_points = kwargs.get('Number_of_points')
        condition_patches = kwargs.get('Number_of_Patches')
        if condition_points is not None:
            del kwargs['Number_of_points']
            kwargs[self.Number_of_points] = condition_points
        if condition_patches is not None:
            del kwargs['Number_of_Patches']
            kwargs[self.Number_of_Patches] = condition_patches
        # valeurs par défaut
        if self.Number_of_Patches not in kwargs.keys():
            kwargs[self.Number_of_Patches] = 0
        # contrôles des arguments d'appel de la méthode
        if variable_name not in self.listvar():
            raise VarNameException(variable_name, self.path)
        ncvariable = self.dataset.variables[variable_name]
        if variable_name in self.varcache:
            ncvariable_data = self.varcache[variable_name]
        else:
            ncvariable_data = self.dataset.variables[variable_name]
        dims = ncvariable.dimensions
        slices = []
        for dimname in dims:
            slices.append(kwargs.get(dimname, slice(None)))
        slices = tuple(slices)
        result = ncvariable_data[slices]
        # if (isinstance(result, np.ma.core.MaskedConstant) or not(isinstance(result, np.ma.core.MaskedArray))):
            # result = np.ma.MaskedArray(result)
        return result

    def get_points(self, **kwargs):
        """
        Return the values of dimension :data:`number of points<Number_of_points>` correpsonding to
        a subset defined by variables aspect, ZS, massif_num and slope according to named arguments
        passed to the function.
        """
        if not( all([(self.dataset.variables[varname].dimensions == (self.Number_of_points,)) for varname in kwargs.keys()])):
            raise TypeError("""Le filtrage ne peut se faire que sur des variables géographiques (ZS, slope, aspect, massif_num)""")
        nop = np.arange(len(self.dataset.dimensions[self.Number_of_points]))
        locations_bool = np.ones(len(nop))
        for varname, values in six.iteritems(kwargs):
            locations_bool = np.logical_and(locations_bool, np.in1d(self.dataset.variables[varname], values))
        return np.where(locations_bool)[0]

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

    def extract(self, varname, var, selectpoint=-1, removetile=True, hasTime=True, hasDecile=False):
        """
        Extract data as a numpy array, possibly selecting point, and removing useless dimensions

        :meta private:
        """

        vardims = self.dataset.variables[varname].dimensions
        try:
            allpointstest = all(selectpoint == -1)
        except TypeError:
            allpointstest = selectpoint == -1

        # Special case
        if len(var.shape) == 0:
            if allpointstest:
                return var
            else:
                raise ValueError('Could not extract a point. The {} dimension was not found for variable {}'.format(self.Number_of_points, varname))

        selector = [slice(None, None, None)] * len(vardims)

        # Point extraction
        if not allpointstest:
            if self.Number_of_points not in vardims:
                raise ValueError('Could not extract a point. The {} dimension was not found for variable {}'.format(self.Number_of_points, varname))
            axispoint = vardims.index(self.Number_of_points)
            selector[axispoint] = selectpoint

        if removetile:
            for key in self.Number_of_Tiles:
                if key in vardims:
                    tileindex = vardims.index(key)
                    selector[tileindex] = 0

        return var[tuple(selector)]

    def read(self, varname, fill2zero=False, selectpoint=-1, keepfillvalue=False, removetile=True, needmodif=False,
             hasDecile = False):
        """
        Read data from a variable in the netCDF file.

        :param varname: the variable name
        :type varname: str
        :param fill2zero: if True, will replace undefined data with the value of 0. Otherwise, ``np.nan`` is used
                          except if ``keepfillvalue`` is set.
        :type fill2zero: bool
        :param selectpoint: Select a point. If -1, select all points.
        :type selectpoint: int
        :param removetile: Removethe tile dimension, if present.
        :type removetile: bool
        :param keepfillvalue: Do not replace the undefined data with ``np.nan`` and keep the fill value used in the
                              netCDF file.
        :type keepfillvalue: bool
        :param needmodif: If True, return also the variable object
        :type needmodif: bool
        """

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
        # gestion time/pas time en dimension (prep files)
        if "time" not in list(self.dataset.variables.keys()):
            var = self.extract(varname, varnc, selectpoint=selectpoint, removetile=removetile, hasTime=False,
                               hasDecile=hasDecile)
        else:
            var = self.extract(varname, varnc, selectpoint=selectpoint, removetile=removetile, hasDecile=hasDecile)
        # Remplissage des valeurs manquantes si nécessaire
        if (len(var.shape) > 1 or (len(var.shape) == 1 and var.shape[0] > 1)) and not keepfillvalue:
            try:
                if fill2zero:
                    array = var.filled(fill_value=0)
                    # print("Fill missing data with 0 for variable " + varname)
                else:
                    array = var.filled(fill_value=np.nan)
                    # print("Fill missing data with np.nan for variable " + varname)
            except Exception:
                if avail_fillvalue:
                    if fill2zero:
                        array = np.where(var == fillvalue, 0, var)
                        # print("Fill missing data with 0 for variable " + varname + " (old method)")
                    else:
                        array = np.where(var == fillvalue, np.nan, var)
                        # print("Fill missing data with np.nan for variable " + varname + " (old method)")
                else:
                    array = var
                    # print("Unable to fill data with 0 or np.nan for variable " + varname)

        else:
            array = var

        if needmodif:
            return array, varnc
        else:
            return array

    # Pour compatibilité anciens codes, on conserve les routines obsolètes read1d et read2d

    def read1d(self, varname, fill2zero=False, indpoint=0):
        """
        .. warning:: Obsolete method

        :meta private:
        """
        return self.read(varname, fill2zero=fill2zero, selectpoint=indpoint)

    def read2d(self, varname, fill2zero=False):
        """
        .. warning:: Obsolete method

        :meta private:
        """
        return self.read(varname, fill2zero=fill2zero)

    def checktime(self, nametime, timeref):

        newtime = self.read(nametime)

#        Provisoirement on compare brutalement. A terme il faudra faire en fonction du units ou sélectionner une période commune.
        if (newtime[:] != timeref[:]).all():
            raise TimeException(self.path)

    def close(self):
        """
        Close the netCDF dataset.
        """
        self.dataset.close()

    def integration(self, variable, nstep, start=0):
        # Renvoie les valeurs cumulées tous les nstep pas de temps
        cumsum = np.cumsum(np.insert(variable, 0, 0, axis=0), axis=0)
        if len(variable.shape) == 1:
            temp = cumsum[nstep:] - cumsum[:-nstep]
            return temp[np.arange(0, len(variable) - nstep, nstep)]
        elif len(variable.shape) == 2:
            temp = cumsum[nstep:, :] - cumsum[:-nstep, :]
            return temp[np.arange(start, len(variable) - nstep, nstep), :]
        else:
            sys.exit("integration of 3D variables not implemented")

    def moytempo(self, precip, nstep, start=0):
        return self.integration(precip, nstep, start=start) / nstep

    def __enter__(self):
        return self

    def __exit__(self, e_type, e_value, e_traceback):
        self.close()


class prosimu_old(prosimu):
    """
    In the old operationnal format (before 2018), some dimensions have different
    names, this class allows to deal with them easily

    .. warning::
       Should not be used nowadays
    """

    Number_of_points = 'location'
    Number_of_Patches = 'tile'


