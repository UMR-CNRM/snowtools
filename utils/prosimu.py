#! /usr/bin/env python
# -*- coding: utf-8 -*-

'''
Created on 4 oct. 2012

@author: lafaysse
'''
import glob
import os
import sys

import netCDF4

import numpy as np

from .FileException import FileNameException, FileOpenException, VarNameException, TimeException, MultipleValueException


# Fichier PRO.nc issu d'une simulation SURFEX post-traitée
class prosimu():

    Number_of_points = 'Number_of_points'
    Number_of_Patches = 'Number_of_Patches'

    def __init__(self, path, ncformat='NETCDF3_CLASSIC', openmode='r'):
        """
        forceread : si True force lors de la lecture d'une variable par la
        méthode read_var à mettre l'ensemble des données de cette variable en
        cache - utile lorsque de grands nombre d'accès à la même variable sont
        nécessaires
        """
        if type(path) is list or len(glob.glob(path)) > 1:  # BC add the possibility to give wildcards to prosimu
            if type(path) is not list:
                path = sorted(glob.glob(path))
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
                    self.dataset = netCDF4.Dataset(path, openmode, format=ncformat)
                else:
                    self.dataset = netCDF4.Dataset(path, openmode)
            except Exception:
                raise FileOpenException(path)
        else:
            print("I am going to crash because there is a filename exception")
            print(path)
            print(type(path))
            raise FileNameException(path)

        self.varcache = {}

        if "time" in self.dataset.dimensions:
            self.timedim = list(range(len(self.dataset.dimensions['time'])))
        else:
            self.timedim = None
        if self.Number_of_points in self.dataset.dimensions:
            self.pointsdim = list(range(len(self.dataset.dimensions[self.Number_of_points])))
        else:
            self.pointsdim = None

    def force_read_in_cache(self):
        """
        Force la lecture des variables du netcdf sous forme de tableau numpy.
        Ces tableaux sont stockés dans l'attribut varcache de la classe
        Le cache est utilisé par la méthode read_var, son utilisation n'est pas
        implémentée pour les autres méthodes
        Utile lorsque de nombreuses lectures _de la même variable sont requises
        """
        self.varcache = {}
        for varname, var in list(self.dataset.variables.items()):
            slices = []
            dims = var.dimensions
            for dimname in dims:
                slices.append(slice(None))
            slices = tuple(slices)
            self.varcache[varname] = var[slices]

    def format(self):
        try:
            return self.dataset.data_model
        except AttributeError:
            print("WARNING : old version of netCDF4 module. We cannot check the format of your forcing file. We assume that you provide a NETCDF3_CLASSIC file.")
            return 'NETCDF3_CLASSIC'

    def listdim(self):
        return self.dataset.dimensions.copy()

    def listvar(self):
        return list(self.dataset.variables.keys())

    def getlendim(self, dimname):
        return len(self.dataset.dimensions[dimname])

    def getdimvar(self, varname):
        return np.array(self.dataset.variables[varname].dimensions)

    def getrankvar(self, varname):
        return len(self.dataset.variables[varname].shape)

    def listattr(self, varname):
        return self.dataset.variables[varname].ncattrs()

    def getattr(self, varname, attname):
        return getattr(self.dataset.variables[varname], attname)

    def gettypevar(self, varname):
        return self.dataset.variables[varname].dtype

    def getfillvalue(self, varname):
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
        time = self.dataset.variables["time"]
        return time, time.units

    def readtime(self):
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

        return np.array(netCDF4.num2date(time[:], time.units))

    def get_time(self, time_asdatetime):
        """
        Renvoie l'indice de la dimension time correspondant au datetime donné en
        argument
        """
        return np.where( self.readtime() == time_asdatetime )[0][0]

    def read_var(self, variable_name, **kwargs):
        """
        variable_name : nom de la variable
        **kwargs : spécifier la sous-sélection sous la forme  dimname = value
        ou dimname est le nom de la dimension d'intéret, value est une valeur
        numérique ou un objet slice python pour récupérer une plage de valeurs
        Retourne : un tableau numpy.ma.MaskedArray (on peut toujours remplacer
        les éléments masqués par un indicateur de valeur manquante - pas
        implémenté)

        Exemples:
            snowtemp = prosimu.read_var('SNOWTEMP',time=0,Number_of_points = slice(100,125))
            snowtemp = prosimu.read_var('SNOWTEMP',time= slice(0,10,2), Number_of_points=1,snow_layer=slice(0,10))
            etc...
        peut-être utilisé en combinaison avec les méthodes get_point et get_time
        pour récupérer un point / un instant donné :
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
        if self.Number_of_Patches not in list(kwargs.keys()):
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
        Renvoie les valeurs de la dimension Number_of_points correspondant à une
        sous-selection de variables aspect,ZS,massif_num,slope
        """
        if not( all([(self.dataset.variables[varname].dimensions == (self.Number_of_points,)) for varname in list(kwargs.keys())])):
            raise TypeError("""Le filtrage ne peut se faire que sur des variables géographiques (ZS, slope, aspect, massif_num)""")
        nop = np.arange(len(self.dataset.dimensions[self.Number_of_points]))
        locations_bool = np.ones(len(nop))
        for varname, values in list(kwargs.items()):
            locations_bool = np.logical_and(locations_bool, np.in1d(self.dataset.variables[varname], values))
        return np.where(locations_bool)[0]

    def get_point(self, **kwargs):
        """
        get_points mais pour un seul point - exception si plusieurs points ou
        aucun dans la réponse
        """
        point_list = self.get_points(**kwargs)
        if len(point_list) > 1:
            raise MultipleValueException()
        elif len(point_list) == 0:
            raise IndexError('No point matching the selection')
        return point_list[0]

    def extract(self, varname, var, selectpoint=-1, removetile=True, hasTime = True):

        if removetile:
            vardims = self.dataset.variables[varname].dimensions
            needremovetile = "tile" in vardims or 'Number_of_Tile' in vardims or 'Number_of_Patches' in vardims
        else:
            needremovetile = False
        rank = len(var.shape)
        if hasTime is True:
            if selectpoint == -1:
                if needremovetile:
                    if rank == 1:
                        # Pour cas de la variable tile dans comparaisons automatiques
                        var_extract = var[0]
                    if rank == 2:
                        var_extract = var[:, 0]
                    elif rank == 3:
                        var_extract = var[:, 0, :]
                    elif rank == 4:
                        var_extract = var[:, 0, :, :]
                    elif rank == 5:
                        var_extract = var[:, 0, :, :, :]
                else:
                    if rank == 0:
                        var_extract = var
                    elif rank == 1:
                        var_extract = var[:]
                    elif rank == 2:
                        var_extract = var[:, :]
                    elif rank == 3:
                        var_extract = var[:, :, :]
                    elif rank == 4:
                        var_extract = var[:, :, :, :]
                    elif rank == 5:
                        var_extract = var[:, :, :, :, :]
            else:
                if needremovetile:
                    if rank == 1:
                        # Pour cas de la variable tile dans comparaisons automatiques
                        var_extract = var[0]
                    elif rank == 3:
                        var_extract = var[:, 0, selectpoint]
                    elif rank == 4:
                        var_extract = var[:, 0, :, selectpoint]
                    elif rank == 5:
                        var_extract = var[:, 0, :, :, selectpoint]
                else:
                    if rank == 0:
                        var_extract = var
                    elif rank == 1:
                        var_extract = var[selectpoint]
                    elif rank == 2:
                        var_extract = var[:, selectpoint]
                    elif rank == 3:
                        var_extract = var[:, :, selectpoint]
                    elif rank == 4:
                        var_extract = var[:, :, :, selectpoint]
                    elif rank == 5:
                        var_extract = var[:, :, :, :, selectpoint]

        else:  # if isPrep, no time dimension, tile is the first dim
            if selectpoint == -1:
                if needremovetile:
                    if rank == 1:
                        # Pour cas de la variable tile dans comparaisons automatiques
                        var_extract = var[0]
                    if rank == 2:
                        var_extract = var[0, :]
                    elif rank == 3:
                        var_extract = var[0, :, :]
                    elif rank == 4:
                        var_extract = var[0, :, :, :]
                    elif rank == 5:
                        var_extract = var[0, :, :, :, :]
                else:
                    if rank == 0:
                        var_extract = var
                    elif rank == 1:
                        var_extract = var[:]
                    elif rank == 2:
                        var_extract = var[:, :]
                    elif rank == 3:
                        var_extract = var[:, :, :]
                    elif rank == 4:
                        var_extract = var[:, :, :, :]
                    elif rank == 5:
                        var_extract = var[:, :, :, :, :]
            else:
                if needremovetile:
                    if rank == 1:
                        # Pour cas de la variable tile dans comparaisons automatiques
                        var_extract = var[0]
                    elif rank == 3:
                        var_extract = var[0, :, selectpoint]
                    elif rank == 4:
                        var_extract = var[0, :, :, selectpoint]
                    elif rank == 5:
                        var_extract = var[0, :, :, :, selectpoint]
                else:
                    if rank == 0:
                        var_extract = var
                    elif rank == 1:
                        var_extract = var[selectpoint]
                    elif rank == 2:
                        var_extract = var[:, selectpoint]
                    elif rank == 3:
                        var_extract = var[:, :, selectpoint]
                    elif rank == 4:
                        var_extract = var[:, :, :, selectpoint]
                    elif rank == 5:
                        var_extract = var[:, :, :, :, selectpoint]

        return var_extract

    def read(self, varname, fill2zero=False, selectpoint=-1, keepfillvalue=False, removetile=True, needmodif=False):

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
            var = self.extract(varname, varnc, selectpoint=selectpoint, removetile=removetile, hasTime=False)
        else:
            var = self.extract(varname, varnc, selectpoint=selectpoint, removetile=removetile)
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
        return self.read(varname, fill2zero=fill2zero, selectpoint=indpoint)

    def read2d(self, varname, fill2zero=False):
        return self.read(varname, fill2zero=fill2zero)

    def checktime(self, nametime, timeref):

        newtime = self.read(nametime)

#        Provisoirement on compare brutalement. A terme il faudra faire en fonction du units ou sélectionner une période commune.
        if (newtime[:] != timeref[:]).all():
            raise TimeException(self.path)

    def close(self):
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


class prosimu_old(prosimu):
    """
    In the old operationnal format (before 2018), some dimensions have different
    names, this class allows to deal with them easily
    """

    Number_of_points = 'location'
    Number_of_Patches = 'tile'
