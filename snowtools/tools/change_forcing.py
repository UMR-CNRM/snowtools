# -*- coding: utf-8 -*-

"""
Created on 3 Aug. 2017

@author: lafaysse
"""

import os
import numpy as np
import datetime
import netCDF4

# For compatibility python 2 / python 3

from snowtools.utils.sun import sun
from snowtools.utils.prosimu import prosimu
from snowtools.utils.infomassifs import infomassifs
# Take care : exceptions have to been imported with snowtools prefix to be recognized by vortex
from snowtools.utils.FileException import (
    FileNameException, DirNameException, VarWriteException,
    GeometryException, MassifException, TimeListException)
from snowtools.utils.dates import TypeException

from snowtools.utils.resources import print_used_memory
from snowtools.utils.S2M_standard_file import StandardSAFRAN, StandardCROCUS


class forcinput_tomerge:
    """
    This class represents a group of forcing files which must be merged
    in a single one through the ``Number_of_points`` dimension.

    :param forcin: List of input files names
    :type forcin: list
    :param forcout: Name of merged output file
    :type forcout: str
    """
    printmemory = False
    formatout = "NETCDF4_CLASSIC"

    def __init__(self, forcin, forcout, *args, **kwargs):
        """Generic method to open and merge multiple forcing files"""

        if type(forcin) is list:
            forcin = list(map(str, forcin))
        else:
            raise TypeException(type(forcin), list)

        init_forcing_file = []

        for ficin in forcin:
            if os.path.isfile(ficin):
                init_forcing_file.append(prosimu(ficin))
            else:
                raise FileNameException(ficin)

        self.checktime(init_forcing_file, forcin)

        dirout = os.path.dirname(forcout)

        if not (dirout == '' or os.path.isdir(dirout)):
            raise DirNameException(dirout)

        new_forcing_file = StandardSAFRAN(forcout, "w", format=self.formatout)

        self.merge(init_forcing_file, new_forcing_file, args)

        for openedfic in init_forcing_file:
            openedfic.close()

        new_forcing_file.GlobalAttributes(**kwargs)
        new_forcing_file.add_standard_names()

        new_forcing_file.close()

    def checktime(self, init_forcing_file, forcin):
        """Check time consistency of forcing files to merge.

        :param init_forcing_file: Input files to merge object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param forcin: file names
        :type forcin: list
        """

        dimtime = []
        for unitfile in init_forcing_file:
            dimtime.append(unitfile.getlendim("time"))

        if len(set(dimtime)) > 1:
            raise TimeListException(forcin, dimtime)

    def merge(self, init_forcing_file, new_forcing_file, *args):
        """Merge forcing files.

        :param init_forcing_file: Input files to merge object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object
        :type new_forcing_file: :class:`utils.prosimu.prosimu`

        """

        spatial_dim_name = "Number_of_points"

        index_deb = 0
        len_dim = 0
        spatial_index = []
        for unitfile in init_forcing_file:

            dim_file = unitfile.getlendim(spatial_dim_name)
            index_end = index_deb + dim_file - 1
            spatial_index.append((index_deb, index_end))

            len_dim += dim_file
            index_deb = index_end + 1

        new_forcing_file.createDimension("time", None)
        new_forcing_file.createDimension(spatial_dim_name, len_dim)

        dictdim = init_forcing_file[0].listdim()

        del dictdim["time"]
        del dictdim[spatial_dim_name]

        for dimname, dim in dictdim.items():
            print("Create dimension " + dimname + " " + str(len(dim)))
            new_forcing_file.createDimension(dimname, len(dim))

        time = init_forcing_file[0].readtime()

        listvar = init_forcing_file[0].listvar()
        savevar = {}

        for varname in listvar:
            print(varname)
            if self.printmemory:
                print_used_memory()
            vartype, rank, array_dim, varFillvalue, var_attrs = init_forcing_file[0].infovar(varname)
            if varname == "DIR_SWdown":
                direct = new_forcing_file.createVariable(varname, vartype, array_dim, fill_value=varFillvalue)
                var = direct
            elif varname == "SCA_SWdown":
                diffus = new_forcing_file.createVariable(varname, vartype, array_dim, fill_value=varFillvalue)
                var = diffus
            else:
                var = new_forcing_file.createVariable(varname, vartype, array_dim, fill_value=varFillvalue)

            for attname in var_attrs:
                if not attname == '_FillValue':
                    setattr(var, attname, init_forcing_file[0].getattr(varname, attname))

            if rank >= 1 and spatial_dim_name in array_dim:

                for i, unitfile in enumerate(init_forcing_file):

                    var_array = unitfile.read(varname, keepfillvalue=True)

                    if varname in ["LAT", "LON", "aspect", "slope", "DIR_SWdown",
                                   "SCA_SWdown", "massif_number", "station"]:
                        savevar[varname + str(i)] = var_array

                    if varname not in ["DIR_SWdown", "SCA_SWdown"]:
                        if rank == 1:
                            var[spatial_index[i][0]:spatial_index[i][1] + 1] = var_array[:]
                        elif rank == 2:
                            var[:, spatial_index[i][0]:spatial_index[i][1] + 1] = var_array[:, :]

            else:
                var[:] = unitfile.read(varname, keepfillvalue=True)[:]

        for i, unitfile in enumerate(init_forcing_file):
            direct_array, diffus_array = self.compute_solar_radiations(time, savevar, i)
            direct[:, spatial_index[i][0]:spatial_index[i][1] + 1] = direct_array[:, :]
            diffus[:, spatial_index[i][0]:spatial_index[i][1] + 1] = diffus_array[:, :]

    def compute_solar_radiations(self, time, savevar, i):
        return savevar["DIR_SWdown" + str(i)], savevar["SCA_SWdown" + str(i)]


class forcinput_applymask(forcinput_tomerge):
    """
    This class represents a group of forcing files which must be merged
    in a single one through the ``Number_of_points`` dimension.
    and for which incoming shortwave radiation must be corrected from shadows.
    Or a single forcing file for which incoming shortwave radiation must be corrected from shadows.
    """

    def compute_solar_radiations(self, time, savevar, i):
        if "station" + str(i) in list(savevar.keys()):
            INFO = infomassifs()
            list_list_azim = []
            list_list_mask = []
            print("ACCOUNT FOR SURROUNDING MASKS FOR THE FOLLOWING STATIONS:")
            for poste in map(self.stringstation, list(savevar["station" + str(i)])):
                azim, mask = INFO.maskposte(poste)
                list_list_azim.append(azim)
                list_list_mask.append(mask)
                if not azim == [0, 360]:
                    print(poste, INFO.nameposte(poste))
        else:
            list_list_azim = None
            list_list_mask = None

        return sun().slope_aspect_correction(savevar["DIR_SWdown" + str(i)], savevar["SCA_SWdown" + str(i)],
                                             time, savevar["LAT" + str(i)], savevar["LON" + str(i)],
                                             savevar["aspect" + str(i)], savevar["slope" + str(i)],
                                             list_list_azim=list_list_azim, list_list_mask=list_list_mask,
                                             lnosof_surfex=True)

    def stringstation(self, station):
        station = str(station)
        if len(station) == 7:
            station = "0" + station
        return station


class forcinput_tomodify:
    """This class represents a forcing file for which modifications are needed before being used as SURFEX input
    Instanciation opens the initial forcing file to read and create the modified forcing file.

    The :meth:`change_forcing.forcinput_tomodify.modify` must be defined in child classes.

    :param forcin: Input file name
    :type forcin: str
    :param forcout: Output file name
    :type forcout: str

    """
    printmemory = False
    formatout = "NETCDF4_CLASSIC"

    def __init__(self, forcin, forcout, *args, **kwargs):

        if type(forcin) is int:
            forcin = str(forcin)

        if not os.path.isfile(forcin):
            raise FileNameException(forcin)

        dirout = os.path.dirname(forcout)

        if not (dirout == '' or os.path.isdir(dirout)):
            raise DirNameException(dirout)

        self.filename = forcin

        if forcin == forcout:
            init_forcing_file = prosimu(forcin, openmode='r+')
            self.modify(init_forcing_file, init_forcing_file, args)
        else:
            init_forcing_file = prosimu(forcin)
            print("INFO INPUT FORCING FILE FORMAT: " + init_forcing_file.format())
            new_forcing_file = self.StandardFILE(forcout, "w", format=self.formatout)
            self.modify(init_forcing_file, new_forcing_file, args)

        init_forcing_file.close()

        if forcin != forcout:
            new_forcing_file.GlobalAttributes(**kwargs)
            new_forcing_file.add_standard_names()
            new_forcing_file.close()

    def StandardFILE(self, *args, **kwargs):
        return StandardSAFRAN(*args, **kwargs)

    def modify(self, init_forcing_file, new_forcing_file, *args):
        """The key method to be overriden

        :param init_forcing_file: Input file object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object
        :type new_forcing_file: :class:`utils.prosimu.prosimu`
        """
        pass

    def add_massif_variables(self, init_forcing_file, new_forcing_file, savevar={}):
        """Add massif-scale diagnostics (isotherm 0 and rain snow limit elevation)

        :param init_forcing_file: Input file object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object
        :type new_forcing_file: :class:`utils.prosimu.prosimu`
        :param savevar: Dictionnary of variables in cache
        :type savevar: dict
        """
        # if new_forcing_file is prosimu instance, take the dataset class instead
        if type(new_forcing_file) not in [netCDF4.Dataset, StandardSAFRAN]:
            new_forcing_file = new_forcing_file.dataset

        self.create_massif_dimension(init_forcing_file, new_forcing_file, savevar)
        if "isoZeroAltitude" not in list(new_forcing_file.variables.keys()):
            self.add_iso_zero(init_forcing_file, new_forcing_file, savevar)
        if "rainSnowLimit" not in list(new_forcing_file.variables.keys()):
            self.add_snow_rain_limit(init_forcing_file, new_forcing_file, savevar)

    def create_massif_dimension(self, init_forcing_file, new_forcing_file, savevar):
        """create massif dimension from existing massifs in the forcing file

        :param init_forcing_file: Input file object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object
        :type new_forcing_file: :class:`utils.prosimu.prosimu`
        :param savevar: Dictionnary of variables in cache
        :type savevar: dict
        """

        massif_dim_name = "massif"

        if "massif_number" in list(savevar.keys()):
            self.massif_number = list(map(int, savevar["massif_number"]))
        else:
            self.massif_number = list(map(int, init_forcing_file.read("massif_number")))
        self.list_massifs = np.unique(self.massif_number)
        self.nmassifs = len(self.list_massifs)

        if massif_dim_name not in new_forcing_file.dimensions.copy():
            new_forcing_file.createDimension(massif_dim_name, self.nmassifs)
            var = new_forcing_file.createVariable(massif_dim_name, 'i4', ["massif"], fill_value=0)
            var[:] = self.list_massifs[:]

        if "slope" in list(savevar.keys()):
            self.slope = savevar["slope"]
        else:
            self.slope = init_forcing_file.read("slope")

        if "ZS" in list(savevar.keys()):
            self.zs = savevar["ZS"]
        else:
            self.zs = init_forcing_file.read("ZS")

    def add_snow_rain_limit(self, init_forcing_file, new_forcing_file, savevar):
        """Adds a massif-scale snow-rain limit elevation diagnostic

        :param init_forcing_file: Input file object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object
        :type new_forcing_file: :class:`utils.prosimu.prosimu`
        :param savevar: Dictionnary of variables in cache
        :type savevar: dict
        """

        if "Snowf" in list(savevar.keys()):
            snowf = savevar["Snowf"]
        else:
            snowf = init_forcing_file.read("Snowf")

        if "Snowf" in list(savevar.keys()):
            rainf = savevar["Rainf"]
        else:
            rainf = init_forcing_file.read("Rainf")

        ntime = snowf.shape[0]
        lpn = np.empty((ntime, self.nmassifs))

        for m, massif in enumerate(self.list_massifs):
            indflat = (self.slope == 0.) & (self.massif_number == massif)
            altitude = self.zs[indflat]

            if len(altitude) < 2:
                return

            rain_full = rainf[:, indflat]
            snow_full = snowf[:, indflat]
            preciptot = rain_full + snow_full

            phase = np.where(snow_full > 0, -1, np.where(rain_full > 0, 1, 0))

            lowestlevelindex = np.argmin(phase, axis=1)
            lpn_temp = altitude[lowestlevelindex] - 150

            lpn_temp = np.where(snow_full[:, 0] > 0., -3, lpn_temp)
            lpn_temp = np.where(rain_full[:, -1] > 0., -2, lpn_temp)
            lpn_temp = np.where(np.all(preciptot < 2.E-4, axis=1) & np.any(preciptot < 1.E-8, axis=1), -1, lpn_temp)

            lpn[:, m] = lpn_temp
        var = new_forcing_file.createVariable("rainSnowLimit", 'float', ["time", "massif"], fill_value=-9999.)
        var.long_name = ('Rain-snow transition altitude (resolution 300 m). -1 if no precipitation;'
                         '-2 if above the top of the massif; -3 if below the bottom of the massif.')
        var.units = 'm'
        var[:] = lpn

    def add_iso_zero(self, init_forcing_file, new_forcing_file, savevar):
        """Adds a massif-scale 0 degree isotherm diagnostic
        """
        zero = 273.16

        if "Tair" in list(savevar.keys()):
            tair = savevar["Tair"]
        else:
            tair = init_forcing_file.read("Tair")

        ntime = tair.shape[0]
        isozero = np.empty((ntime, self.nmassifs))

        for m, massif in enumerate(self.list_massifs):
            indflat = (self.slope == 0.) & (self.massif_number == massif)
            altitude = self.zs[indflat]

            if len(altitude) < 2:
                return

            zmax = np.max(altitude)

            tair_full = tair[:, indflat]
            # Temperature of the level just above
            # Last column will not be used but is necessary to conform shapes.
            tair_full_up = np.concatenate((tair_full[:, 1:], np.zeros_like(tair_full[:, 0:1]) + 999), axis=1)

            # Store both temperatures in the same variable
            x = (tair_full * 100.).astype('int') + tair_full_up / 1000.

            # Elevations with positive temperatures
            # Decimal part represents the temperature of the correponding level and the temperature above
            altipos = np.where(tair_full > zero, altitude + x / 100000., -999.)

            # Identify the maximum level of positive temperature
            z = np.max(altipos, axis=1)

            # Separate elevation and group of temperatures
            y, zlow = np.modf(z)
            zup = zlow + 300

            # Separate and reconstruct both temperatures
            ttempup, ttemplow = np.modf(y*100000)

            tlow = np.where(ttemplow > 0., ttemplow / 100., -999.)  # Temperature of the level just below freezing level
            tup = np.where(ttempup > 0., ttempup * 1000., -998.)  # Temperature of the level just above freezing level
            # Note that in cases of missing values, tup and tlow must be different to avoid a division by 0 just after

            # Freezing level is computed with a linear interpolation.
            isozero_temp = ((tlow - zero) * zup + (zero - tup) * zlow) / (tlow - tup)
            isozero_temp = np.where(zlow == -999., -3, isozero_temp)  # Freezing level below the lowest level
            isozero_temp = np.where(zlow == zmax, -2, isozero_temp)  # Freezing level above the lowest level

            isozero[:, m] = isozero_temp

        var = new_forcing_file.createVariable("isoZeroAltitude", 'float', ["time", "massif"], fill_value=-9999.)
        var.long_name = ('Freezing level altitude obtained by interpolation from SAFRAN standard levels.'
                         '-2 if above the top of the massif ; -3 if below the bottom of the massif.')
        var.units = 'm'
        var[:] = isozero

    def addfirstdimension(self, array, length):
        return np.tile(array, (length, 1))


class forcinput_addmeteomassif(forcinput_tomodify):
    """
    This class represents a forcing file for which massif-scale meteorological diagnostics must be added.
    """

    def modify(self, init_forcing_file, new_forcing_file, *args):
        """Add massif-scale meteorological diagnostics

        :param init_forcing_file: Input file object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object
        :type new_forcing_file: :class:`utils.prosimu.prosimu`
        """

        self.add_massif_variables(init_forcing_file, new_forcing_file)


class forcinput_select(forcinput_tomodify):
    """This class represents a forcing file for which the geometry has to been modified
    before being used as SURFEX input including selection of massifs, elevation levels, slopes or aspects,
    duplication of slopes.
    """

    # This class was first implemented by G. Lecourt for spatial reduction of a forcing file
    # M Lafaysse generalized the method to both FORCING and PRO files (June 2016)
    # M Lafaysse added a treatement to increase the number of slopes (July 2016)
    # M Lafaysse added a treatment to create coordinates for direct compatibilty with the new SAFRAN output (Aug 2017)

    def massifvarname(self):
        return 'massif_number'

    def modify(self, init_forcing_file, new_forcing_file, *args):
        """Selection of massifs, elevation levels, slopes or aspects, duplication of slopes.

        :param init_forcing_file: Input file object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object
        :type new_forcing_file: :class:`utils.prosimu.prosimu`
        :param args: list of massif numbers, minimum elevation, maximum elevation, list of slopes, list of aspects
        :type args: list, int, int, list, list
        """

        print("Modify forcing file towards the prescribed geometry:")

        (list_massif_number, min_alt, max_alt, liste_pentes, list_exp) = args[:][0]

        list_exp_degres = list_exp[:]  # Necessary to not modify this value in the module call for next iteration
        # print list_exp_degres
        liste_pentes_int = list(map(int, liste_pentes))

        listvar = init_forcing_file.listvar()

        init_alt = init_forcing_file.read("ZS", keepfillvalue=True)
        b_points_alt = (init_alt >= min_alt) * (init_alt <= max_alt)

        if self.massifvarname() in listvar:
            init_massif_nb_sop = init_forcing_file.read(self.massifvarname(), keepfillvalue=True)
            b_points_massif = np.in1d(init_massif_nb_sop, list_massif_number)
            if np.sum(b_points_massif) == 0:
                raise MassifException(list_massif_number, list(set(init_massif_nb_sop)))
        else:
            b_points_massif = b_points_alt[:]

        if min_alt > np.max(init_alt) or max_alt < np.min(init_alt):
            raise GeometryException(np.min(init_alt), np.max(init_alt))

        init_slopes = init_forcing_file.read("slope", keepfillvalue=True)
        init_exp = init_forcing_file.read("aspect", keepfillvalue=True)

        # list_pentes is the user-defined target
        if "0" in liste_pentes:
            nb_slope_angles_notflat = len(liste_pentes) - 1  # Number of target slopes excluding flat
            nb_aspect_angles_notflat = len(list_exp_degres) - 1  # Number of target aspects excluding flat
            nb_slopes_bylevel = 1 + (nb_slope_angles_notflat *
                                     nb_aspect_angles_notflat)  # Number of slopes for a given elevation level
        else:
            nb_slope_angles_notflat = len(liste_pentes)  # Number of target slopes excluding flat
            nb_aspect_angles_notflat = len(list_exp_degres)  # Number of target aspects excluding flat
            nb_slopes_bylevel = (nb_slope_angles_notflat *
                                 nb_aspect_angles_notflat)  # Number of slopes for a given elevation level

        # print nb_slopes_bylevel, nb_slope_angles_notflat, nb_aspect_angles_notflat

        # Extend aspects mode only if input file have only -1 aspects
        extendaspects = nb_slopes_bylevel > 1 and np.all(init_exp == -1)
        # Extend slopes if input file already have several aspects but we want more slope values
        extendslopes = not extendaspects and (len(liste_pentes) > len(np.unique(init_slopes)))

        if extendaspects:
            print("Extend aspects of the input forcing file")
        if extendslopes:
            print("Extend slopes of the input forcing file")

        if extendaspects:
            # Indexes of points to extract: only flat values if create new aspects
            b_points_slope = np.in1d(init_slopes, [0])
            b_points_aspect = np.in1d(init_exp, [-1])

        else:
            # Indexes of points to extract: can be a subset of available slopes or the whole available slopes
            b_points_slope = np.in1d(init_slopes, liste_pentes_int)
            b_points_aspect = np.in1d(init_exp, list_exp_degres)

        # Identify points to extract
        index_points = np.where(b_points_massif * b_points_alt * b_points_slope * b_points_aspect)[0]

        if extendslopes:
            # Points to duplicate correspond to all indexes but -1 aspect
            points_to_duplicate = np.invert(np.in1d(init_exp[index_points], [-1]))

        if extendaspects or extendslopes:
            # In these cases, we remove flat cases of output slopes list because it is dealt in a specific way
            if "0" in liste_pentes:
                liste_pentes_int.remove(0)
            if -1 in list_exp_degres:
                list_exp_degres.remove(-1)

        init_forcing_file_dimensions = init_forcing_file.listdim()

        massif_dim_name = "massif"
        nbpoints_dim_name = "Number_of_points"
        loc_dim_name = "location"

        # Il faut créer la dimension time en premier (obligatoire au format NETCDF4_CLASSIC)
        new_forcing_file.createDimension("time", None)

        if massif_dim_name in init_forcing_file_dimensions and massif_dim_name in init_forcing_file.listvar():
            init_massif = init_forcing_file.read(massif_dim_name, keepfillvalue=True)
            index_massif = np.where(np.in1d(init_massif, list_massif_number))[0]
            len_dim = len(index_massif)

            if len_dim == 0:
                raise MassifException(list_massif_number, init_massif)

            new_forcing_file.createDimension(massif_dim_name, len_dim)
            del init_forcing_file_dimensions[massif_dim_name]

        if nbpoints_dim_name in init_forcing_file_dimensions:
            spatial_dim_name = nbpoints_dim_name
        elif loc_dim_name in init_forcing_file_dimensions:
            spatial_dim_name = loc_dim_name
        else:
            spatial_dim_name = "missing"

        if spatial_dim_name in init_forcing_file_dimensions:
            # print (extendaspects)
            # print ("NB slopes by level")
            # print (nb_slopes_bylevel)
            # print (len(index_points))
            if extendaspects:
                len_dim = len(index_points) * nb_slopes_bylevel
            elif extendslopes:
                nslopes_to_create = len(liste_pentes) - 2
                len_dim = len(index_points) + np.sum(points_to_duplicate) * nslopes_to_create

                indflat = np.arange(0, len_dim, nb_slope_angles_notflat * len(list_exp_degres) + 1)
                indnoflat = np.delete(np.arange(0, len_dim, 1), indflat)

            else:
                len_dim = len(index_points)
            print("create dimension :" + spatial_dim_name + " " + str(len_dim))
            len_dim_spatial = len_dim
            new_forcing_file.createDimension(spatial_dim_name, len_dim)
            del init_forcing_file_dimensions[spatial_dim_name]

        for dimname, dim in init_forcing_file_dimensions.items():
            print("Create dimension " + dimname + " " + str(len(dim)))
            if not dimname == "time":
                new_forcing_file.createDimension(dimname, len(dim))

        savevar = {}

        for varname in listvar:
            print(varname)
            if self.printmemory:
                print_used_memory()
                print(datetime.datetime.today())
            vartype, rank, array_dim, varFillvalue, var_attrs = init_forcing_file.infovar(varname)

            if len(array_dim) > 0:
                index_dim_massif = np.where(array_dim == massif_dim_name)[0]
                index_dim_nbpoints = np.where(array_dim == spatial_dim_name)[0]
                var_array = init_forcing_file.read(varname, keepfillvalue=True, removetile=False)
            else:
                index_dim_massif = []
                index_dim_nbpoints = []
                var_array = init_forcing_file.read(varname, keepfillvalue=True, removetile=False).getValue()

            if len(index_dim_massif) == 1:
                var_array = np.take(var_array, index_massif, index_dim_massif[0])
            if len(index_dim_nbpoints) == 1:
                var_array = np.take(var_array, index_points, index_dim_nbpoints[0])

                if extendaspects or extendslopes:
                    if varname == "aspect":
                        expo_1level_notflat = np.tile(list_exp_degres, nb_slope_angles_notflat)
                        if "0" in liste_pentes:
                            expo_1level = np.append(-1, expo_1level_notflat)
                        else:
                            expo_1level = expo_1level_notflat

                        if extendaspects:
                            var_array = np.tile(expo_1level, len(index_points))
                        elif extendslopes:
                            var_array = np.tile(expo_1level, len(indflat))

                    elif varname == "slope":
                        slope_1level_notflat = np.repeat(liste_pentes_int, nb_aspect_angles_notflat)
                        if "0" in liste_pentes:
                            slope_1level = np.append(0, slope_1level_notflat)
                        else:
                            slope_1level = slope_1level_notflat

                        if extendaspects:
                            var_array = np.tile(slope_1level, len(index_points))
                        elif extendslopes:
                            var_array = np.tile(slope_1level, len(indflat))

                if varname not in ["aspect", "slope"]:

                    if rank >= 2:
                        if extendaspects:
                            var_array = np.repeat(var_array, nb_slopes_bylevel, axis=1)
                        elif extendslopes:
                            newvar_array = np.empty((var_array.shape[0], len_dim_spatial), vartype)
                            newvar_array[:, indflat] = var_array[:, ~points_to_duplicate]

                            # WE CAN NOT USE NP.REPEAT IN THAT CASE BECAUSE WE WANT TO DUPICATE SEQUENCES OF 8 ASPECTS
                            # WITH THE ASPECT VARYING FASTER THAN THE SLOPE ANGLE.
                            # USE NP.SPLIT TO SEPARATE EACH MASSIF-ELEVATION (GROUPS OF 8 ASPECTS),
                            # THEN USE NP.TILE TO DUPICATE THE SLOPES,
                            # FINALLY NP.HSTACK CONCATENATE THE SEQUENCES ALONG THE LAST DIMENSION

                            newvar_array[:, indnoflat] = np.hstack(
                                np.tile(np.split(var_array[:, points_to_duplicate], len(indflat), axis=1),
                                        1 + nslopes_to_create))

                            del var_array
                            var_array = newvar_array

                    elif rank >= 1:
                        if extendaspects:
                            var_array = np.repeat(var_array, nb_slopes_bylevel)
                        elif extendslopes:
                            newvar_array = np.empty(len_dim_spatial)
                            newvar_array[indflat] = var_array[~points_to_duplicate]

                            # THIS IS EQUIVALENT TO THE SEQUENCE ABOVE FOR RANK 2 VARIABLES
                            newvar_array[indnoflat] = np.tile(
                                np.array(np.split(var_array[points_to_duplicate], len(indflat))),
                                1 + nslopes_to_create).flatten()


#                             print indflat
#                             print indnoflat
                            del var_array
                            var_array = newvar_array

#             print "BEFORE CREATE", datetime.datetime.today()
            var = new_forcing_file.createVariable(varname, vartype, array_dim, fill_value=varFillvalue)
#             print "AFTER CREATE", datetime.datetime.today()

            for attname in var_attrs:
                if not attname == '_FillValue':
                    setattr(var, attname, init_forcing_file.getattr(varname, attname))
            try:
                if not (varname in ["DIR_SWdown", "SCA_SWdown"] and (extendaspects or extendslopes)):
                    # print "BEFORE WRITE", datetime.datetime.today()

                    # do not write direct solar radiations if aspects and slopes were extended
                    # because we need to recompute the values
                    if rank == 0:
                        var[:] = var_array
                    elif rank == 1:
                        var[:] = var_array
                    elif rank == 2:
                        var[:, :] = var_array
                    elif rank == 3:
                        var[:, :, :] = var_array
                    elif rank == 4:
                        var[:, :, :, :] = var_array
                    elif rank == 5:
                        var[:, :, :, :, :] = var_array
#                     print ("AFTER WRITE", datetime.datetime.today())

            except Exception:
                print(var_array)
                raise VarWriteException(varname, var_array.shape, var.shape)

            # Some variables need to be saved for solar computations
            if varname in ["time"]:
                savevar[varname] = init_forcing_file.readtime()
            if varname in ["LAT", "LON", "ZS", "aspect", "slope", "DIR_SWdown",
                           "SCA_SWdown", self.massifvarname(), "Tair", "Rainf", "Snowf"]:
                savevar[varname] = var_array
            if varname == self.massifvarname():
                save_array_dim = array_dim

        if 'snow_layer' in init_forcing_file_dimensions:
            return

        if "LAT" not in init_forcing_file.listvar():
            lat, lon = self.addCoord(new_forcing_file, savevar[self.massifvarname()], save_array_dim, varFillvalue)
        else:
            lat = savevar["LAT"]
            lon = savevar["LON"]

        # Compute new solar radiations according to the new values of slope and aspect
        if extendaspects or extendslopes:
            # print ("BEFORE RADIATION COMPUTATIONS", datetime.datetime.today())
            direct, diffus = sun().slope_aspect_correction(savevar["DIR_SWdown"], savevar["SCA_SWdown"],
                                                           savevar["time"], lat, lon, savevar["aspect"],
                                                           savevar["slope"])
            # print ("AFTER RADIATION COMPUTATIONS", datetime.datetime.today())
            new_forcing_file.variables["DIR_SWdown"][:] = direct
            new_forcing_file.variables["SCA_SWdown"][:] = diffus
            del savevar["DIR_SWdown"]
            del savevar["SCA_SWdown"]
            del savevar["time"]
            del savevar["aspect"]

            print("AFTER WRITE RADIATIONS", datetime.datetime.today())

        self.add_massif_variables(init_forcing_file, new_forcing_file, savevar=savevar)
        print("AFTER MASSIF VARIABLES", datetime.datetime.today())

        del savevar

    def addCoord(self, forcing, massifnumber, dimension, varFillValue):
        """Routine to add coordinates in the forcing file for the SAFRAN massifs
        """
        INFOmassifs = infomassifs()
        dicLonLat = INFOmassifs.getAllMassifLatLon()

        lat = np.empty(massifnumber.shape, np.float)
        lon = np.empty(massifnumber.shape, np.float)

        for point in range(0, len(massifnumber)):
            lonlat = dicLonLat[massifnumber[point]]
            lat[point] = lonlat[1]
            lon[point] = lonlat[0]

        var = forcing.createVariable("LAT", np.float, dimension, fill_value=varFillValue)
        setattr(var, 'long_name', 'latitude')
        setattr(var, 'units', 'degrees_north')
        var[:] = lat
        var = forcing.createVariable("LON", np.float, dimension, fill_value=varFillValue)
        setattr(var, 'long_name', 'longitude')
        setattr(var, 'units', 'degrees_east')
        var[:] = lon

        return lat, lon


class proselect(forcinput_select):
    """
    This class is designed to extract a selection of massifs, elevation levels, slopes or aspects from a PRO file.
    """

    def massifvarname(self):
        return 'massif_num'

    def add_massif_variables(self, init_forcing_file, new_forcing_file, savevar={}):
        pass

    def StandardFILE(self, *args, **kwargs):
        return StandardCROCUS(*args, **kwargs)


class forcinput_ESMSnowMIP(forcinput_tomodify):
    """
    This class prepares FORCING files from the ESMSnowMIP offical netcdf dataset
    """
    formatout = "NETCDF3_CLASSIC"

    def upscale_tab_time(self, var, theshape):

        # array slope can be considered at the same shape than the other arrays (simplify matricial traitment)
        bigvar = np.ma.zeros(theshape)

        if len(theshape) == 2:
            for i in range(0, theshape[1]):
                bigvar[:, i] = var[:]
        elif len(theshape) == 3:
            for ilat in range(0, theshape[1]):
                for ilon in range(0, theshape[2]):
                    bigvar[:, ilat, ilon] = var[:]

        else:
            print("error on indices in upscale_tab_time")

        return bigvar

    def modify(self, init_forcing_file, new_forcing_file, *args):
        """ Prepare ESM-SnowMIP forcings.

        :param init_forcing_file: Input file object from ESM-SnowMIP database
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object (SURFEX format)
        :type new_forcing_file: :class:`utils.prosimu.prosimu`
        """
        site = os.path.basename(self.filename)[11:14]
        source = os.path.basename(self.filename)[4:10]
        if site == "cdp":
            const = {"LAT": 45.3, "LON": 5.77, "ZS": 1325, "slope": 0, "aspect": -1,
                     "ZREF": 37, "UREF": 38, "CO2air": 0.00062, "Wind_DIR": 0, "SCA_SWdown": 0}
            if source == "insitu":
                timeshift = 0
            elif source == "gswp3c":
                timeshift = -1
        if site == "oas":
            const = {"LAT": 53.63, "LON": -106.2, "ZS": 600, "slope": 0, "aspect": -1,
                     "ZREF": 37, "UREF": 38, "CO2air": 0.00062, "Wind_DIR": 0, "SCA_SWdown": 0}
            if source == "insitu":
                timeshift = 6
            elif source == "gswp3c":
                timeshift = -1
        elif site == "obs":
            const = {"LAT": 53.99, "LON": -105.12, "ZS": 629, "slope": 0,
                     "aspect": -1, "ZREF": 25, "UREF": 26, "CO2air": 0.00062, "Wind_DIR": 0, "SCA_SWdown": 0}
            if source == "insitu":
                timeshift = 6
            elif source == "gswp3c":
                timeshift = -1
        elif site == "ojp":
            const = {"LAT": 53.92, "LON": -104.69, "ZS": 579, "slope": 0,
                     "aspect": -1, "ZREF": 28, "UREF": 29, "CO2air": 0.00062, "Wind_DIR": 0, "SCA_SWdown": 0}
            if source == "insitu":
                timeshift = 6
            elif source == "gswp3c":
                timeshift = -1
        elif site == "rme":
            const = {"LAT": 43.19, "LON": -116.78, "ZS": 2060, "slope": 0,
                     "aspect": -1, "ZREF": 3, "UREF": 3, "CO2air": 0.00062, "Wind_DIR": 0, "SCA_SWdown": 0}
            if source == "insitu":
                timeshift = 7
            elif source == "gswp3c":
                timeshift = -1
        elif site == "sap":
            const = {"LAT": 43.08, "LON": 141.34, "ZS": 15, "slope": 0,
                     "aspect": -1, "ZREF": 1.5, "UREF": 1.5, "CO2air": 0.00062, "Wind_DIR": 0, "SCA_SWdown": 0}
            if source == "insitu":
                timeshift = -9
            elif source == "gswp3c":
                timeshift = -1
        elif site == "snb":
            const = {"LAT": 37.91, "LON": -107.73, "ZS": 3714, "slope": 0,
                     "aspect": -1, "ZREF": 3.8, "UREF": 4, "CO2air": 0.00062, "Wind_DIR": 0, "SCA_SWdown": 0}
            if source == "insitu":
                timeshift = 7
            elif source == "gswp3c":
                timeshift = -1
        elif site == "sod":
            const = {"LAT": 67.37, "LON": 26.63, "ZS": 179, "slope": 0,
                     "aspect": -1, "ZREF": 2, "UREF": 2, "CO2air": 0.00062, "Wind_DIR": 0, "SCA_SWdown": 0}
            if source == "insitu":
                timeshift = 0
            elif source == "gswp3c":
                timeshift = -1
        elif site == "swa":
            const = {"LAT": 37.91, "LON": -107.71, "ZS": 3371, "slope": 0,
                     "aspect": -1, "ZREF": 3.4, "UREF": 3.8, "CO2air": 0.00062, "Wind_DIR": 0, "SCA_SWdown": 0}
            if source == "insitu":
                timeshift = 7
            elif source == "gswp3c":
                timeshift = -1
        elif site == "wfj":
            const = {"LAT": 46.82962, "LON": 9.80934, "ZS": 2540, "slope": 0,
                     "aspect": -1, "ZREF": 4.5, "UREF": 5.5, "CO2air": 0.00062, "Wind_DIR": 0, "SCA_SWdown": 0}
            if source == "insitu":
                timeshift = 0
            elif source == "gswp3c":
                timeshift = -1

        if source == "gswp3c":
            timeshift = -1
            const["ZREF"] = 2.
            const["UREF"] = 10.

        const["PSurf"] = 101325 * ((288 - 0.0065 * const["ZS"]) / 288)**5.255

        new_forcing_file.createDimension("time", None)
        new_forcing_file.createDimension("Number_of_points", 1)

        for varname, ncvar in init_forcing_file.dataset.variables.iteritems():
            var_array = ncvar[:]
            if varname == "time":
                var = new_forcing_file.createVariable(varname, 'd', ("time"), fill_value=-9999999.)
                time_array = var_array[:] + timeshift
                var[:] = time_array
                unit_time = getattr(ncvar, "units")
            elif varname == "SWdown":
                vardir = new_forcing_file.createVariable("DIR_SWdown", 'd', ("time", "Number_of_points"),
                                                         fill_value=-9999999.)
                setattr(vardir, "long_name", "Direct downward shortwave radiation")
                vardif = new_forcing_file.createVariable("SCA_SWdown", 'd', ("time", "Number_of_points"),
                                                         fill_value=-9999999.)
                setattr(vardif, "long_name", "Diffuse downward shortwave radiation")
                vardir[:, 0], vardif[:, 0] = sun().directdiffus(var_array, netCDF4.num2date(time_array, unit_time),
                                                                const["LAT"], const["LON"], const["slope"],
                                                                const["aspect"], site)
            else:
                var = new_forcing_file.createVariable(varname, 'd', ("time", "Number_of_points"), fill_value=-9999999.)
                var[:, 0] = var_array[:]

            if varname != "SWdown":
                for attname in ncvar.ncattrs():
                    setattr(var, attname, getattr(ncvar, attname))

        frc = new_forcing_file.createVariable('FRC_TIME_STP', 'd', fill_value=-999999)
        frc[:] = 3600.

        for varname in ["LAT", "LON", "ZS", "slope", "aspect", "ZREF", "UREF"]:
            var = new_forcing_file.createVariable(varname, 'd', ("Number_of_points"), fill_value=-9999999.)
            var[:] = const[varname]

        for varname in ["PSurf", "CO2air", "Wind_DIR"]:
            var = new_forcing_file.createVariable(varname, 'd', ("time", "Number_of_points"), fill_value=-9999999.)
            var[:, 0] = const[varname]

        return False


class forcinput_extract(forcinput_tomodify):
    """This class allows to extract from an original forcing file all the variables corresponding
    to a pre-defined list of points.

    Implemented by C. Carmagnola in November 2018 (PROSNOW project).
    """

    def modify(self, init_forcing_file, new_forcing_file, *args):
        """ Extract a pre-defined list of points in a forcing file.

        :param init_forcing_file: Input file object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object
        :type new_forcing_file: :class:`utils.prosimu.prosimu`
        :param args: .txt file name containing the list of points to be extracted
        :type args: str
        """

        # Read data from file

        if not os.path.isfile(args[0][0]):
            raise FileNameException(args[0][0])

        mdat = open(args[0][0])

        list_massif_number, list_sru, list_alt, list_asp, list_slo = np.loadtxt(mdat, delimiter=' ',
                                                                                usecols=(2, 3, 1, 6, 7), unpack=True)

#         print "Points to be extracted:"
#         print "massif = " + str(list_massif_number)
#         print "elevation = " + str(list_alt)
#         print "aspect = " + str(list_asp)
#         print "slope = " + str(list_slo)
#         print "---------------"

        # Variables

        listvar = init_forcing_file.listvar()
        listvar.append(u'stations')

        # Massif / Elevation / Aspect / Slope

        init_massif_nb_sop = init_forcing_file.read("massif_number", keepfillvalue=True)
        init_alt = init_forcing_file.read("ZS", keepfillvalue=True)
        init_exp = init_forcing_file.read("aspect", keepfillvalue=True)
        init_slopes = init_forcing_file.read("slope", keepfillvalue=True)

        list_asp_degres = list_asp[:]
        list_slo_int = list(map(int, list_slo))

        # Indices

        index_points = np.zeros(len(list_massif_number), 'int')

        for i, j in ((a, b) for a in range(len(init_massif_nb_sop)) for b in range(len(list_massif_number))):
            if init_massif_nb_sop[i] == list_massif_number[j] and init_alt[i] == list_alt[j] and \
               init_slopes[i] == list_slo_int[j] and init_exp[i] == list_asp_degres[j]:
                index_points[j] = i

#         print "Indices:"
#         print index_points
#         print "---------------"

        # Create dimension

        init_forcing_file_dimensions = init_forcing_file.listdim()

        massif_dim_name = "massif"
        nbpoints_dim_name = "Number_of_points"
        loc_dim_name = "location"

        new_forcing_file.createDimension("time", None)

        if massif_dim_name in init_forcing_file_dimensions:
            init_massif = init_forcing_file.read("massif", keepfillvalue=True)
            index_massif = np.where(np.in1d(init_massif, list_massif_number))[0]
            len_dim = len(index_massif)
            new_forcing_file.createDimension(massif_dim_name, len_dim)
            del init_forcing_file_dimensions[massif_dim_name]

        if nbpoints_dim_name in init_forcing_file_dimensions:
            spatial_dim_name = nbpoints_dim_name
        elif loc_dim_name in init_forcing_file_dimensions:
            spatial_dim_name = loc_dim_name
        else:
            spatial_dim_name = "missing"

        if spatial_dim_name in init_forcing_file_dimensions:
            len_dim = len(index_points)
            new_forcing_file.createDimension(spatial_dim_name, len_dim)
            del init_forcing_file_dimensions[spatial_dim_name]

#             print (spatial_dim_name + ": ")
#             print str(len_dim)

        # Fill new file

        for varname in listvar:

            # Variable containing the sru numbers

            if varname == 'stations':

                vartype = 'float32'
                rank = 1
                array_dim = [u'Number_of_points']
                varFillvalue = -1e+07
                var_attrs = [u'_FillValue', u'long_name', u'units']

                var = new_forcing_file.createVariable(varname, vartype, array_dim, fill_value=varFillvalue)
                var[:] = list_sru

            # All other variables

            else:

                vartype, rank, array_dim, varFillvalue, var_attrs = init_forcing_file.infovar(varname)

                if len(array_dim) > 0:
                    index_dim_massif = np.where(array_dim == massif_dim_name)[0]
                    index_dim_nbpoints = np.where(array_dim == spatial_dim_name)[0]
                    var_array = init_forcing_file.read(varname, keepfillvalue=True)

                else:
                    index_dim_massif = []
                    index_dim_nbpoints = []
                    var_array = init_forcing_file.read(varname, keepfillvalue=True).getValue()

                if len(index_dim_massif) == 1:
                    var_array = np.take(var_array, index_massif, index_dim_massif[0])
                if len(index_dim_nbpoints) == 1:
                    var_array = np.take(var_array, index_points, index_dim_nbpoints[0])

                var = new_forcing_file.createVariable(varname, vartype, array_dim, fill_value=varFillvalue)

                for attname in var_attrs:
                    if not attname == '_FillValue':
                        setattr(var, attname, init_forcing_file.getattr(varname, attname))
                try:
                    if rank == 0:
                        var[:] = var_array
                    elif rank == 1:
                        var[:] = var_array
                    elif rank == 2:
                        var[:, :] = var_array
                    elif rank == 3:
                        var[:, :, :] = var_array
                    elif rank == 4:
                        var[:, :, :, :] = var_array
                    elif rank == 5:
                        var[:, :, :, :, :] = var_array
                except Exception:
                    print(var_array)
                    raise VarWriteException(varname, var_array.shape, var.shape)


class forcinput_changedates(forcinput_tomodify):

    """This class allows to change the dates of a forcing file from the climatology
    Implemented by C. Carmagnola in November 2018 (PROSNOW project).
    """

    def modify(self, init_forcing_file, new_forcing_file, *args):
        """ Change the dates of a forcing file for climatological forecast.

        :param init_forcing_file: Input file object
        :type init_forcing_file: :class:`utils.prosimu.prosimu`
        :param new_forcing_file: Output file object
        :type new_forcing_file: :class:`utils.prosimu.prosimu`
        :param args: New initial date
        :type args: :class:`bronx.stddtypes.date.Date`
        """

        # Open file
        file_name = netCDF4.Dataset(init_forcing_file.path, "a")
        nc_time = file_name.variables["time"]
        nc_unit = file_name.variables["time"].units

        # Compute new time
        date_time_old = netCDF4.num2date(nc_time[:], units=nc_unit)
        delta = args[0][0] - date_time_old[0]
        date_time_new = date_time_old + delta

        # Insert new time in file
        nc_time[:] = netCDF4.date2num(date_time_new, units=nc_unit)

        # Close file
        file_name.close()
