#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 6 déc. 2018

@author: lafaysse

usage: python postprocess.py [-b YYYYMMDD] [-e YYYYMMDD] [-o diroutput]

    #) extracts operational simulation results
    #) Plots maps for the Alps, the Pyrenees the Corse, Vosges, Massif Central, Jura
    #) Creates spaghetti plots for all massifs and stations
"""

import os

import numpy as np

from snowtools.utils.prosimu import prosimu
from snowtools.utils.infomassifs import infomassifs


class Ensemble(object):
    """
    Describes an ensemble of simulations

    """

    def __init__(self):
        """
        """
        self.ensemble = {}  #: data dict with variable names as keys and np.arrays as values

    @property
    def spatialdim(self):
        """Name of spatial dimension"""
        return "Number_of_points"

    def open(self, listmembers):
        """
        Opens simulation files

        :param listmembers: list of ensemble members (filenames)
        :type listmembers: list
        :ivar simufiles: list of prosimu objects with simulation file
        :vartype simufiles: list
        :ivar inddeterministic: index of the deterministic member
        :vartype inddeterministic: int
        :ivar nmembers:  number of members
        :vartype nmemeber: int
        :ivar ~.time: time variable from the first simulation file
        :vartype ~.time: numpy array
        :ivar indpoints: list with spatial indices
        :vartype indpoints: list
        :ivar npoints: total number of spatial points
        :vartype npoints: int
        :ivar nech: number of time steps (forecast steps)
        :vartype nech: int
        """
        print(listmembers)
        self.simufiles = []
        for m, member in enumerate(listmembers):
            p = prosimu(member)
            if m == 0:
                self.nech = p.getlendim("time")
            ntime = p.getlendim("time")
            if ntime == self.nech:
                self.simufiles.append(p)
                if 'mb035' in member:
                    self.inddeterministic = len(self.simufiles) - 1

        self.nmembers = len(self.simufiles)
        print(self.nmembers)
        self.time = self.simufiles[0].readtime()

        self.indpoints = self.select_points()
        self.npoints = self.get_npoints()

    def select_points(self):
        """
        Get a list of spatial indices from the spatial dimension length in the first simulation file.

        :return: list of spatial points (indices)
        :rtype: list
        """
        indpoints = range(0, self.simufiles[0].getlendim(self.spatialdim))
        return indpoints

    def get_npoints(self):
        """
        Get the number of spatial points.

        :return: total number of spatial points
        :rtype: int
        """
        if type(self.indpoints) is tuple:
            npoints = 0
            for indpoints in self.indpoints:
                npoints += len(indpoints)
        else:
            npoints = len(self.indpoints)

        return npoints

    def read(self, varname):
        """
        Read a variable and store it in :py:attr:`~.ensemble`

        :param varname: name of the variable to be read (corresponds to the variable name of the simulation
                        NetCDF files)
        :type varname: str

        """

        self.ensemble[varname] = np.empty([self.nech, self.npoints, self.nmembers])

        kwargs = dict()

        for m, member in enumerate(self.simufiles):
            print("read " + varname + " for member" + str(m))
            import datetime
            before = datetime.datetime.today()

            if type(self.indpoints) is tuple:
                sections = []
                for indpoints in self.indpoints:
                    kwargs[self.spatialdim] = indpoints
                    sections.append(member.read_var(varname, **kwargs))

                self.ensemble[varname][:, :, m] = np.concatenate(tuple(sections), axis=1)
            else:
                kwargs[self.spatialdim] = self.indpoints
                self.ensemble[varname][:, :, m] = member.read_var(varname, **kwargs)

            after = datetime.datetime.today()
            print(after - before)

        # Verrues (à éviter)
        if varname == 'NAT_LEV':
            self.ensemble[varname][:, :, :] = np.where(self.ensemble[varname] == 6., 0., self.ensemble[varname])

    def read_geovar(self, varname):
        """
        Read a variable from the first simulation file.

        :param varname: NetCDF variable name of the variable to read
        :type varname: str
        :return: data read
        :rtype: numpy array
        """
        if type(self.indpoints) is tuple:
            sections = []
            kwargs = dict()
            for indpoints in self.indpoints:
                kwargs[self.spatialdim] = indpoints
                sections.append(self.simufiles[0].read_var(varname, **kwargs))

            return np.concatenate(tuple(sections))
        else:
            kwargs[self.spatialdim] = self.indpoints
            return self.simufiles[0].read_var(varname, **kwargs)

    def probability(self, varname, seuilinf=-999999999, seuilsup=999999999):
        """
        Calculates probability as the proportion of ensemble members with values larger than :py:attr:`seuilinf`
        and smaller than :py:attr:`seuilsup`.

        :param varname: name of the variable for which to calculate the probability
        :type varname: str
        :param seuilinf: lower threshold
        :param seuilsup: upper threshold
        :return: probability field (or np.nan if the ensemble is not defined)
        :rtype: numpy array
        """

        if varname not in self.ensemble.keys():
            self.read(varname)

        condition = (self.ensemble[varname] > seuilinf) & (self.ensemble[varname] < seuilsup)
        probability = np.sum(condition, axis=2) / (1. * self.nmembers)

        return np.where(np.isnan(self.ensemble[varname][:, :, 0]), np.nan, probability)
        # On renvoit des nan quand ensemble n'est pas défini

    def quantile(self, varname, level):
        """
        Calculates ensemble percentiles for a given variable and given percentile levels.

        :param varname: Variable name
        :type varname: str
        :param level: list of percentiles to calculate
        :type level: list of numbers between 0 and 100
        :return: array of percentiles
        :rtype: numpy array
        """

        if varname not in self.ensemble.keys():
            self.read(varname)

        quantile = np.where(np.isnan(self.ensemble[varname][:, :, 0]), np.nan,
                            np.percentile(self.ensemble[varname], level, axis=2))
        return quantile

    def mean(self, varname):
        """
        Calculate ensemble mean for a given variable.

        :param varname: variable name
        :type varname: str
        :return: ensemble mean
        :rtype: numpy array
        """

        if varname not in self.ensemble.keys():
            self.read(varname)

        return np.nanmean(self.ensemble[varname], axis=2)

    def spread(self, varname):
        """
        Calculate ensemble spread (standard deviation) for a given variable.

        :param varname: variable name
        :type varname: str
        :return: ensemble spread
        :rtype: numpy array
        :rtype: numpy array
        """

        if varname not in self.ensemble.keys():
            self.read(varname)

        return np.nanstd(self.ensemble[varname], axis=2)

    def close(self):
        """
        Close simulation files and remove data from :py:attr:`ensemble`.

        """
        for member in self.simufiles:
            member.close()
        self.ensemble.clear()

    def get_metadata(self):
        """
        Get a tuple with spatial indices.

        :return: indpoints, indpoints
        """
        indpoints = self.select_points()
        return indpoints, indpoints

    def get_alti(self):
        """
        Get altitude variable from the first simulation file.

        :return: altitude variable
        :rtype: numpy array
        """
        if not hasattr(self, "alti"):
            self.alti = self.read_geovar("ZS")
        return self.alti

    def get_aspect(self):
        """
        Get aspect variable from the first simulation file.

        :return: aspect variable
        :rtype: numpy array
        """
        if not hasattr(self, "aspect"):
            self.aspect = self.read_geovar("aspect")
        return self.aspect


class _EnsembleMassif(Ensemble):
    """
    Metaclass for ensemble simulations on a massif geometry (SAFRAN like).

    :ivar InfoMassifs: Information of Massifs
    """

    InfoMassifs = infomassifs()

    @property
    def geo(self):
        """
        Geometry

        :return: "massifs"
        """
        return "massifs"

    def read(self, varname):
        """
        Read data for a given variable name into the :py:attr:`ensemble` instance variable.

        :param varname: variable name
        :type varname: str

        """
        if varname == 'naturalIndex':
            nmassifs = len(self.get_massifvar())
            self.ensemble[varname] = np.empty([self.nech, nmassifs, self.nmembers])
            for m, member in enumerate(self.simufiles):
                self.ensemble[varname][:, :, m] = member.read_var(varname)
        else:
            super(_EnsembleMassif, self).read(varname)

    def get_massifdim(self):
        """
        Read massif_num variable from the first simulation file.

        :return: massif numbers
        :rtype: numpy array
        """
        if not hasattr(self, "massifdim"):
            self.massifdim = self.read_geovar("massif_num")
        return self.massifdim

    def get_massifvar(self):
        """
        Read "massif" variable from the first simulation file.

        :return: massif numbers
        :rtype: numpy array
        """
        if not hasattr(self, "massifvar"):
            self.massifvar = self.simufiles[0].read_var("massif")
        return self.massifvar

    def get_metadata(self, nolevel=False):
        """
        Construct filenames and plot titles from massif and altitude variables in the first simulation file.

        :param nolevel: if True the altitude is not included in the filenames and titles.
        :return: a list of filenames and a list of titles
        :rtype: two lists.
        """

        if nolevel:
            massif = self.get_massifvar()
            alti = [None] * len(massif)
        else:
            alti = self.get_alti()
            massif = self.get_massifdim()

        return [self.build_filename(mas, alt) for mas, alt in zip(massif, alti)], \
               [self.build_title(mas, alt) for mas, alt in zip(massif, alti)]

    def build_filename(self, massif, alti):
        """
        Construct a filename from massif and altitude information.

        :param massif: a massif number
        :param alti: an altitude level
        :return: filename
        :rtype: str
        """
        filename = str(massif)
        if alti:
            filename += "_" + str(int(alti))
        return filename

    def build_title(self, massif, alti):
        """
        Construct a figure title from massif and altitude information.

        :param massif: a massif number
        :param alti: an altitude level
        :return: a title
        :rtype: unicode str
        """
        title = self.InfoMassifs.getMassifName(massif)  # type unicode
        if alti:
            title += u" %d m" % int(alti)
        return title  # matplotlib needs unicode


class EnsembleFlatMassif(_EnsembleMassif):
    """
    Class for ensemble simulations on a massif geometry (SAFRAN like) where all data points are considered
    on flat terrain (zero slope and no orientation information).
    """

    def select_points(self):
        """
        Select spatial indices from the first simulation file where aspect=-1 (zero slope, no orientation information).

        :return: spatial indices to be read from simulation files
        :rtype: numpy boolean array
        """
        return self.simufiles[0].get_points(aspect=-1)


class EnsembleNorthSouthMassif(_EnsembleMassif):
    """
    Class for ensemble simulations on a massif geometry (SAFRAN like) where data points are considered at a
    slope of 40 degrees and two orientations: North and South.
    """

    def select_points(self):
        """
        Select spatial indices from the first simulation file where slope=40 and aspect either 0 or 180.

        :return: spatial indices to be read from simulation files
        :rtype: a tuple of numpy boolean array, where the first component corresponds to the Northern orientation
                and the second to the Southern one.
        """
        # return np.sort(np.concatenate((self.simufiles[0].get_points(aspect = 0, slope = 40),
        # self.simufiles[0].get_points(aspect = 180, slope = 40))))
        # TAKE CARE : It is extremely more efficient to read regular sections of the netcdf files
        return (self.simufiles[0].get_points(aspect=0, slope=40),
                self.simufiles[0].get_points(aspect=180, slope=40))


class EnsembleMassifPoint(_EnsembleMassif):
    """
    Class for extracting one specific point in massif geometry
    """

    def __init__(self, massif_num, alti, aspect, slope):
        self.massif_num = massif_num
        self.alti = alti
        self.aspect = aspect
        self.slope = slope

        super(EnsembleMassifPoint, self).__init__()

    def select_points(self):
        """Select index of the corresponding point"""

        return self.simufiles[0].get_points(massif_num= self.massif_num, aspect=self.aspect, slope=self.slope,
                                            ZS=self.alti)


class EnsembleStation(Ensemble):
    """
    Class for ensemble simulations at station locations.

    :ivar InfoMassifs: Information on the Massif the station is situated.
    """

    InfoMassifs = infomassifs()

    @property
    def geo(self):
        """
        Geometry information.

        :return: "stations"
        """
        return "stations"

    def get_station(self):
        """
        Read station numbers from the first simulation file.

        :return: station numbers
        :rtype: numpy array
        """
        return self.simufiles[0].read_var("station", Number_of_points=self.indpoints)

    def get_metadata(self, **kwargs):
        """
        Construct filenames and plot titles from altitude and station information

        :param kwargs:
        :return: a list of filenames and a list of plot titles
        """
        alti = self.simufiles[0].read_var("ZS", Number_of_points=self.indpoints)
        station = self.get_station()
        return [self.build_filename(stat, alt) for stat, alt in zip(station, alti)], \
               [self.build_title(stat, alt) for stat, alt in zip(station, alti)]

    def build_filename(self, station, alti):
        """
        Construct a filename from a station number

        :param station: station number
        :param alti: altitude level (not used)
        :return: filename
        :rtype: station number as 8digit integer.
        """
        return '%08d' % station

    def build_title(self, station, alti):
        """
        Construct a title from a station number and altitude level.

        :param station: station number
        :param alti: station altitude
        :return: title string composed of the station name and the altitude.
        :rtype: unicode str
        """
        # nameposte gives unicode
        # matplotlib expects unicode
        return self.InfoMassifs.nameposte(station) + " %d m" % int(alti)


class EnsembleDiags(Ensemble):
    """
    Probabilities and quantiles from ensembles.
    """

    def __init__(self):
        """
        """
        self.proba = {}  #: probabilities
        self.quantiles = {}  #: percentiles
        super(EnsembleDiags, self).__init__()

    def diags(self, list_var, list_quantiles, list_seuils):
        """
        Calculate quantiles and exceedance probabilities for a list of variables, quantiles and thresholds.
        The quantiles are stored in :py:attr:`~.quantiles` and the probabilities in :py:attr:`~.proba`

        :param list_var: list of variables
        :type list_var: iterable
        :param list_quantiles: list of percentiles to be calculated
        :type list_quantiles: iterable
        :param list_seuils: list of thresholds for each variable
        :type list_seuils: dict of iterables with variable names as keys
        """
        for var in list_var:
            if var in list_seuils.keys():
                for seuil in list_seuils[var]:
                    self.proba[(var, seuil)] = self.probability(var, seuilinf=seuil)

        for var in list_var:
            self.quantiles[var] = []
            for quantile in list_quantiles:
                print("Compute quantile " + str(quantile) + " for variable " + var)
                self.quantiles[var].append(self.quantile(var, quantile))

    def close(self):
        """
        Close simulation files and remove data from :py:attr:`~.plots.pearps2m.postprocess.Ensemble.ensemble`,
        :py:attr:`.proba` and :py:attr:`.quantiles`
        """
        super(EnsembleDiags, self).close()
        self.proba.clear()
        self.quantiles.clear()


class EnsemblePostproc(_EnsembleMassif):
    """
    Class for ensemble post-processing.
    """
    def __init__(self, variables, inputfiles, decile=range(10, 100, 10), outdir='.'):
        """
        :param variables: list of variables to process
        :param inputfiles: list of input files
        :param decile: list of percentiles
        :param outdir: Output directory
        :type outdir: str
        """
        print(inputfiles)
        super(EnsemblePostproc, self).__init__()
        # self.ensemble = self  #: ensemble data
        self.variables = variables  #: list of variables
        # self.ensemble.open(inputfiles)
        self.open(inputfiles)

        #: output filename
        self.outfile = os.path.join(outdir, 'PRO_post.nc')
        # self.outfile = os.path.join(outdir, 'PRO_post_{0}_{1}.nc'.format(datebegin.ymdh, dateend.ymdh))
        #: list of percentiles
        self.decile = decile

    @property
    def standardvars(self):
        """variables always written to the output file"""
        return ['time', 'ZS', 'aspect', 'slope', 'massif_num', 'longitude', 'latitude']

    def create_outfile(self):
        """
        Create output data set.

        :ivar outdataset: output data set
        :vartype outdataset: :py:class:`utils.prosimu.prosimu`
        """
        # if not os.path.isdir(self.outfile):
        # print(self.outfile)
        #     raise DirNameException(self.outfile)
        self.outdataset = prosimu(self.outfile, ncformat='NETCDF4_CLASSIC', openmode='w')

    def init_outfile(self):
        """
        Copy global attributes, dimensions and standard variables from the first simulation file to the output file.
        """
        # copy global attributes all at once via dictionary
        self.outdataset.dataset.setncatts(self.simufiles[0].dataset.__dict__)
        # copy dimensions
        for name, dimension in self.simufiles[0].dataset.dimensions.items():
            self.outdataset.dataset.createDimension(
                name, (len(dimension) if not dimension.isunlimited() else None))
        print(self.outdataset.listdim())

        # copy standard variables
        for name, variable in self.simufiles[0].dataset.variables.items():
            if name in self.standardvars:
                fillval = self.simufiles[0].getfillvalue(name)
                self.outdataset.dataset.createVariable(name, variable.datatype, variable.dimensions,
                        fill_value=fillval)
                # copy variable attributes without _FillValue since this causes an error
                for att in self.simufiles[0].listattr(name):
                    if att != '_FillValue':
                        print(self.simufiles[0].getattr(name, att))
                        self.outdataset.dataset[name].setncatts({att: self.simufiles[0].getattr(name, att)})
                self.outdataset.dataset[name][:] = self.simufiles[0].dataset[name][:]
                print('data copied')
        print(self.outdataset.listvar())

    def postprocess(self):
        """
        Do postprocessing

        #) create output file
        #) copy global attributes and standard variables to the output file
        #) calculate deciles for the data variables and put them to the output file
        #) close all input and output data sets.

        Calls :py:meth:`.create_outfile`, :py:meth:`.init_outfile`, :py:meth:`.deciles` and close methods for
        all data sets.

        """
        self.create_outfile()
        self.init_outfile()
        print('init done')
        # self.median()
        # print('median done')
        self.deciles()
        self.outdataset.close()
        self.close()

    def deciles(self):
        """
        Calculates percentiles given in :py:attr:`.decile` for variables in :py:attr:`.variables` and adds them
        to the output data set including the corresponding dimension, coordinate variable and attributes.
        """
        # create decile dimension
        self.outdataset.dataset.createDimension('decile', len(self.decile))
        # create decile variable
        self.outdataset.dataset.createVariable('decile', 'i4', 'decile')
        self.outdataset.dataset['decile'][:] = self.decile[:]
        atts = {'long_name': "Percentiles of the ensemble forecast"}
        self.outdataset.dataset['decile'].setncatts(atts)
        for name, variable in self.simufiles[0].dataset.variables.items():
            if name in self.variables:
                # calculate deciles
                vardecile = self.quantile(name, self.decile)
                # get decile axis in the right place
                vardecile = np.moveaxis(vardecile, 0, -1)
                fillval = self.simufiles[0].getfillvalue(name)
                self.outdataset.dataset.createVariable(name, variable.datatype, variable.dimensions + ('decile',),
                        fill_value=fillval)
                # copy variable attributes all at once via dictionary, but without _FillValue
                attdict = self.simufiles[0].dataset[name].__dict__
                attdict.pop('_FillValue', None)
                self.outdataset.dataset[name].setncatts(attdict)
                print(vardecile.shape)
                self.outdataset.dataset[name][:] = vardecile[:]

    def median(self):
        """
        Calculates the median for variables in :py:attr:`.variables` and adds the median
        to the output data set including the corresponding attributes.
        """
        print('entered median')
        for name, variable in self.simufiles[0].dataset.variables.items():
            if name in self.variables:
                median = self.quantile(name, 50)
                fillval = self.simufiles[0].getfillvalue(name)
                self.outdataset.dataset.createVariable(name, variable.datatype, variable.dimensions,
                        fill_value=fillval)
                # copy variable attributes all at once via dictionary, but without _FillValue
                attdict = self.simufiles[0].dataset[name].__dict__
                attdict.pop('_FillValue', None)
                self.outdataset.dataset[name].setncatts(attdict)
                print(self.outdataset.dataset[name][:].size)
                print(median.size)
                self.outdataset.dataset[name][:] = median[:]


class EnsembleHydro(EnsemblePostproc):
    """
    Class to provide a synthesis of ensemble hydrological diagnostics of S2M
    """

    @property
    def spatialdim(self):
        return 'basin'

    @property
    def standardvars(self):
        return ['time', 'basin']
