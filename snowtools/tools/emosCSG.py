#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 25 June 2018

@author: lafaysse
"""

import locale
import datetime
import os

from abc import ABC, abstractmethod
import rpy2.robjects as robjects
import numpy as np
import netCDF4
from scipy.stats import gamma

from snowtools.plots.pearps2m.postprocess import Config, Ensemble, EnsembleOperDiagsFlatMassif,\
    EnsembleStation
from snowtools.tasks.oper.get_oper_files import S2MExtractor
from snowtools.utils.FileException import DirNameException
from snowtools.utils.dates import pretty_date
from snowtools.DATA import SNOWTOOLS_DIR


def csg_mean(a_1, a_2, a_3, a_4, muclim, ensmean, ens_pop):
    """
    calculate the mean of the csg distribution given csg parameters and ensemble characteristics

    :param a_1: first csg parameter for mean
    :param a_2: second csg parameter for mean
    :param a_3: third csg parameter for mean
    :param a_4: fourth csg parameter for mean
    :param muclim: climatological mean
    :param ensmean: ensemble mean
    :param ens_pop: probability of precipitation given the ensemble forecast
    """
    return (muclim / a_1) * np.log1p(np.expm1(a_1) * (a_2 + a_3 * ens_pop + a_4 * ensmean))


def csg_std(b_1, b_2, muclim, sigmaclim, emosmean, ensspread):
    """
    calculate standard deviation of the csg distribution given csg parameters and ensemble
    characteristics

    :param b_1: first csg parameter for spread
    :param b_2: second csg parameter for spread
    :param muclim: climatological mean
    :param sigmaclim: climatological standard deviation
    :param emosmean: ensemble mean
    :param ensspread: ensemble spread, ensemble standard deviation
    """
    return b_1 * sigmaclim * np.sqrt(emosmean / muclim) + b_2 * ensspread


def csg_delta(ensmean, deltaclim):
    """
    return climatological delta parameter

    :param ensmean: ensemble mean
    :param deltaclim: climatological delta parameter
    """
    return ensmean * 0. + deltaclim


def quantiles_CSGD_1point(csgmean, csgstd, csgdelta, quantiles):
    """
    get quantiles of a csg distribution given mean, standard deviation, deltan and quantiles

    :param csgmean: mean of csg distribution
    :param csgstd: standard deviation of csg distribution
    :param csgdelta: delta parambeter of csg distribution
    :param quantiles: list of quantiles to calculate
    """
    tmp = gamma.ppf(q=quantiles, a=(csgmean / csgstd) ** 2, scale=(csgstd ** 2) / csgmean,
                    loc=csgdelta)
    return np.where(tmp >= 0, tmp, 0)


class PostprocessEnsemble(ABC, Ensemble):
    """
    Class for postprocessed ensemble
    """

    newsnow_var_name = 'SD_1DY_ISBA'
    location_dim_name = 'Number_of_points'
    ndays_leadtime = 4
    ppquantiles = np.arange(0.1, 1.0, 0.1)
    nquantiles = len(ppquantiles)

    massifs_learning = np.concatenate([np.arange(1, 24), np.arange(64, 75)])

    # @property
    # def geo(self):
    #     """'station' or 'massifs' geometry"""
    #     return self._geo
    #
    # @geo.setter
    # def geo(self, value):
    #     self._geo = value

    @abstractmethod
    def create_location_var(self, dataset, fill_value):
        """
        method to create a location variable

        :param dataset: NetCDF4 dataset
        :param fill_value: fill value for the location variable
        """
        raise NotImplementedError("Child classes should implement create_location_var method")

    def create_pp_file(self, filename):
        """
        Create a netCDF file for postprocessed variables.

        :param filename: file name with full path
        """

        dirout = os.path.dirname(filename)
        if not os.path.isdir(dirout):
            raise DirNameException(dirout)

        newdataset = netCDF4.Dataset(filename, "w", format='NETCDF4_CLASSIC')

        dims_in = self.simufiles[0].getdimvar(self.newsnow_var_name)
        rank = self.simufiles[0].getrankvar(self.newsnow_var_name)
        fill_value = self.simufiles[0].getfillvalue(self.newsnow_var_name)
        dims_out = tuple(list(dims_in) + ["decile"])

        for dim in dims_out:
            if dim == "decile":
                dimlen = self.nquantiles
            elif dim == "time":
                dimlen = None
            elif dim == self.location_dim_name:
                dimlen = self.npoints
            else:
                dimlen = self.simufiles[0].getlendim(dim)

            newdataset.createDimension(dim, dimlen)

        timevar = newdataset.createVariable("time", 'double', ('time'), fill_value=fill_value)
        timevar.units = 'hours since 2019-08-01 06:00:00'
        timevar[:] = netCDF4.date2num(self.time, timevar.units)

        self.create_location_var(newdataset, fill_value)
        zsvar = newdataset.createVariable("ZS", 'float', (self.location_dim_name),
                                          fill_value=fill_value)
        zsvar[:] = self.alti

        aspectvar = newdataset.createVariable("aspect", 'float', (self.location_dim_name),
                                              fill_value=fill_value)
        aspectvar[:] = self.aspect

        var = newdataset.createVariable("PP_" + self.newsnow_var_name, 'float', dims_out,
                                        fill_value=fill_value)

        csgmean, csgstd, csgdelta = self.get_csg_param()
        pp_forecast = self.get_quantiles_CSGD(csgmean, csgstd, csgdelta, self.ppquantiles)

        if rank == 2:
            var[:, :, :] = pp_forecast[:, :, :]
        elif rank == 3:
            var[:, :, :, :] = pp_forecast[:, :, :, :]

        newdataset.close()

    @property
    def reg_coef(self):
        """ object containing regression coefficients"""
        return self._reg_coef

    @reg_coef.setter
    def reg_coef(self, value):
        self._reg_coef = value

    @property
    def clim_par(self):
        """climatological distribution parameters"""
        return self._clim_par

    @clim_par.setter
    def clim_par(self, value):
        self._clim_par = value

    def read_emos_param(self, filename='EMOS_HN.Rdata'):
        """
        read emos parameters from Rdata object.

        :param filename: file name of .Rdata file
        """
        robj = robjects.r.load(filename)  # pylint: disable=possibly-unused-variable
        self.reg_coef = robjects.r['par.reg']
        self.clim_par = robjects.r['par.climo']

    def get_emos_param(self, massif, leadtime):
        """
        get emos parameters for a given massif and lead time

        :param massif: massif number
        :param leadtime: lead time index
        """
        indmassif = np.where(self.massifs_learning == massif)

        if len(indmassif[0]) == 1:
            return np.array(self.reg_coef)[0, indmassif[0][0], leadtime, :]
        return np.nan, np.nan, np.nan, np.nan, np.nan, np.nan

    def get_clim_param(self, massif):
        """
        get climatological distribution parameters for a given massif

        :param massif: massif number
        """
        indmassif = np.where(self.massifs_learning == massif)
        if len(indmassif[0]) == 1:
            return np.array(self.clim_par)[0, indmassif[0][0], :]
        return np.nan, np.nan, np.nan

    @abstractmethod
    def get_massifnumber(self):
        """ get array with massif numbers """
        raise NotImplementedError("child classes are supposed to implement get_massifnumber method")

    def get_csg_param(self):
        """
        get csg parameters from raw predictors
        :returns: csgmean, csgstd, csgdelta
        """

        # Read raw forecast metadata
        massif_number = self.get_massifnumber()

        ntime = len(self.time)
        list_indtime = []

        delta = self.time - self.time[0].replace(hour=6)

        for day_leadtime in range(1, self.ndays_leadtime + 1):
            list_indtime.append(np.where(
                (delta <= datetime.timedelta(days=day_leadtime)) &
                (delta > datetime.timedelta(days=day_leadtime - 1)))[0])

        list_massifs = np.unique(massif_number)

        # Extract regression parameters
        a_1, a_2, a_3, a_4, b_1, b_2 = np.empty((6, ntime, self.npoints))
        muclim, sigmaclim, deltaclim = np.empty((3, self.npoints))

        for massif in list_massifs:

            indmassif = np.where(massif_number == massif)[0]
            cst_muclim, cst_sigmaclim, cst_deltaclim = self.get_clim_param(massif)

            for leadtime in range(0, self.ndays_leadtime):
                cst_a1, cst_a2, cst_a3, cst_a4, cst_b1, cst_b2 = self.get_emos_param(massif,
                                                                                     leadtime)
                begin = list_indtime[leadtime][0]
                end = list_indtime[leadtime][-1]

                a_1[slice(begin, end + 1), indmassif] = cst_a1
                a_2[slice(begin, end + 1), indmassif] = cst_a2
                a_3[slice(begin, end + 1), indmassif] = cst_a3
                a_4[slice(begin, end + 1), indmassif] = cst_a4
                b_1[slice(begin, end + 1), indmassif] = cst_b1
                b_2[slice(begin, end + 1), indmassif] = cst_b2

            muclim[indmassif] = cst_muclim
            sigmaclim[indmassif] = cst_sigmaclim
            deltaclim[indmassif] = cst_deltaclim

        # Extract raw ensemble predictors
        ensmean = self.mean(self.newsnow_var_name) * 100.
        ensspread = self.spread(self.newsnow_var_name) * 100.
        ens_pop = self.probability(self.newsnow_var_name, seuilinf=1.E-6)

        # Compute CSGD parameters with regression laws
        csgmean = csg_mean(a_1, a_2, a_3, a_4, muclim, ensmean, ens_pop)
        csgstd = csg_std(b_1, b_2, muclim, sigmaclim, ensmean, ensspread)
        csgdelta = csg_delta(ensmean, deltaclim)

        return csgmean, csgstd, csgdelta

    @property
    def quantiles_CSGD(self):
        """postprocessed quantiles"""
        return self._quantiles_CSGD

    @quantiles_CSGD.setter
    def quantiles_CSGD(self, value):
        self._quantiles_CSGD = value

    def get_quantiles_CSGD(self, csgmean, csgstd, csgdelta, quantiles):
        """
        get quantiles of csg distribution for arrays of mean, standard deviation and delta

        :param csgmean: array of means of csg distributions
        :param csgstd: array of standard deviations of csg distributions
        :param csgdelta: array of delta parameters of csg distributions
        :param quantiles: list of quantiles
        """

        shapein = list(csgmean.shape)
        shapeout = shapein[:]
        shapeout.append(len(quantiles))
        shapeout = tuple(shapeout)

        varout = np.empty(shapeout)

        if len(shapein) == 1:
            for i in range(0, shapein[0]):
                varout[i, :] = quantiles_CSGD_1point(csgmean[i], csgstd[i], csgdelta[i], quantiles)

        elif len(shapein) == 2:
            for i in range(0, shapein[0]):
                for j in range(0, shapein[1]):
                    varout[i, j, :] = quantiles_CSGD_1point(csgmean[i, j], csgstd[i, j],
                                                            csgdelta[i, j], quantiles)

        elif len(shapein) == 3:
            for i in range(0, shapein[0]):
                for j in range(0, shapein[1]):
                    for k in range(0, shapein[2]):
                        varout[i, j, k, :] = quantiles_CSGD_1point(csgmean[i, j, k],
                                                                   csgstd[i, j, k],
                                                                   csgdelta[i, j, k], quantiles)

        self.quantiles_CSGD = varout

        return varout

    @property
    def quantiles(self):
        """ List of quantiles for all variables (list of lists)"""
        return self._quantiles

    @quantiles.setter
    def quantiles(self, value):
        self._quantiles = value

    def diags(self, list_var, list_quantiles):
        """
        calculate csg distribution quantiles for a list of variables and a list of quantiles

        :param list_var: list of variables
        :param list_quantiles: list of quantiles
        """
        for var in list_var:
            print(type(self.quantiles))
            print(var)
            self.quantiles[var] = []
            for quantile in list_quantiles:
                indquantile = quantile / 10 - 1
                self.quantiles[var].append(self.quantiles_CSGD[:, :, indquantile])


class PostprocessMassif(EnsembleOperDiagsFlatMassif, PostprocessEnsemble):
    """ Class for postprocessed data on a Massif geometry"""
    levelmin = 600
    levelmax = 3000
    list_var_map = ['PP_SD_1DY_ISBA']

    def create_location_var(self, dataset, fill_value):
        """
        create massif variable in output netCDF file

        :param dataset: dataset to create variable in
        :type dataset: netCDF4.Dataset
        :param fill_value: fill value for netCDF variable
        """
        massifvar = dataset.createVariable("massif_num", 'i4', (self.location_dim_name),
                                           fill_value=fill_value)
        massifvar[:] = self.massifdim

    def get_massifnumber(self):
        """
        get massif numbers
        """
        return self.massifvar


class PostprocessStations(EnsembleStation, PostprocessEnsemble):
    """
    Postprocess simulations at station locations
    """
    def create_location_var(self, dataset, fill_value):
        """
        create station variable in output netCDF file

        :param dataset: dataset to create variable in
        :type dataset: netCDF4.Dataset
        :param fill_value: fill value for netCDF variable
        """
        stationsvar = dataset.createVariable("station", 'i4', (self.location_dim_name),
                                             fill_value=fill_value)
        stationsvar[:] = self.get_station()

    def get_massifnumber(self):
        """
        get the massif numbers corresponding to the stations

        :return: massif_number array
        """
        station = self.simufiles[0].read_var("station")
        massif_number = map(self.InfoMassifs.massifposte, station)
        return massif_number


if __name__ == "__main__":

    C = Config()
    C.list_members = list(range(0, 35))
    os.chdir(C.diroutput)
    S2ME = S2MExtractor(C)
    SNOW_MEMBERS, SNOW_XPID = S2ME.get_snow()

    locale.setlocale(locale.LC_TIME, 'fr_FR.UTF-8')
    SUPTITLE = 'Adaptations statistiques PEARP-S2M du ' + pretty_date(S2ME.conf.rundate)

    LIST_DOMAINS = SNOW_MEMBERS.keys()

    for domain in LIST_DOMAINS:

        if domain == "postes":
            E = PostprocessStations()
        else:
            E = PostprocessMassif()

        E.read_emos_param(filename=os.path.join(SNOWTOOLS_DIR, 'DATA/EMOS_HN.Rdata'))
        E.open(SNOW_MEMBERS[domain])

        ppfile = C.diroutput + "/" + domain + "/PP_" + S2ME.conf.rundate.strftime("%Y%m%d%H") + \
            ".nc"
        E.create_pp_file(ppfile)

        if domain == "postes":
            pass
        else:
            E.diags(E.list_var_map, E.list_q)
            E.pack_maps(domain, SUPTITLE, C.diroutput_maps)

        E.close()
        del E
