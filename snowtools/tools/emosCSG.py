#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Created on 25 June 2018

@author: lafaysse

Script and classes for applying Censored Shifted Gamma (CSG) EMOS to new snow forecasts (SD_1DY_ISBA).

The formulae used are those of Nousu and Evin, using a normalisation with climatological parameters.

"""

import locale
import datetime
import os

import rpy2.robjects as robjects
import numpy as np
import netCDF4
from scipy.stats import gamma

from snowtools.plots.pearps2m.postprocess import (config, Ensemble,
                                                  EnsembleOperDiagsFlatMassif, EnsembleStation)
from snowtools.utils.FileException import DirNameException
from snowtools.utils.dates import pretty_date
from snowtools.DATA import SNOWTOOLS_DIR


class postprocess_ensemble(Ensemble):
    """
    Class for ensemble forecast postprocessing with CSG EMOS.
    """

    newsnow_var_name = 'SD_1DY_ISBA'
    location_dim_name = 'Number_of_points'
    ndays_leadtime = 4
    ppquantiles = np.arange(0.1, 1.0, 0.1)
    nquantiles = len(ppquantiles)

    massifs_learning = np.concatenate([np.arange(1, 24), np.arange(64, 75)])

    def create_pp_file(self, filename):
        """
        create output NetCDF file for postprocessed forecast.

        copy the dimensions from the new-snow variable in the first simulation file adding a "decile" dimension.
        copy also the time and location (station or massif, ZS, aspect) coordinate variables.
        calculate the postprocessed forecast and write it to the file.

        :param filename: output filename

        """

        dirout = os.path.dirname(filename)
        if not os.path.isdir(dirout):
            raise DirNameException(dirout)

        newdataset = netCDF4.Dataset(filename, "w", format='NETCDF4_CLASSIC')

        dims_in = self.simufiles[0].getdimvar(self.newsnow_var_name)
        rank = self.simufiles[0].getrankvar(self.newsnow_var_name)
        fillvalue = self.simufiles[0].getfillvalue(self.newsnow_var_name)
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

        timevar = newdataset.createVariable("time", 'double', ('time'), fill_value=fillvalue)
        timevar.units = 'hours since 2019-08-01 06:00:00'
        timevar[:] = netCDF4.date2num(self.time, timevar.units)

        print(self.geo)
        if self.geo == 'massifs':
            fillvalue_massif = self.simufiles[0].getfillvalue("massif_num")
            massifvar = newdataset.createVariable("massif_num", 'i4', (self.location_dim_name),
                                                  fill_value=fillvalue_massif)
            massifvar[:] = self.get_massifdim()
        elif self.geo == 'stations':
            fillvalue_station = self.simufiles[0].getfillvalue("station")
            stationsvar = newdataset.createVariable("station", 'i4', (self.location_dim_name),
                                                    fill_value=fillvalue_station)
            stationsvar[:] = self.get_station()

        zsvar = newdataset.createVariable("ZS", 'float', (self.location_dim_name), fill_value=fillvalue)
        zsvar[:] = self.get_alti()

        aspectvar = newdataset.createVariable("aspect", 'float', (self.location_dim_name), fill_value=fillvalue)
        aspectvar[:] = self.get_aspect()

        var = newdataset.createVariable("PP_" + self.newsnow_var_name, 'float', dims_out, fill_value=fillvalue)

        csgmean, csgstd, csgdelta = self.get_csg_param()
        pp_forecast = self.quantiles_CSGD(csgmean, csgstd, csgdelta, self.ppquantiles)

        if rank == 2:
            var[:, :, :] = pp_forecast[:, :, :]
        elif rank == 3:
            var[:, :, :, :] = pp_forecast[:, :, :, :]

        newdataset.close()

    def read_emos_param(self, filename='EMOS_HN.Rdata'):
        """
        read .Rdata file with EMOS coefficients.

        sets self.reg_coef and self.clim_par

        :param filename: input file name. .Rdata file
        """
        robj = robjects.r.load(filename)  # pylint: disable=possibly-unused-variable
        self.reg_coef = robjects.r['par.reg']
        self.clim_par = robjects.r['par.climo']

    def get_emos_param(self, massif, leadtime):
        """
        get the regression coefficients from self.reg_coef for a given Massif and leadtime.

        :param massif: massif number
        :param leadtime: leadtime index
        :type leadtime: int
        :return: regression coefficients
        :rtype: array of length 6
        """
        indmassif = np.where(self.massifs_learning == massif)

        if len(indmassif[0]) == 1:
            return np.array(self.reg_coef)[0, indmassif[0][0], leadtime, :]
        else:
            return np.nan, np.nan, np.nan, np.nan, np.nan, np.nan

    def get_clim_param(self, massif):
        """
        get climatological CSG parameters from self.clim_par for a given Massif.

        :param massif: massif number
        :return: CSG parameters
        :rtype: array of length 3
        """
        indmassif = np.where(self.massifs_learning == massif)
        if len(indmassif[0]) == 1:
            return np.array(self.clim_par)[0, indmassif[0][0], :]
        else:
            return np.nan, np.nan, np.nan

    def csg_mean(self, a1, a2, a3, a4, muclim, ensmean, ensPOP):
        """
        Calculate mu of predictive censored shifted gamma distribution from regression coefficients alpha 1 to
        alpha 4, climalological mu, ensemble mean and probability of non-zero values of the ensemble.

        :param a1: regression coefficient alpha 1
        :type a1: float
        :param a2: regression coefficient alpha 2
        :type a2: float
        :param a3: regression coefficient alpha 3
        :type a3: float
        :param a4: regression coefficient alpha 4
        :type a4: float
        :param muclim: climatological mu
        :type muclim: float
        :param ensmean: ensemble mean
        :type ensmean: float
        :param ensPOP: probability of non-zero values of the ensemble [0., 1.]
        :type ensPOP: float
        :return: mu of predictive censored shifted gamma distribution
        :rtype: float
        """
        return (muclim / a1) * np.log1p(np.expm1(a1) * (a2 + a3 * ensPOP + a4 * ensmean))

    def csg_std(self, b1, b2, muclim, sigmaclim, emosmean, ensspread):
        """
        Calculate sigma of predictive censored shifted gamma distribution from regression coefficients
        beta 1 and beta 2, climatological mu and sigma, ensemble mean and ensemble spread.

        :param b1: regression coefficient beta 1
        :type b1: float
        :param b2: regression coefficient beta 2
        :type b2: float
        :param muclim: climatological mu
        :type muclim: float
        :param sigmaclim: climatological sigma
        :type sigmaclim: float
        :param emosmean: ensemble mean
        :type emosmean: float
        :param ensspread: ensemble spread
        :type ensspread: float
        :return: sigma of predictive censored shifted gamma distribution
        :rtype: float
        """
        return b1 * sigmaclim * np.sqrt(emosmean / muclim) + b2 * ensspread

    def csg_delta(self, ensmean, deltaclim):
        """
        Calculates delta of predictive censored shifted gamma distribution from
        a coefficient, the ensemble mean and the climatological delta parameter. But actually the
        coefficient is 0., which means that the method always returns deltaclim.

        :param ensmean: ensemble mean
        :type ensmean: float
        :param deltaclim: climatological delta
        :type deltaclim: float
        :return: delta of predictive censored shifted gamma distribution
        :rtype: float
        """
        return ensmean * 0. + deltaclim

    def get_csg_param(self):
        """
        get the censored shifted gamma regression coefficients and climatological
        paramters for each massif and lead time and apply them to the ensemble forecast
        and return the parameters mu, sigma and delta of the predictive censored shifted gamma distribution.

        :return: mu, sigma, delta
        """

        # Read raw forecast metadata

        if self.geo == "stations":
            station = self.simufiles[0].read_var("station")
            massif_number = map(self.InfoMassifs.massifposte, station)
        else:
            massif_number = self.get_massifdim()

        ntime = len(self.time)
        list_indtime = []

        delta = self.time - self.time[0].replace(hour=6)

        for day_leadtime in range(1, self.ndays_leadtime + 1):
            list_indtime.append(np.where(
                (delta <= datetime.timedelta(days=day_leadtime)) &
                (delta > datetime.timedelta(days=day_leadtime - 1)))[0])

        list_massifs = np.unique(massif_number)

        # Extract regression parameters
        a1, a2, a3, a4, b1, b2 = np.empty((6, ntime, self.npoints))
        muclim, sigmaclim, deltaclim = np.empty((3, self.npoints))

        for massif in list_massifs:

            indmassif = np.where(massif_number == massif)[0]

            for leadtime in range(0, self.ndays_leadtime):
                cst_a1, cst_a2, cst_a3, cst_a4, cst_b1, cst_b2 = self.get_emos_param(massif, leadtime)
                cst_muclim, cst_sigmaclim, cst_deltaclim = self.get_clim_param(massif)

                begin = list_indtime[leadtime][0]
                end = list_indtime[leadtime][-1]

                a1[slice(begin, end + 1), indmassif] = cst_a1
                a2[slice(begin, end + 1), indmassif] = cst_a2
                a3[slice(begin, end + 1), indmassif] = cst_a3
                a4[slice(begin, end + 1), indmassif] = cst_a4
                b1[slice(begin, end + 1), indmassif] = cst_b1
                b2[slice(begin, end + 1), indmassif] = cst_b2

            muclim[indmassif] = cst_muclim
            sigmaclim[indmassif] = cst_sigmaclim
            deltaclim[indmassif] = cst_deltaclim

        # Extract raw ensemble predictors
        ensmean = self.mean(self.newsnow_var_name) * 100.
        ensspread = self.spread(self.newsnow_var_name) * 100.
        ensPOP = self.probability(self.newsnow_var_name, seuilinf=1.E-6)

        # Compute CSGD parameters with regression laws
        csgmean = self.csg_mean(a1, a2, a3, a4, muclim, ensmean, ensPOP)
        csgstd = self.csg_std(b1, b2, muclim, sigmaclim, ensmean, ensspread)
        csgdelta = self.csg_delta(ensmean, deltaclim)

        return csgmean, csgstd, csgdelta

    def quantiles_CSGD_1point(self, csgmean, csgstd, csgdelta, quantiles):
        """
        get quantiles of the censored shifted gamma (CSG) distribution with given mu, sigma and delta for one point.

        :param csgmean: mu parameter of CSG
        :type csgmean: float
        :param csgstd: sigma parameter of CSG
        :type csgstd: float
        :param csgdelta: delta parameter of CSG
        :type csgdelta: float
        :param quantiles: quantiles wanted
        :type quantiles: array of floats
        :return: array of quantiles
        :rtype: array of same length as quantiles.
        """

        tmp = gamma.ppf(q=quantiles, a=(csgmean / csgstd) ** 2, scale=(csgstd ** 2) / csgmean, loc=csgdelta)
        return np.where(tmp >= 0, tmp, 0)

    def quantiles_CSGD(self, csgmean, csgstd, csgdelta, quantiles):
        """
        get quantiles of the censored shifted gamma (CSG) distribution with given mu, sigma and delta arrays.

        :param csgmean: array of mu parameter of CSG
        :param csgstd: array of sigma parameter of CSG
        :param csgdelta: array of delta parameter of CSG
        :param quantiles: quantiles wanted
        :return: array of quantiles. If the rank of the arrays of parameters is N, the output array has rank N+1.
            The additional dimension corresponds to the quantiles and has the length of the quantiles array.
        """

        shapein = list(csgmean.shape)
        shapeout = shapein[:]
        shapeout.append(len(quantiles))
        shapeout = tuple(shapeout)

        varout = np.empty(shapeout)

        if len(shapein) == 1:
            for i in range(0, shapein[0]):
                varout[i, :] = self.quantiles_CSGD_1point(csgmean[i], csgstd[i], csgdelta[i], quantiles)

        elif len(shapein) == 2:
            for i in range(0, shapein[0]):
                for j in range(0, shapein[1]):
                    varout[i, j, :] = self.quantiles_CSGD_1point(csgmean[i, j], csgstd[i, j], csgdelta[i, j], quantiles)

        elif len(shapein) == 3:
            for i in range(0, shapein[0]):
                for j in range(0, shapein[1]):
                    for k in range(0, shapein[2]):
                        varout[i, j, k, :] = self.quantiles_CSGD_1point(csgmean[i, j, k], csgstd[i, j, k],
                                                                        csgdelta[i, j, k], quantiles)

        self.quantiles_CSGD = varout

        return varout

    def diags(self, list_var, list_quantiles):
        for var in list_var:
            print(type(self.quantiles))
            print(var)
            self.quantiles[var] = []
            for quantile in list_quantiles:
                indquantile = quantile / 10 - 1
                self.quantiles[var].append(self.quantiles_CSGD[:, :, indquantile])


class postprocess_massif(postprocess_ensemble, EnsembleOperDiagsFlatMassif):
    """
    Class for postprocessing ensemble forecasts for Massifs with CSG EMOS.
    """

    levelmin = 600
    levelmax = 3000
    list_var_map = ['PP_SD_1DY_ISBA']


class postprocess_stations(postprocess_ensemble, EnsembleStation):
    """
    Class for postprocessing ensemble forecasts at station locations with CSG EMOS
    """

    pass


if __name__ == "__main__":

    c = config()
    # Should not import that above to avoid problems when importing the module from vortex
    from snowtools.tasks.oper.get_oper_files import FutureS2MExtractor
    os.chdir(c.diroutput)
    S2ME = FutureS2MExtractor(c)
    snow_members = S2ME.get_snow()

    locale.setlocale(locale.LC_TIME, 'fr_FR.UTF-8')
    suptitle = u'Adaptations statistiques PEARP-S2M du ' + pretty_date(S2ME.conf.rundate).decode('utf-8')

    list_domains = snow_members.keys()

    for domain in list_domains:

        if domain == "postes":
            E = postprocess_stations()
        else:
            E = postprocess_massif()

        E.read_emos_param(filename=os.path.join(SNOWTOOLS_DIR, 'DATA/EMOS_HN.Rdata'))
        E.open(snow_members[domain])

        ppfile = c.diroutput + "/" + domain + "/PP_" + S2ME.conf.rundate.strftime("%Y%m%d%H") + ".nc"
        E.create_pp_file(ppfile)

        if domain == "postes":
            pass
        else:
            E.diags(E.list_var_map, E.list_q)
            E.pack_maps(domain, suptitle, c.diroutput_maps)

        E.close()
        del E
