# -*- coding: utf-8 -*-
"""
Created on 20 March 2018

@author: lafaysse
"""

import numpy as np
from snowtools.utils.prosimu import prosimu

ESMSnowMIP_dicvarnames = dict(snowdepth="snd_auto", snowswe="snw_auto", snowdepthman="snd_man", snowsweman="snw_man",
                              tsurf="ts", albedo="albs")
ESMSnowMIP_alternate_varnames = dict(snd_auto="snd_can_auto")

SURFEX_dicvarnames = dict(snowdepth="DSN_T_ISBA", snowswe="WSN_T_ISBA", snowdepthman="DSN_T_ISBA",
                          snowsweman="WSN_T_ISBA", tsurf="TS_ISBA", albedo="ASN_ISBA")


class EnsembleScores:
    """
    Ensemble scores

    """

    startwinter = 10
    endwinter = 6

    def __init__(self, timeObs, timeSim, obs, ensemble):
        """
        Constructor
        """
        self.obs_common = obs
        self.ens_common = ensemble

    def CRPS(self):
        """
        # CRPS is computed from
        # https://pypi.python.org/pypi/properscoring#downloads

        # for each date, sort members (increasingly)
        # one line is not anymore one single member
        """
        ens_common = np.sort(self.ens_common, axis=0)
        evenement = 0

        # compute the integral for each obs
        CRPS_vector = np.ones(len(self.obs_common))
        for obs in self.obs_common:
            # cumulated distribution function
            # Heaviside function for obs
            obs_CDF = 0
            ensemble_CDF = 0
            prec_forecast = 0
            integrale = 0
            n_m = 0

            # Number of valid members at this date
            valid = np.ma.masked_invalid(ens_common[:, evenement])

            n_valid = np.ma.count(valid)
            if n_valid > 0:
                for prevision in valid.compressed():
                    """
                    if np.isnan(prevision):
                        #If the first forecast is nan, they all are
                        # (nan at the end in the sort step)
                        # --> do not compute crps
                        if  n==0:
                            integral=np.nan
                        #otherwise
                        prevision = prec_forecast
                        break
                    """
                    # immediately after obs
                    if obs_CDF == 0 and obs < prevision:
                        integrale += (obs - prec_forecast) * (ensemble_CDF ** 2)
                        integrale += (prevision - obs) * (ensemble_CDF - 1) ** 2
                        obs_CDF = 1.
                    # otherwise
                    else:
                        integrale += ((prevision - prec_forecast) * (ensemble_CDF - obs_CDF) ** 2)
                    # add 1/Ndate to CDF ensemble
                    ensemble_CDF += 1. / float(n_valid)
                    prec_forecast = prevision
                    n_m += 1
                # if obs > all forecasts
                if obs_CDF == 0:
                    integrale += obs - prevision

                CRPS_vector[evenement] = integrale

            # if no simulations for this obs
            else:
                CRPS_vector[evenement] = np.nan
            evenement += 1

        CRPS = np.mean(np.ma.masked_invalid(CRPS_vector))
        return CRPS

    def CRPS_decomp(self):
        """
        # BC implementing the Hersbach et al. formulation for decomposition.
        # inspired on https://github.com/brankart/ensdam/blob/master/src/EnsScores/score_crps.F90
        # aggregated assuming that realizations are independant.
        """
        ens_common = np.sort(self.ens_common, axis=0)

        nbmb = np.shape(ens_common)[0]

        a_i = np.zeros((nbmb + 1,))
        b_i = np.zeros((nbmb + 1,))
        p_i = np.arange(0, nbmb + 1.) / nbmb
        oi_0 = 0.
        oi_nbmb = len(self.obs_common)
        evenement = 0
        for obs in self.obs_common:
            if not np.ma.is_masked(obs):
                # Number of valid members at this date
                valid = np.ma.masked_invalid(ens_common[:, evenement])
                ens = valid.compressed()
                n_valid = np.ma.count(valid)

                if n_valid > 0:
                    # obs smaller than all
                    if obs < ens[0]:
                        a_i[0] += 0.
                        b_i[0] += ens[0] - obs
                        oi_0 += 1.
                    # obs bigger than all
                    elif obs > ens[nbmb - 1]:
                        a_i[nbmb] += obs - ens[nbmb - 1]
                        b_i[nbmb] += 0.
                        oi_nbmb -= 1.
                    # obs within
                    for i in range(1, nbmb):
                        if obs >= ens[i]:
                            a_i[i] += ens[i] - ens[i - 1]
                            b_i[i] += 0.
                        elif ens[i] > obs >= ens[i - 1]:
                            a_i[i] += obs - ens[i - 1]
                            b_i[i] += ens[i] - obs
                        elif obs < ens[i - 1]:
                            a_i[i] += 0.
                            b_i[i] += ens[i] - ens[i - 1]
                        else:
                            raise Exception
                else:
                    raise Exception
            evenement += 1
        ai_avg = a_i / len(self.obs_common)
        bi_avg = b_i / len(self.obs_common)
        gi_avg = np.zeros(np.shape(ai_avg))
        oi_avg = np.zeros(np.shape(ai_avg))
        for i in range(1, nbmb):
            gi_avg[i] = ai_avg[i] + bi_avg[i]
            if gi_avg[i] > 0.:
                oi_avg[i] = bi_avg[i] / gi_avg[i]

        oi_avg[0] = oi_0 / len(self.obs_common)
        oi_avg[nbmb] = oi_nbmb / len(self.obs_common)
        if oi_avg[0] > 0.:
            gi_avg[0] = bi_avg[0] / oi_avg[0]
        if oi_avg[nbmb] < 1:
            gi_avg[nbmb] = ai_avg[nbmb] / (1. - oi_avg[nbmb])
        CRPS = np.nansum(ai_avg * p_i ** 2 + bi_avg * (1. - p_i)**2)
        reli = np.nansum(gi_avg * (oi_avg - p_i)**2)
        resol = np.nansum(gi_avg * oi_avg * (1. - oi_avg))

        return CRPS, reli, resol

    def meanEnsemble(self):
        """
        Calculate ensemble mean with potentially non-valid ensemble members.
        """
        return np.ma.masked_invalid(self.ens_common).mean(axis=0)

    def dispersionEnsemble(self):
        """
        spread over all dates
        """
        # NbMembres = self.ens_common.shape[0]

        mean_common = self.meanEnsemble()

        # bc 21/10/19 code optim
        disp = np.sqrt(np.mean([np.mean((np.ma.masked_invalid(m)-mean_common)**2) for m in self.ens_common]))
        rmse_mean = np.sqrt(np.mean(np.square(mean_common - self.obs_common)))

        return disp, rmse_mean, disp / rmse_mean


class ESCROC_EnsembleScores(EnsembleScores):
    """
    Ensemble scores for ESCROC ensemble

    used by vortex/src/cen/algo/scores.py
    """

    def __init__(self, profiles, obsfile, varname):
        """
        :param profiles: list of netcdf filenames for the different ensemble members
        :param obsfile: observation filename (netcdf format)
        :param varname: variable name to read
        """
        # be careful with calling the superclass constructor:
        # self.obs_common and self.ens_common are defined at the end of the self.read method
        self.read(profiles, obsfile, varname)

    def read_var_ifpresent(self, dataNc, varname, convert1d=False):
        """
        Read variable from prosimu object.
        If the variable is not in the file, return an array of np.nan.

        :param dataNc: data
        :type dataNc: prosimu
        :param varname: name of the variable to get.
        :type varname: str
        :param convert1d:
        :type convert1d: bool
        """

        if varname not in dataNc.listvar():
            if varname in list(ESMSnowMIP_alternate_varnames.keys()):
                varname = ESMSnowMIP_alternate_varnames[varname]

        if varname in dataNc.listvar():
            if convert1d:
                array = dataNc.read1d(varname)  # warning: obsolete method !
                if varname == "ts":
                    array = np.where(array > 273.16, np.nan, array)
                return array
            if varname == "ts":
                delta = 273.15
            else:
                delta = 0
            return dataNc.read(varname) + delta

        ntime_nc = dataNc.getlendim("time")
        emptyvar = np.empty(ntime_nc, "float")
        emptyvar[:] = np.nan
        return emptyvar

    def read_sim_ifpresent(self, dataNc, varname):
        """
        read simulation.

        :param dataNc: simulation data
        :type dataNc: prosimu
        :param varname: variable name to read
        :type varname: str
        """
        return self.read_var_ifpresent(dataNc, varname, convert1d=True)

    def varsimname(self, varname):
        """
        get NetCDF variable name used in the simulation file.
        """
        return SURFEX_dicvarnames[varname]

    def varobsname(self, varname):
        """
        get NetCDF variable name used in SnowMIP.
        """
        return ESMSnowMIP_dicvarnames[varname]

    def read(self, profiles, obsfile, varname):
        """
        read ensemble simulations and observations

        :param profiles: list of NetCDF file names with each of the files containing an
        ensemble member simulation.
        :param obsfile: observation file (NetCDF file name)
        :param varname: variable name
        """
        for p_i, profile in enumerate(profiles):
            print("open file " + profile)
            data_sim = prosimu(profile)
            if p_i == 0:
                time_sim = data_sim.readtime()
                ensemble = np.empty((len(profiles), len(time_sim)), "float")

            var_sim = self.read_sim_ifpresent(data_sim, self.varsimname(varname))
            ensemble[p_i, :] = var_sim
            print("close file")
            data_sim.close()

        print("open obs")
        data_obs = prosimu(obsfile)
        time_obs = data_obs.readtime()
        var_obs = self.read_var_ifpresent(data_obs, self.varobsname(varname))
        data_obs.close()
        print("close obs")
        print(var_obs.shape)

        # Extract common date between observations and simulations
        # Identify winter period
        winter = np.empty_like(time_obs, 'bool')
        for i, t_i in enumerate(time_obs):
            winter[i] = t_i.month >= self.startwinter or t_i.month <= self.endwinter

        # First reduce observation vector to available observations and winter
        ind_obs_ok = np.invert(np.isnan(var_obs)) & winter
        print("number of valid obs in winter")
        print(np.sum(ind_obs_ok))
        time_obs_ok = time_obs[ind_obs_ok]
        obs_ok = var_obs[ind_obs_ok]

        mask_sim = np.in1d(time_sim, time_obs_ok)
        mask_obs = np.in1d(time_obs_ok, time_sim)

        self.obs_common = obs_ok[mask_obs]
        self.ens_common = ensemble[:, mask_sim]
