# -*- coding: utf-8 -*-
"""
Created on 20 March 2018

@author: lafaysse
"""

import time

import numpy as np

from snowtools.utils.prosimu import prosimu

ESMSnowMIP_dicvarnames = dict(snowdepth="snd_auto", snowswe="snw_auto", snowdepthman="snd_man",
                              snowsweman="snw_man", tsurf="ts", albedo="albs")
"""
dict mapping common variable names to ESMSnowMIP variable names
"""
ESMSnowMIP_alternate_varnames = dict(snd_auto="snd_can_auto")
"""
dict mapping ESMSnowMIP_alternate_varnames
"""

SURFEX_dicvarnames = dict(snowdepth="DSN_T_ISBA", snowswe="WSN_T_ISBA", snowdepthman="DSN_T_ISBA",
                          snowsweman="WSN_T_ISBA", tsurf="TS_ISBA", albedo="ASN_ISBA")
"""
dict mapping common variable names to surfex variable names
"""


class DeterminsticScores(object):
    """
    Abstract class for deterministic scores
    """
    _abstract = True
    startwinter = 10 
    endwinter = 6

    def __init__(self):
        self.obsCommon = None
        self.simCommon = None

    def nvalues(self):
        """Number of values accounted for in scores computation"""
        return len(self.simCommon)

    def diff(self):
        """Vector of difference between simulations and observations"""
        return self.simCommon - self.obsCommon

    @property
    def diff_common(self):
        return self.diff()

    def squarediff(self):
        """Vector of square difference between simulations and observations"""
        return np.square(self.diff_common)

    @property
    def squarediff_common(self):
        return self.squarediff()

    @property
    def rmse(self):
        """Root mean square error"""
        return np.sqrt(np.mean(self.squarediff_common))

    @property
    def bias(self):
        """Relative bias"""
        return np.mean(self.diff_common)

    @property
    def mae(self):
        """Mean absolute error"""
        return np.mean(np.abs(self.diff_common))

    def scores_all_values(self):
        """
        calculate scores (bias and RMSE) including cases of 0 values (no snow for example)
        :return: number of values, bias, RMSE, mean value of simulations
        """
        """Pour calculer les scores en conservant les valeurs nulles"""
        nvalues = len(self.simCommon)
        return nvalues, self.bias, self.rmse, self.meansim

    def scores_with_positive_values_only(self):
        """
        calculate scores (bias and RMSE) excluding cases where both the observation and the simulation
        have a value of 0
        :return: number of values, bias, RMSE, mean value of simulations
        """
        sim = self.simCommon[(self.simCommon != 0) & (self.obsCommon != 0)]
        obs = self.obsCommon[(self.simCommon != 0) & (self.obsCommon != 0)]
        nvalues = len(sim)
        diff = sim - obs
        bias = np.mean(diff)
        squarediff = np.square(diff)
        rmse = np.sqrt(np.mean(squarediff))
        mean = np.mean(sim)

        return nvalues, bias, rmse, mean

    @property
    def meanobs(self):
        """Mean observation value over the common period."""
        return np.mean(self.obsCommon)
    
    @property
    def meansim(self):
        """Mean simulation value over the common period."""
        return np.mean(self.simCommon)

    def read_var_ifpresent(self, data_nc, varname, convert1d=False):
        """
        read a given variable from a netcdf file if it is present in the file.
        If the variable is not present under the given variable name, check if it
        is present under an alternate name from ESMSnowMIP.

        :param data_nc: dataset to read in
        :type data_nc: prosimu
        :param varname: variable name
        :type varname: str
        :param convert1d:
        :return: Numpy data array containing the data of the selected variable,
         or an array filled with `np.nan` if the requested variable was not present in the file.
        :rtype: numpy array
        """

        if varname not in data_nc.listvar():
            if varname in list(ESMSnowMIP_alternate_varnames.keys()):
                varname = ESMSnowMIP_alternate_varnames[varname]

        if varname in data_nc.listvar():
            if convert1d:
                array = data_nc.read1d(varname)
                if varname == "ts":
                    array = np.where(array > 273.16, np.nan, array)
                return array
            else:
                if varname == "ts":
                    delta = 273.15
                else:
                    delta = 0
                return data_nc.read(varname) + delta
        else:
            ntime_nc = data_nc.getlendim("time")
            emptyvar = np.empty(ntime_nc, "float")
            emptyvar[:] = np.nan
            return emptyvar

    def read_sim_ifpresent(self, data_nc, varname):
        """
        read a variable from the simulation file if the variable is present in there.
        :param data_nc: open netcdf dataset
        :type data_nc: prosimu
        :param varname: variable name of the variable to be read
        :type varname: str
        :return: array with requested data
        """

        return self.read_var_ifpresent(data_nc, varname, convert1d=True)


class DeterministicScores_Homogeneous(DeterminsticScores):
    def __init__(self, obs, sim):
        """
        Constructor for observations and simulations covering the same periods
        """
        super().__init__()
        self.obsCommon = obs
        self.simCommon = sim


class DeterministicScores_Mask(DeterminsticScores):
    def __init__(self, mask_obs, mask_sim, obs, sim):
        """
        Constructor for observations and simulations covering different periods, masks provided
        :param mask_obs: mask array for observation
        :type mask_obs: ndarray of bool
        :param mask_sim: mask array for simulation
        :type mask_sim: ndarray of bool
        :param obs: array of observations
        :param sim: array of simulations
        """
        super().__init__()
        self.modelMask = mask_sim
        self.obsMask = mask_obs
        t1 = time.time()
        self.obsCommon = obs[self.obsMask]
        t2 = time.time()
        print('Extract obs array {0:f}'.format(t2-t1))

        self.simCommon = sim[self.modelMask]
        t3 = time.time()
        print('Extract sim array {0:f}'.format(t3-t2))
        # self.extract_common_vectors(mask_obs, mask_sim, obs, sim)

    def extract_common_vectors(self, mask_obs, mask_sim, obs, sim):
        """Extract common date between observations and simulations"""
        pass


class DeterministicScores_CommonObs(DeterminsticScores):
    def __init__(self, obs_common, mask_sim, sim):
        """
        Constructor for observations and simulations covering different periods,
         observations and simulation masks provided

        :param obs_common: observations
        :param mask_sim: mask for simulation data
        :param sim: simulations data
        """

        super().__init__()
        self.obsCommon = obs_common
        self.modelMask = mask_sim
        self.simCommon = sim[self.modelMask]
        # self.extract_common_vectors(maskSim, sim)

    # def extract_common_vectors(self, maskSim, sim):
    #     '''Extract common date between observations and simulations'''


class DeterministicScores_Heterogeneous(DeterministicScores_Mask):
    def __init__(self, time_obs, time_sim, obs, sim):
        """
        Constructor for observations and simulations covering different periods, times provided

        :param time_obs: array of observation times
        :param time_sim: array of simulation times
        :param obs: array of observation values
        :param sim: array of simulation values
        """
        mask_obs, mask_sim, obs_unique, sim_unique = self.extract_common_vectors(time_obs, time_sim, obs, sim)
        super().__init__(mask_obs, mask_sim, obs_unique, sim_unique)

    def extract_common_vectors(self, time_obs, time_sim, obs, sim):
        """
        Extract common date between observations and simulations selecting only winter periods.
        :param time_obs: array of observation times
        :param time_sim: array of simulation times
        :param obs: array of observation values
        :param sim: array of simulation values
        :return:
        """
        # Identify winter period
        t1 = time.time()
        winter = np.empty_like(time_obs, 'bool')
        for i, t in enumerate(time_obs):
            winter[i] = t.month >= self.startwinter or t.month <= self.endwinter

        # First reduce observation vector to available observations and winter
        ind_obs_ok = np.invert(np.isnan(obs)) & winter
        time_obs_ok = time_obs[ind_obs_ok]
        obs_ok = obs[ind_obs_ok]

        time_obs_unique, ind_obs_unique = np.unique(time_obs_ok, return_index=True)
        obs_unique = obs_ok[ind_obs_unique]

        ind_sim_ok = np.invert(np.isnan(sim))

        time_sim_ok = time_sim[ind_sim_ok]
        sim_ok = sim[ind_sim_ok]

        time_sim_unique, ind_sim_unique = np.unique(time_sim_ok, return_index=True)
        sim_unique = sim_ok[ind_sim_unique]

        mask_sim = np.in1d(time_sim_unique, time_obs_unique)
        mask_obs = np.in1d(time_obs_unique, time_sim_unique)
        t2 = time.time()
        print('Compute masks in {0:f}s'.format(t2-t1))

        return mask_obs, mask_sim, obs_unique, sim_unique


class S2M_Score(object):
    """
    Abstract class for Surfex scores
    """
    _abstract = True

    def varsimname(self, varname):
        """
        get surfex variable name given a variable name from `SURFEX_dicvarnames.keys()`
        :param varname: variable name
        :type varname: str
        :return: surfex variable name
        :rtype: str
        """
        return SURFEX_dicvarnames[varname]

    def varobsname(self, varname):
        """
        get ESMSnowMIP variable name given a variable name from `ESMSnowMIP_dicvarnames.keys()`
        :param varname: variable name
        :type varname: str
        :return: ESMSnowMIP variable name
        :rtype: str
        """
        return ESMSnowMIP_dicvarnames[varname]


class S2M_DeterministicScores_Heterogeneous(DeterministicScores_Heterogeneous, S2M_Score):

    def __init__(self, profile, obsfile, varname):
        """

        :param profile: surfex output file name containing the simulations
        :param obsfile: netcdf file name containing the observations (will be read by prosimu)
        :param varname: name of the variable to be analysed, one of `SURFEX_dicvarnames.keys()`
        """
        print(varname)
        data_sim = prosimu(profile)
        time_sim = data_sim.readtime()
        var_sim = self.read_sim_ifpresent(data_sim, self.varsimname(varname))
        data_sim.close()

        data_obs = prosimu(obsfile)
        time_obs = data_obs.readtime()
        var_obs = self.read_var_ifpresent(data_obs, self.varobsname(varname))
        data_obs.close()
        self.timeSim = time_sim
        self.timeObs = time_obs

        super(S2M_DeterministicScores_Heterogeneous, self).__init__(time_obs, time_sim, var_obs, var_sim)


class S2M_DeterministicScores_CommonObs(DeterministicScores_CommonObs, S2M_Score):
    def __init__(self, profile, varname, obs_common, mask_sim):
        """

        :param profile: surfex output file name containing the simulations
        :param varname: name of the variable to be analysed, one of `SURFEX_dicvarnames.keys()`
        :param obs_common: observations
        :param mask_sim: simulation mask
        """

        data_sim = prosimu(profile)
        var_sim = self.read_sim_ifpresent(data_sim, self.varsimname(varname))
        data_sim.close()

        super(S2M_DeterministicScores_CommonObs, self).__init__(obs_common, mask_sim, var_sim)


class S2M_DeterministicScores_Mask(DeterministicScores_Mask, S2M_Score):
    def __init__(self, profile, obsfile, varname, mask_sim, mask_obs):
        """

        :param profile: surfex output file name containing the simulations
        :param obsfile: netcdf file name containing the observations (will be read by prosimu)
        :param varname: name of the variable to be analysed, one of `SURFEX_dicvarnames.keys()`
        :param mask_sim: simulation mask
        :param mask_obs: observation mask
        """

        data_sim = prosimu(profile)
        var_sim = self.read_sim_ifpresent(data_sim, self.varsimname(varname))
        data_sim.close()

        data_obs = prosimu(obsfile)
        var_obs = self.read_var_ifpresent(data_obs, self.varobsname(varname))
        data_obs.close()

        super(S2M_DeterministicScores_Mask, self).__init__(mask_obs, mask_sim, var_obs, var_sim)


class S2M_DeterministicScores_Time(DeterministicScores_Heterogeneous, S2M_Score):
    def __init__(self, profile, obsfile, varname, time_sim, time_obs):
        """

        :param profile: surfex output file name containing the simulations
        :param obsfile: netcdf file name containing the observations (will be read by prosimu)
        :param varname: name of the variable to be analysed, one of `SURFEX_dicvarnames.keys()`
        :param time_sim: simulation times
        :param time_obs: observation times
        """

        data_sim = prosimu(profile)
        var_sim = self.read_sim_ifpresent(data_sim, self.varsimname(varname))
        data_sim.close()

        data_obs = prosimu(obsfile)
        var_obs = self.read_var_ifpresent(data_obs, self.varobsname(varname))
        data_obs.close()

        super(S2M_DeterministicScores_Time, self).__init__(time_obs, time_sim, var_obs, var_sim)


class ESMSnowMIP(S2M_Score):
    pass

    # def varobsname(self, varname):
    #     return ESMSnowMIP_dicvarnames[varname]


class ESMSnowMIP_DeterministicScores_Mask(S2M_DeterministicScores_Mask, ESMSnowMIP):
    pass


class ESMSnowMIP_DeterministicScores_Time(S2M_DeterministicScores_Time, ESMSnowMIP):
    pass


class ESMSnowMIP_DeterministicScores_Heterogeneous(S2M_DeterministicScores_Heterogeneous, ESMSnowMIP):
    pass
