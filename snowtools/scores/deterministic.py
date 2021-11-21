# -*- coding: utf-8 -*-
'''
Created on 20 mars 2018

@author: lafaysse
'''

import time

import numpy as np

from snowtools.utils.prosimu import prosimu

ESMSnowMIP_dicvarnames = dict(snowdepth="snd_auto", snowswe="snw_auto", snowdepthman="snd_man", snowsweman="snw_man", tsurf="ts", albedo="albs")
ESMSnowMIP_alternate_varnames = dict(snd_auto="snd_can_auto")

SURFEX_dicvarnames = dict(snowdepth="DSN_T_ISBA", snowswe="WSN_T_ISBA", snowdepthman="DSN_T_ISBA", snowsweman="WSN_T_ISBA", tsurf="TS_ISBA", albedo="ASN_ISBA")


class DeterminsticScores(object):
    '''
    Abstract class for deterministic scores
    '''
    _abstract = True
    startwinter = 10 
    endwinter = 6

    def nvalues(self):
        '''Number of values accounted for in scores computation'''
        return len(self.simCommon)

    def diff(self):
        '''Vector of difference between simulations and observations'''
        return self.simCommon - self.obsCommon

    def squarediff(self):
        '''Vector of square difference between simulations and observations'''
        if not hasattr(self, "diffCommon"):
            self.diffCommon = self.diff()
        return np.square(self.diffCommon)

    def rmse(self):
        '''Root mean square error'''
        if not hasattr(self, "squarediffCommon"):
            self.squarediffCommon = self.squarediff()

        return np.sqrt(np.mean(self.squarediffCommon))

    def bias(self):
        '''Relative bias'''
        if not hasattr(self, "diffCommon"):
            self.diffCommon = self.diff()

        return np.mean(self.diffCommon)

    def mae(self):
        '''Mean absolute error'''
        if not hasattr(self, "diffCommon"):
            self.diffCommon = self.diff()

        return np.mean(np.abs(self.diffCommon))

    def scores_all_values(self):
        """Pour calculer les scores en conservant les valeurs nulles"""
        sim = self.simCommon
        obs = self.obsCommon
        nvalues = len(sim)
        diff = sim - obs
        bias = np.mean(diff)
        squarediff = np.square(diff)
        rmse = np.sqrt(np.mean(squarediff))
        mean = np.mean(sim)

        return nvalues, bias, rmse, mean

    def scores_with_positive_values_only(self):
        """Pour calculer les scores sans les 0 communs aux vecteurs d'obs et de simu"""
        sim = self.simCommon[(self.simCommon!=0)&(self.obsCommon!=0)]
        obs = self.obsCommon[(self.simCommon!=0)&(self.obsCommon!=0)]
        nvalues = len(sim)
        diff = sim - obs
        bias = np.mean(diff)
        squarediff = np.square(diff)
        rmse = np.sqrt(np.mean(squarediff))
        mean = np.mean(sim)

        return nvalues, bias, rmse, mean

    @property
    def meanobs(self):
        '''Mean observation value over the common period.'''
        return np.mean(self.obsCommon)
    
    @property
    def meansim(self):
        '''Mean simulation value over the common period.'''
        return np.mean(self.simCommon)


    def read_var_ifpresent(self, dataNc, varname, convert1d=False):

        if varname not in dataNc.listvar():
            if varname in list(ESMSnowMIP_alternate_varnames.keys()):
                varname = ESMSnowMIP_alternate_varnames[varname]

        if varname in dataNc.listvar():
            if convert1d:
                array = dataNc.read1d(varname)
                if varname == "ts":
                    array = np.where(array > 273.16, np.nan, array)
                return array
            else:
                if varname == "ts":
                    delta = 273.15
                else:
                    delta = 0
                return dataNc.read(varname) + delta
        else:
            ntimeNc = dataNc.getlendim("time")
            emptyvar = np.empty(ntimeNc, "float")
            emptyvar[:] = np.nan
            return emptyvar

    def read_sim_ifpresent(self, dataNc, varname):
        return self.read_var_ifpresent(dataNc, varname, convert1d=True)


class DeterministicScores_Homogeneous(DeterminsticScores):
    def __init__(self, obs, sim):
        '''
        Constructor for observations and simulations covering the same periods
        '''
        self.obsCommon = obs
        self.simCommon = sim


class DeterministicScores_Mask(DeterminsticScores):
    def __init__(self, maskObs, maskSim, obs, sim):
        '''
        Constructor for observations and simulations covering different periods, masks provided
        '''
        self.extract_common_vectors(maskObs, maskSim, obs, sim)

    def extract_common_vectors(self, maskObs, maskSim, obs, sim):
        '''Extract common date between observations and simulations'''
        self.modelMask = maskSim
        self.obsMask = maskObs
        t1 = time.time()
        self.obsCommon = obs[self.obsMask]
        t2 = time.time()
        print('Extract obs array {0:f}'.format(t2-t1))

        self.simCommon = sim[self.modelMask]
        t3 = time.time()
        print('Extract sim array {0:f}'.format(t3-t2))

class DeterministicScores_CommonObs(DeterminsticScores):
    def __init__(self, obsCommon, maskSim, sim):
        '''
        Constructor for observations and simulations covering different periods, observations and simulation masks provided
        '''
        self.obsCommon = obsCommon
        self.extract_common_vectors(maskSim, sim)

    def extract_common_vectors(self, maskSim, sim):
        '''Extract common date between observations and simulations'''

        self.modelMask = maskSim
        self.simCommon = sim[self.modelMask]


class DeterministicScores_Heterogeneous(DeterministicScores_Mask):
    def __init__(self, timeObs, timeSim, obs, sim):
        '''
        Constructor for observations and simulations covering different periods, times provided
        '''
        self.extract_common_vectors(timeObs, timeSim, obs, sim)

    def extract_common_vectors(self, timeObs, timeSim, obs, sim):
        '''Extract common date between observations and simulations'''
        # Identify winter period
        t1 = time.time()
        winter = np.empty_like(timeObs, 'bool')
        for i, t in enumerate(timeObs):
            winter[i] = t.month >= self.startwinter or t.month <= self.endwinter

        # First reduce observation vector to available observations and winter
        indObs_ok = np.invert(np.isnan(obs)) & winter
        timeObs_ok = timeObs[indObs_ok]
        obs_ok = obs[indObs_ok]

        timeObs_unique, indObs_unique = np.unique(timeObs_ok, return_index=True)
        obs_unique = obs_ok[indObs_unique]

        indSim_ok = np.invert(np.isnan(sim))

        timeSim_ok = timeSim[indSim_ok]
        sim_ok = sim[indSim_ok]

        timeSim_unique, indSim_unique = np.unique(timeSim_ok, return_index=True)
        sim_unique = sim_ok[indSim_unique]

        maskSim = np.in1d(timeSim_unique, timeObs_unique)
        maskObs = np.in1d(timeObs_unique, timeSim_unique)
        t2 = time.time()
        print('Compute masks in {0:f}s'.format(t2-t1))

        super(DeterministicScores_Heterogeneous, self).extract_common_vectors(maskObs, maskSim, obs_unique, sim_unique)


class S2M_Score(object):
    '''
    Abstract class for Surfex scores
    '''
    _abstract = True

    def varsimname(self, varname):
        return SURFEX_dicvarnames[varname]


class S2M_DeterministicScores_Heterogeneous(DeterministicScores_Heterogeneous, S2M_Score):

    def __init__(self, profile, obsfile, varname):

        dataSim = prosimu(profile)
        timeSim = dataSim.readtime()
        varSim = self.read_sim_ifpresent(dataSim, self.varsimname(varname))
        dataSim.close()

        dataObs = prosimu(obsfile)
        timeObs = dataObs.readtime()
        varObs = self.read_var_ifpresent(dataObs, self.varobsname(varname))
        dataObs.close()
        self.timeSim = timeSim
        self.timeObs = timeObs

        super(S2M_DeterministicScores_Heterogeneous, self).__init__(timeObs, timeSim, varObs, varSim)


class S2M_DeterministicScores_CommonObs(DeterministicScores_CommonObs, S2M_Score):
    def __init__(self, profile, varname, obsCommon, maskSim):

        dataSim = prosimu(profile)
        varSim = self.read_sim_ifpresent(dataSim, self.varsimname(varname))
        dataSim.close()

        super(S2M_DeterministicScores_CommonObs, self).__init__(obsCommon, maskSim, varSim)


class S2M_DeterministicScores_Mask(DeterministicScores_Mask, S2M_Score):
    def __init__(self, profile, obsfile, varname, maskSim, maskObs):

        dataSim = prosimu(profile)
        varSim = self.read_sim_ifpresent(dataSim, self.varsimname(varname))
        dataSim.close()

        dataObs = prosimu(obsfile)
        varObs = self.read_var_ifpresent(dataObs, self.varobsname(varname))
        dataObs.close()

        super(S2M_DeterministicScores_Mask, self).__init__(maskObs, maskSim, varObs, varSim)


class S2M_DeterministicScores_Time(DeterministicScores_Heterogeneous, S2M_Score):
    def __init__(self, profile, obsfile, varname, timeSim, timeObs):

        dataSim = prosimu(profile)
        varSim = self.read_sim_ifpresent(dataSim, self.varsimname(varname))
        dataSim.close()

        dataObs = prosimu(obsfile)
        varObs = self.read_var_ifpresent(dataObs, self.varobsname(varname))
        dataObs.close()

        super(S2M_DeterministicScores_Time, self).__init__(timeObs, timeSim, varObs, varSim)


class ESMSnowMIP(object):
    def varobsname(self, varname):
        return ESMSnowMIP_dicvarnames[varname]


class ESMSnowMIP_DeterministicScores_Mask(S2M_DeterministicScores_Mask, ESMSnowMIP):
    pass


class ESMSnowMIP_DeterministicScores_Time(S2M_DeterministicScores_Time, ESMSnowMIP):
    pass


class ESMSnowMIP_DeterministicScores_Heterogeneous(S2M_DeterministicScores_Heterogeneous, ESMSnowMIP):
    pass
