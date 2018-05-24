'''
Created on 20 mars 2018

@author: lafaysse
'''

import numpy as np
from utils.prosimu import prosimu

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

    def diff(self):
        '''Vector of difference between simulations and observations'''
        print "sim"
        print self.simCommon.shape
        print self.simCommon
        print "obs"
        print self.obsCommon.shape
        print self.obsCommon
        print np.isnan(self.obsCommon)
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

    def read_var_ifpresent(self, dataNc, varname, convert1d=False):
        
        if varname not in dataNc.listvar():
            if varname in ESMSnowMIP_alternate_varnames.keys():
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

        self.obsCommon = obs[self.obsMask]
        self.simCommon = sim[self.modelMask]


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
        print "extract common vectors"
        self.extract_common_vectors(timeObs, timeSim, obs, sim)
        print "fin extract common vectors"

    def extract_common_vectors(self, timeObs, timeSim, obs, sim):
        '''Extract common date between observations and simulations'''
        # Identify winter period
        winter = np.empty_like(timeObs, 'bool')
        for i, t in enumerate(timeObs):
            winter[i] = t.month >= self.startwinter or t.month <= self.endwinter

        # First reduce observation vector to available observations and winter
        print "number of obs"
        print len(timeObs)
        print "number of obs in winter"
        print np.sum(winter)
        print "number of valid obs"
        print type(obs)
        print obs[:]
        print type(obs[0])
        print np.isnan(obs)
        print type(np.isnan(obs))
        print np.sum(np.invert(np.isnan(obs)))
        indObs_ok = np.invert(np.isnan(obs)) & winter
        print "number of valid obs in winter"
        print np.sum(indObs_ok)
        timeObs_ok = timeObs[indObs_ok]
        obs_ok = obs[indObs_ok]

        indSim_ok = np.invert(np.isnan(sim))
        timeSim_ok = timeSim[indSim_ok]
        sim_ok = sim[indSim_ok]

        maskSim = np.in1d(timeSim_ok, timeObs_ok)
        maskObs = np.in1d(timeObs_ok, timeSim_ok)

        super(DeterministicScores_Heterogeneous, self).extract_common_vectors(maskObs, maskSim, obs_ok, sim_ok)


class S2M_Score(object):
    '''
    Abstract class for Surfex scores
    '''
    _abstract = True

    def varsimname(self, varname):
        return SURFEX_dicvarnames[varname]


class S2M_DeterministicScores_Heterogeneous(DeterministicScores_Heterogeneous, S2M_Score):

    def __init__(self, profile, obsfile, varname):

        print "open file"
        print profile
        print type(profile)
        dataSim = prosimu(profile)
        print "read time"
        timeSim = dataSim.readtime()
        print "read var"
        varSim = self.read_sim_ifpresent(dataSim, self.varsimname(varname))
        print "close file"
        dataSim.close()

        print "open obs"
        dataObs = prosimu(obsfile)
        timeObs = dataObs.readtime()
        varObs = self.read_var_ifpresent(dataObs, self.varobsname(varname))
        dataObs.close()
        print "close obs"
        print varObs.shape
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

    