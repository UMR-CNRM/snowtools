# -*- coding: utf-8 -*-
'''
Created on 20 mars 2018

@author: lafaysse
'''

import numpy as np
from snowtools.utils.prosimu import prosimu

ESMSnowMIP_dicvarnames = dict(snowdepth="snd_auto", snowswe="snw_auto", snowdepthman="snd_man", snowsweman="snw_man", tsurf="ts", albedo="albs")
ESMSnowMIP_alternate_varnames = dict(snd_auto="snd_can_auto")

SURFEX_dicvarnames = dict(snowdepth="DSN_T_ISBA", snowswe="WSN_T_ISBA", snowdepthman="DSN_T_ISBA", snowsweman="WSN_T_ISBA", tsurf="TS_ISBA", albedo="ASN_ISBA")


class EnsembleScores(object):
    '''
    Ensemble scores
    '''

    startwinter = 10
    endwinter = 6

    def __init__(self, timeObs, timeSim, obs, ensemble):
        '''
        Constructor
        '''
        self.obsCommon = obs
        self.ensCommon = ensemble

    def CRPS(self):
        # CRPS is computed from
        # https://pypi.python.org/pypi/properscoring#downloads

        # for each date, sort members (increasingly)
        # one line is not anymore one single member
        ensCommon = np.sort(self.ensCommon, axis = 0)
        evenement = 0
        # compute the integral for each obs
        CRPSVector = np.ones(len(self.obsCommon))
        for obs in self.obsCommon:
            # cumulated distribution function
            # Heaviside function for obs
            obsCDF = 0
            ensembleCDF = 0
            precPrevision = 0
            integrale = 0
            n = 0

            # Number of valid members at this date
            valid = np.ma.masked_invalid(ensCommon[:, evenement])

            NValid = np.ma.count(valid)
            if NValid > 0:
                for prevision in valid.compressed():
                    """
                    if np.isnan(prevision):
                        #If the first forecast is nan, they all are
                        # (nan at the end in the sort step)
                        # --> do not compute crps
                        if  n==0:
                            integral=np.nan
                        #otherwise
                        prevision = precPrevision
                        break
                    """
                    # immediately after obs
                    if obsCDF == 0 and obs < prevision:
                        integrale += (obs - precPrevision) * (ensembleCDF ** 2)
                        integrale += (prevision - obs) * (ensembleCDF - 1) ** 2
                        obsCDF = 1.
                    # otherwise
                    else:
                        integrale += ((prevision - precPrevision) * (ensembleCDF - obsCDF) ** 2)
                    # add 1/Ndate to CDF ensemble
                    ensembleCDF += 1. / float(NValid)
                    precPrevision = prevision
                    n += 1
                # if obs > all forecasts
                if obsCDF == 0:
                    integrale += obs - prevision

                CRPSVector[evenement] = integrale

            # if no simulations for this obs
            else:
                CRPSVector[evenement] = np.nan
            evenement += 1

        CRPS = np.mean(np.ma.masked_invalid(CRPSVector))
        return CRPS

    def CRPS_decomp(self):
        # BC implementing the Hersbach et al. formulation for decomposition.
        # inspired on https://github.com/brankart/ensdam/blob/master/src/EnsScores/score_crps.F90
        # aggregated assuming that realizations are independant.
        ensCommon = np.sort(self.ensCommon, axis = 0)

        nbmb = np.shape(ensCommon)[0]

        ai = np.zeros((nbmb + 1,))
        bi = np.zeros((nbmb + 1,))
        pi = np.arange(0, nbmb + 1.) / nbmb
        oi_0 = 0.
        oi_nbmb = len(self.obsCommon)
        evenement = 0
        for obs in self.obsCommon:
            if not np.ma.is_masked(obs):
                # Number of valid members at this date
                valid = np.ma.masked_invalid(ensCommon[:, evenement])
                ens = valid.compressed()
                NValid = np.ma.count(valid)

                if NValid > 0:
                    # obs smaller than all
                    if obs < ens[0]:
                        ai[0] += 0.
                        bi[0] += ens[0] - obs
                        oi_0 += 1.
                    # obs bigger than all
                    elif obs > ens[nbmb - 1]:
                        ai[nbmb] += obs - ens[nbmb - 1]
                        bi[nbmb] += 0.
                        oi_nbmb -= 1.
                    # obs within
                    for i in range(1, nbmb):
                        if obs >= ens[i]:
                            ai[i] += ens[i] - ens[i - 1]
                            bi[i] += 0.
                        elif (ens[i] > obs) and (obs >= ens[i - 1]):
                            ai[i] += obs - ens[i - 1]
                            bi[i] += ens[i] - obs
                        elif obs < ens[i - 1]:
                            ai[i] += 0.
                            bi[i] += ens[i] - ens[i - 1]
                        else:
                            raise Exception
                else:
                    raise Exception
            evenement += 1
        ai_avg = ai / len(self.obsCommon)
        bi_avg = bi / len(self.obsCommon)
        gi_avg = np.zeros(np.shape(ai_avg))
        oi_avg = np.zeros(np.shape(ai_avg))
        for i in range(1, nbmb):
            gi_avg[i] = ai_avg[i] + bi_avg[i]
            if gi_avg[i] > 0.:
                oi_avg[i] = bi_avg[i] / gi_avg[i]

        oi_avg[0] = oi_0 / len(self.obsCommon)
        oi_avg[nbmb] = oi_nbmb / len(self.obsCommon)
        if oi_avg[0] > 0.:
            gi_avg[0] = bi_avg[0] / oi_avg[0]
        if oi_avg[nbmb] < 1:
            gi_avg[nbmb] = ai_avg[nbmb] / (1. - oi_avg[nbmb])
        CRPS = np.nansum(ai_avg * pi ** 2 + bi_avg * (1. - pi)**2)
        Reli = np.nansum(gi_avg * (oi_avg - pi)**2)
        Resol = np.nansum(gi_avg * oi_avg * (1. - oi_avg))

        return (CRPS, Reli, Resol)

    def meanEnsemble(self):
            # deal with only some members valid

        return np.ma.masked_invalid(self.ensCommon).mean(axis=0)

    def dispersionEnsemble(self):
        """
        spread over all dates
        """
        NbMembres = self.ensCommon.shape[0]

        meanCommon = self.meanEnsemble()

        # bc 21/10/19 code optim
        disp = np.sqrt(np.mean([np.mean((np.ma.masked_invalid(m) - meanCommon)**2) for m in self.ensCommon]))
        rmseMean = np.sqrt(np.mean(np.square(meanCommon - self.obsCommon)))

        return disp, rmseMean, disp / rmseMean


class ESCROC_EnsembleScores(EnsembleScores):

    def __init__(self, profiles, obsfile, varname):
        self.read(profiles, obsfile, varname)

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

    def varsimname(self, varname):
        return SURFEX_dicvarnames[varname]

    def varobsname(self, varname):
        return ESMSnowMIP_dicvarnames[varname]

    def read(self, profiles, obsfile, varname):

        for p, profile in enumerate(profiles):
            print("open file " + profile)
            dataSim = prosimu(profile)
            if p == 0:
                timeSim = dataSim.readtime()
                ensemble = np.empty((len(profiles), len(timeSim)), "float")

            varSim = self.read_sim_ifpresent(dataSim, self.varsimname(varname))
            ensemble[p, :] = varSim
            print("close file")
            dataSim.close()

        print("open obs")
        dataObs = prosimu(obsfile)
        timeObs = dataObs.readtime()
        varObs = self.read_var_ifpresent(dataObs, self.varobsname(varname))
        dataObs.close()
        print("close obs")
        print(varObs.shape)

        '''Extract common date between observations and simulations'''
        # Identify winter period
        winter = np.empty_like(timeObs, 'bool')
        for i, t in enumerate(timeObs):
            winter[i] = t.month >= self.startwinter or t.month <= self.endwinter

        # First reduce observation vector to available observations and winter
        indObs_ok = np.invert(np.isnan(varObs)) & winter
        print("number of valid obs in winter")
        print(np.sum(indObs_ok))
        timeObs_ok = timeObs[indObs_ok]
        obs_ok = varObs[indObs_ok]

        maskSim = np.in1d(timeSim, timeObs_ok)
        maskObs = np.in1d(timeObs_ok, timeSim)

        self.obsCommon = obs_ok[maskObs]
        self.ensCommon = ensemble[:, maskSim]
