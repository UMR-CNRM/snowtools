# -*- coding: utf-8 -*-
"""
Created on 21 March 2018

@author: lafaysse
"""

import numpy as np
import netCDF4
import xarray as xr

from snowtools.scores.deterministic import ESMSnowMIP_DeterministicScores_Heterogeneous, \
        ESMSnowMIP_DeterministicScores_Time, S2M_DeterministicScores_CommonObs


class ESCROC_list_scores(object):
    """
    Class for all deterministic scores of a given ESM-SnowMIP simulation member
    """

    def __init__(self):
        '''
        Constructor
        '''
        self.timeread = False
        self.obsread = {}
        self.masksim = {}
        self.simread = {}
        self.lastprofileread = None

    def read_data(self, profile, obsfile, varname):
        """

        :param profile:
        :param obsfile:
        :param varname:
        :return:
        """

#         if profile is not self.lastprofileread:
#             self.simread = {}
#             self.lastprofileread = profile
#
#         if varname in self.obsread.keys():
#             if varname not in self.simread.keys():
#                 # Do not work because some simulated variables are not defined at the same time
#                 timeSim = self.scores.timeSim
#                 timeObs = self.scores.timeObs
#                 self.scores = S2M_DeterministicScores_CommonObs(profile, varname,
        #                 self.obsread[varname], self.masksim[varname])
#                 self.scores.timeSim = timeSim
#                 self.scores.timeObs = timeObs
#                 self.simread[varname] = self.scores.simCommon
#             else:
#                 pass
#         else:
# Do not work because different observations have different availabilities
#             if self.timeread:
#                 timeSim = self.scores.timeSim
#                 timeObs = self.scores.timeObs
#                 self.scores = ESMSnowMIP_DeterministicScores_Time(profile, obsfile, varname,
        #                 self.scores.timeSim, self.scores.timeObs)
#                 self.scores.timeSim = timeSim
#                 self.scores.timeObs = timeObs
#             else:
        self.scores = ESMSnowMIP_DeterministicScores_Heterogeneous(profile, obsfile, varname)
#             self.timeread = True
#
#             self.obsread[varname] = self.scores.obsCommon
#             self.masksim[varname] = self.scores.modelMask

    def compute_scores_1member(self, profile, obsfile, list_scores, list_var):
        """

        :param profile:
        :param obsfile:
        :param list_scores:
        :param list_var:
        :return:
        """

        tab_scores = np.empty((len(list_scores), len(list_var)), float)

        for v, varname in enumerate(list_var):
            print("before read")
            self.read_data(profile, obsfile, varname)
            print("after read")
            print(list_scores)
            for s, score in enumerate(list_scores):
                print("compute score" + score)
                if score == "bias":
                    print("compute bias")
                    tab_scores[s, v] = self.scores.bias
                elif score == "rmse":
                    print("compute rmse")
                    tab_scores[s, v] = self.scores.rmse
                elif score == "mae":
                    tab_scores[s, v] = self.scores.mae

        return tab_scores

    def compute_scores_allmembers(self, list_pro, obsfile, list_scores, list_var):
        """
        compute scores for every ensemble member
        :param list_pro: list of simulation files
        :param obsfile: observation file
        :param list_scores: list of scores
        :param list_var: list of variables
        :return: array of scores dimensions : (score, member, variable)
        :rtype: numpy array
        """

        tab_scores = np.empty((len(list_scores), len(list_pro), len(list_var)), float)

        for p, profile in enumerate(list_pro):
            tab_scores[:, p, :] = self.compute_scores_1member(profile, obsfile, list_scores, list_var)

        return tab_scores


class scores_file(netCDF4.Dataset):
    """
    class for writing scores with dimensions (member, variable, station)
    """
    def __init__(self, *args, **kwargs):
        """
        open a netcdf dataset and create dimensions.
        :param args: passed to netCDF4.Dataset
        :param kwargs:  passed to netCDF4.Dataset
        """
        super(scores_file, self).__init__(*args, **kwargs)
        self.cree_dims()

    def cree_dims(self):
        self.createDimension("member", None)
        self.createDimension("variable", None)
        self.createDimension("stat", None)

    def write(self, scorename, scoretab):
        self.createVariable(scorename, 'f8', ('member', 'variable', 'stat'))
        # self.createVariable(scorename, 'f8', ('member', 'variable'))
        print("write the following table")
        print(scoretab[:, :, :])
        self.variables[scorename][:, :, :] = scoretab[:, :, :]
        # print(scoretab)
        # self.variables[scorename][:, :] = scoretab


class ensemble_scores_file(netCDF4.Dataset):

    def __init__(self, *args, **kwargs):
        super(ensemble_scores_file, self).__init__(*args, **kwargs)
        self.cree_dims()

    def cree_dims(self):
        self.createDimension("iteration", None)
        self.createDimension("members", None)

    def write_members(self, members):
        self.createVariable("members", 'int', ('iteration', 'members'))
        self.variables["members"][:, :] = members

    def write(self, scorename, scoretab):
        self.createVariable(scorename, 'f8', 'iteration')
        self.variables[scorename][:] = scoretab[:]


class SpatialScoreFile(xr.Dataset):
    """
    Class for writing spatial scores.
    """

    def __init__(self, list_of_experiments, list_of_kernels, list_of_thresholds, list_of_threshold_increments):
        """

        :param list_of_experiments:
        :param list_of_kernels:
        :param list_of_thresholds:
        :param list_of_threshold_increments:
        """
        super().__init__()
        self.coords['experiment'] = list_of_experiments
        self.coords['kernel'] = list_of_kernels
        self.coords['threshold'] = list_of_thresholds
        self['threshold_increment'] = xr.DataArray(list_of_threshold_increments, dims='threshold')
        self['POD'] = xr.DataArray(np.empty((len(list_of_experiments), len(list_of_kernels), len(list_of_thresholds))),
                                   dims=('experiment', 'kernel', 'threshold'),
                                   attrs={'long_name': 'probability of detection'})
        self['POD'].data.fill(np.nan)
        self['FAR'] = xr.DataArray(np.empty((len(list_of_experiments), len(list_of_kernels), len(list_of_thresholds))),
                                   dims=('experiment', 'kernel', 'threshold'),
                                   attrs={'long_name': 'false alarm ratio'})
        self['FAR'].data.fill(np.nan)
        self['CSI'] = xr.DataArray(np.empty((len(list_of_experiments), len(list_of_kernels), len(list_of_thresholds))),
                                   dims=('experiment', 'kernel', 'threshold'),
                                   attrs={'long_name': 'critical success index'})
        self['CSI'].data.fill(np.nan)
        self['ETS'] = xr.DataArray(np.empty((len(list_of_experiments), len(list_of_kernels), len(list_of_thresholds))),
                                   dims=('experiment', 'kernel', 'threshold'),
                                   attrs={'long_name': 'equitable threat score'})
        self['ETS'].data.fill(np.nan)
        self['HK'] = xr.DataArray(np.empty((len(list_of_experiments), len(list_of_kernels), len(list_of_thresholds))),
                                  dims=('experiment', 'kernel', 'threshold'),
                                  attrs={'long_name': 'Hanssen and Kuippers skill score or True Skill Statistic'})
        self['HK'].data.fill(np.nan)
        self['ACC'] = xr.DataArray(np.empty((len(list_of_experiments), len(list_of_kernels), len(list_of_thresholds))),
                                   dims=('experiment', 'kernel', 'threshold'),
                                   attrs={'long_name': 'Accuracy'})
        self['ACC'].data.fill(np.nan)
        self['PAG'] = xr.DataArray(np.empty((len(list_of_experiments), len(list_of_kernels), len(list_of_thresholds))),
                                   dims=('experiment', 'kernel', 'threshold'),
                                   attrs={'long_name': 'Post Agreement'})
        self['PAG'].data.fill(np.nan)

    # def to_netcdf(self, **kwargs):
    #     """
    #     write dataset to netcdf file
    #     :param filename: filename (with path)
    #     :type filename: pathlike
    #     """
    #     super().to_netcdf(**kwargs)


