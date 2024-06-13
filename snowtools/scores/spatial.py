# -*- coding: utf-8 -*-
"""
Created on 23 April 2024

@author: radanovics

based on code from Ange Haddjeri thesis.
"""
import numpy as np
import xarray as xr
from scipy.signal import convolve2d
from abc import ABC, abstractmethod
from snowtools.scores.list_scores import SpatialScoreFile


def mincoverage_small(fc, obs, kl, threshold_lower, threshold_increment, perzone=None):
    """
    Calculate POD, FAR, TS, ETS, HK between two fields using a given neighborhood size kl, and
    a given threshold and threshold increment to define an event.

    :param fc: forecast field
    :type fc: np.array (2d)
    :param obs: observation field
    :type obs: np.array (2d)
    :param kl: neighbourhood (kernel) size in number of pixels in each direction ex. : 3 for a 3x3 pixel kernel
        impair number
    :type kl: int
    :param threshold_lower: lower bound of event interval
    :param threshold_increment: size of event interval
    :param perzone: minimum fraction of values needed to be present in the neighborhood in order to take
        the pixel into account for score calculation. Default None will be set to 1 / (kl * kl)
    :type perzone: float
    :return: POD, FAR, TS, ETS, HK
    """
    if perzone is None:
        perzone = 1 / (kl * kl)

    kernel = np.ones((kl, kl))
    # number of values other than nan inside the kernel
    knnan_fc = convolve2d(np.invert(np.isnan(fc)).astype('float'), kernel, mode='same',
                         boundary='fill')  # nombre de nonnan dans les fenetres
    knnan_obs = convolve2d(np.invert(np.isnan(obs)).astype('float'), kernel, mode='same', boundary='fill')
    knnan_fc[knnan_fc == 0] = np.nan  # si fenetre full nan => on met un nan a cet endroid
    knnan_obs[knnan_obs == 0] = np.nan

    forcast = convolve2d(xr.where(((fc.fillna(0) >= threshold_lower) &
                                   (fc.fillna(0) <= threshold_lower + threshold_increment)), 1, 0).to_numpy(),
                         kernel, mode='same', boundary='fill') / knnan_fc  # simu
    observation = convolve2d(xr.where(((obs.fillna(0) >= threshold_lower) &
                                       (obs.fillna(0) <= threshold_lower + threshold_increment)), 1, 0).to_numpy(),
                             kernel, mode='same', boundary='fill') / knnan_obs  # obs

    if np.size(forcast[~np.isnan(forcast)]) == np.size(observation[~np.isnan(observation)]):
        N = np.size(observation[~np.isnan(observation)])
    else:
        raise ValueError('error nan mask not the same btwn forecast and obs')

    bin_o = np.where(np.nan_to_num(observation) >= perzone, 1, 0)
    bin_o_nan = np.where(np.isnan(observation), np.nan, bin_o)

    bin_m = np.where(np.nan_to_num(forcast) >= perzone, 1, 0)
    bin_m_nan = np.where(np.isnan(forcast), np.nan, bin_m)

    hit = np.nansum(bin_m_nan * bin_o_nan)
    corej = np.nansum((1 - bin_m_nan) * (1 - bin_o_nan))
    fa = np.nansum(np.where((bin_m_nan - bin_o_nan) == 1, 1, 0))
    mis = np.nansum(np.where((bin_m_nan - bin_o_nan) == -1, 1, 0))
    hitrand = (np.nansum(bin_m_nan) * np.nansum(bin_o_nan)) / np.sum(~np.isnan(bin_m_nan))

    POD = hit / (hit + mis)
    FAR = fa / (hit + fa)
    TS = hit/(hit+mis+fa)
    ETS = (hit-hitrand)/(hit+mis+fa-hitrand)
    HK = hit-fa

    return POD, FAR, TS, ETS, HK


class SpatialScores(ABC):
    def __init__(self, fc_filenames, list_of_experiments, obs_filename, list_of_kernels, list_of_thresholds,
                 list_of_threshold_increments, per_zone=None, score_file=True, score_file_name="spatial_scores.nc",
                 perf_plot=True, perf_plot_file="perfdiag.png"):
        self.fc_data = self.get_fc_data(fc_filenames, list_of_experiments)
        self.obs_data = self.get_obs_data(obs_filename)
        self.score_ds = SpatialScoreFile(list_of_experiments, list_of_kernels, list_of_thresholds,
                                         list_of_threshold_increments)
        self.experiments = list_of_experiments
        self.kernels = list_of_kernels
        self.thresholds = list_of_thresholds
        self.threshold_incs = list_of_threshold_increments
        self.score_file = score_file
        self.score_file_name = score_file_name
        self.perf_plot = perf_plot
        self.perf_plot_file = perf_plot_file
        self.per_zone = per_zone

    @abstractmethod
    def get_fc_data(self, filenames, list_of_experiments):
        """
        read forecast data

        :param list_of_experiments: list of experiment labels
        :param filenames: list of forecast file names (paths)
        :return: dict with experiment labels as keys and np.arrays of forecast fields as values
        """
        pass

    # def maskgf(pro, method='nearest'):
    #     masque = xr.open_dataset('/home/haddjeria/masque_glacier2017_foret_ville_riviere.nc').Band1.interp_like(pro,
    #                                                                                                             method=method)
    #     return pro.where(masque == 0)
    # simu = maskgf(Sn.DSN_T_ISBA)
    # obs = maskgf(Q.DSN_T_ISBA)
    # simu = maskgf(Sn_30.DSN_T_ISBA)
    # obs = maskgf(Q.DSN_T_ISBA)
    # simu = maskgf(S.DSN_T_ISBA)
    # obs = maskgf(Q.DSN_T_ISBA)
    # simu = maskgf(S_30.DSN_T_ISBA)
    # obs = maskgf(Q.DSN_T_ISBA)
    #
    # simu = maskgf(an.DSN_T_ISBA)
    # obs = maskgf(Q.DSN_T_ISBA)
    # simu = maskgf(a.DSN_T_ISBA)
    # obs = maskgf(Q.DSN_T_ISBA)
    #
    # simu = maskgf(An.DSN_T_ISBA)
    # obs = maskgf(Q.DSN_T_ISBA)
    # simu = maskgf(A.DSN_T_ISBA)
    # obs = maskgf(Q.DSN_T_ISBA)

    @abstractmethod
    def get_obs_data(self, filename):
        """
        read observation data
        :param filename: observation filename
        :return: np.array with observation field
        """
        pass

    def make_fuzzy_scores(self):
        """
        calculate POD, FAR, TS, ETS, HK for different forecast experiments.

        :param fc_experiments: dict of forecast fields where dict keys are experiment names
         and values the corresponding fields. (2d np.array)
        :type fc_experiments: dict
        :param observation: observation or reference field
        :type observation: np.array (2d)
        :param list_kernel_size: list of kernel sizes for which to calculate scores.
            neighbourhood (kernel) size in number of pixels in each direction ex. : 3 for a 3x3 pixel kernel
            impair number
        :param list_thresholds: list of event thresholds. lower bounds of event intervals.
        :type list_thresholds: list
        :param list_threshold_increment: list of corresponding sizes of event intervals.
        :type list_threshold_increment: list
        :param perzone: minimum fraction of values needed to be present in the neighborhood in order to take
            the pixel into account for score calculation. Default None will be set to 1 / (kl * kl)
        :type perzone: float
        :return:
        """

        for iexp, exp in enumerate(self.experiments):
            for ik, kernel in enumerate(self.kernels):
                for ithres, thres, thres_inc in enumerate(zip(self.thresholds, self.threshold_incs)):
                    self.score_ds["POD"].data[iexp, ik, ithres], \
                        self.score_ds["FAR"].data[iexp, ik, ithres], \
                        self.score_ds["CSI"].data[iexp, ik, ithres], \
                        self.score_ds["ETS"].data[iexp, ik, ithres], \
                        self.score_ds["HK"].data[iexp, ik, ithres], = mincoverage_small(self.fc_data[exp],
                                                                                        self.obs_data, kernel,
                                                                                        thres, thres_inc,
                                                                                        self.per_zone)

    def process(self):
        self.make_fuzzy_scores()
        if self.score_file:
            self.score_ds.to_netcdf(self.score_file_name)

##################################
# structural similarity
###############################
# SSIM(x, y) = ((2 μx μy + C1)*(2 σxy + C2))/((μx² + μy² + C1)*(σx² + σy² + C2))
# @ARTICLE{1284395,
#   author={Zhou Wang and Bovik, A.C. and Sheikh, H.R. and Simoncelli, E.P.},
#   journal={IEEE Transactions on Image Processing},
#   title={Image quality assessment: from error visibility to structural similarity},
#   year={2004},
#   volume={13},
#   number={4},
#   pages={600-612},
#   keywords={Image quality;Humans;Transform coding;Visual system;Visual perception;Data mining;Layout;Quality assessment;Degradation;Indexes},
#   doi={10.1109/TIP.2003.819861}}

# from skimage.metrics import structural_similarity,mean_squared_error
#
# statistic = structural_similarity(s.fillna(0).to_numpy(),q.fillna(0).to_numpy(),data_range=(np.min((s.fillna(0).to_numpy().min(),q.fillna(0).to_numpy().min())),np.max((s.fillna(0).to_numpy().max(),q.fillna(0).to_numpy().max()))))
# statisticn =structural_similarity(sn.fillna(0).to_numpy(),q.fillna(0).to_numpy(),data_range=(np.min((sn.fillna(0).to_numpy().min(),q.fillna(0).to_numpy().min())),np.max((sn.fillna(0).to_numpy().max(),q.fillna(0).to_numpy().max()))))
# mse = mean_squared_error(s,q)
# msen =mean_squared_error(sn,q)

###############################
# correlation
#################################
# a=np.lib.stride_tricks.sliding_window_view(s, (3,3))
# def vec_corrcoef(X, y, axis=1):
#     Xm = np.mean(X, axis=axis, keepdims=True)
#     ym = np.mean(y)
#     n = np.sum((X - Xm) * (y - ym), axis=axis)
#     d = np.sqrt(np.sum((X - Xm)**2, axis=axis) * np.sum((y - ym)**2))
#     return n / d
# vec_corrcoef(a, np.arange(3))


from scipy.stats import pearsonr

def pearson_corr(simu_data, ref_data, p_value=True):
    """
    Compute Pearson correlation coefficient and p-value for testing non-correlation (optional).
    Important: Nan areas must be identical.

    Args:
        simu_data: The simulation dataset.
        ref_data: The reference dataset.
        p_value: option to return p-value

    Returns:
        statistic : float
            Pearson product-moment correlation coefficient.
        p-value : float (if option is True)
            The p-value associated with the default scipy function options.
    """
    simu_data=simu_data.to_numpy().ravel()
    ref_data=ref_data.to_numpy().ravel()
    if (p_value):
        return pearsonr(simu_data[~np.isnan(simu_data)],ref_data[~np.isnan(ref_data)])
    else:
        statistic, pvalue = pearsonr(simu_data[~np.isnan(simu_data)],ref_data[~np.isnan(ref_data)])
        return statistic