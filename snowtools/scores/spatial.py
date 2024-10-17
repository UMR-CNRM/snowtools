# -*- coding: utf-8 -*-
"""
Created on 23 April 2024

@author: radanovics

based on code from Ange Haddjeri thesis.
"""

import epygram
import footprints
from footprints import proxy as fpx
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import xarray as xr
# import libpysal
from scipy.signal import convolve2d
from scipy.stats import pearsonr
from abc import ABC, abstractmethod
from snowtools.scores.list_scores import SpatialScoreFile
from snowtools.utils.prosimu import prosimu_xr
from snowtools.plots.scores.moran_scatter import MoranScatter, MoranScatterColored, get_moran_palette
from snowtools.plots.maps.cartopy import MoranMap

try:
    from snowtools.scores import crps
except ImportError:
    raise ImportError("failed to import compiled module crps. \nMake sure the library was compiled.")


def rolling_window(array, window_shape):
    """
    rolling window for 2D array
    :param array: 2d array
    :type array: np.ndarray
    :param window_shape: shape of the desired rolling window
    :type window_shape: tuple
    """
    if np.__version__ >= '1.20.0':
        return np.lib.stride_tricks.sliding_window_view(array, window_shape)
    else:
        s = (array.shape[0] - window_shape[0] + 1,) + (array.shape[1] - window_shape[1] + 1,) + window_shape
        strides = array.strides + array.strides
        return np.lib.stride_tricks.as_strided(array, shape=s, strides=strides)


class LocalMoranData:
    """
    class containing local Moran's Is statistics.
    """
    def __init__(self, field, neighbors=3):
        """

            :param field: field to analyse
            :type field: xarray.DataArray
            :param neighbors: size of the neighborhood to consider for the local moran calculation.
            Total number of cells in one direction. Exemple: neighbors=3 will calculate the local moran Is
            for neighborhood of 3x3 grid cells.
            :type neighbors: int (odd >=3)
        """
        # check number of neighbors
        if not isinstance(neighbors, int):
            raise TypeError("neighbors argument must be type integer")
        min_shape = min(field.shape[0], field.shape[1])
        if neighbors < 3 or neighbors > min_shape:
            raise ValueError("Minimum neighborhood size is 3, maximum neighborhood size for given field is ", min_shape)
        if (neighbors % 2) == 0:
            raise ValueError("Neighborhood size must be an odd integer number")
        self.neighbors = neighbors
        # prepare weights matrix
        self.weights = self.get_weight_matrix()
        self.field_anom = field - np.nanmean(field)
        self.field_anom_lagged = self.get_spatially_lagged_field()
        self.sum_z2 = np.nansum(self.field_anom[~np.isnan(self.field_anom_lagged)] ** 2)
        self.local_moran_I = self.field_anom * self.field_anom_lagged/self.sum_z2
        self.moran_I = np.nansum(self.local_moran_I)
        self.random_sigma = self.get_random_sigma()
        self.quadrant_numbers = self.get_quadrant_numbers(significance=(self.random_sigma * 1.96)) # 0) #
        # self.quadrant_numbers = self.get_quadrant_numbers(significance=(self.random_sigma * 0))  # 0)

    def plot_moran_scatter_simple(self, variable_name, filename=None, **kwargs):
        """
        do a simple moran scatter plot with all the points the same color

        :param variable_name: which variable is plotted? used to construct axis labels
        :type variable_name: str
        :param filename: optional output file name for the graphic
        :type filename: pathlike
        :param kwargs:
        :return:
        """
        if 'title' in kwargs.keys():
            pl = MoranScatter(variable_name=variable_name, title=kwargs['title'])
        else:
            pl = MoranScatter(variable_name=variable_name)
        pl.plot_var(self.field_anom, self.field_anom_lagged)
        if filename is None:
            plt.show()
        else:
            pl.save(filename, formatout='png')

    def plot_moran_scatter_colored(self, variable_name, filename=None, **kwargs):
        """
        Do a colored moran scatter plot coloring the points according to the self.quadrant_numbers

        :param variable_name: which variable is plotted? used to construct axis labels
        :type variable_name: str
        :param filename: optional output file name for the graphic
        :type filename: pathlike
        :param kwargs: used plot kwargs: 'title'
        """
        if 'title' in kwargs.keys():
            pl = MoranScatterColored(variable_name=variable_name, title=kwargs['title'])
        else:
            pl = MoranScatterColored(variable_name=variable_name)

        pl.plot_var(self.field_anom, self.field_anom_lagged, color=self.quadrant_numbers)
        if filename is None:
            plt.show()
        else:
            pl.save(filename, formatout='png')
            pl.close()

    def plot_quadrant_map(self, x, y, geometry, filename=None):
        """
        plot a map where pixels are colored according to the quadrant of the Moran Scatter plot for
        its local Morans I.

        :param x: x-coordinates
        :type x: 1D array like
        :param y: y-coordinates
        :type y: 1D array like
        :param geometry: map projection
        :type geometry: epygram.geometry
        :param filename: ptional output file name for the graphic
        :type filename: pathlike
        """
        field_kwargs = {'fid': {'netCDF' : 'moran_quadrant_numbers'}}
        field_kwargs['geometry'] = geometry
        field_kwargs['structure'] = geometry.structure
        field = fpx.field(**field_kwargs)
        field.setdata(self.quadrant_numbers)
        palette = get_moran_palette()
        fig, ax = field.cartoplot(parallels=0.05, meridians=0.05,
                                  title = 'Local Moran Scatter Quadrant Map',
                                  pcolormesh_kw={'vmin': 0, 'vmax': 4},
                                  colormap_helper=epygram.colormapping.ColormapHelper('moran_colors',
                                                                                      normalize=False,
                                                                                      explicit_colorbounds=[0, 1, 2, 3, 4],
                                                                                      explicit_ticks=[0.4, 1.2, 2, 2.8, 3.6]),
                                  minmax_along_colorbar=False,
                                  colorbar_kw={'format': matplotlib.ticker.FixedFormatter(['ns',
                                                                                           'hot', 'doghnut',
                                                                                           'cold', 'diamond'])})
        # pl = MoranMap(projection=crs)
        # pl.add_gridlines(crs=crs)
        # pl.map.pcolormesh(x, y, self.quadrant_numbers, cmap=pl.palette, norm=pl.norm)
        if filename is None:
            plt.show()
        else:
            fig.savefig(filename, format='png')
            fig.clear()
            plt.close(fig)

    def get_random_sigma(self):
        """
        calculate local moran Is for the randomized field and returns the standard
        deviation of the resulting local moran Is values.
        :return: standard deviation of local moran Is for the randomized anomaly field.
        :rtype: float
        """
        rng = np.random.default_rng(11234)
        rand_field = self.field_anom.copy()
        rng.shuffle(rand_field)
        rand_field_lagged = self.get_spatially_lagged_field(rand_field)
        rand_local_moran_I = rand_field*rand_field_lagged/self.sum_z2
        return np.nanstd(rand_local_moran_I)

    def get_weight_matrix(self):
        """
        get weight matrix with constant weights over the neighborhood and zero weight for the
        center point
        :return: weight matrix with shape (neighbors, neighbors)
        :rtype: np.ndarray (2D)
        """
        weights = np.ones((self.neighbors, self.neighbors))
        center_index = np.floor_divide((self.neighbors - 1), 2)
        weights[center_index, center_index] = 0
        # weights = weights / weights.sum()
        return weights

    def get_spatially_lagged_field(self, field=None):
        """
        Create a spatially lagged field by applying weighted averages over rolling windows.
        :param field: Optional
        :type field: np.ndarray (2D)
        :return: padded (with np.nan) array with lagged anomalies
        :rtype: np.ndarray (2D)
        """
        if field is None:
            field = self.field_anom

        # half_window = np.floor_divide(self.neighbors, 2)
        # print(half_window)
        # print(field[(6-half_window):(6+half_window+1), (35-half_window):(35+half_window+1)])
        # for i in range(half_window, field.shape[0]-half_window):
        #     for j in range(half_window, field.shape[1]-half_window):
        #         # print(field[(i-half_window):(i+half_window+1),
        #         #                                            (j-half_window):(j+half_window+1)])
        #         # print(i, j)
        #         field[i, j] = np.nan_to_num(field[i, j],
        #                                     nan=np.nanmean(field[(i-half_window):(i+half_window+1),
        #                                                    (j-half_window):(j+half_window+1)])).reshape(-1)
        # create rolling windows
        rolling_anom_field = rolling_window(field, (self.neighbors, self.neighbors))
        # calculate weighted averages over windows
        field_anom_lagged = np.empty(rolling_anom_field.shape[0:2])
        for i in range(rolling_anom_field.shape[0]):
            for j in range(rolling_anom_field.shape[1]):
                weight_sum = np.sum(self.weights[~np.isnan(rolling_anom_field[i, j, :, :])])
                if weight_sum > 0:
                    field_anom_lagged[i, j] = np.nansum(self.weights * rolling_anom_field[i, j, :, :])/weight_sum
                else:
                    field_anom_lagged[i, j] = np.nan

        # field_anom_lagged = np.nanmean(self.weights * rolling_anom_field, axis=(2, 3))
        # pad the lagged anomaly array in order to be of same shape as the original field.
        pad_width = np.floor_divide(self.neighbors, 2)
        field_anom_lagged = np.pad(field_anom_lagged, pad_width=pad_width, mode='constant',
                                   constant_values=np.nan)
        return field_anom_lagged

    def get_quadrant_numbers(self, significance=0.):
        """
        construct an array of quadrant numbers indicating in which quadrant of
        the Moran Scatterplot a data point is situated. (from pysal.esda)
        :return: array of quadrant numbers
        :rtype: np.ndarray (2D)
        """
        zp = self.field_anom > 0
        lp = self.field_anom_lagged > 0
        sig = abs(self.local_moran_I) > significance
        pp = zp * lp * sig
        np = (1 - zp) * lp * sig
        nn = (1 - zp) * (1 - lp) * sig
        pn = zp * (1 - lp) * sig

        q0, q1, q2, q3 = [1, 2, 3, 4]
        return (q0 * pp) + (q1 * np) + (q2 * nn) + (q3 * pn)


def pearson_corr(simu_data, ref_data, p_value=True):
    """
    Compute Pearson correlation coefficient and p-value for testing non-correlation (optional).
    Important: Nan areas must be identical.

    :param simu_data: The simulation dataset.
    :type simu_data: xarray dataset
    :param ref_data: The reference dataset.
    :type ref_data: xarray dataset
    :param p_value: option to return p-value
    :type p_value: bool

    :returns:
        statistic : float
            Pearson product-moment correlation coefficient.
        p-value : float (if option is True)
            The p-value associated with the default scipy function options.
    """
    simu_data = simu_data.to_numpy().ravel()
    ref_data = ref_data.to_numpy().ravel()
    if p_value:
        return pearsonr(simu_data[~np.isnan(simu_data)], ref_data[~np.isnan(ref_data)])
    else:
        statistic, pvalue = pearsonr(simu_data[~np.isnan(simu_data)], ref_data[~np.isnan(ref_data)])
        return statistic


def call_crps(fc_in, obs_in):
    """
    calls the crps subroutine
    :param fc_in: array or list of forcasted values (ensemble members)
    :param obs_in: array of observations or reference
    :return: crpsval
    """
    fc = fc_in.copy().reshape(-1)
    obs = obs_in.copy().reshape(-1)
    fc = fc[~np.isnan(fc)]
    obs = obs[~np.isnan(obs)]

    crpsval = crps.crps(fc, obs, len(fc), len(obs))
    return crpsval


def mincoverage_small(fc, obs, kl, threshold_lower, threshold_increment, perzone=None):
    """
    Calculate POD (probability of detection), FAR (false alarm ratio), TS (threat score),
     ETS (equitable threat score), HK (Hanssen & Kuipper’s Skill Score, True Skill
      Statistic (TSS) or Pierce’s Skill Score),
      ACC (accuracy), PAG (post agreement) between two fields using a given neighborhood size kl, and
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
    :return: POD, FAR, TS, ETS, HK, ACC, PAG
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
    #print(forcast)
    observation = convolve2d(xr.where(((obs.fillna(0) >= threshold_lower) &
                                       (obs.fillna(0) <= threshold_lower + threshold_increment)), 1, 0).to_numpy(),
                             kernel, mode='same', boundary='fill') / knnan_obs  # obs
    #print(observation)
    # print(np.size(forcast[~np.isnan(forcast)]))
    # print(np.size(observation[~np.isnan(observation)]))
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
    PAG = hit / (hit + fa)
    TS = hit/(hit+mis+fa)
    ETS = (hit-hitrand)/(hit+mis+fa-hitrand)
    HK = hit-fa
    ACC = hit + (corej / (hit + mis)) + (hit + corej + fa + mis)

    return POD, FAR, TS, ETS, HK, ACC, PAG


class SpatialScores(ABC):
    def __init__(self, fc_filenames, list_of_experiments, obs_filename, varname, list_of_kernels, list_of_thresholds,
                 list_of_threshold_increments, per_zone=None, score_file=True, score_file_name="spatial_scores.nc",
                 perf_plot=True, perf_plot_file="perfdiag.png"):
        self.obs_data = self.get_obs_data(obs_filename, varname)
        self.fc_data = self.get_fc_data(fc_filenames, list_of_experiments, varname)
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
    def get_fc_data(self, filenames, list_of_experiments, varname):
        """
        read forecast data

        :param list_of_experiments: list of experiment labels
        :param filenames: list of forecast file names (paths)
        :param varname: variable name in data set
        :type varname: str
        :return: dict with experiment labels as keys and xarray.DataArrays of forecast fields as values
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
    def get_obs_data(self, filename, varname):
        """
        read observation data

        :param filename: observation filename
        :param varname: variable name in data set
        :type varname: str
        :return: xarray.DataArray with observation field
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
                for ithres, (thres, thres_inc) in enumerate(zip(self.thresholds, self.threshold_incs)):
                    # print(self.fc_data[exp].data.shape)
                    # print(self.obs_data.data.shape)
                    self.score_ds["POD"].data[iexp, ik, ithres], \
                        self.score_ds["FAR"].data[iexp, ik, ithres], \
                        self.score_ds["CSI"].data[iexp, ik, ithres], \
                        self.score_ds["ETS"].data[iexp, ik, ithres], \
                        self.score_ds["HK"].data[iexp, ik, ithres], \
                        self.score_ds["ACC"].data[iexp, ik, ithres], \
                        self.score_ds["PAG"].data[iexp, ik, ithres] = mincoverage_small(self.fc_data[exp],
                                                                                        self.obs_data, kernel,
                                                                                        thres, thres_inc,
                                                                                        self.per_zone)

    def make_spatial_probability_score(self):
        """
        calculate spatial probability score (spatial CRPS) and write it in a new variable in self.score_ds
        """
        self.score_ds['SPS'] = xr.DataArray(np.empty((len(self.experiments))),
                                   dims=('experiment'),
                                   attrs={'long_name': 'Spatial Probability Score'})
        self.score_ds['SPS'].data.fill(np.nan)
        for iexp, exp in enumerate(self.experiments):
            self.score_ds['SPS'].data[iexp] = call_crps(self.fc_data[exp].data, self.obs_data.data)

    def process(self):
        self.make_fuzzy_scores()
        self.make_spatial_probability_score()
        if self.score_file:
            self.score_ds.to_netcdf(self.score_file_name)


class ProVsPleiade(SpatialScores):
    """
    class for comparing simulations data with a pleiade image.
    """

    def __init__(self, fc_filenames, list_of_experiments, obs_filename, varname, list_of_kernels, list_of_thresholds,
                 list_of_threshold_increments, per_zone=None, score_file=True, score_file_name="spatial_scores.nc",
                 perf_plot=True, perf_plot_file="perfdiag.png"):
        super(ProVsPleiade, self).__init__(fc_filenames, list_of_experiments, obs_filename,
                                           varname, list_of_kernels, list_of_thresholds,
                                           list_of_threshold_increments, per_zone=per_zone,
                                           score_file=score_file, score_file_name=score_file_name,
                                           perf_plot=perf_plot, perf_plot_file=perf_plot_file)
        self.select_fc_at_obs()

    def get_fc_data(self, filenames, list_of_experiments, varname):
        """
        read forecast data for different experiments.

        :param filenames: list of filenames with path
        :type filenames: str or path-like
        :param list_of_experiments: list of experiment names or tags
        :type list_of_experiments: list of strings
        :param varname: variable name to get from the input files
        :type varname: str
        :return: dict with forecast data with experiment names as keys and xarray data variables as values.
        :rtype: dict
        """
        outdict = {}
        for exp, path in zip(list_of_experiments, filenames):
            fc_ds = prosimu_xr(path)
            outdict[exp] = fc_ds.dataset[varname]
        return outdict

    def get_obs_data(self, filename, varname):
        """
        read pleiade observation data

        :param filename: file name
        :return: xarray data array
        """
        obs_ds = prosimu_xr(filename)
        obs_ds.dataset = obs_ds.dataset.rename(x="xx", y="yy")
        return obs_ds.dataset[varname]

        #     time = obs_ds.readtime()
        #     snowheight = obs_ds.read_var('DSN_T_ISBA')
        #     x = obs_ds.read_var('x')
        #     y = obs_ds.read_var('y')
        #
        # return dict(time=time, x=x, y=y, values=snowheight)

    def select_fc_at_obs(self):
        """
        select a subset of the forecast data at times and location where the observation data
        (Pleiade image or alike) are available.
        """
        for exp in self.fc_data.keys():
            self.fc_data[exp] = self.fc_data[exp].sel(time=self.obs_data['time'], xx=self.obs_data['xx'],
                                                      yy=self.obs_data['yy'])

    def get_mask(self, maskfile='masque_glacier2017_foret_ville_riviere.nc', mask_varname='Band1',
                   method='nearest'):
        """
        get a mask object.

        :param maskfile: a netcdf file defining the mask
        :param mask_varname: netcdf variable name containing the mask
        :param method: interpolation method to use in order to map the mask onto the observation data grid.
        :return: mask on self.obs_data grid.
        """
        masque = xr.open_dataset(maskfile)[mask_varname].rename(x="xx", y="yy").interp_like(self.obs_data,
                                                                                            method=method)
        return self.obs_data.where(masque == 0)

    def apply_mask(self, maskfile='masque_glacier2017_foret_ville_riviere.nc', mask_varname='Band1',
                   method='nearest'):
        """
        apply a mask to the forecast and observation data.

        :param maskfile: a netcdf file defining the mask
        :param mask_varname: netcdf variable name containing the mask
        :param method: interpolation method to use in order to map the mask onto the observation data grid.
        """
        mask = self.get_mask(maskfile=maskfile, mask_varname=mask_varname, method=method)
        for exp in self.fc_data.keys():
            self.fc_data[exp] = self.fc_data[exp].where(~np.isnan(mask))
        self.obs_data = self.obs_data.where(~np.isnan(mask))





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



