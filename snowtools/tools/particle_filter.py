import abc
import glob
import logging
import os
import shutil
from collections.abc import Callable
from dataclasses import dataclass

import netCDF4
import numpy as np
import pandas as pd
import xarray as xr
from bronx.stdtypes.date import Date
from numpy.random import PCG64, Generator

logger = logging.getLogger("logger")
logging.basicConfig(level=logging.INFO)


class AssimilationProblemError(Exception):
    pass


@dataclass
class StateVector:
    values: np.ndarray

    @property
    def n_points(self):
        return len(self.values)


@dataclass
class Ensemble:
    members: list[StateVector]

    @property
    def n_member(self):
        return len(self.members)

    @property
    def n_points(self):
        return self.members[0].n_points

    def as_array(self) -> np.ndarray:
        return np.array([member.values for member in self.members])


def dickinson(sd: np.ndarray | xr.DataArray, b: float = 1.22, a: float = 0.11) -> np.ndarray | xr.DataArray:
    return np.minimum(1, (b * sd) / (sd + a))


def compute_snow_thickness_mass_from_prep(prep_ds: xr.Dataset) -> float:
    # Merci Bastien
    # Thickness != depth
    # To obtain depth (output on PRO), need to divide by the cosinus of the slope

    thickness_layers = []
    mass_layers = []

    for i in range(1, 51):  # Boucle sur les 50 couches
        wsn_var = f"WSN_VEG{i}"  # Masse en kg/m2
        rsn_var = f"RSN_VEG{i}"  # Masse volumique en kg/m3

        if wsn_var in prep_ds and rsn_var in prep_ds:
            thickness_layers.extend(prep_ds[wsn_var].values / prep_ds[rsn_var].values)
            mass_layers.extend(prep_ds[wsn_var].values)
        else:
            raise ValueError(f"Variables {wsn_var} ou {rsn_var} manquantes dans le fichier NetCDF")

    total_thickness = np.nansum(thickness_layers, axis=0)
    total_mass = np.nansum(mass_layers, axis=0)

    x_coords = np.unique(prep_ds.data_vars["XX"].values)
    y_coords = np.unique(prep_ds.data_vars["XY"].values)
    new_shape = (len(y_coords), len(x_coords))
    total_thickness_da = xr.DataArray(np.reshape(total_thickness, new_shape), coords={"y": y_coords, "x": x_coords})
    total_mass_da = xr.DataArray(np.reshape(total_mass, new_shape), coords={"y": y_coords, "x": x_coords})
    out_dataset = xr.Dataset({"snow_thickness": total_thickness_da, "swe": total_mass_da})
    out_dataset = out_dataset.reindex(y=total_thickness_da.coords["y"].sortby("y", ascending=False))

    return out_dataset


@dataclass
class AssimilationProblem:
    state: StateVector | Ensemble
    observation: np.ndarray
    observation_operator: Callable
    prior_covariance_matrix: np.ndarray | None = None
    observation_error_covariance_matrix: np.ndarray | None = None
    inverse_prior_covariance_matrix: np.ndarray | None = None
    inverse_observation_error_covariance_matrix: np.ndarray | None = None

    @property
    def ensemble(self):
        if isinstance(self.state, StateVector):
            raise AssimilationProblemError("Use ensemble attribute only for ensemble assimilation methods.")
        return self.state


def point_particle_filter(y: np.ndarray, y_hat: np.ndarray, inv_R_trace: np.ndarray) -> np.ndarray:
    """Vectorized particle filtering separate for each variable
    y given in an array of shape (n_particles, n_points)
    R is supposed diagonal so we only use its trace here (n_points,)"""
    innovation = y - y_hat
    likelihood = np.exp(-1 / 2 * (innovation * inv_R_trace * innovation))
    evidence = np.sum(likelihood, axis=0)
    new_weights = likelihood / evidence
    return new_weights


def effective_sample_size(weights: np.ndarray):
    return 1 / np.sum(weights**2, axis=0)


def reorder_1d_array(resampling_duplication_point: np.ndarray) -> np.ndarray:
    unique_values, counts = np.unique(resampling_duplication_point, return_counts=True)
    reordered_array = np.zeros_like(resampling_duplication_point)
    reordered_array[unique_values - 1] = unique_values
    leftovers = np.repeat(unique_values[counts > 1], counts[counts > 1] - 1)
    reordered_array[reordered_array != np.arange(1, len(reordered_array) + 1)] = leftovers
    return reordered_array


def reorder_duplicated_particles(resampling_duplication_table: np.ndarray) -> np.ndarray:
    return np.apply_along_axis(reorder_1d_array, 1, resampling_duplication_table)


def kitagawa_resampling(weights: np.ndarray, reorder: bool = True) -> np.ndarray:
    """Vectorized Kiatagawa et al. 1996 resampling algorithm"""
    random_generator = Generator(PCG64())
    random_draw_init = random_generator.uniform(size=(weights.shape[1], 1)) / weights.shape[0]
    random_draw = random_draw_init[None, :] + np.arange(start=0, step=1 / 17, stop=1)
    random_draw = random_draw[0]  # stray dimension
    bin_low = np.zeros_like(random_draw)
    bin_low[:, 1:] = np.cumsum(weights.T[:, :-1], axis=1)
    bin_low = np.expand_dims(bin_low, axis=1)
    bin_high = np.expand_dims(np.cumsum(weights.T, axis=1), axis=1)
    random_draw_expanded = np.expand_dims(random_draw, axis=2)
    duplication_table = (random_draw_expanded > bin_low) * (random_draw_expanded < bin_high)
    n_duplicated_particles = np.argmax(duplication_table, axis=2) + 1
    if reorder:
        n_duplicated_particles = reorder_duplicated_particles(n_duplicated_particles)
    return n_duplicated_particles  # particle to duplicate vector


class ParticleFilter:
    """Abstract Particle filter"""

    def __init__(self, assimilation_problem: AssimilationProblem, resampling_algorithm: str):
        self.assimilation_problem = assimilation_problem
        self.resampling_algorithm = resampling_algorithm
        self.weights = np.ones((self.assimilation_problem.ensemble.n_member, self.assimilation_problem.ensemble.n_points))

    @property
    def particles(self):
        return self.assimilation_problem.ensemble.as_array()

    @abc.abstractmethod
    def update_weights(self):
        pass

    def update(self):
        self.update_weights()
        return self.resampling()

    def resampling(self) -> np.ndarray:
        no_resample_mask = (
            np.all(np.isclose(self.weights, 1 / self.assimilation_problem.ensemble.n_member, rtol=1e-5), axis=0)
        ) + (np.all(np.isnan(self.weights), axis=0))
        algorithm_dict = {"kitagawa": kitagawa_resampling}
        duplicated_particles = np.array(
            [
                np.arange(1, self.assimilation_problem.ensemble.n_member + 1)
                for i in range(self.assimilation_problem.ensemble.n_points)
            ]
        )
        duplicated_particles[~no_resample_mask] = algorithm_dict[self.resampling_algorithm](self.weights[:, ~no_resample_mask])
        return duplicated_particles


class PointParticleFilter(ParticleFilter):
    def __init__(self, assimilation_problem: AssimilationProblem, inflation: bool = False, min_n_eff: int = 8, min_alpha=0.1):
        super().__init__(assimilation_problem, resampling_algorithm="kitagawa")
        self.inflation = inflation
        self.min_n_eff = min_n_eff
        self.min_alpha = min_alpha
        self.alpha = np.ones((self.assimilation_problem.ensemble.n_member, self.assimilation_problem.ensemble.n_points))

    def inflate(self, old_weights: np.ndarray, old_alpha: np.ndarray | None = None, inflation_step: float = 0.01):
        y = self.assimilation_problem.observation
        y_hat = self.assimilation_problem.observation_operator(self.particles)

        inv_R_trace = np.diag(self.assimilation_problem.inverse_observation_error_covariance_matrix)
        if old_alpha is None:
            alpha = np.ones_like(inv_R_trace)
        else:
            alpha = old_alpha
        old_n_eff = effective_sample_size(old_weights)
        degenerated_mask = old_n_eff < self.min_n_eff

        iterations = 1
        n_eff = old_n_eff
        weights = old_weights
        alpha[degenerated_mask] -= inflation_step
        logger.info("Enter inflation loop")

        while np.sum(degenerated_mask) > 0 and np.min(alpha) > self.min_alpha:
            logger.info(
                f"Iteration no. {iterations},"
                f"degenerated points {np.sum(degenerated_mask)},"
                f"alpha={np.min(alpha):.1f},"
                f"R multiplication factor= {1 / np.min(alpha):.1f}"
            )

            new_weights = point_particle_filter(
                y=y[degenerated_mask],
                y_hat=y_hat[:, degenerated_mask],
                inv_R_trace=inv_R_trace[degenerated_mask] * alpha[degenerated_mask],
            )
            weights[:, degenerated_mask] = new_weights
            n_eff[degenerated_mask] = effective_sample_size(weights=new_weights)
            degenerated_mask = n_eff < self.min_n_eff
            iterations += 1
            alpha[degenerated_mask] -= inflation_step
        new_weights = point_particle_filter(
            y=y[degenerated_mask],
            y_hat=y_hat[:, degenerated_mask],
            inv_R_trace=inv_R_trace[degenerated_mask] * alpha[degenerated_mask],
        )
        weights[:, degenerated_mask] = new_weights

        return weights, alpha

    def update_weights(self) -> np.ndarray:
        y = self.assimilation_problem.observation
        y_hat = self.assimilation_problem.observation_operator(self.particles)
        inv_R_trace = np.diag(self.assimilation_problem.inverse_observation_error_covariance_matrix)
        new_weights = point_particle_filter(y=y, y_hat=y_hat, inv_R_trace=inv_R_trace)
        n_eff = effective_sample_size(weights=new_weights)
        if self.inflation and np.any(n_eff < self.min_n_eff):
            new_weights, alpha = self.inflate(new_weights, old_alpha=np.ones_like(inv_R_trace))
        self.weights = new_weights
        self.alpha = alpha


class EdelweissParticleFilter(PointParticleFilter):
    def __init__(self, assim_date: Date, inflation=True, min_n_eff=8, min_alpha=0.1):
        self.assim_date = assim_date
        snow_thickness = self.read_preps()
        snow_thickness_ensemble = Ensemble(
            members=[StateVector(snow_thickness.sel(member=i).values) for i in range(snow_thickness.sizes["member"])]
        )
        observation = self.read_observation()
        inv_R = self.set_observation_error(observation=observation, sigma_const=0.15)
        logger.info("Create particle filter object")
        assimilation_problem = AssimilationProblem(
            state=snow_thickness_ensemble,
            observation=observation.values,
            observation_operator=dickinson,
            inverse_observation_error_covariance_matrix=inv_R,
        )
        super().__init__(assimilation_problem, inflation=inflation, min_n_eff=min_n_eff, min_alpha=min_alpha)

    @property
    def vars_to_copy(self) -> list[str]:
        # In SODA only prognostic variables are copied in the function PUT_ALL_VAR_PF
        # Here we copy all variables of the background because I don't understand what variables are copied in PUT_ALL_VAR_PF
        # The advantage is that we will not need to update the variables to copy list every time Crocus changes
        # The disdvantage is likely more computational time
        # The code is designed to support any list of variables, just change it here
        return list(self.backround_preps.data_vars)

    def list_prep_files(self) -> list[str]:
        return sorted(glob.glob(f"mb*/background/PREP_{self.assim_date.ymdh}.nc"))

    def read_preps(self) -> xr.DataArray:
        logger.info("Read PREPs")
        prep_files = self.list_prep_files()
        background_preps = xr.concat([xr.open_dataset(prep_file) for prep_file in prep_files], dim="member")
        self.backround_preps = background_preps
        snow_thickness = (
            background_preps.groupby("member").map(compute_snow_thickness_mass_from_prep).data_vars["snow_thickness"]
        )
        snow_thickness = snow_thickness.reindex(
            y=snow_thickness.coords["y"].sortby("y"), x=snow_thickness.coords["x"].sortby("x")
        )
        snow_thickness = snow_thickness.stack(n_point=("y", "x"))
        return snow_thickness

    def read_observation(self) -> xr.DataArray:
        obs_filename = f"./OBSERVATIONS_{self.assim_date.ymdHh}.nc"
        logger.info("Read observation")
        observation = xr.open_dataset(obs_filename)
        observation = observation.rename({"xx": "x", "yy": "y"}).stack(n_point=("y", "x")).data_vars["SCF"]
        return observation

    def set_observation_error(self, observation: xr.DataArray, sigma_const: float) -> np.ndarray:
        logger.info("Set observation error")
        inv_R = 1 / sigma_const**2 * np.eye(observation.sizes["n_point"])
        return inv_R

    def reindex_preps(self, duplicated_particles: np.ndarray) -> xr.DataArray:
        logger.info("Reindex PREP files to obtain analysis ensemble")
        # 1. Create an indexing Data Array for xarray slicing
        duplicated_particles_data_array = xr.DataArray((duplicated_particles - 1).T, dims=("member", "Number_of_points"))
        duplicated_particles_data_array = duplicated_particles_data_array.assign_coords(
            {
                "member": self.backround_preps.coords["member"],
                "Number_of_points": self.backround_preps.coords["Number_of_points"],
            }
        )
        # 2. Slice to the analysis points only
        unchanged_points_mask = np.all(duplicated_particles == np.arange(1, self.backround_preps.sizes["member"] + 1), axis=1)
        idxs_analysis = np.where(~unchanged_points_mask)[0]
        indexer_sub = duplicated_particles_data_array.sel(Number_of_points=idxs_analysis)

        # 3. Reindex analysis data on a subset of data variables (vars_to_copy)

        analysis_preps = self.backround_preps.copy(deep=False)  # shallow copy; non-prog vars are views
        for dv in self.vars_to_copy:
            if "member" and "Number_of_points" in self.backround_preps[dv].dims:
                # Pointwise reindex only the analysis points
                analysis_preps[dv].loc[{"Number_of_points": idxs_analysis}] = self.backround_preps[dv].sel(
                    Number_of_points=idxs_analysis,
                    member=indexer_sub,
                )
        return analysis_preps

    def export_analysis_preps(self, analysis_preps: xr.Dataset):
        # 4. Export to file
        logger.info("Export analysis PREP files")
        for mb in range(self.assimilation_problem.ensemble.n_member):
            out_file_path = f"mb{mb:04d}/analysis/PREP_{self.assim_date.ymdh}.nc"
            os.makedirs(f"mb{mb:04d}/analysis", exist_ok=True)
            analysis_preps.sel(member=mb).to_netcdf(out_file_path)

    def run(self):
        # Run the filter: new weights and resmple particles
        logger.info("Update weights")
        super().update_weights()
        duplicated_particles = self.resampling()

        # Create analysis initial conditions for OFFLINE
        analysis_preps = self.reindex_preps(duplicated_particles=duplicated_particles)
        self.export_analysis_preps(analysis_preps=analysis_preps)

        # Diagnostics
        logger.info("Export diagnostics")
        pd.DataFrame(duplicated_particles).to_csv("PART_python", header=False, index=False)
        pd.DataFrame(self.alpha).to_csv("ALPHA_python", header=False, index=False)
