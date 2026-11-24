# -*- coding: utf-8 -*-

"""
:Authors:
    - Diego Monteiro
    - LVG

Common atmosphere/meteorological conversions that are not elsewhere but commonly used
in particular in the FORCING preparation.

Note : The dependency bronx proposes numerous tools to represent atmosphere state
in ``bronx.meteo.thermo module``. If you do not find what you need in this module,
please check what you need is not yet implemented in bronx.

In particular conversion between relative humidity (rh) and specific humidity (qair) could
be done the following way (knowing the air temperature ``Tair`` and atmospheric pressure ``PSurf``):

.. code-block:: python

   import numpy as np
   from bronx.meteo.thermo import Thermo

   # rh -> qair:
   qair = Thermo(['v', 'c'], data=dict(P=PSurf, Huw=hu, T=Tair, rc=np.zeros_like(Tair))).get('qv')

   # qair -> rh
   rh = Thermo(['v', 'c'], data=dict(P=PSurf, qv=qair, qt=qair, T=Tair)).get('Huw')
   # Note: it may be necessary to cap to 100% when there is condensation:
   rh[rh > 100] = 100
   rh[rh < 0] = 0

"""

import numpy as np
import xarray as xr
from scipy.spatial import Delaunay
from scipy.interpolate import LinearNDInterpolator
from scipy.interpolate import griddata
from scipy.spatial import KDTree
from pyproj import Transformer


CO2_CONCENTRATION = 0.00062
"""Mean CO2 concentration"""


def splitting_prectot(
    prectot: np.array, tair: np.array, rh: np.array, alpha: float = 22, beta: float = -2.7, gamma: float = -0.2
):
    """
    Function used to partition total precipitation into liquid and solid part.

    The computation is using air temperature and relative humidity.

    Alpha, beta and gamma are parameters of logistic values that can be adjusted
    depending on the site considered following Froidurot et al., (2014) method
    (default values are taken from Koistinen and Saltikoff (1998)).

    :param prectot: total precipitation (units in is unit out)
    :type prectot: np.array
    :param tair: air temperature (°C)
    :type tair: np.array
    :param rh:  near surface relative humidity (%)
    :type rh: np.array
    :param alpha: parameters or logistic function, defaults to 22
    :type alpha: float, optional
    :param beta: parameters or logistic function, defaults to -2.7
    :type beta: float, optional
    :param gamma: parameters or logistic function, defaults to -0.2
    :type gamma: float, optional
    :return: Tuple of precliq and precsol (same units as the prectot input)
    :rtype: Tuple of np.array
    """

    # Compute probability of rain and snow using air temperature and relative humidity
    prain = 1 / (1 + np.exp(alpha + beta * tair + gamma * rh))
    psnow = 1 - prain

    # Create output liquid and solid precipitation amount
    # precliq : liquid precipitation (units in is unit out)
    # precsol : solid precipitation (units in is unit out)

    precliq = prectot * prain
    precsol = prectot * psnow

    return precliq, precsol


def psurf(elevation):
    """
    Compute the mean air pressure for a given elevation
    (hypothesis of standard atmosphere)

    :param elevation: Elevation (m a.s.l)
    :type elevation: float or numpy array
    :returns: Surface pressure
    :rtype: same as elevation
    """
    return 101325 * ((288 - 0.0065 * elevation) / 288)**5.255


def reproject(
        x_coords: np.ndarray,
        y_coords: np.ndarray,
        crs_in: str = "EPSG:4326",
        crs_out: str = "EPSG:2154"
) -> tuple[np.ndarray, np.ndarray]:
    """
    Conversion of geographic coordinates from one CRS into another CRS

    :param x_coords: 1D or 2D array of coordinates along the horizontal axis
    :type x_coords: np.ndarray
    :param y_coords: 1D or 2D array of coordinates along the vertical axis
    :type y_coords: np.ndarray
    :param crs_in: CRS of the input object
    :type crs_in: str
    :param crs_out: CRS of the output object
    :type crs_out: str
    """

    # Reprojection on the target grid's CRS
    transformer = Transformer.from_crs(crs_in, crs_out, always_xy=True)

    # Convert coordinates as 2D arrays in order to apply the "transform" method
    if len(np.shape(x_coords)) == 1 and len(np.shape(y_coords)) == 1:
        x_coords, y_coords = np.meshgrid(x_coords, y_coords)
    elif len(np.shape(x_coords)) > 2 or len(np.shape(y_coords)) > 2:
        raise ValueError('X and Y dimension must be 1D or 2D')

    x_out, y_out = transformer.transform(x_coords, y_coords)

    return x_out, y_out


class DelaunayFixGridsInterpolator:
    def __init__(
        self,
        x_source,
        y_source,
        x_target,
        y_target,
    ):
        """
        On suppose qu'on n'est pas dans un cas tordu ou les coordonnées sont inversées,
        i.e. x(yy,xx) ...
        Pour les grib extraits de la BDAP on sera sur latitude(y) longitude(x) par exemple comme noms de dimensions
        """
        self.x_source = x_source
        self.y_source = y_source
        self.x_target = x_target
        self.y_target = y_target
        self.points_source = self._shape_coord_(x_source, y_source)
        self.points_target = self._shape_coord_(x_target, y_target)

    def _shape_coord_(self, x_array, y_array):
        if isinstance(x_array, np.ndarray) and isinstance(y_array, np.ndarray):
            return np.array([x_array.ravel(), y_array.ravel()]).T
        if len(np.shape(x_array)) == 1 and len(np.shape(y_array)) == 1:
            X, Y = np.meshgrid(x_array.values, y_array.values)
            return np.array([X.ravel(), Y.ravel()]).T
        elif len(np.shape(x_array)) == 2 and len(np.shape(y_array)) == 2:
            return np.array([x_array.values.ravel(), y_array.values.ravel()]).T

    def _values_to_numpy_(self, values_array):
        """
        L'array de valeurs est supposé bidimensionnel (dépend de la dim associée
        à x et de celle associée à y
        """
        if values_array.dims == self.xy_source_dim_names:
            return values_array.values.ravel()
        if values_array.dims == self.xy_source_dim_names[::-1]:
            return values_array.values.T.ravel()
        raise Exception("non implémenté")

    def compute_interpolation_coeffs(self):
        triangulation = Delaunay(self.points_source)
        indexes = triangulation.find_simplex(self.points_target)
        # pour chaque index
        T = triangulation.transform[
            indexes, :2
        ]  # pour chaque index, la transformation affine du triangle
        r = triangulation.transform[indexes, 2]  # le barycentre pour chaque index
        ab = np.array(
            [np.dot(T[i], self.points_target[i] - r[i]) for i in range(T.shape[0])]
        )
        c = 1 - ab.sum(axis=1)
        self.index_interpolating_triangle_vertex = triangulation.simplices[indexes]
        self.regression_coeff_on_interpolating_triangle = np.c_[ab, c]

    def compute_interpolated_values_on_target(self, values_array: xr.DataArray):
        values = values_array.values.ravel()
        try:
            values_shaped = values[self.index_interpolating_triangle_vertex]
            values_on_target = (
                self.regression_coeff_on_interpolating_triangle * values_shaped
            ).sum(axis=1)
            return values_on_target.reshape(self.x_target.shape)
        except AttributeError:
            print("Please call compute_interpolation_coeffs method first")


def bilinear_downscaling(
    da_coarse: xr.DataArray,
    # dem_coarse: xr.DataArray,
    dem_fine: xr.DataArray,
    crs_coarse: str = "EPSG:4326",
    crs_fine: str = "EPSG:2154",
    # grad_std: float = -0.0065,  # Default value in K/m
    method: str = 'LinearNDInterpolator',
) -> xr.DataArray:
    """
    Downscale a 2D field from a coarse resolution to a finer resolution with a simple interpolation.


    Input data arrays can have different CRS, in this case the coarse grid is first reprojected on the target
    fine grid's CRS.

    Once both grid are on the same CRS, data from the coarse grid are interpolated on the fine grid.
    The main problem arises when the input coarse grid is irregular (2D arrays as coordinates). This is
    for example the case of the native AROME grid, which looks like :

                              ..........
                 ....................................
           ................................................
             ............................................
               ........................................
                 ....................................
                  .................................
                   ...............................
                    .............................
                     ...........................
                     ..........        .........

    In this case, the most straightforward interpolation methods such as xarray's "interp" do not apply.



    :param da_coarse: Input field(s) on the coarse resolution grid
    :type da_coarse: xr.DataArray
    :param da_fine: Output field(s) on the finer resolution grid
    :type da_fine: xr.DataArray
    :param dem_coarse: Digital elevation model of the input coarse resolution grid (m)
    :type dem_coarse: xr.DataArray
    :param dem_fine: Digital elevation model of the output fine resolution grid (m)
    :type dem_fine: xr.DataArray
    :param crs_coarse: CRS of the input object
    :type crs_coarse: str
    :param crs_fine: CRS of the output object
    :type crs_fine: str
    :param grad_std: Elevation correction to apply to the interpolated field for 1m of elevation difference
    :type grad_std: float

    :return: Output field(s) on the finer resolution grid
    :rtype: xr.DataArray
    """

    # TODO : add coherence check between da_coarse and dem_coarse grids

    # Reprojected the coarse grid on the target fine grid's CRS if necessary
    if crs_coarse != crs_fine:
        x_coarse, y_coarse = reproject(da_coarse["xx"].data, da_coarse["yy"].data, crs_in=crs_coarse, crs_out=crs_fine)
    else:
        x_coarse = da_coarse["xx"].values
        y_coarse = da_coarse["yy"].values
    values_coarse = da_coarse.data
    # z_coarse = dem_coarse.data

    # Flatten 2D data in case of an irregular grid
    if len(np.shape(x_coarse)) == 2 and len(np.shape(y_coarse)) == 2:
        x_coarse = x_coarse.ravel()
        y_coarse = y_coarse.ravel()
        values_coarse = values_coarse.ravel()
        # z_coarse = z_coarse.ravel()

    # Coordinates of the input coarse grid
    coarse_points = np.column_stack((x_coarse, y_coarse))

    # Coordinates of the target fine grid
    x_fine, y_fine = np.meshgrid(dem_fine.xx.data, dem_fine.yy.data)
    fine_points = np.column_stack((x_fine.ravel(), y_fine.ravel()))

    # Interpolation
    if method == 'LinearNDInterpolator':
        interpolator = LinearNDInterpolator(coarse_points, values_coarse, fill_value=np.nan)
        interpolated_data = interpolator(fine_points).reshape(x_fine.shape)
    elif method == 'griddata':
        interpolated_data = griddata(
            coarse_points,
            values_coarse,
            (x_fine, y_fine),
            method="linear",
            fill_value=np.nan,
        )
    elif method == 'KDTree':
        tree = KDTree(coarse_points)
        # Find nearest points indices
        _, indices = tree.query(fine_points)
        # interpolation
        interpolated_data = values_coarse[indices].reshape(x_fine.shape)

#    # Elevation
#    interpolator = LinearNDInterpolator(coarse_points, z_coarse, fill_value=np.nan)
#    interpolated_z = interpolator(fine_points).reshape(x_fine.shape)

#    # Elevation correction
#    """
#    da_fine = interpolation[da_coarse] + grad_std * (interpolation[dem_coarse] - dem_fine)
#    """
#    out_data  = interpolated_values + grad_std * (dem_fine.data - interpolated_z)

    # Conversion of the output into a DataArray
    out = xr.DataArray(
        interpolated_data,
        dims=dem_fine.dims,
        coords={dim: dem_fine[dim].data for dim in dem_fine.dims},
    )

    return out
