# -*- coding: utf-8 -*-

"""

Functions (accessors) provided by snowtools adaptation of xarray
----------------------------------------------------------------

The module xarray_snowtools_accessor aims at wrapping and extending the xarray module for snowtools-specific usage.
The wrapping of existing methods is designed to reduce dependency to native xarray method changes (in order to
centralise required adaptations).

Following the xarray project's recomandations, it is based on the use of accessor :
https://tutorial.xarray.dev/advanced/accessors/01_accessor_examples.html

This accessor is automatically made available when you import ``snowtools.utils.xarray_snowtools``.


Usage examples
^^^^^^^^^^^^^^

.. code-block:: python

     from snowtools.utils import xarray_snowtools
     import xarray as xr

     ds = xr.open_dataset('INPUT.nc', engine='snowtools')

1. Select subset of points from a S2M file in the massif geometry, based on the massif number,
   elevation, slope and aspect

.. code-block:: python

    ds.semidistributed.sel_points(massif_num=3, ZS=[900, 1800, 2700, 3600], slope=40)

2. Project gridded data from Lambert-93 to lat/lon :

.. code-block:: python

    ds.distributed.proj(crs_in="EPSG:2154", crs_out="EPSG:4326")

3. Interpret time-like variable or dimension as datetime :

.. code-block:: python

    ds.surfex.decode_time_variable(varname)

4. Compute 24-hour precipitation accumulations from 6h J to 6h J+1 from hourly precipitation dataset:

.. code-block:: python

    ds.Precipitation.meteo.daily_accumulation()

5. Example of groupby with the first-test PRO file:

.. code-block:: python

     from snowtools.utils import xarray_snowtools
     import xarray as xr
     import matplotlib.pyplot as plt
     ds = xr.open_dataset('PRO_2010080106_2011080106.nc', engine='snowtools')
     dszs = ds.semidistributed.sel_points(ZS=2400)
     meanmonthgroup = dszs.groupby("time.month").mean() # mean the variables on a monthly base
     meanmonthgroup.TG1.plot() # choose one variable to plot
     plt.show()

6. Example of resample with the first-test PRO file:

.. code-block:: python

     from snowtools.utils import xarray_snowtools
     import xarray as xr
     ds = xr.open_dataset('PRO_2010080106_2011080106.nc', engine='snowtools')
     dszs = ds.semidistributed.sel_points(ZS=2400)
     dszs.resample(time='12h').mean() # time resampling to 12h timestep

7. Use of custom "daily_accumulation" method

Resample Rainf variable from hourly values to daily accumulations, starting at 03:00 :

.. code-block:: python

     from snowtools.utils import xarray_snowtools
     import xarray as xr

     ds_hourly = xr.open_dataset('FORCING_test_2d.nc', engine='snowtools')
     ds_daily = ds_hourly.Rainf.snowtools.daily_accumulation(start_hour=3)


New features integration rules:

Any native xarray function/method NOT part of xarray’s public API can be overwritten in these accessors in order to
centralise required adaptations in case of any change of behavior of the native method.

Informations on the list of xarray function/method considered public API can be found in the xarray documentation :

- https://docs.xarray.dev/en/v2023.09.0/getting-started-guide/faq.html (section "What parts of xarray are considered
  public API?")
- https://docs.xarray.dev/en/v2023.09.0/api.html#api

"""

from typing import Union

import numpy as np
import xarray as xr


@xr.register_dataset_accessor("snowtools")
@xr.register_dataarray_accessor("snowtools")
class SnowtoolsAccessor:
    """
    Common snowtools-specific additionnal methods for xarray
    """

    def __init__(self, xarray_obj):
        self.ds = xarray_obj

    def transpose(self):
        """
        Put time dimension as first dimension in case of data processing through numpy arrays

        """

        if 'time' in self.ds.dims:
            return self.ds.transpose('time', ...)
        else:
            return self.ds

    def squeeze(self):
        """
        Drop dimensions with only 1 coordinate (ex : Number_of_patches / Number_of_tiles)
        """
        return self.ds.squeeze()

    def daily_accumulation(self, start_hour=6):
        """
        Compute 24-hour accumulation starting at *strat_hour*, with an output label set at the end of the
        accumulation period.
        This method will return the sum of all values between "start_hour" at day J and "start_hour" at day J+1
        whatever the available frequency in the original dataset.

        It is important to use dask when calling the resample function for huge dataset to optimise computation time.
        Since the operations are carried out along the time dimension by slices of 24 hours, chunk sizes are
        automatically set to 24 over the time dimension and the dimension's length over all other dimensions.

        Execution time for a file over the Grandes Rousses at 25m resolution (~1M of points, total file size ~177 Go)
            - 1000 hourly time steps : < 30s
            - 2000 hourly time steps : ~ 2 minutes
            - 4000 hourly time steps (~6 months)    : ~ 6 minutes
            - 1 full year (8760 hourly timte steps) : crash

        Execution time for a file over the French Alps at 250m resolution (~2.5M of points, total file size 13 Go) over
        1 month (721 hourly time steps) : ~ 20s

        Usage example:

        Compute 24-hour precipitation accumulations from 6h J to 6h J+1 from hourly precipitation dataset,
        so that the new index 'YYYY-MM-DD O6' contains the sum of all indices between
        'YYYY-MM-{DD-1} 07' and 'YYYY-MM-DD 06' (corresponding to precipitation accumulations between
        'YYYY-MM-{DD-1} 06' and 'YYYY-MM-DD 06')

        .. code-block:: python

            tmp = ds.snowtools.daily_accumulation()
            out = tmp.sel(time='2021-12-05 06')

        is equivalent to :

        .. code-block:: python

            out = ds.sel({'time': slice('2021-12-04 07', '2021-12-05 06')}).sum('time')

        :param start_hour: Hour of the day from which the 24-hour accumulation is to be computed
        :type start_hour: int

        """

        # Re-chunking a dataset is expensive, so only if the gain in computation time is worth it
        nbpoints = 1
        for dim in self.ds.dims:
            nbpoints = nbpoints * self.ds.sizes[dim]
        if (nbpoints > 1000000000):
            self.ds = self.ds.squeeze()
            chunks = {'time': 24}
            for dim in list(self.ds.dims):
                chunks[dim] = self.ds.sizes[dim]
            self.ds = self.ds.chunk(chunks)

        if xr.__version__ <= '2022.03.0':
            return self.ds.resample(indexer={'time': 'D'}, base=start_hour, loffset=f'{start_hour}h', closed='right',
                    label='right').sum()
        else:
            return self.ds.resample(time='D', offset=f'{start_hour}h', closed='right', label='right').sum()

    def snow_cover_stats(self, snow_depth_variable='DSN_T_ISBA', snow_depth_threshold=0.2):
        """
        Compute snow cover statistics from a snow depth time serie.
        Returned stats are:

        - LCSCD  : Longest Concurent Snow Cover Duration period
        - LCSMOD : Snow Melt Out Date of the Longest Concurent snow cover period
        - LCSOD  : Snow Cover Onset date of the Longest Concurent snow cover period
        - SD     : Snow duration : total number of snow coverage

        This method calls the "lcscd" function defined in snowtools/tools/SnowCoverDuration.py, see the associated
        documentation for more information.

        Usage example:

        .. code-block:: python

            from snowtools.utils import xarray_snowtools
            import xarray as xr
            ds=xr.open_dataset('PRO_WJF_2010-2016.nc', engine='snowtools')
            stats=ds.snowtools.snow_cover_stats()

        :param snow_depth_variable: Name of the variable containing the snow depth (default value for SURFEX outpus)
        :type start_hour: str
        :param threshold: Snow depth threshold to consider a given point / day as "snow covered"
        :type start_hour: float
        """

        from snowtools.tools.SnowCoverDuration import lcscd
        if isinstance(self.ds, xr.Dataset) and snow_depth_variable in (self.ds.keys()):
            htn = self.ds[snow_depth_variable]
        elif isinstance(self.ds, xr.DatArray):
            htn = self.ds

        return lcscd(htn, threshold=snow_depth_threshold)

    def backtrack_preprocess(self):
        """
        Method to undo the changes done in the preprocess step if the dataset was opened with the "snowtools" backend.
        Initial variable / dimension names come from the 'original_name' attribute created during the preprocess step.

        Usage example:

        .. code-block:: python

            from snowtools.utils import xarray_snowtools
            import xarray as xr
            ds = xr.open_dataset('old_PRO_20180807032000_002400.nc', engine='snowtools')
            original = ds.backtrack_preprocess()

        """
        if 'original_name' in self.ds.attrs.keys():
            backtrack = self.ds.attrs['original_name']
            mapping = {k.strip(): v.strip() for k, v in [item.split(':') for item in backtrack.split(',')]}
            self.ds = self.ds.rename(mapping)
        return self.ds

    # TODO : prévoir une méthode pour retirer les attributs 'original_variable_name' et 'original_dimension_name'
    # dans le cas où on veut écrire le dataset dans un fichier NetCDF pour éviter les erreurs du type :
    # TypeError: Invalid value for attr 'original_variable_name': {'massif_num': 'massif_number'}.
    # For serialization to netCDF files, its value must be of one of the following types:
    # str, Number, ndarray, number, list, tuple


@xr.register_dataset_accessor("meteo")
@xr.register_dataarray_accessor("meteo")
class MeteoAccessor(SnowtoolsAccessor):
    """
    Accessor designed to deal with meteorological files.

    Usage example:

    .. code-block:: python

         from snowtools.utils import xarray_snowtools
         import xarray as xr

         ds = xr.open_dataset('FORCING.nc', engine='snowtools')
         ds.meteo.[...]
    """


@xr.register_dataset_accessor("surfex")
@xr.register_dataarray_accessor("surfex")
class SurfexAccessor(SnowtoolsAccessor):
    """
    Accessor designed to deal with SURFEX output files.

    Usage example:

    .. code-block:: python

         from snowtools.utils import xarray_snowtools
         import xarray as xr

         ds = xr.open_dataset('PRO.nc', engine='snowtools')
         ds.surfex.decode_time_variable('time')
    """

    def decode_time_variable(self, varname):
        """
        Manually decode any time-like variable from a SURFEX output

        :param varname: Name of the variable to decode
        :type varname: str
        """

        timevar = xr.Dataset({varname: self.ds[varname]})
        timevar = xr.decode_cf(timevar)
        self.ds[varname] = timevar[varname]

    def drop_tile_dimension(self, tile=0):
        """
        Select a single value for "tile" dimension (or equivalent) and squeeze the dataset to drop the dimension.

        :param tile: Value of the "tile" dimension to select
        :type tile: int
        """

        for drop_dim in ['Number_of_patches', 'tile', 'Number_of_Tile']:
            if drop_dim in self.ds.dims:
                self.ds = self.ds.sel(drop_dim=tile).squeeze()

        return self.ds


@xr.register_dataset_accessor("semidistributed")
@xr.register_dataarray_accessor("semidistributed")
class SemiDistributedAccessor(SnowtoolsAccessor):
    """
    Additionnal methods in semi-distributed geometry (ex: S2M simulaitions)

    Usage example:

    .. code-block:: python

         from snowtools.utils import xarray_snowtools
         import xarray as xr

         ds = xr.open_dataset('INPUT.nc', engine='snowtools')
         ds.semidistributed.sel_points(massif_num=3, ZS=[900, 1800, 2700, 3600], slope=40)
    """

    def sel_points(self, massif_num=None, ZS=None, slope=None, aspect=None):
        """
        Method used to select a user-defined list of points in semi-distributed geometry (SAFRAN massifs geometry)
        from their elevation (ZS), massif number (massif_num), slope and aspect.

        **NB :**
        More advanced indexing (for example to select all elevations above 1800m or use a slice as argument), use the
        native xarray "where" method directly.

        :param massif_num: Massif number(s) of points to select
        :param massif_num: list, range, int or float
        :param ZS: Elevation(s) of points to select
        :param ZS: list, range, int or float
        :param slope: Slope(s) of points to select
        :param slope: list, range, int or float
        :param aspect: Aspects(s) of points to select
        :param aspect: list, range, int or float

        """

        if isinstance(self.ds, xr.DataArray):
            raise TypeError("This method only applies to Dataset objects")

        indexer = None
        for var in ['massif_num', 'ZS', 'slope', 'aspect']:
            if eval(var) is not None:
                if var not in list(self.ds.keys()):
                    raise ValueError(f'Variable "{var}" does not exist')
                else:
                    value = eval(var)
                    if isinstance(value, str):
                        value = eval(value)

                    if isinstance(value, list) or isinstance(value, range):
                        tmp = self.ds[var].isin([float(x) for x in value])
                    elif isinstance(value, int) or isinstance(value, float):
                        tmp = self.ds[var] == value
                    else:
                        raise TypeError(f"{var} should be a list, range or int")

                    if indexer is None:
                        indexer = tmp
                    else:
                        indexer = indexer & tmp

        if indexer is not None:
            indexer = indexer.compute()
            # When all elements of the indexer are "False", calling "where" raises the following error:
            # IndexError: The indexing operation you are attempting to perform is not valid on netCDF4.Variable object.
            # Try loading your data into memory first by calling .load().
            if any(indexer.data.flatten()):
                out = self.ds.where(indexer, drop=True)
            else:
                print("WARNING : No entry found with the given arguments, returning an empty Dataset")
                print('Arguments :')
                for var in ['massif_num', 'ZS', 'slope', 'aspect']:
                    print(var, '=', eval(var))
                return xr.Dataset()
        else:
            print("WARNING : arguments where empty or could not be interpreted, nothing changed.")
            out = self.ds
        return out


@xr.register_dataset_accessor("distributed")
@xr.register_dataarray_accessor("distributed")
class DistributedAccessor(SnowtoolsAccessor):
    """
    Additionnal methods in distributed geometry (ex: EDELWEISS)

    """

    def proj(self, crs_in="EPSG:4326", crs_out="EPSG:2154"):
        """
        Projection of an xarray dataset or dataarray into a new CRS.
        This method implies a dependency to rioxarray.

        Usage example:

        .. code-block:: python

             from snowtools.utils import xarray_snowtools
             import xarray as xr

             ds = xr.open_dataset('INPUT.nc', engine='snowtools')
             ds.distributed.proj("EPSG:4326", "EPSG:2154")

        :param ds: xarray object to preprocess
        :type ds: xarray Dataset or Dataarray
        :param crs_in: CRS of the input object
        :type crs_in: str
        :param crs_out: CRS of the output object
        :type crs_out: str
        """
        import rioxarray  # noqa

        # TODO extract from rioxarray documentation :
        # "If you use one of xarray’s open methods such as xarray.open_dataset to load netCDF files with the default
        # engine, it is recommended to use decode_coords="all". This will load the grid mapping variable into
        # coordinates for compatibility with rioxarray."
        # TODO : check crs_in ?
        self.ds.rio.set_spatial_dims(x_dim='xx', y_dim='yy', inplace=True)
        self.ds.rio.write_crs(crs_in, inplace=True)
        out = self.ds.rio.reproject(crs_out).rename(x='xx', y='yy')

        return out

    def sel_stations(self, lons, lats, station_numbers=None, x_dim='longitude', y_dim='latitude', method='nearest'):
        """
        Method used to select a list of points defined by their coordinates from a dataset in a distributed geometry.
        By default, the extracted value comes from the nearest pixel. This default behavior can be modified with
        the 'method' arguments, equivalent to the one in the xarray native 'sel' method.

        .. code-block:: python

            from snowtools.utils import xarray_snowtools
            import xarray as xr

            df = pd.read_csv('liste_stations.txt')
            ds = xr.open_dataset('INPUT_2D.nc', engine='snowtools')
            ds.distributed.sel_stations(df.lons.values, df.lats.values, station_numbers=df.num_poste.values)

        :param lons: List of longitudes of the points to be extracted
        :param lons: array_like
        :param lats: List of latitudes of the points to be extracted
        :param lats: array_like
        :param station_numbers: List of station numbers to use as new coordinate
        :param station_numbers: array_like
        :param x_dim: Name of the 'X' dimension in the dataset
        :param x_dim: str
        :param y_dim: Name of the 'Y' dimension in the dataset
        :param y_dim: str
        :param method: Method to use for inexact matches (see xarray 'sel' method)
        :param method: str

        """

        out = self.ds.sel(
            {
                x_dim: xr.DataArray(np.array(lons), dims='station'),
                y_dim: xr.DataArray(np.array(lats), dims='station'),
            },
            method = method,
        )

        if station_numbers is not None:
            out['station'] = station_numbers

        return out

    def plot_ensemble(self, variable=None, vmin=None, vmax=None, cmap=None, dem=None, isolevels=None,
                      members: Union[str, int] = 'all', projection=None):
        """
        Plot field(s) from an ensemble. To control the members of the ensemble to be plotted, use the "members"
        argument.
        The dataset must have a 'member' dimension, and the spatial dimensions ('xx', 'yy'). This implies that
        the selection of the time step must be done before calling the method.

        :param variable: Variable name to plot (Dataset only)
        :type variable: str
        :param vmin: Min colorbar value.
        :type vmin: float
        :param vmax: Max colorbar value.
        :type vmax: float
        :param cmap: Matplotlib colormap
        :type cmap: str
        :param dem: Digital Elevation Model covering the dataset area.
        :type dem: DataArray
        :param isolevels: List of iso-levels to plot
        :type isolevels: list
        :param members: How to plot the ensemble data:
                        'all': Plot all ensemble members on the same figure;
                        'mean': Plot the mean ensemble field;
                        int: Plot the given ensemble member only.
        :type members: str or int
        """

        if isinstance(self.ds, xr.Dataset):
            if variable is None:
                raise ValueError("A variable name should be provided")
            elif variable not in list(self.ds.keys()):
                raise ValueError(f"Variable {variable} not in Dataset")
            else:
                ensemble = self.ds[variable]
        else:
            ensemble = self.ds

        if 'member' not in self.ds.dims:
            raise AttributeError("The 'member' dimension is missing")

        if not set(list(self.ds.dims)) == set(['xx', 'yy', 'member']):
            raise AttributeError("The dimensions of the dataset must be exactly ('xx', 'yy', 'member')")
        else:
            import matplotlib.pyplot as plt
            from snowtools.plots.maps import plot2D

            if members == 'all':
                ensemble.load()
                if vmin is None:
                    vmin = ensemble.min().data
                if vmax is None:
                    vmax = ensemble.max().data
                # Assume ensemble size = 16
                # TODO : Add check on ensemble size
                fig, ax = plt.subplots(nrows=4, ncols=4, figsize=(22, 15))
                i = 0
                j = 0
                for mb in ensemble.member.data[1:]:
                    tmp = ensemble.sel({'member': mb})
                    im = plot2D.plot_field(tmp, ax=ax[i, j], vmin=vmin, vmax=vmax, cmap=cmap, dem=dem,
                            isolevels=isolevels, add_colorbar=False)
                    j = j + 1
                    if j == 4:
                        j = 0
                        i = i + 1
                for axis in ax.flatten():
                    axis.margins(0.02)
                    axis.set_title('')
                    axis.set_xticks([])
                    axis.set_yticks([])
                    axis.set_xlabel('')
                    axis.set_ylabel('')
                fig.subplots_adjust(left=0.01, top=0.99, bottom=0.01, right=0.85, wspace=0.02, hspace=0.02)
                cax = fig.add_axes([0.86, 0.02, 0.05, 0.96])
                cb = fig.colorbar(im, cax=cax)
                # cb.ax.tick_params(labelsize=20)

                cb.set_label(ensemble.name, size=24)

                return fig

            elif members == 'mean':
                field = ensemble.mean(dim='member')
            elif isinstance(members, int):
                field = ensemble.sel(member=members)
            else:
                raise ValueError(f'Could not interpret argument members ({members}). '
                                 'Should be an interger, "mean" or "all".')

            field.load()

            if vmin is None:
                vmin = field.min().data
            if vmax is None:
                vmax = field.max().data
            gridlines = projection is not None
            im = plot2D.plot_field(field, vmin=vmin, vmax=vmax, cmap=cmap, dem=dem,
                          isolevels=isolevels, gridlines=gridlines, projection=projection)

            return im
