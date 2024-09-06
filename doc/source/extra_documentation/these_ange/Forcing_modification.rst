.. Author: Ange Haddjeri
.. Date: 2024

Blowing snow simulation preparation
###################################

| During my PhD, we developped with Matthieu Baron **SnowPappus** [#f0]_, a distributed blowing snow model. :)
| For blowing snow transport simulations, having a realistic wind is essential. In our simulations we used Louis' DEVINE high resolution wind.
| To proceed to our simulations we therefore had to hand-cook wind modified simulation forcings.


.. _simscratch:

Typical workflow for a blowing snow experiment from scratch on Belenos :
************************************************************************

#. :ref:`S2M Geometry creation <geom>`

#. :ref:`Spinup creation <spinup>` (PREP + PGD + raw forcings (SAFRAN))

#. Forcing modification :

    #. :ref:`High resolution wind resampling <louis>`

    #. :ref:`Forcing wind modification <fmod>` (or any other field)

#. :ref:`Simulation launch with S2M <s2m-command>`

    Don't forget to remove the :file:`-g` option for classical S2M launch with precomputed spinup.

.. warning::
  Do not forget to clean the vortex cache before launching simulation with modified forcings to **override the previous cached forcing files**.


.. _geom:

1. S2M geometry creation from scratch:
======================================

.. |ico1| image:: https://i.ibb.co/KrtJCMV/Capture-d-cran-2024-06-19-10-36-19-copie.png
    :alt: shapefile
    :width: 50

.. |ico2| image:: https://i.ibb.co/GFk7mZJ/Capture-d-cran-2024-06-19-10-36-40.png
    :alt: rectangle
    :width: 50

To generate a simulation geometry from scratch, the first step is to determine the simulation area.
I suggest to use the following workflow:

1. Generate a shapefile of the simulation area

    The shapefile need to be a rectangle in the EPSG:2154 projection and can be made using QGIS |ico1| and the rectangle creation tool from center |ico2| (to get target aligned pixels).

2. Create the S2M geometry definition file using the :file:`/snowtools/interpolation/shapefile2NetCDF_2D.py` script.

    This command create a NetCDF geometry definition file containing the DEM and massif number values needed for the spinup creation.
    An example of command to create this file can be found bellow. Option :file:`-rlon -rlat` for resolution, :file:`--MNT_alt` to get different path for the DEM file and :file:`-m` to precribe a massif number for the entire area.

    A list of CEN MNT/DEM files can be found on `confluence <http://confluence.meteo.fr/pages/viewpage.action?pageId=276547824>`_

    :file:`python shapefile2NetCDF_2D.py ~/Téléchargements/eaudolle\@gdmaison/domaineEAUDOLLE.shp -m 12 -rlon 250 -rlat 250 -o eaudolle_250m.nc`

    This command also output the geometry specific description that need to be added the namelist::

      In namelist &NAM_IGN:
      XCELLSIZE= 250
      XX_LLCORNER= 929876
      XY_LLCORNER= 6445476
      !! Check with NETCDF dimensions (gap of 1 possible):
      NCOLS= 127
      NROWS= 129
      !! For BELENOS, take ntasks < min(NCOLS, NROWS)

.. warning::
  To get more uniform forcings in the Grandes Rousses domain, we did choose a single massif number for the entire domain. This might need DEM alteration for the step of forcing interpolation as the MIN and MAX elevation need to be within the bound of the S2M reanalysis elevation.  Otherwise, the interpolation will crash.


3. Don't forget to create or modify your namelist and move the geometry seed file and the namelist on Belenos HPC.


.. _spinup:

2. Spinup creation on BELENOS
=============================

#. Name and add the new geometry info in the :file:`~/.vortexrc/geometries.ini` file on BELENOS server::

    [eaudolle250]
    info = zone de simulation pour le bassin EDF de EAUDOLLE
    kind = projected
    resolution = 0.250
    area = eaudolle250
    runit = km


.. warning::
  Do not use capital letter in the geometry and area name.

2. Launch first simulation on Belenos to create SPINUP, PREP, PGD and forcings interpolation (SAFRAN) of the new geometry using a command like the folowing::

    s2m research --ntasks=60 --walltime=23:59:00 -b 20070801 -e 20170801 -m s2m -f reanalysis2020.2@lafaysse -r alp_flat:eaudolle250:/home/cnrm_other/cen/mrns/haddjeria/eaudolle_250m.nc -n /home/cnrm_other/cen/mrns/haddjeria/git/namelist/GRID_EAUDOLLE_250.nam -g --geotype grid -o spinup

.. note::
  To have a correct ground temperature initialisation using the :file:`-g` S2M option, the good practice is to do a spinup over a period of 10 years or more before the target simulation date (to allow time for the soil temperature to stabilise).

Louis' DEVINE wind
******************

.. image:: https://raw.githubusercontent.com/louisletoumelin/wind_downscaling_cnn/master/images/SchemeDevine.png
    :width: 600

Louis Le Toumelin developped a machine learning method to downscale the wind speed and direction for AROME coarse simulation model named **DEVINE**. [#f1]_ [#f2]_
If the simulation forcings and the wind are on the same grid (30m) the Wind and Wind_DIR forcing fields can be replaced in files transparently.
If the two grid are different, the two wind fields (Wind and Wind_DIR) need to be resample to the simulation grid.

.. _louis:

3.1 Louis' wind resampling
==========================


This regridding workflow is based on the folowing functions defined by Louis in is `github repo <https://github.com/louisletoumelin/bias_correction>`_ <3::

  #source : https://github.com/louisletoumelin/bias_correction/blob/12e806af084d086d30e429b21deb8ab7f243a381/bias_correction/train/wind_utils.py#L37
  def wind2comp(uv, dir, unit_direction="radian"):
        """
        Converts wind speed and direction from polar coordinates to rectangular components (u, v).

        Args:
            uv (float): Wind speed magnitude.
            dir (float): Wind direction in degrees or radians (depending on unit_direction).
            unit_direction (str, optional): Unit of wind direction. Defaults to "radian".

        Returns:
            tuple: A tuple containing two elements:
                - u (float): Easting component of the wind (positive for eastward wind).
                - v (float): Northing component of the wind (positive for northward wind).
        """

      # Convert wind direction to radians if necessary
      if unit_direction == "degree":
          dir = np.deg2rad(dir)

      # Calculate easting (u) and northing (v) components using trigonometric functions
      u = -np.sin(dir) * uv
      v = -np.cos(dir) * uv

      # Return the calculated easting and northing components
      return u, v


  #source: https://github.com/louisletoumelin/bias_correction/blob/12e806af084d086d30e429b21deb8ab7f243a381/bias_correction/train/wind_utils.py#L48
  def comp2dir(u, v, unit_output="degree"):
    """
    Calculates wind direction from rectangular components (u, v).

    Args:
        u (float): Easting component of the wind.
        v (float): Northing component of the wind.
        unit_output (str, optional): Desired unit for the output wind direction.
            Defaults to "degree".

    Returns:
        float: Wind direction in the specified unit (degrees or radians).

    Raises:
        NotImplementedError: If the desired unit_output is not "degree".
    """

        # Check if desired output unit is degree
        if unit_output == "degree":
          # Calculate direction in radians using arctangent function
          direction_rad = np.arctan2(u, v)
          # Convert direction to degrees and ensure it's between 0 and 360
          direction_deg = np.mod(180 + np.rad2deg(direction_rad), 360)
          # Return wind direction in degrees
          return direction_deg
      else:
          # Raise an error if the unit is not supported
          raise NotImplementedError("Wind direction calculation is only implemented for 'degree' output unit.")

  #source: https://github.com/louisletoumelin/bias_correction/blob/12e806af084d086d30e429b21deb8ab7f243a381/bias_correction/train/wind_utils.py#L4
  def comp2speed(u, v, w=None):
    """
    Calculates wind speed from rectangular components (u, v) or (u, v, w).

    Args:
        u (float): Easting component of the wind.
        v (float): Northing component of the wind.
        w (float, optional): Vertical component of the wind. Defaults to None
            (assuming a 2D wind field).

    Returns:
        float: Wind speed magnitude.
    """

      # Check if vertical wind component is provided
      if w is None:
      # Calculate speed for a 2D wind field using Pythagorean theorem
          speed = np.sqrt(u**2 + v**2)
      else:
          # Calculate speed for a 3D wind field using Pythagorean theorem
          speed = np.sqrt(u**2 + v**2 + w**2)

        # Return the calculated wind speed
      return speed



.. note::
  To start regridding you will need the high resolution wind files and the target simulation grid.
  At the moment of the writing of this file, the high resolution wind database was located on sxcen server at :file:`/mnt/lfs/d10/mrns/users/NO_SAVE/gouttevini/ARCHIVE_LeToumelin_NOSAVE/letoumelinl/Wind_250m/
  latest/Wind_2017_08_02_to_2020_05_31.nc`
  but it is best to ask Hugo or Isabelle for the file.

The regridding unfolds in tree steps :
--------------------------------------

#. Convert Louis' wind speed and direction to rectangular components (u,v) (*wind2comp*)
#. Regrid the rectangular components (u,v) to the desired grid (*rio.reproject_match*)
#. Convert back the rectangular components to the wind speed and direction format (*comp2dir*, *comp2speed*)

The following code result in two files *devine_speed_250m_rioxarray.nc* and *devine_direction_250m_rioxarray.nc* containing the resampled wind speed and direction.

.. note::
  You can find bellow a code example to regrid Louis' wind to the 250m grid used in my paper.
  Please note that the path need to be changed. The regridding uses *rioxarray* library to average wind to the simulation grid **(bilinear method is not recommended for resampling to coarser grid)**.
  In this example, files are saved at each steps, the amount of intermediate files can be reduced for same results.

::

  # Load Louis' wind data from Netcdf storage (sdir)
  spd = xr.open_dataset('devine_wind.nc/')
  # Assuming 'spd' contains wind data with a variable named 'Wind' and 'Wind_DIR'

  # Convert wind speed and direction to rectangular components (u, v)
  # using wind2comp function, specifying wind direction is in degrees
  u, v = wind2comp(spd.Wind, sdp.Wind_DIR, unit_direction="degree")

  # Save the calculated easting (u) and northing (v) components to netcdf storage
  u.to_netcdf('devine_u.nc')
  v.to_netcdf('devine_v.nc')

  # Import rioxarray library for geospatial data handling
  import rioxarray

  # Reload the u and v components from netcdf storage
  u = xr.open_dataset('devine_u.nc/')
  v = xr.open_dataset('devine_v.nc/')

  # Load reference wind speed data from netcdf storage (assuming it has a 'Wind' variable)
  ref = xr.open_dataset('devine_speed_250m.nc/').Wind

  # Set the Coordinate Reference System (CRS) information for the reference data (likely EPSG:2154)
  ref.Wind.rio.write_crs(2154)

  # Set the CRS information for the u and v components to match the reference data (2154)
  u.__xarray_dataarray_variable__.rio.write_crs(2154)
  v.__xarray_dataarray_variable__.rio.write_crs(2154)

  # Reproject u and v components to match the reference data's CRS (2154)
  # using average interpolation (resampling="average") not bilinear
  u_250 = u.__xarray_dataarray_variable__.rio.write_crs(2154).rio.reproject_match(ref.rio.write_crs(2154), resampling="average")

  # Delete the original u data after creating the reprojected version
  del u

  # Save the reprojected easting component (u_250) to netcdf storage
  u_250.to_netcdf('devine_u_250_rioxarray.nc/')

  # Similar process for the northing component (v)
  v_250 = v.__xarray_dataarray_variable__.rio.write_crs(2154).rio.reproject_match(ref.rio.write_crs(2154), resampling="average")
  del v
  v_250.to_netcdf('devine_v_250_rioxarray.nc/')

  # Reload the reprojected u and v components
  v_250 = xr.open_dataset('devine_v_250_rioxarray.nc/')
  u_250 = xr.open_dataset('devine_u_250_rioxarray.nc/')

  # Calculate wind direction from reprojected components using comp2dir function
  # specifying degrees as the output unit
  dir_250_rioxarray = comp2dir(u_250.__xarray_dataarray_variable__, v_250.__xarray_dataarray_variable__, unit_output="degree")

  # Save the calculated wind direction to nectdf storage
  dir_250_rioxarray.to_netcdf('devine_direction_250m_rioxarray.nc/')

  # Calculate wind speed from reprojected components using comp2speed function
  speed_250_rioxarray = comp2speed(u_250.__xarray_dataarray_variable__, v_250.__xarray_dataarray_variable__, w=None)

  # Save the calculated wind speed to nectdf storage
  speed_250_rioxarray.to_necdf('devine_speed_250m_rioxarray.nc/')

.. _fmod:

3.2 Forcing modification
========================


You can find bellow an example to replace Wind and Wind_DIR forcing fields in forcings.

.. note::
  In this example, already resampled 250m Louis' wind is added to 250m SAFRAN forcing.
  *Same method can be applied to different fields and resolution.*
  Please note that the path need to be changed.

Imports::

  # Import libraries for working with xarray data (xr)
  import xarray as xr

  # Import NumPy for numerical operations
  import numpy as np

  # Import garbage collector (gc)
  import gc
  gc.collect()  # Collect garbage before starting further processing

Downloading wind and SAFRAN Forcing Data::

  # Load wind speed data from netcdf storage with a variable renamed to 'Wind'
  windlouis_speed=xr.open_dataset('/devine_speed_250m_rioxarray.nc/').rename({'__xarray_dataarray_variable__':'Wind'})

  # Load wind direction data from netcdf storage with a variable renamed to 'Wind_DIR'
  windlouis_direction=xr.open_dataset('/devine_direction_250m_rioxarray.nc/').rename({'__xarray_dataarray_variable__':'Wind_DIR'})

  # Download SAFRAN forcing data for a specific time range (2017-08-01 to 2018-08-01)
  louismixtapesafran17=xr.open_dataset("/scratch/mtool/haddjeria/hendrix/gr250ls/rawsafran/meteo/FORCING_2017080106_2018080106.nc")

Time Series Generation (aim is to check for missing time steps)::

  # Create an array of timestamps from the starting time of SAFRAN data with hourly intervals until the end time + 1 hour
  a=np.arange(louismixtapesafran17.time[0].values, louismixtapesafran17.time[-1].values+ np.timedelta64(1, "h"), np.timedelta64(1, "h"))

  # Print the length of the generated time series (should match the expected number of time steps)
  print(len(a))
  a

Selecting Wind Data for Matching Time Range (aim is to check for missing time steps)::

  # Select wind direction data for the same time range as SAFRAN data
  wind_dir = windlouis_direction.sel(time=slice(louismixtapesafran17.time[0].values,louismixtapesafran17.time[-1].values))

  # Select wind speed data for the same time range as SAFRAN data
  wind_speed = windlouis_speed.sel(time=slice(louismixtapesafran17.time[0].values,louismixtapesafran17.time[-1].values))

  # Check if the lengths of time series in wind data and SAFRAN data match
  len(a) == len(wind_dir.time)
  len(a) == len(wind_speed.time)

  # Find any differences (exclusive OR) between time steps in wind data and SAFRAN data (should ideally be empty)
  print(np.setxor1d(wind_dir.time,louismixtapesafran17.time))

Adding Wind Data to SAFRAN Dataset::

  # Check the wind direction data (Wind_DIR)
  wind_dir.Wind_DIR  # likely for verification purposes

  # Add wind direction data as a variable named 'Wind_DIR' to the SAFRAN dataset
  louismixtapesafran17['Wind_DIR'] = wind_dir.Wind_DIR

  # Add wind speed data as a variable named 'Wind' to the SAFRAN dataset
  louismixtapesafran17['Wind'] = wind_speed.Wind

  # Set reference height (UREF) in the SAFRAN dataset to a constant value of 10 meters (assuming Arome model reference height)
  louismixtapesafran17['UREF'].values = np.ones(louismixtapesafran17.UREF.shape,dtype=np.float32)*10

  # Add attributes to the 'Wind' variable in the SAFRAN dataset
  louismixtapesafran17["Wind"]=louismixtapesafran17.Wind.assign

Adding Attributes to Variables::

  # Add attributes to the 'Wind' variable in the SAFRAN dataset
  louismixtapesafran17["Wind"] = louismixtapesafran17.Wind.assign_attrs(
    {'long_name': 'Wind Speed',  # Descriptive name of the variable
     'units': 'm/s',               # Units of the data (meters per second)
     'standard_name': 'wind_speed', # Standard name for wind speed data in climate and forecasting models
     'origin' : 'Arome Wind downscaled with DEVINE2 by Louis'  # Source of the data
  })

  # Similarly, add attributes to the 'Wind_DIR' variable
  louismixtapesafran17["Wind_DIR"] = louismixtapesafran17.Wind_DIR.assign_attrs(
    {'long_name': 'Wind Direction',  # Descriptive name of the variable
     'units': 'deg',                 # Units of the data (degrees)
     'standard_name': 'wind_from_direction',  # Standard name for wind direction data
     'origin' : 'Arome Wind downscaled with DEVINE2 by Louis'  # Source of the data
  })

  # Add attributes to the 'time' variable
  louismixtapesafran17['time'] = louismixtapesafran17.time.assign_attrs(
    {'long_name': 'time',  # Descriptive name of the variable
     'standard_name': 'time'  # Standard name for time data in climate and forecasting models
  })

  # Print the modified SAFRAN dataset for inspection (likely commented out for brevity)
  louismixtapesafran17

Encoding and saving:

.. warning::

  Please note that the generation of the final forcing file to be read by SURFEX need precised characteristics. File need to be in NETCDF4_CLASSIC format, the time dimension need to be UNLIMITED and encoded in int32.
  If the file is large, it can be compressed to reduce transfert time (at cost of small read overtime)

::

  # Compression settings for the NetCDF file
  # Enable zlib compression with compression level 5 (higher level means better compression but slower processing)
  comp = dict(zlib=True, complevel=5)

  # Create a dictionary to define encoding for each variable in the dataset
  # Apply the compression settings (comp dictionary) to all data variables (data_vars)
  encoding = {var: comp for var in louismixtapesafran17.data_vars}

  # Update the encoding dictionary specifically for the 'time' variable
  # Set the data type to 'int32' (hardcoded in Fortran SURFEX)
  encoding.update({'time':{"dtype": "int32"}})

  # Print the resulting encoding dictionary (likely for verification purposes)
  print(encoding)

  # Define the path to save the NetCDF file
  path = '/scratch/mtool/haddjeria/tc/forcing/250m/Safran/'
  file = 'FORCING_2017080106_2018080106.nc'

  # Combine path and filename to create the full temporary filename
  tempfile = path + file

  # Save the modified SAFRAN dataset as a NetCDF file (format='NETCDF4_CLASSIC')
  # Specify unlimited dimension for 'time' to allow for future growth
  # Use the defined encoding dictionary for compression and data type settings
  louismixtapesafran17.to_netcdf(tempfile, unlimited_dims={'time':True}, format='NETCDF4_CLASSIC', encoding=encoding)

Cleaning::

  # to relieve RAM
  del louismixtapesafran17
  del a
  del wind_speed
  del wind_dir
  gc.collect() # Collect garbage

.. rubric:: Footnotes

.. [#f0] https://doi.org/10.5194/gmd-17-1297-2024
.. [#f1] https://doi.org/10.5194/npg-31-75-2024
.. [#f2] https://doi.org/10.1175/AIES-D-22-0034.1
