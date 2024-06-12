.. Author: Ange Haddjeri
.. Date: 2024

Methods of regridding
=====================

Spatial regrid with `ESMF <https://earthsystemmodeling.org/>`_
--------------------------------------------------------------
In some situation it can be interesing to regrid simulations files.
For my PhD I had to downgrade high resolution simulation from 30m to 250m.
The advantage of regriding with `ESMF <https://earthsystemmodeling.org/>`_ is that the dimension of the input file is not limited
to 2 (can regrid NetCDF with time dimension, which is not the case for gdal).
Can regrid using advanced methods such 1st and 2nd order conservative regridding methods (usefull for downscalling resolution)
or regrid non-map data.

.. note::
  Additional references:

  https://earthsystemmodeling.org/esmpy_doc/release/latest/html/examples.html#regridding

  https://earthsystemmodeling.org/esmpy_doc/release/latest/html/api.html#regridding

  https://ncar.github.io/esds/posts/2022/cam-se-regridding/

Basic principle:
****************

| Resample a 30m simulation to 250m using conservative method. The two simulations are named *S18_30* (30m) and *S18* (250m).
| **Need esmpy python library** (the python wraper of ESMF regrid lib)

1. Format the dataset for ESMF with *ESMF_format_dataset(S18_30)*.
2. Format the dataset for ESMF with *ESMF_format_dataset(s18)*.
3. Build the start grid with *grid_create_from_coordinates(t30.x.to_numpy(),t30.y.to_numpy(),xcorners=t30.x_bnds.to_numpy(),ycorners=t30.y_bnds.to_numpy(),corners=True)*.
4. Build the arrival grid with *grid_create_from_coordinates(t250.x.to_numpy(),t250.y.to_numpy(),xcorners=t250.x_bnds.to_numpy(),ycorners=t250.y_bnds.to_numpy(),corners=True)*.
5. Create the original regrid field and fill it with the dataset *esmpy.Field(source_grid, name='Source data 30m')*
6. Create the original regrind field *esmpy.Field(dest_grid, name='Resampled data 250m')*
7. Create the interpolator using the conservative method *regrid_conserve = esmpy.Regrid(sourcefield, destfield, regrid_method=esmpy.RegridMethod.CONSERVE, unmapped_action=esmpy.UnmappedAction.IGNORE)*.
8. Apply interpolator and fill *regrid_conserve(sourcefield, destfield)* field



Function to format dataset for ESMF
***********************************
::

  def ESMF_format_dataset (input_data,clean_dataset=False,inputpEPSG=2154):
      """
      Format dataset to be used in ESMF to regrif lamb93 data
      :param input_data: the dataset to be formated
      :param inputpEPSG: the epsg code of the datset
      :param clean_dataset: remove variable in the dataset to keep only used values
      :return: input_data
      """
      from pyproj import CRS
      from pyproj import Transformer
      import numpy as np

      print("Made with\033[0;31m <3 \033[0;30mby Ange")
      # test inputs
      try :
          input_data.x
      except:
          print('Wrong dimension name ! Rename X dimension to x')
      try :
          input_data.y
      except:
          print('Wrong dimension name ! Rename Y dimension to y')

      if clean_dataset==True:
          input_data=input_data.drop(input_data.keys())
          try :
              input_data =  input_data.drop('time')
          except:
              print('No time dimension in dataset, continuing...')


      # create geometry to recompute correct latlon values
      # I recommend not using lat/lon from surfex

      crs = CRS.from_epsg(inputpEPSG)
      crs.geodetic_crs

      proj = Transformer.from_crs(crs,crs.geodetic_crs)
      proj



      X2D,Y2D = np.meshgrid(input_data.x.to_numpy(),input_data.y.to_numpy())
      coords = np.column_stack((Y2D.ravel(),X2D.ravel()))

      lat,lon = proj.transform(X2D.ravel(),Y2D.ravel())
      #print(lat,lon)

      # Savoir si on a une grille a espacement continue
      if (np.all(np.unique(np.diff(input_data.x)))!=True):
              raise ValueError("An exception occurred : not regular spacing in grid")
      else:
          print('x spacing=',np.diff(input_data.x)[0])

      if (np.all(np.unique(np.diff(input_data.y)))!=True):
              raise ValueError("An exception occurred : not regular spacing in grid")
      else:
          print('y spacing=',np.diff(input_data.y)[0])


      #pour 30 et x
      # cell center donné par
      # check sorted
      a=input_data.x
      if (np.all(a[:-1] <= a[1:])!=True):
          raise ValueError('Error x value not sorted, aborting')
          return -1

      # cell corner donné par
      x_corner30 = np.append(np.asanyarray(input_data.x - np.unique(np.diff(input_data.x))/2),input_data.x[-1] + np.unique(np.diff(input_data.x))[0]/2)

      # check size
      if (len(x_corner30) != len(a)+1):
              raise ValueError('Error len(X_corner), aborting')
              return
      #pour 30 et y
      # cell center donné par
      # check sorted
      a=input_data.y
      if (np.all(a[:-1] <= a[1:])!=True):
          raise ValueError('Error y value not sorted, aborting')
          return -1


      # cell corner donné par
      y_corner30 = np.append(np.asanyarray(input_data.y - np.unique(np.diff(input_data.y))/2),input_data.y[-1] + np.unique(np.diff(input_data.y))[0]/2)

      # check size
      if (len(y_corner30) != len(a)+1):
              raise ValueError('Error len(Y_corner), aborting')
              return -1

      # save lat/lon and corners values

      input_data['lat']=xr.DataArray(
          data=lat.reshape((len(input_data.x.to_numpy()),len(input_data.y.to_numpy()) )),
          dims=['x','y'],
          name='lat',
          attrs=crs.geodetic_crs.cs_to_cf()[0]
      )
      input_data['lon']=xr.DataArray(
          data=lon.reshape((len(input_data.x.to_numpy()),len(input_data.y.to_numpy()) )),
          dims=['x','y'],
          name='lon',
          attrs=crs.geodetic_crs.cs_to_cf()[1]
      )


      input_data['x']=input_data.x.assign_attrs(crs.cs_to_cf()[0])
      input_data["y"]=input_data.y.assign_attrs(crs.cs_to_cf()[1])

      input_data=input_data.set_coords(('lat','lon'))

      x_bounds=np.empty((len(input_data.x),2))
      for i in range(len(x_corner30)-1):
          x_bounds[i,0]=x_corner30[i]
          x_bounds[i,1]=x_corner30[i+1]

      y_bounds=np.empty((len(input_data.y),2))
      for i in range(len(y_corner30)-1):
          y_bounds[i,0]=y_corner30[i]
          y_bounds[i,1]=y_corner30[i+1]

      input_data=input_data.merge(
          xr.DataArray(
          data=x_bounds,
          dims=['x','nv_p'],
          name='x_bnds',
          attrs=crs.cs_to_cf()[0]
          ),
      ).merge(
          xr.DataArray(
          data=y_bounds,
          dims=['y','nv_p'],
          name='y_bnds',
          attrs=crs.cs_to_cf()[1]
          )
      )


      input_data['y']=input_data.y.assign_attrs({'bounds':'y_bnds'})
      input_data['x']=input_data.x.assign_attrs({'bounds':'x_bnds'})
      input_data

      return input_data

  def grid_create_from_coordinates(xcoords, ycoords, xcorners=False, ycorners=False, corners=False, domask=False, doarea=False, ctk=esmpy.TypeKind.R8):
      """
      Create a 2 dimensional Grid using the bounds of the x and y coordiantes.
      :param xcoords: The 1st dimension or 'x' coordinates at cell centers, as a Python list or numpy Array
      :param ycoords: The 2nd dimension or 'y' coordinates at cell centers, as a Python list or numpy Array
      :param xcorners: The 1st dimension or 'x' coordinates at cell corners, as a Python list or numpy Array
      :param ycorners: The 2nd dimension or 'y' coordinates at cell corners, as a Python list or numpy Array
      :param domask: boolean to determine whether to set an arbitrary mask or not
      :param doarea: boolean to determine whether to set an arbitrary area values or not
      :param ctk: the coordinate typekind
      :return: grid
      """
      print("Made with\033[0;31m <3 \033[0;30mby Ange")
      [x, y] = [0, 1]

      # create a grid given the number of grid cells in each dimension, the center stagger location is allocated, the
      # Cartesian coordinate system and type of the coordinates are specified
      max_index = np.array([len(xcoords), len(ycoords)])
      grid = esmpy.Grid(max_index, staggerloc=[esmpy.StaggerLoc.CENTER], coord_sys=esmpy.CoordSys.CART, coord_typekind=ctk)

      # set the grid coordinates using numpy arrays, parallel case is handled using grid bounds
      gridXCenter = grid.get_coords(x)
      x_par = xcoords[grid.lower_bounds[esmpy.StaggerLoc.CENTER][x]:grid.upper_bounds[esmpy.StaggerLoc.CENTER][x]]
      gridXCenter[...] = x_par.reshape((x_par.size, 1))

      gridYCenter = grid.get_coords(y)
      y_par = ycoords[grid.lower_bounds[esmpy.StaggerLoc.CENTER][y]:grid.upper_bounds[esmpy.StaggerLoc.CENTER][y]]
      gridYCenter[...] = y_par.reshape((1, y_par.size))

      # create grid corners in a slightly different manner to account for the bounds format common in CF-like files
      if corners:
          grid.add_coords([esmpy.StaggerLoc.CORNER])
          lbx = grid.lower_bounds[esmpy.StaggerLoc.CORNER][x]
          ubx = grid.upper_bounds[esmpy.StaggerLoc.CORNER][x]
          lby = grid.lower_bounds[esmpy.StaggerLoc.CORNER][y]
          uby = grid.upper_bounds[esmpy.StaggerLoc.CORNER][y]

          gridXCorner = grid.get_coords(x, staggerloc=esmpy.StaggerLoc.CORNER)
          for i0 in range(ubx - lbx - 1):
              gridXCorner[i0, :] = xcorners[i0+lbx, 0]
          gridXCorner[i0 + 1, :] = xcorners[i0+lbx, 1]

          gridYCorner = grid.get_coords(y, staggerloc=esmpy.StaggerLoc.CORNER)
          for i1 in range(uby - lby - 1):
              gridYCorner[:, i1] = ycorners[i1+lby, 0]
          gridYCorner[:, i1 + 1] = ycorners[i1+lby, 1]

      # add an arbitrary mask
      if domask:
          mask = grid.add_item(esmpy.GridItem.MASK)
          mask[:] = 1
          mask[np.where((1.75 <= gridXCenter.any() < 2.25) &
                        (1.75 <= gridYCenter.any() < 2.25))] = 0

      # add arbitrary areas values
      if doarea:
          area = grid.add_item(esmpy.GridItem.AREA)
          area[:] = 5.0

      return grid




Example of script to spatially resample simulation from 30m to 250m:
********************************************************************
::


  S18_30=xr.open_dataset('/scratch/mtool/haddjeria/hendrix/grandesroussesfull30louissafran/Safran_tc_pap/pro/PRO_2018080106_2019080106.nc').rename({'xx':'x','yy':'y'}).sel(time='2019-05-13T10:00')
  # donéee haute resolution a regriller

  s18=xr.open_dataset('/scratch/mtool/haddjeria/hendrix/gr250ls/Safran_tc_pap/pro/PRO_2018080106_2019080106.nc',chunks='auto').rename(xx="x",yy='y').sel(time='2019-05-13T10:00')
  # grille a 250m a remplir

  t30=ESMF_format_dataset(S18_30) #formate de dataset 30m

  t250=ESMF_format_dataset(s18) #formate de dataset 250m

  source_grid=grid_create_from_coordinates(t30.x.to_numpy(),t30.y.to_numpy(),xcorners=t30.x_bnds.to_numpy(),ycorners=t30.y_bnds.to_numpy(),corners=True)
  dest_grid=grid_create_from_coordinates(t250.x.to_numpy(),t250.y.to_numpy(),xcorners=t250.x_bnds.to_numpy(),ycorners=t250.y_bnds.to_numpy(),corners=True)

  import esmpy
  variable="DSN_T_ISBA" # variable a regriller
  twrite=t250[variable] # variable du dataset 250m a remplacer (on garde les coordonées et les attributs)
  time_slice = t30.time # dimension temporelle
  sourcefield = esmpy.Field(source_grid, name='Source data 30m') # creation du champ à regriller
  sourcefield.data[...] = t30[variable].transpose().to_numpy() # remplissage du champ avec les valeur du dataset

  destfield = esmpy.Field(dest_grid, name='Resampled data 250m') # creation du champ apres regrid

  # creation de l'interpolateur
  regrid_conserve = esmpy.Regrid(sourcefield, destfield, regrid_method=esmpy.RegridMethod.CONSERVE, unmapped_action=esmpy.UnmappedAction.IGNORE)
  # https://earthsystemmodeling.org/esmpy_doc/release/latest/html/RegridMethod.html#esmpy.api.constants.RegridMethod
  # https://earthsystemmodeling.org/esmpy_doc/release/latest/html/regrid.html

  destfield = regrid_conserve(sourcefield, destfield)# regrillage

  twrite.data=destfield.data.transpose() # ecriture du champ regrillé dans un nouveau dataset


  twrite=twrite.expand_dims({'time':1}) # ajout de time domme une dimension
  twrite.to_zarr('/scratch/mtool/haddjeria/regrid/tc_30m_2_250m_2019-05-13.zarr') # sauvegarde  en zarr car plus efficace que le netcdf mais fonctionne aussi



Temporal regrid with xarray
---------------------------


.. image:: https://docs.xarray.dev/en/stable/_static/Xarray_Logo_RGB_Final.svg
    :alt: my-picture1
    :width: 300


Time re-gridding may be necessary to calculate smod from september to september.
This can be achieved with xarray. In this following example we average simulations to a single value a day::

  import xarray as xr
  tc_pap_lsm=xr.open_mfdataset('/scratch/mtool/haddjeria/hendrix/gr250ls/Safran_tc_pap/pro/*').rename(xx='x',yy='y') # ouverture de toutes les simulations
  pap_lsm=tc_pap_lsm.sel(time=slice('2018-09-01T00:00','2019-09-01T00:00')).DSN_T_ISBA.chunk((15000,101,143)).resample(time='1D').mean() # on chunk le netcdf selon la dimension temp, choix d'une année => on moyenne la valeur de htn
  #pap_lsm.persist() # start computation asynchonous
  pap_lsm.to_dataset().to_zarr("/scratch/mtool/haddjeria/hendrix/tc/pap_lsm_2018-1D.zarr") # on enregistre en zarr car plus efficace que le netcdf


Regridding PDG or transpose Number_of_points to cartesian (X, Y)
****************************************************************

.. image:: https://i.ibb.co/2d2xwPM/Capture-d-cran-2024-05-23-16-43-01.png
  :width: 600

In some situations, it can be interesting to transpose PGD or PREP files from Number_of_points to X Y. I put the following code for
the records. It does a transposition from Number_of_points to X Y dims, interpolate the variables to a new grid an then stack back
the X Y coordinates to Number_of_points.
The first part of the code can be used to only transpose Number_of_points to (X, Y) coordinates.
::

  import xarray as xr
  import pandas as pd

  p = xr.open_dataset('~/PGD_gr250ls.nc')# fichier a interpoler avec Number_of_points
  i = xr.open_dataset('~/scriptMNT30.nc')# grille source d'interpolation
  index = pd.MultiIndex.from_arrays([p.XX.values,p.XY.values],names=['x','y']) # création de l'array bijectif Numberof point => xy
  p1 = p
  p1['Number_of_points']=index# remplacement de number of point
  p1= p1.unstack() # suppression des doublons dans xxxxxxx yyyyyy => xy
  pi =p1.interp_like(i.ZS)# interpolation, les dimension et coordonées doivent avoir strictement le meme nom !!
  # stack back to Number_of_points
  pi=pi.stack(Number_of_points=[...])# regrillage de l'array de x,y en number of points
  pi
  with ProgressBar(): # ecriture dans un netcdf
      file = 'PGD_grandesrousses30LouisSafran.nc'
      pi.to_netcdf(file,format='NETCDF4_CLASSIC')
