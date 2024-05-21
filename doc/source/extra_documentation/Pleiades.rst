Pleiades observation
====================

Pleiades observations are high resolution snow depth images retrived using stereo-imagery method of a single scene. [#f1]_ [#f2]_

Those images are provided by César Deschamps-Berger et Simon Gascoin and need a little bit of processing before being used.


Building the Pleiades reference
*******************************

This section present an example to pre-process Pleiades images from files provided by César ou Simulations.

.. note::

  There are 3 image files for each Pleiades shot:

  1. classif file (*classif_r*) => gives the type of surface on each pixel (snow not snow other...)
  2. height file (*dH_XXXXXX*)=> gives the "raw" stereo-imagery value
  3. forests files (*classif_Glb_foret*) => gives forest pixels

  Filters must be applied to obtain heights on snow only.
  In addition, unrealistic snow heights are filtered out [-0.5 , 20m] (cf mail and cesar paper [#f2]_)

  Path must be changed

This  example is based on the files of the 3 2018 Lautaret images (Janv 2018, March 2018, June 2018)::

  import xarray as xr
  import rioxarray
  import hvplot.xarray
  import numpy as np

  classif1801=xr.open_dataset('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/20180123_classif_r-r.tif').sel(band=1).band_data.chunk('2MB').drop('band').drop('spatial_ref')
  dh1801=xr.open_dataset('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/dH_20180123_minus_20160928-r.tif').sel(band=1).band_data.chunk('2MB').drop('band').drop('spatial_ref')
  classif1803=xr.open_dataset('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/20180316_classif_r-r.tif').sel(band=1).band_data.chunk('2MB').drop('band').drop('spatial_ref')
  dh1803=xr.open_dataset('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/dH_20180316_minus_20160928-r.tif').sel(band=1).band_data.chunk('2MB').drop('band').drop('spatial_ref')
  classif1806=xr.open_dataset('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/20180628_classif_r-r.tif').sel(band=1).band_data.chunk('2MB').drop('band').drop('spatial_ref')
  dh1806=xr.open_dataset('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/dH_20180628_minus_20160928-r.tif').sel(band=1).band_data.chunk('2MB').drop('band').drop('spatial_ref')
  foret1801=xr.open_dataset('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/20180123_classif_Glb_foret.tif',engine="rasterio").sel(band=1).band_data.chunk('2MB').drop('band').drop('spatial_ref')
  foret1803=xr.open_dataset('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/20180316_classif_Glb_foret.tif',engine="rasterio").sel(band=1).band_data.chunk('2MB').drop('band').drop('spatial_ref')
  foret1806=xr.open_dataset('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/20180628_classif_Glb_foret.tif',engine="rasterio").sel(band=1).band_data.chunk('2MB').drop('band').drop('spatial_ref')

.. note::
  Classif pleiades:

  1: snow

  2: forêt

  3: terrain stable

  5: snow on shadow

  6: forêt on shadow

  9: glacier folowing  RGI


We only keep 1 and 5 classification::

  combi1801 = xr.where((classif1801==1)|(classif1801==5),dh1801,np.nan)
  combi1803 = xr.where((classif1803==1)|(classif1803==5),dh1803,np.nan)
  combi1806 = xr.where((classif1806==1)|(classif1806==5),dh1806,np.nan)

  combi1801 = xr.where(foret1801==1,np.nan,combi1801)
  combi1803 = xr.where(foret1803==1,np.nan,combi1803)
  combi1806 = xr.where(foret1806==1,np.nan,combi1806)

Filtering extreme values [-0.5 , 20m] and saving::

  f1801=xr.where(combi1801<20,combi1801,np.nan)
  combi1801filtred = xr.where(f1801>-.5,f1801,np.nan)
  f1803=xr.where(combi1803<20,combi1803,np.nan)
  combi1803filtred = xr.where(f1803>-.5,f1803,np.nan)
  f1806=xr.where(combi1806<20,combi1806,np.nan)
  combi1806filtred = xr.where(f1806>-.5,f1806,np.nan)

  combi1801filtred.to_netcdf('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/pleiade1801_f-05_20_nosnow0.nc')
  combi1803filtred.to_netcdf('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/pleiade1803_f-05_20_nosnow0.nc')
  combi1806filtred.to_netcdf('/home/cnrm_other/cen/mrns/haddjeria/Lautaret_Pleiades_2018_4ange/pleiade1806_f-05_20_nosnow0.nc')







Resampling Pleiades reference to simulation grid
************************************************

.. warning::
  This is the critical part to use Pleiades images as simulation references.
  We put the images values to the simulation grid.
  The algo is slow, not optimised and work as followed:

  1. Select the Pleiades pixels contained in the 250m pixel
  2. Check the number of NaN values in 250m (max of 70% of NaN)
  3. Average non NaN values

Open Pleiades and simulation dataset::

  l1803=xr.open_dataset('/pleiade1803_f-05_20_nosnow0.nc',chunks='auto').band_data
  S17=xr.open_dataset('/scratch/mtool/haddjeria/hendrix/gr250ls/Safran_tc_pap/pro/PRO_2017080106_2018080106.nc',chunks='auto').rename(xx="x",yy='y')

Resampling (adapt window size to target resolution)::

  simudata=S17.sel(x=slice(l1803.x.min().values,l1803.x.max().values)).sel(y=slice(l1803.y.min().values,l1803.y.max().values)).sel(time='2018-03-16T10:00')
  quantiledata17=xr.full_like(simudata.DSN_T_ISBA,np.nan)# empty full of nan

  for xcenter, ycenter in progressbar(itertools.product(simudata.x,simudata.y),max_value=(len(simudata.x)*len(simudata.y))):
      data = l1803.sel(x=slice(xcenter-125,xcenter+125)).sel(y=slice(ycenter+125,ycenter-125)).values.ravel() # selection du pixel 250 dans les données pleiades + flatten
      datasnan = data[~(np.isnan(data))] # suppression des nans
      if (len(datasnan)>(125*125)*.3): # au moint 30% de non nan (pour avoir un minimum de representitativité du pixel)
          quantiledata17.loc[dict(x=float(xcenter), y=float(ycenter))] = np.mean(datasnan)

  # copie de projection types et modif des attributes
  quantiledata17['Projection_Type']=S17.Projection_Type
  quantiledata17=quantiledata17.DSN_T_ISBA.assign_attrs(source='Resampled from Pleiades')

Saving::

  quantiledata17.to_nectdf('~/P250_Glb_16_03_18.nc')

.. rubric:: Footnotes

.. [#f1] https://doi.org/10.5194/tc-10-1361-2016
.. [#f2] https://doi.org/10.5194/tc-14-2925-2020
