Theia Snow observations
=======================

`Theia snow observations <https://www.theia-land.fr/en/product/snow/>`_ are generated from Sentinel-2 (20m resolution, every 5 days or less) and Landsat-8 images over selected areas of the globe.

They can be downloaded using the `theia_download <https://github.com/olivierhagolle/theia_download>`_ command. Theia snow observation present in the form of tiles that can be merged to diplay a larger area. The folowing workflow present a method to download and merge tiles of Theia snow observations.

Donwload observations using `theia_download <https://github.com/olivierhagolle/theia_download>`_::

  mkdir zip
  cd zip
  python ../theia_download.py --lonmin=-2.40 --latmin=41.20 --lonmax=10 --latmax=49.22 -c Snow -a ../config_theia.cfg -d 2021-09-01 --snow_level L3B-SNOW

Unzip archives and create folder achitecture::

  cd ..
  mkdir unzip
  unzip 'zip/*.zip' -d unzip
  mkdir SCD SOD SMD
  find unzip/ -name '*SOD_R2.tif' -exec cp "{}" SOD/ \;
  find unzip/ -name '*SMD_R2.tif' -exec cp "{}" SMD/ \;
  find unzip/ -name '*SCD_R2.tif' -exec cp "{}" SCD/ \;

We merge the tiles using `gdalwarp <https://gdal.org/programs/gdalwarp.html>`_ bash function from GDAL::

  gdalwarp SMOD/*.tif SMOD_mosaic.tif


.. warning::

  The tiles have a small area overlaid. Using this method, they are superimposed following file order of the command (last on top).

  Perhaps `orfeotoolbox <https://www.orfeo-toolbox.org/CookBook/Applications/app_Mosaic.html>`_ is better for merging mosaics with overlay as there is a specific method for overlayed areas management (to be explored, can ask César for help)
