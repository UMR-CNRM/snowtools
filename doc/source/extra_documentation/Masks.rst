Masks
=====


Forest Masks
************

.. image:: https://geoservices.ign.fr/sites/default/files/2021-05/bdforet_Visuel.png
    :alt: my-picture1
    :width: 300

Forest masks were made by Matthieu Baron from the forest dataset of IGN `BD Foret V2 <https://geoservices.ign.fr/bdforet>`_.
The code and a note [#f2]_ to reproduce the masks can be found in his git depot [#f3]_.

To simplify, the BD foret dataset present polygones of different vegetation cover. Different vegetation types are selected. Vegetation formation of "landes" and "formation herbacées" are removed. The
The polygones are then rasterized to the simulation grid.


RGI Glacier Masks
*****************

.. image:: http://www.glims.org/rgi_user_guide/_static/rgi_logo_square.png
    :alt: my-picture1
    :width: 100

The glaciers mask is simply from a rasterisation of the `RGI glacier inventory <https://www.glims.org/RGI/>`_ to the simulation grid.


Lakes Rivers Cities Masks
*************************

.. image:: https://geoservices.ign.fr/sites/default/files/2023-01/BDTOPO_3-3.png
    :alt: my-picture1
    :width: 300

Lake, rivers and cities masks are hand picked from IGN shapefile `BD TOPO <https://geoservices.ign.fr/documentation/donnees/vecteur/bdtopo>`_.
Lakes and rivers are taken form the hydrographic shapefiles, cities from the "commune" shapefile.


Geomorphons Masks
*****************

Geomorphons detection are methods for classification and mapping of landform elements from a DEM based on the principle of pattern recognition rather than differential geometry. [#f1]_

I used a dedicated software to perform this analysis, `Whitebox Tools <https://www.whiteboxgeo.com/>`_, producing the pixel classification .
The geomorphons concept is based on line-of-sight analysis for the eight topographic profiles in the cardinal directions surrounding each grid cell in the input DEM.

.. image:: https://ars.els-cdn.com/content/image/1-s2.0-S0169555X12005028-gr3.jpg
    :alt: my-picture1

I used the command line interface of the whitebox_tools software (see website to install) with the *-r=Geomorphons* option. This command take as input a DEM (need to be on the same grid as the simulation),
the DEM zone must be larger than the zone of interest because we are using a search radius to detect the different landform feature. I chose a search radius of 2.5 km (100 pixel at 250m resolution). The larger the search radius is, the less dependant to resolution the classification is.
I also choose a flat threshold of 1 degree. Finally the output is presented for the 10 most common landforms as in [#f1]_.

Full command documentation : https://www.whiteboxgeo.com/manual/wbt_book/available_tools/geomorphometric_analysis.html#geomorphons

Example of command::

  ./whitebox_tools -r=Geomorphons -v --wd=. --dem=DEM_FRANCE_L93_250m_bilinear.tif -o=output_100_r.tif --residuals --search=100 --threshold=1 --forms

Then to convert the geomorphon tif to netcdf (to be used as input by snowtools), you can use `gdal_translate <https://gdal.org/programs/gdal_translate.html>`_ ::

  gdal_translate -of NetCDF <input_filename> <output_filename>











.. rubric:: Footnotes

.. [#f1] https://doi.org/10.1016/j.geomorph.2012.11.005
.. [#f2] https://git.meteo.fr/lafaysse/chapitre-1/-/blob/8e74d1507300c48944952b1a3ede7bfc5a0a8bee/traitement-zones-problematiques/notes.txt
.. [#f3] https://git.meteo.fr/lafaysse/chapitre-1/-/tree/master/traitement-zones-problematiques
