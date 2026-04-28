.. _interpolation:

Interpolation tools
===================

The SAFRAN reanalysis gives FORCING files for SURFEX-Crocus simulations in the semi-distributed geometry since 1958.

You can interpol this reanalysis in order to generate:

* FORCING files and solar mask for isolated points (simulation `postes`)
* FORCING files for a 2D simulation on a regular grid

You need a shapefile which define your geometry and then use the interpolation tools.

How to create FORCING files for isolated points ?
-------------------------------------------------
You can use the script `shapefile2NetCDF.py` which use a shapefile of isolated points.
The documentation is below and you can read it in the header of the python script.

.. automodule:: interpolation.shapefile2NetCDF
   :members:

This module is also an executable script:

.. include:: ./autoscripts/interpolation--shapefile2NetCDF.rst

How to create FORCING files for 2D simulation ?
-----------------------------------------------
You can use the script `shapefile2NetCDF_2D.py` which use a shapefile around the area of interest.
The documentation is below and you can read it in the header of the python script.

.. automodule:: interpolation.shapefile2NetCDF_2D
   :members:

This module is also an executable script:

.. include:: ./autoscripts/interpolation--shapefile2NetCDF_2D.rst

Netcdf_add_massif_info script
-----------------------------

.. include:: ./autoscripts/interpolation--Netcdf_add_massif_info.rst

The interpolation tool
----------------------

The interpolation tool is written in FORTRAN.

Makefiles for mageia7 PCs and for MF supercomputers are provided.
Needs MPI and parallel netcdf libraries.

.. f:autoprogram:: INTERPOLATE_SAFRAN


**Module SUBS**

.. f:automodule:: subs

**Module MODN_INTERPOL_SAFRAN**

.. f:automodule:: modn_interpol_safran

**Module MODD_INTERPOL_SAFRAN**

.. f:automodule:: modd_interpol_safran
