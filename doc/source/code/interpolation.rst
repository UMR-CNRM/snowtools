
Interpolation tools
===================

.. todo:
   
   Document the interpolation tools better ?

Netcdf_add_massif_info script
-----------------------------

.. include:: ./autoscripts/interpolation--Netcdf_add_massif_info.rst

shapefile2NetCDF tool
---------------------

.. automodule:: interpolation.shapefile2NetCDF
   :members:

This module is also an executable script:

.. include:: ./autoscripts/interpolation--shapefile2NetCDF.rst

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
