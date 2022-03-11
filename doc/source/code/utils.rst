.. _sec-code-utils:

Tools for reading simulations : the ``utils`` module
====================================================


.. toctree::
   :maxdepth: 1

   utils-prosimu.rst
   utils-massifs.rst
   utils-resources.rst
   utils-exceptions.rst
   utils-sun.rst
   utils-s2mstandardfile.rst
   utils-git.rst

Other tools
-----------

.. contents::
   :local:


NetCDF comparisons
^^^^^^^^^^^^^^^^^^

.. include:: ./autoscripts/utils--operational_reproductibility--compare2versions.rst

Module content:

.. toctree::
   :maxdepth: 1

   utils-operational_repro-compare2versions.rst


File conversions : et and netCDF
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. todo:: 
   Document utils/Met2NetCDF

Read observations from csv files
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. automodule:: utils.obscsv

Dates management
^^^^^^^^^^^^^^^^

.. warning::
   If you want to parse and manage dates, please use :mod:`bronx` tools.
