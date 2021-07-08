.. _sec-code-utils:

Tools for reading simulations : the ``utils`` module
====================================================

.. contents::
   :local:

Read simulations : ``prosimu``
------------------------------

The prosimu class is designed to read netCDF files used in simulations (inputs, outputs).

.. automodule:: utils.prosimu
   :members:

Get massif informations
-----------------------

.. automodule:: utils.infomassifs
   :members:
   :inherited-members:

File manipulations
------------------

``resources`` module
^^^^^^^^^^^^^^^^^^^^

.. automodule:: utils.resources
   :members:
   :inherited-members:

Related exceptions
^^^^^^^^^^^^^^^^^^

.. automodule:: utils.FileException
   :members:

Sun
---

.. automodule:: utils.sun
   :members:


Writing output files
--------------------

.. automodule:: utils.S2M_standard_file
   :members:

NetCDF comparisons
------------------

.. include:: ./autoscripts/utils--operational_reproductibility--compare2versions.rst

File conversions
----------------

Met and netCDF
^^^^^^^^^^^^^^

.. todo:: 
   Document utils/Met2NetCDF

Read observations from csv files
--------------------------------

.. automodule:: utils.obscsv

Dates management
----------------

Please use :mod:`bronx` tools.
