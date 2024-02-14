.. _sec-orga:

Code organization
=================

* ``assim`` folder contains assimilation post-processing
* ``conf`` contains configuration files for research tools
* ``DATA`` contains no code but files frequently used
* ``evals`` contains routines to extract data for further evaluations (which are not in this folder...)
* ``fortran`` contains interpolation tools including tools for managing solar masks
* ``plots`` contains a lot of useful classes for plotting
* ``scores`` is a set of routines to compute scores both for deterministic or ensemble simulations
* ``scripts`` are a set of scripts to get simulation files
* ``tasks`` contains the typical sequence of instructions necessary to run a full simulation, both in operational or research mode
* ``tools`` contains all the classes and methods shared by the vortex project and standalone applications. It is all the physics and links between simulation elements.
* ``utils`` is a set of technical tools to help manipulating simulation objects (read and convert files, sun trajectory, etc.). Please check the tool your need is not already in bronx.

* ``bronx`` is an external package that contains a lot of useful tools (this package is now an external package that have to be installed separately)

A folder ``tests`` contains the test suite of snowtools and the ``doc`` folder contains this documentation code.

Note that the different folder have close relationships:

* The contents of ``utils`` and ``bronx`` can be imported in any other part of the project.
* The contents of ``tools`` should be imported only by modules of ``tasks`` or directly by Vortex
* The contents of ``tasks`` and ``plots`` must never be imported in ``tools`` and ``utils``.
* The contents of ``tests`` must not be imported anywhere else in the project.
* ``bronx`` must stay an independent module. It cannot import any other module of the project.
* Vortex call only ``tools`` and ``utils`` tasks might call some Vortex facilities some day
