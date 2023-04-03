.. _sec-code-plots:

Plotting tools
==============

.. contents::
   :local:

A lot of tools for plotting are available in the ``plots`` folder. It may not directly provide the figure you want, but classes could be inherited not to reinvent the wheel.

.. todo::

   It would be interesting to have image examples with code snippets. It is possible to document it directly in code files.

Script for plotting Crocus output (GUI and CLI)
-----------------------------------------------

A new script, called ``proplotter`` allow to easily and quickly plot Crocus outputs (PRO files). It have a graphical user interface to browse and plot simulation data and all graphs could also be saved directly with command line arguments and then used without any graphical interface. It allows to plot stratigraphies, and functions for plotting stratigraphy can also been used independently to plot snow stratigraphy in your own graphs.

.. toctree::
   :maxdepth: 2

   The GUI/CLI script stratiprofile <plots-proplotter>

Script for plotting results from operational simulations (s2m based on PEARP)
-----------------------------------------------------------------------------

.. toctree::
   :maxdepth: 2

   The script postprocess.py <plots-pearps2m>

Classes or functions
--------------------

.. toctree::
   :maxdepth: 1

   Classes to plot maps <plots-maps>
   Temporal plots and comparison tools <plots-temporal>
   Functions to plot snow stratigraphy <plots-strati>
