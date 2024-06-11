
The proplotter and procompare plotting tools
============================================

Proplotter tool to visualize simulation outputs
-----------------------------------------------

Graphical use
^^^^^^^^^^^^^

A tool for quickly plotting SURFEX-Crocus PRO.nc output can be run with ``plots/stratiprofile/proplotter.py``
(a ``proplotter`` command have been made available if you followed install instructions).
Just run the script with python 3 for GUI access. For usual graphs, the plotting area is divided in two parts:

* on the left, a seasonal graph: time is the x-axis 
* on the right, a vertical profile of a variable at a given date

Moving the mouse on the left plot should change the right plot

The general behaviour to make the Graphical User Interface work is: 

* click the ``Open File`` button to select a PRO.nc file
* select the two variables of interest: one for the left seasonal graph, the other for the profile graph)
* select the point of interest
* click the ``Plot`` button

There are several options for graph type:

* usual graph (default) to plot a PRO file on one point of your simulation
* multiple plot: let free a part a point selection in order to have several points to plot
* member plot: specific to Meteo France system: in order to compare members of a simulation
* height graph: follow a variable in a specific place like 10 cm under snow surface
* compare graph: allow to compare two different PRO files of a simulation

.. figure:: /images/proplotter.png
   :align: center

   Screenshot of ``proplotter`` tool.


Command line use
^^^^^^^^^^^^^^^^

.. include:: ./autoscripts/plots--stratiprofile--proplotter.rst

Procompare to compare two simulation outputs
--------------------------------------------

The ``procompare`` tool is quite similar to ``proplotter`` but designed to compare two profiles from two different simulations files, for instance to visualize impacts of changes in the snow model.

Command line usage is similar to ``proplotter``, use ``procompare --help`` for more details.

Note that the files that are compared should have the same geometry (same points represented for the different graphs).

.. figure:: /images/procompare.png
   :align: center

   Screenshot of ``procompare`` tool with two simulations with a different physical parameterization.
