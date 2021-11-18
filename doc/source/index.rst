Snowtools's |version| documentation
===================================

Documentation of Snowtools |version| generated on |today|.

Presentation of the Snowtools project
-------------------------------------

Snowtools is a series of scripts, mostly written in python, that are designed to make our life simpler in terms of pre- and post-processing of SURFEX-Crocus snow model.

Note that this package is only useful for people interested in using numerical codes of **snowpack modelling** on a **Linux environment**. The package does not include any meteorological or snow data. It may be associated for most of its scripts with the SURFEX project (http://www.umr-cnrm.fr/surfex/spip.php).

General informations about snowpack modelling with SURFEX-Crocus can be found here: http://www.umr-cnrm.fr/spip.php?rubrique73.

Basics
------

.. toctree::
   :maxdepth: 1

   Installation <misc/install.rst>
   Code organization <misc/orga.rst>
   Contribute to snowtools <misc/contribute.rst>
   Documentation <misc/doc.rst>
   Testing <misc/test.rst>


Technical documentation
-----------------------

.. toctree::
   :maxdepth: 1

   S2m scripts <code/s2m.rst>
   Tools for simulations (tools) <code/tools.rst>
   Technical tools around simulations (utils) <code/utils.rst>
   Useful scripts (scripts) <code/scripts.rst>
   Simulation tasks <code/tasks.rst>
   Scores (scores) <code/scores.rst>
   Plots tools (plots) <code/plots.rst>
   Assimilation scripts (assim) <code/assim.rst>
   Interpolation <code/fortran.rst>
   The bronx package <code/bronx.rst>

Other things gathered by snowtools
----------------------------------

.. todo::
   
   Describe a little bit what there is in DATA for instance and some guidelines on what to put here.

Help improving this documentation !
-----------------------------------

If you are an expert of one piece of code, do not hesitate to modify the code and document it better !

Note that there is special pieces of documentation that would be interesting to develop are listed below:

.. toctree::
   :maxdepth: 1

   Todo list for the documentation <misc/todo>

Indices and tables
------------------

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`