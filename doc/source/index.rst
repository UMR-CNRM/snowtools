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
   Code Documentation <misc/doc.rst>
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
   Interpolation <code/interpolation.rst>

Other things gathered by snowtools
----------------------------------

.. toctree::
   :maxdepth: 1

   Data <code/data.rst>

SURFEX Information
------------------

.. toctree::
   :maxdepth: 1

   Installation of SURFEX <misc/surfex-install.rst>
   Run a SURFEX-Crocus simulation <misc/surfex-run.rst>
   Few informations on SURFEX-Crocus simulations <misc/surfex.rst>
   Information for developpers <misc/surfex-dev.rst>

SURFEX-Crocus user documentation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. toctree::
   :maxdepth: 2

   misc/surfex-crocus-user-doc

Developments not yet merged into the ``cen`` branch are documented here :

.. toctree::
   :maxdepth: 1

   Development in progress <misc/surfex-dev-in-progress.rst>



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
