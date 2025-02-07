Snowtools's |version| documentation
===================================

Documentation of Snowtools |version| generated on |today| from release: |release|.

The code is available at https://github.com/UMR-CNRM/snowtools

Presentation of the Snowtools project
-------------------------------------

Snowtools is a series of scripts, mostly written in python, that are designed to make our life simpler in terms of pre- and post-processing of SURFEX-Crocus snow model.

Note that this package is only useful for people interested in using numerical codes of **snowpack modelling** on a **Linux environment**. The package does not include any meteorological or snow data. It may be associated for most of its scripts with the SURFEX project (http://www.umr-cnrm.fr/surfex/spip.php).

General informations about snowpack modelling with SURFEX-Crocus can be found here: http://www.umr-cnrm.fr/spip.php?rubrique73.

For users which meet technical difficulties during the installation or the execution of the codes, we only provide support through the dedicated interface on https://github.com/UMR-CNRM/snowtools-tickets. We will try to answer as soon as possible although we can not guarantee a fixed response time. Note that the access to tickets is limited to known users of the SURFEX-Crocus or snowtools. If you do not already have an access, please ask for access by email to crocus at meteo dot fr.

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
   Running SURFEX on Meteo-France HPC <misc/surfex-hpc.rst>
   Common files around SURFEX simulations <misc/surfex-files.rst>
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

Other documentations
--------------------

.. toctree::
   :maxdepth: 2

   extra_documentation/these_ange/these_ange_index.rst



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
