.. _s2m-command:

The s2m command
===============

Basic usage of s2m is the following:

.. code-block:: bash

   s2m research -b begin_date -e end_date -f forcing [-o path_output] [-n namelist] [-g] [-G] [-s surfex_exe_directory]

On HPC (vortex run), more options are required and a basic use is as follows:

.. code-block:: bash

   s2m research -b begin_date -e end_date -m model -f forcing -r region [-o output_xpid] [-n namelist] [-s surfex_exe_directory] [--walltime=h:mm:ss]

Full documentation is available with ``s2m research --help`` and is reported below:



.. include:: ./autoscripts/s2mresearch.rst
