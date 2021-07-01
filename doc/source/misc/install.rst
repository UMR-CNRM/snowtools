.. _sec-install:

Install Snowtools
=================

Dependencies
------------

The snowtools project is mainly designed for a Linux environment.

The whole project requires at least python 3.5 and the following packages:

* ``six``
* ``numpy``
* ``netCDF4``

In addition, the Graphical Tool Interface to plot simulation outputs requires the following packages:

* ``tkinter``
* ``matplotlib``
* ``pickle``

Install
-------

Snowtools is available on a git repository. See https://opensource.umr-cnrm.fr/projects/snowtools_git/wiki/Procedure_for_new_users for getting access.

Clone the git repository on your computer. You have then to define the environment variable ``SNOWTOOLS_CEN`` and add the install folder to your ``PYTHONPATH``. This can be done by adding these thow following lines to your ``.bashrc`` or ``.bash-profile``: 

.. code-block:: bash
   
   export SNOWTOOLS_CEN=/yourpath/snowtools_git
   export PYTHONPATH=$PYTHONPATH:$SNOWTOOLS_CEN

It is also recommended to create useful aliases for s2m command and proreader graphical user interface in the same file:

.. code-block:: bash

   alias s2m="python $SNOWTOOLS_CEN/tasks/s2m_command.py" 
   alias proreader="python3 $SNOWTOOLS_CEN/plots/GUI_Proreader.py" 
