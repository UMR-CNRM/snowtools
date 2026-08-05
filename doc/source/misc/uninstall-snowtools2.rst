.. _sec-uninstallsnowtools2:

Uninstall an old Snowtools 2 installed as a developer
=====================================================

Remove snowtools from PYTHONPATH environment variable
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If the following command return something:

.. code-block:: bash

   echo $PYTHONPATH | grep snowtools

Then, look in your ``~/.bashrc``, ``~/.bash_profile`` or ``~/.profile`` files for the following lines:

.. code-block:: bash

   export PYTHONPATH=$PYTHONPATH:$SNOWTOOLS_CEN

and make sure to **delete** this line.

Remove vortex from PYTHONPATH environment variable
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Similarily, if the following command return something:

     echo $PYTHONPATH | grep vortex

remove the corresponding lines from your ``~/.bashrc``, ``~/.bash_profile`` or ``~/.profile`` files

.. code-block:: bash

   export PYTHONPATH=$PYTHONPATH:$VORTEX
   or
   export PYTHONPATH=$PYTHONPATH:[...]/vortex/...


.. tip::

    If you want to preserve your old installation of snowtools, you can save your ``~/.bash_profile``, ``~/.bashrc`` and ``~/.profile`` files with different
    names and "source" thes new files when you want to activate the old installation of snowtools.

Vortex geometries
^^^^^^^^^^^^^^^^^

Custom geometries could be defined in ``~/.vortexrc/geometries.ini``. This possibility will continue but the standard geometries (both for research and oper) are now automatically loaded by vortex from the snowtools/vortex-cen packages. Hence, you either have to rmeove completely this file or at least to remove all common geometries from this file.

Ensure changes are taken into account
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
You may need to reconnect to the machine or restart the PC to apply changes.

You can check the deletion with the following commands, which should now return nothing:

.. code-block:: bash

     source ~/.bashrc # or ~/.bash_profile or ~/.profile depending on where the PYTHONPATH was
     echo $PYTHONPATH | grep snowtools
     echo $PYTHONPATH | grep vortex
