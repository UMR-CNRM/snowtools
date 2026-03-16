:orphan:

.. _install-vortex:

Install VORTEX
--------------

**External users do not need to install the vortex package (and they are not allowed to do it). Météo-France users who need to either extract operational S2M files either to run their own experiments on the HPC system need to install the vortex package.**

In the general case, you have to use a pre-installed version of Vortex.

Use a pre-installed version of vortex
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For standard users who do not need to implement modifications in vortex code, it is recommended to directly use the pre-installed stable version of vortex cen_dev branch. For that purpose, you need to add the following items to your .bash_profile (and run it):


On a CEN machine (including your laptop):

.. code-block:: bash

   export VORTEX=/rd/cenfic3/cenmod/home/lafaysse/common/vortex/vortex-cen
   export PYTHONPATH=$PYTHONPATH:$VORTEX/site:$VORTEX/src:$VORTEX/project
   export MTOOLDIR=$HOME

On belenos:

.. code-block:: bash

   export VORTEX=~verolive/vortex/vortex-cen
   export PYTHONPATH=$PYTHONPATH:$VORTEX/site:$VORTEX/src:$VORTEX/project
   export MTOOLDIR=$WORKDIR

On sxcen:

.. code-block:: bash

   export VORTEX=~verolive/vortex/vortex-cen
   export PYTHONPATH=$PYTHONPATH:$VORTEX/site:$VORTEX/src:$VORTEX/project
   export MTOOLDIR=/cnrm/cen/users/NO_SAVE/<login>
 
 On other CNRM machines: 
 
 .. code-block:: bash
 
    export VORTEX=/d0/sync/vortex/vortex-cen
    export PYTHONPATH=$PYTHONPATH:$VORTEX/site:$VORTEX/src:$VORTEX/project
    export MTOOLDIR=/cnrm/cen/users/NO_SAVE/<login>

Note: Make sure that the folder ``/cnrm/cen/users/NO_SAVE/<login>`` exist for sxcen.

Install your own vortex code
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

For users who need to implement new algorithms or resources in the vortex code, it is necessary to install vortex from the git repository.

We recommend to download it locally on your machine with git pull and synchronize it remotely using ``put`` command provided by snowtools.

For a first install, you must belong to the vortex opensource project. For that, ask vortex dot support at meteo dot fr to give you access to the project, specifying that you need to implement new developments (and copy to crocus at meteo dot fr).

You do not need to provide again your ssh key if you have already done it for the other projects.

Then, you'll find more information (install, doc) on : https://opensource.umr-cnrm.fr/projects/vortex?jump=wiki.

You must choose the cen_dev branch.

Basically, installing requires to download the git repo. and add a few vortex paths to your PYTHONPATH.

First, add the following items to your .bash_profile (and run it):

.. code-block:: bash

   export VORTEX=<your_path_to_vortex>
   export PYTHONPATH=$PYTHONPATH:$VORTEX/site:$VORTEX/src:$VORTEX/project
   export MTOOLDIR=$WORKDIR

Attention: for a local installation WORKDIR and therefore MTOOLDIR might be undefined. In this case MTOOLDIR has to be set before executing a task that fetches data.