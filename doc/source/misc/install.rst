.. _sec-install:

Install Snowtools
=================
The snowtools project is mainly designed for a Linux environment, so the installation is designed to be done in a terminal.

If you have already a ssh key for GitHub and you're ok with virtual environments, you can go directly to :ref:`installation_choice`

Prerequisites
-------------
ssh-key for GitHub (if you don't have one):
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To generate a specific ssh key for github, run

.. code-block:: bash

    cd ~/.ssh
    ssh-keygen -t rsa -b 4096 -f github

**NB:** If the folder ``~/.ssh`` does not exist, create it with ``mkdir ~/.ssh``

You will be asked for an optional password to protect your key.
Once the key created, go to your github account, section `SSH keys <https://github.com/settings/keys>`.
Click on "add a SSH key" and copy the content of the file ``~/.ssh/github.pub`` in the "key" field.

You may need to run

.. code-block:: bash

    eval `ssh-agent -s`
    ssh-add ~/.ssh/github

on your computer in order to define the key location on your computer.

Virtual environment:
^^^^^^^^^^^^^^^^^^^^
Virtual environments allow a better reproductibility of your code.
You don't need to know how to use these environments,
we will give you the commands for creation and activation of snowtools environment in this documentation.

We just recommand you to create a specific folder for all your virtual environments:

.. code-block:: bash

    mkdir ~/my_envs

If you want few more infos on virtual environments, please follow :ref:`virtual_env`

.. _installation_choice:

WHICH INSTALLATION FOR SNOWTOOLS ?
----------------------------------

Cleaning of old snowtools configuration (PC, HPC, team server), go to :ref:`MF-PC-cleaning`.

**For new users of after cleaning:**

Meteo-France PC, go to :ref:`MF-PC-install`.

Meteo-France HPC or team Server (SXCEN), go to :ref:`MF-HPC-server-install`.

External users out of Meteo-France system, go to :ref:`exterior-install`.


.. _MF-PC-cleaning:

Cleaning old snowtools installation (PC, team server, HPC)
----------------------------------------------------------

CLEAN PYTHONPATH (PC, team server, HPC)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In your ``~/.bashrc`` file, you must **delete** the line

.. code-block:: bash
    
     export PYTHONPATH=$PYTHONPATH:$SNOWTOOLS_CEN

Please check your ``~/.bash_profile`` and ``~/.profile`` files in they exist.

You have to be sure this line is **deleted.**

You can check the deletion with

.. code-block:: bash
    
     source ~/.bashrc # or ~/.bash_profile or ~/.profile depending on where the PYTHONPATH was
     echo $PYTHONPATH

You must not see snowtools inside the result of the ``echo`` command,

USER environment (PC, team server)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To ensure a reproductible installation, you must **not** have any package installed locally (under your ``~/.local``).

In order to check this, you can see if the folder ``~/.local/lib`` is not empty.

If this folder is not empty, we propose you to re-install the locally installed packages in a proper virtual environment
and clean your local environment with the following commands (it could take time):

.. code-block:: bash
 
    python -m venv ~/my_envs/home_env_YYYYMMDD
    cp -r ~/.local/* ~/my_envs/home_env_YYYYMMDD/.
    pip list --user --format=freeze > ~/my_local_packages.txt
    cat ~/my_local_packages.txt | cut -d'=' -f1 | xargs -n1 pip uninstall -y

Now, you can follow with the installation :ref:`installation_choice`

.. _MF-PC-install:

Install Snowtools on Meteo France personal computer
---------------------------------------------------

Clone the git repository
^^^^^^^^^^^^^^^^^^^^^^^^
We recommand to create a specific folder for all your git repositories:

.. code-block:: bash

    mkdir ~/all_git_repo
    cd ~/all_git_repo

Then you can clone snowtools

.. code-block:: bash

   git clone git@github.com:UMR-CNRM/snowtools.git

Environment and aliases
^^^^^^^^^^^^^^^^^^^^^^^
Set an environment variable pointing to the snowtools repository in your ``~/.bashrc`` file:

.. code-block:: bash

   export SNOWTOOLS_CEN=~/all_git_repo/snowtools

**NB:** Of course, if you have choosen to install your git repository in other place, you must set ``export SNOWTOOLS_CEN=/{path_to_snowtools_repository}/snowtools``

It is also recommended to create useful aliases in the ``~/.bashrc`` file:

.. code-block:: bash

   alias s2m="python $SNOWTOOLS_CEN/snowtools/tasks/s2m_command.py"
   alias proplotter="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/proplotter.py"
   alias procompare="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/procompare.py"
   alias put="$SNOWTOOLS_CEN/cenutils/put"
   alias install_snowtools="python3 $SNOWTOOLS_CEN/cenutils/install_snowtools.py"

Now, source your ``.bashrc`` file in order to take these aliases into account.

.. code-block:: bash

   source ~/.bashrc

Install
^^^^^^^

.. code-block:: bash
 
    cd $SNOWTOOLS_CEN
    git checkout dev
    python -m venv ~/my_envs/snowtools_env
    source ~/my_envs/snowtools_env/bin/activate
    python $SNOWTOOLS_CEN/cenutils/install_snowtools.py -e
    deactivate

Configure Vortex and install UEnv tools
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Vortex Configuration :

.. code-block:: bash

    mkdir ~/.vortex.d/
    cd ~/.vortex.d/

    ln -s $SNOWTOOLS_CEN/vortex-cen/configs/vortex_pc.toml vortex.toml

Install UEnv tools :

.. code-block:: bash

    pip install vortex-gco


And that's it. Now, you have snowtools installed in your git repository ``~/all_git_repo/snowtools`` and a virtual environment associated ``~/my_envs/snowtools_env``

In order to launch a simulation, you'll do the following step:

* activate the virtual environment spectific to snowtools
* use the ``s2m`` command
* deactivate the virtual environment

All this is explained in the page :ref:`surfex_PC_simu`

.. _MF-HPC-server-install:

Install Snowtools on Meteo France HPC (Belenos) or team server (SXCEN)
----------------------------------------------------------------------
Sync Snowtools on Belenos or SXCEN
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In order to install snowtools on a distant server (SXCEN or Belenos), run this command **on your PC** to synchronise your snowtools repository:

.. code-block:: bash

    # For Belenos
    $SNOWTOOLS_CEN/cenutils/put snowtools belenos

    # For SXCEN
    $SNOWTOOLS_CEN/cenutils/put snowtools sxcen

**FOR BELENOS ONLY:** Environment and start installation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. warning::

    As of 2026-04-23, DSI/ICI/CC recently made changes to internet access
    from Belenos. To install Python packages from pypi.org with Pip,
    you'll need to export the following environment variable:

.. code-block:: bash

    export CURL_CA_BUNDLE=/opt/softs/certificats/proxy1_1.pem


As in the personal computer case, set an environment variable pointing to the snowtools repository in **Belenos** ``~/.bashrc`` file

.. code-block:: bash

    export SNOWTOOLS_CEN=~/all_git_repo/snowtools

**NB:** if you have choosen to install your git repository in other place, you must set ``export SNOWTOOLS_CEN=/{path_to_snowtools_repository}/snowtools``

Source the ``~/.bashrc`` file and start installation

.. code-block:: bash

    source ~/.bashrc
    cd $SNOWTOOLS_CEN
    module load python 3.12.12
    module load gcc/15.2.0

**FOR SXCEN ONLY:** Environment and start installation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
As in the personal computer case, set an environment variable pointing to the snowtools repository in **SXCEN** ``~/.bashrc`` file

.. code-block:: bash

    export SNOWTOOLS_CEN=~/all_git_repo/snowtools
    alias python=python3
    # it allows SXCEN to always use the last version of python

**NB:** if you have choosen to install your git repository in other place, you must set ``export SNOWTOOLS_CEN=/{path_to_snowtools_repository}/snowtools``

Source the ``~/.bashrc`` file and start installation

.. code-block:: bash

    source ~/.bashrc
    cd $SNOWTOOLS_CEN


By default, Pip fetches package distributions from the global Python package registry, pypi.org. To install internal vortex plugins, configure Pip so that it can reach MF’s internal Nexus package registry. To do so, add the following lines to ~/.config/pip/pip.ini:

.. code-block:: ini

    [global]
    index = https://nexus-sidev.meteo.fr/repository/pypi-group/pypi
    index-url = https://nexus-sidev.meteo.fr/repository/pypi-group/simple
    extra-index-url = https://nexus.meteo.fr/pypi-vortex-releases/simple


Continue installation for Belenos and SXCEN
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: bash
 
    python -m venv ~/my_envs/snowtools_env
    source ~/my_envs/snowtools_env/bin/activate
    python $SNOWTOOLS_CEN/cenutils/install_snowtools.py -e

Temporary step for Belenos and SXCEN
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Configure gitlab for HPC via SSH using the documentation (in french):

http://confluence.meteo.fr/display/~thomas.carrel-billiard@meteo.fr/Configurer+GitLab

Then clone the following repositories

.. code-block:: bash

    cd ~/all_git_repo
    git clone git@gitlab.meteo.fr:cnrm-gmap/mkjob.git
    git clone git@gitlab.meteo.fr:cnrm-gmap/vortex-gco.git
    git clone git@gitlab.meteo.fr:cnrm-gmap/vortex-olive.git

Install the repositories

.. code-block:: bash

    pip install mkjob/ vortex-gco/ vortex-olive/

Configure Vortex
^^^^^^^^^^^^^^^^

Vortex Configuration

.. code-block:: bash

    mkdir ~/.vortex.d/
    cd ~/.vortex.d/

    # For Belenos
    ln -s $SNOWTOOLS_CEN/vortex-cen/configs/vortex_belenos.toml vortex.toml

    # For SXCEN
    ln -s $SNOWTOOLS_CEN/vortex-cen/configs/vortex_sxcen.toml vortex.toml

Deactivate virtual environment

.. code-block:: bash

    deactivate

That's it, now snowtools is installed on Meteo France HPC belenos or on Meteo France server SXCEN.

In order to launch a simulation, just follow :ref:`surfex_HPC_simu`

.. _exterior-install:

Install Snowtools for external users
------------------------------------

Dependencies
^^^^^^^^^^^^
The whole project requires at least python 3.6 and classical scientific packages (``numpy``, ``netCDF4``...).
Some specific parts of the code (especially tests, scores, documentation generation require an extended set of dependencies that are fully described in ``requirements.txt``.
The installation is based on pip embedded in an internal tool.

Clone the git repository
^^^^^^^^^^^^^^^^^^^^^^^^
We recommand to create a specific folder for all your git repositories:

.. code-block:: bash

    mkdir ~/all_git_repo
    cd ~/all_git_repo

Then you can clone snowtools

.. code-block:: bash

   git clone git@github.com:UMR-CNRM/snowtools.git

Environment and aliases
^^^^^^^^^^^^^^^^^^^^^^^
Set an environment variable pointing to the snowtools repository in your ``~/.bashrc`` file:

.. code-block:: bash

   export SNOWTOOLS_CEN=~/all_git_repo/snowtools

**NB:** Of course, if you have choosen to install your git repository in other place, you must set ``export SNOWTOOLS_CEN=/{path_to_snowtools_repository}/snowtools``

It is also recommended to create useful aliases in the ``~/.bashrc`` file:

.. code-block:: bash

   alias s2m="python $SNOWTOOLS_CEN/snowtools/tasks/s2m_command.py"
   alias proplotter="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/proplotter.py"
   alias procompare="python3 $SNOWTOOLS_CEN/snowtools/plots/stratiprofile/procompare.py"
   alias put="$SNOWTOOLS_CEN/cenutils/put"
   alias install_snowtools="python3 $SNOWTOOLS_CEN/cenutils/install_snowtools.py"

Now, source your ``.bashrc`` file in order to take these aliases into account.

.. code-block:: bash

   source ~/.bashrc

Install
^^^^^^^

.. code-block:: bash
 
    cd $SNOWTOOLS_CEN
    git checkout dev
    python -m venv ~/my_envs/snowtools_env
    source ~/my_envs/snowtools_env/bin/activate
    python $SNOWTOOLS_CEN/cenutils/install_snowtools.py -o all -e
    deactivate

And that's it. Now, you have snowtools installed in your git repository ``~/all_git_repo/snowtools`` and a virtual environment associated ``~/my_envs/snowtools_env``

In order to launch a simulation, you'll do the following step:

* activate the virtual environment spectific to snowtools
* use the ``s2m`` command
* deactivate the virtual environment

All this is explained in the page :ref:`surfex_PC_simu`


.. _virtual_env:

Few infos on virtual environments
---------------------------------
When you are using specific Python libraries in a project, you can have dependencies between some versions of different libraries.
When, like in snowtools package, your project is growing, it is interesting to trace and freeze which version of each library you're using.
Furthermore, in order to avoid interaction with the global Python installation of the user, virtual environment is a way to create and use an environment only for this specific project.

The creation of a virtual environment is made by the command

.. code-block:: bash
 
    python -m venv [name_of_env_folder]

You activate the environment with

.. code-block:: bash
 
    source  [name_of_env_folder]/bin/activate

You can install a specific Python library in your virtual environment

.. code-block:: bash
 
    pip install [name_of_package] # After the activation of the virtual environment

:warning: you don't need to do this for snowtools. Only the necessary packages are installed (with the optimal version). If you don't install other packages, you ensure the reproductibility of your work.

You deactivate the environment with

.. code-block:: bash
 
    deactivate

And that's all. Activate before using your project and deactivate after, in this way, you ensure reproductibility and stability.

More infos on https://docs.python.org/3/tutorial/venv.html

If you want to go back to snowtools installation, please follow :ref:`installation_choice`


Optional installations
----------------------

Spatial interpolator for SAFRAN
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- **On Meteo-France super-computers**, a precompiled binary is provided in the CEN uenv environment.
- **On your PC at CEN**, a pre-compiled binary is also provided on shared filesystems.
- **Otherwise** (or if you want to use a custom version, e.g. for development) you will need to compile the ``interpol`` binaty and set the environment variable ``SNOWTOOLS_INTERPOL`` to point to the location of the interpol compiled binary. To do so, the procedure is detailed below.

To compile the interpol binary:

1. go into the ``snowtools/interpolation`` folder
2. On Meteo-France HPC only, load the necessary modules :

.. code-block:: bash

   module purge
   moudle load intel
   module load intelmpi

3. On external PC only (outside of Meteo-France), you need to have a Fortran90 compiler, a MPI compiler, openmpi, netcdf-parallel and netcdff libraries with headers availables. On Ubuntu, it means installing the following packages : ``build-essential libopenmpi-dev libnetcdf-mpi-dev libnetcdff-dev``.

4. Remove Makefile if defined with ``rm Makefile`` and then link to the correct one depending on your situation:

   - On Meteo-France HPC : ``ln -s Makefile_belenos Makefile``
   - On Meteo-France PC : ``ln -s Makefile_pc_mf Makefile``
   - On external PC : ``ln -s Makefile_pc_nomf Makefile``

5. You just have to run ``make``. That's all. You now have an ``interpol`` binary in the current folder.


CRPS scores
^^^^^^^^^^^
If you need to use CRPS scoring tools, which parts are written in Fortran, you need to compile them.

For CRPS scores, go to the ``snowtools/scores``, and run ``./install_ubuntu.sh`` (or ``./install_belenos.sh`` if you are on a Meteo-France super computer).
