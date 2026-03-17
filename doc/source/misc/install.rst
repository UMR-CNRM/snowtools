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

Cleaning of old snowtools configuration (PC, HPC, server), go to :ref:`MF-PC-cleaning`.

**For new users of after cleaning:**

Meteo-France PC, go to :ref:`MF-PC-install`.

Meteo-France HPC, go to :ref:`MF-HPC-install`.

Meteo-France server (SXCEN for example), go to :ref:`MF-server-install`.

Other computers, go to :ref:`exterior-install`.


.. _MF-PC-cleaning:

Cleaning old snowtools installation (PC, server, HPC)
-----------------------------------------------------

CLEAN PYTHONPATH (PC, server, HPC)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
In your ``~/.bashrc`` file, you must **delete** the line

.. code-block:: bash
    
     export PYTHONPATH=$PYTHONPATH:$SNOWTOOLS_CEN

Please check your ``~/.bash_profile`` and ``~/.profile`` files in they exist.

You have to be sure this line is **deleted.**

USER environment (PC, server)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
    python -m venv --system-site-packages ~/my_envs/snowtools_env
    source ~/my_envs/snowtools_env/bin/activate
    python $SNOWTOOLS_CEN/cenutils/install_snowtools.py -e
    deactivate

And that's it. Now, you have snowtools installed in your git repository ``~/all_git_repo/snowtools`` and a virtual environment associated ``~/my_envs/snowtools_env``

.. _MF-HPC-install:

Install Snowtools on Meteo France HPC
-------------------------------------
Sync Snowtools
^^^^^^^^^^^^^^
In order to install snowtools on Belenos, run this command **on your PC** to synchronise your snowtools repository:

.. code-block:: bash
    
    $SNOWTOOLS_CEN/cenutils/put snowtools belenos

Environment
^^^^^^^^^^^
As in the personal computer case, set an environment variable pointing to the snowtools repository in your Belenos ``~/.bashrc`` file:

.. code-block:: bash

   export SNOWTOOLS_CEN=/{path_to_snowtools_repository}/snowtools

And source this

.. code-block:: bash

   source ~/.bashrc

Start installation on Belenos
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. code-block:: bash
 
    cd $SNOWTOOLS_CEN
    module load python 3.12.12
    python -m venv --system-site-packages ~/my_envs/snowtools_env
    source ~/my_envs/snowtools_env/bin/activate
    python $SNOWTOOLS_CEN/cenutils/install_snowtools.py -e

Temporary step
^^^^^^^^^^^^^^
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

Package installation
^^^^^^^^^^^^^^^^^^^^
Install vortex-cen
.. code-block:: bash

    pip install --upgrade setuptools
    cd $SNOWTOOLS_CEN/vortex-cen
    pip install --no-build-isolation -e .

Vortex Configuration
^^^^^^^^^^^^^^^^^^^^
.. code-block:: bash

    mkdir ~/.vortex.d/
    cd ~/.vortex.d/
    ln -s $SNOWTOOLS_CEN/vortex-cen/configs/vortex_{server}.toml vortex.toml

.. note::
    What do we do of: you must load python version 3.10.12 or 3.12.12. (just for simulation launch, in .bashrc ?)
    Référence to installation of Vortex ? obsolete ?
    Add deactivate to finish (we activate and deactivate in each simulation)

That's it, now snowtools is installed on Meteo France HPC belenos.

.. _MF-server-install:

Install Snowtools on Meteo France server SXCEN
----------------------------------------------
In order to install snowtools on a remote server, sync your snowtools repository on this server:

.. code-block:: bash

    $SNOWTOOLS_CEN/cenutils/put snowtools sxcen

In order to always use the last version of python in SXCEN, add to your ``.bashrc`` file:

.. code-block::bash

    alias python=python3

Start installation on sxcen
^^^^^^^^^^^^^^^^^^^^^^^^^^^
.. code-block:: bash
 
    cd ~/all_git_repo/snowtools
    python -m venv --system-site-packages ~/my_envs/snowtools_env
    source ~/my_envs/snowtools_env/bin/activate
    python $SNOWTOOLS_CEN/cenutils/install_snowtools.py -e

.. note::
    How do the rest of the installation follow ?
    Total copy of belenos installation ? 



.. _exterior-install:

Install Snowtools out of Meteo France system
--------------------------------------------

Dependencies
^^^^^^^^^^^^
The whole project requires at least python 3.6 and classical scientific packages (``numpy``, ``netCDF4``...). Some specific parts of the code (especially tests, scores, documentation generation require an extended set of dependencies that are fully described in ``requirements.txt``. The vortex toolbox is also needed for run on Meteo-France super-computers.


Install dependencies with pip
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Most of the packages can easily be installed in a virtual environment with ``pip`` (for instance with ``pip install -r requirements.txt``). This is useless if you plan to then install snowtools with  ``pip``.

Only GDAL python binding need to be installed manually to be installed consistently with your installed ``libgdal-dev`` version. Please install before the system packages ``ligdal`` and ``libgdal-dev`` (or similar) and run: ``pip install GDAL==$(gdal-config --version) --global-option=build_ext --global-option="$(gdal-config --cflags)"``.

.. note::
    How do the rest of the installation follow ?
    More or less copy of Meteo France PC installation ? 


.. _virtual_env:

Few infos on virtual environments
---------------------------------