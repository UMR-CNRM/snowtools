.. Author: Ange Haddjeri
.. Date: 2024

Documentations from Ange's PhD (Blowing snow)
=============================================

Below you can find a documentation (part of it formatted from my Jupyter notebooks) with the main information to reproduce my PhD simulations.

| Have fun with all of this!
| Cheer,

Ange

.. toctree::
   :maxdepth: 1

   Build snow simulation from scratch and modify wind forcings for blowing snow <Forcing_modification.rst>
   Pleiades observation <Pleiades.rst>
   Theia Snow observations <Sentinel_2.rst>
   Masks used in the experiments <Masks.rst>
   Methods for simulation and PGD regridding <Regridding.rst>
   Available User Environments (UEnv) <uenv.rst>

|

.. image:: https://i.ibb.co/93t9ZvS/visu-these-ange.png
    :alt: Snow cover over Grandes Rousses
    :width: 100%


Additional information :
########################

.. warning::
   This is not recommended !

| How to connect to a Jupyter Notebook running on a Belenos computing nodes ?
| (to avoid spending your life on Belenoslogin like I did)

1. Connect to belenos and ask for a node in interactive mode

  :file:`salloc`

  |    salloc: Pending job allocation 42337169
  |    salloc: job 42337169 queued and waiting for resources
  |    salloc: job 42337169 has been allocated resources
  |    salloc: Granted job allocation 42337169
  |    salloc: Waiting for resource configuration
  |    salloc: Nodes belenos284 are ready for job

2. Connect to node

  :file:`ssh belenos284`

3. Start Notebook (will open port 8888 by default)

  :file:`jupyter lab`

4. In a NEW terminal send the folowing command to foward ports from belenos284:8888 to your local computer localhost:8000 while bouncing on belenoslogin0.

  :file:`ssh -NL 8000:localhost:8888 -J belenoslogin0 belenos284`

5. Dont forget to :file:`scancel` your node allocation once the job is done. :)


Suggestion of shortcuts I used for belenos::

    in .bashrc

    alias golastdebug=$'cd /scratch/mtool/haddjeria/depot; lstxp=(`ls|sort -r`); cd "$lstxp";unset lstxp'
    alias golastabort=$'cd /scratch/mtool/haddjeria/abort; lstxp=(`ls|sort -r`); cd "$lstxp";unset lstxp;cd root/surfex_vortex_task/'
    alias golastspool=$'cd /scratch/mtool/haddjeria/spool; lstxp=(`ls|sort -r`); cd "$lstxp";unset lstxp;cd root/surfex_vortex_task/'

