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

|

.. image:: https://i.ibb.co/93t9ZvS/visu-these-ange.png
    :alt: my-picture1
    :width: 800


Suggestion of shortcuts for belenos::

    in .bashrc

    alias golastdebug=$'cd /scratch/mtool/haddjeria/depot; lstxp=(`ls|sort -r`); cd "$lstxp";unset lstxp'
    alias golastabort=$'cd /scratch/mtool/haddjeria/abort; lstxp=(`ls|sort -r`); cd "$lstxp";unset lstxp;cd root/surfex_vortex_task/'
    alias golastspool=$'cd /scratch/mtool/haddjeria/spool; lstxp=(`ls|sort -r`); cd "$lstxp";unset lstxp;cd root/surfex_vortex_task/'


Suggestion of belenos prompt using starship bash prompt to get more info on simulation outputs::

  in starship.toml

  [custom.xpid]
  detect_files=["step.01"]
  command="grep 'xpid' step.01| cut -c 19-"
  format = "xpid:[$output](250)"

  [custom.elapsedtime]
  detect_files=["step.04.out"]
  command="grep 'Elapsed time' step.02.out | cut -c 24-32"
  format = " time:[$output](250)"

.. image:: https://i.ibb.co/nM5F9LK/Capture-d-cran-2024-06-21-11-33-51.png
    :width: 800
