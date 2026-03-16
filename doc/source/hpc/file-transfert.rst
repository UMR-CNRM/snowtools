File transfers with hendrix archive and sxcen
---------------------------------------------

When a simulation is launched on belenos or taranis, Vortex automatically downloads input files from hendrix or the belenos/taranis cache (/scratch/mtool/<yourname>/cache/vortex/), and uploads the output of the simulations on hendrix and the cache. To allow Vortex to use ftserv from belenos/taranis to hendrix, it is necessary to register your password by running the following command on belenos/taranis:

.. code-block:: bash

   ftmotpasse -h hendrix.meteo.fr -u <yourname>

The same procedure applies if you'd like to upload light output data to your laptop or any server other than hendrix, e.g. sxcen.cnrm.meteo.fr :

.. code-block:: bash

   ftmotpasse -h sxcen.cnrm.meteo.fr -u <yourname>

Do not forget to re-run these commands every time you change your password (as Météo-France asks you on a 6-month basis). Otherwise, the authentication for the transfer of outputs will fail.

File transfers to a local machine
---------------------------------

Make sure that your user creditals for hendrix are stored in your .netrc file (in the home directory). There should be a line like::

   machine hendrix.meteo.fr login <yourusername> password <yourpassword>

and that only the user has read and write permissions for the .netrc file::

   chmod og-rw .netrc

(removes read and write permissions for group and other).

That's it !