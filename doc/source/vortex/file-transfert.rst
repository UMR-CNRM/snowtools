File transfers with Vortex
--------------------------

The Vortex tool allows to transfer files between PC, servers and archives in a transparent and optimal way (see https://vortex-nwp.readthedocs.io/en/latest/tutorial/fetch.html for more information).
To allow Vortex to use the ftserv tool designed to transfer data between MF's servers, it is necessary to register your password by running the following command :

.. code-block:: bash

   ftmotpasse -h <server-name>.meteo.fr -u <yourname>

For example, to enable data transfer to/from the Hendrix archive:

.. code-block:: bash

   ftmotpasse -h hendrix.meteo.fr -u <yourname>

The same procedure applies if you'd like to transfer data from/to your laptop or any server other than hendrix, e.g. sxcen.cnrm.meteo.fr :

.. code-block:: bash

   ftmotpasse -h sxcen.cnrm.meteo.fr -u <yourname>

Do not forget to re-run these commands every time you change your password (as Météo-France asks you on a 6-month basis). Otherwise, the authentication for the transfer of outputs will fail.

.. note::

    Make sure that your user creditals for hendrix are stored in your .netrc file (in the home directory). There should be a line like::

       machine hendrix.meteo.fr login <yourusername> password <yourpassword>

    and that only the user has read and write permissions for the .netrc file::

       chmod og-rw .netrc

    (removes read and write permissions for group and other).

That's it !
