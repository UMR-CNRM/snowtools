:mod:`bronx.system` --- System related tools
============================================

.. automodule:: bronx.system
   :synopsis: System related tools

.. moduleauthor:: The Vortex Team & many contributors
.. sectionauthor:: The Vortex Team & many contributors
.. versionadded:: 1.2.0

.. autodata:: __all__


Sub-modules
-----------

* :mod:`bronx.system.cpus`: To get informations on CPUs.
* :mod:`bronx.system.hash`: Interface to the Python's secure hash algorithms.
* :mod:`bronx.system.interrupt`: Advanced signal catching.
* :mod:`bronx.system.memory`: To get informations on memory.
* :mod:`bronx.system.mf`: Meteo France specific system related tools.
* :mod:`bronx.system.numa`: To get informations on the platform's NUMA nodes.
* :mod:`bronx.system.unistd`: Utilities to work on file descriptors through the standard Linux C layer

NB: All of these sub-modules have to be imported manually (depending on your
needs).
