:mod:`bronx.system.cpus` --- Module in charge of getting informations on CPUs
=============================================================================

.. automodule:: bronx.system.cpus
   :synopsis: Module in charge of getting informations on CPUs

.. moduleauthor:: The Vortex Team & many contributors
.. sectionauthor:: The Vortex Team & many contributors
.. versionadded:: 1.2.0


Functions
---------

.. autofunction:: get_affinity

.. autofunction:: set_affinity

Exceptions
----------

.. autoclass:: CpusToolUnavailableError
   :show-inheritance:
   :members:
   :member-order: alphabetical

Abstract classes
----------------

.. autoclass:: CpusInfo
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: CpuInfo
   :show-inheritance:
   :members:
   :member-order: alphabetical

OS-specific implementations
---------------------------

.. autoclass:: LinuxCpusInfo
   :show-inheritance:
   :members:
   :member-order: alphabetical
