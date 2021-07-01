:mod:`bronx.system.numa` --- This module provides informations on the NUMA partitioning of the CPUs
===================================================================================================

.. automodule:: bronx.system.numa
   :synopsis: This module provides informations on the NUMA partitioning of the CPUs

.. moduleauthor:: The Vortex Team & many contributors
.. sectionauthor:: The Vortex Team & many contributors
.. versionadded:: 1.7.0


Functions
---------

.. autofunction:: numa_nodes_info

Abstract Classes
----------------

.. autoclass:: NumaNodesInfo
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: NumaNodeInfo
   :show-inheritance:
   :members:
   :member-order: alphabetical

Concrete Implementations
------------------------

.. autoclass:: LibNumaNodesInfo
   :show-inheritance:
   :members:
   :member-order: alphabetical

Utility Classes
---------------

.. autoclass:: LibnumaGateway
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: _NumaAbstractCpuIdDispencer
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: _NumaPackedCpuIdDispenser
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: _NumaBalancedCpuIdDispenser
   :show-inheritance:
   :members:
   :member-order: alphabetical
