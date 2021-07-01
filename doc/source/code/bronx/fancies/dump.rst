:mod:`bronx.fancies.dump` --- Dumper for python structures and objects
======================================================================

.. automodule:: bronx.fancies.dump
   :synopsis: Dumper for python structures and objects

.. moduleauthor:: The Vortex Team
.. sectionauthor:: The Vortex Team
.. versionadded:: 1.5.0

Interface
---------

.. autofunction:: get

.. autofunction:: fulldump

.. autofunction:: lightdump

Abstract Dumper class
---------------------

.. autoclass:: _AbstractDumper
   :show-inheritance:
   :members: reset, dump, cleandump
   :member-order: alphabetical

Actual Dumper class
-------------------

.. autoclass:: JsonableDumper
   :show-inheritance:
   :members: reset, dump, cleandump
   :member-order: alphabetical

.. autoclass:: XmlDomDumper
   :show-inheritance:
   :members: reset, dump, cleandump
   :member-order: alphabetical

.. autoclass:: TxtDumper
   :show-inheritance:
   :members: reset, dump, cleandump
   :member-order: alphabetical

.. autoclass:: OneLineTxtDumper
   :show-inheritance:
   :members: reset, dump, cleandump
   :member-order: alphabetical

Utility functions
-----------------

.. autofunction:: is_an_instance

.. autofunction:: is_class
