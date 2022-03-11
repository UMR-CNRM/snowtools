:mod:`bronx.fancies.loggers` --- Helper function for the logging module
=======================================================================

.. automodule:: bronx.fancies.loggers
   :synopsis: Helper function for the loggign module

.. moduleauthor:: The Vortex Team
.. sectionauthor:: The Vortex Team
.. versionadded:: 1.5.0


Data definition
---------------

.. autodata:: __all__

.. autodata:: roots

.. autodata:: default_console


Main Interface functions
------------------------

.. autofunction:: getLogger

.. autofunction:: setGlobalLevel

.. autofunction:: contextboundGlobalLevel

.. autofunction:: fdecoGlobalLevel

.. autofunction:: unittestGlobalLevel

Other Interface functions
-------------------------

.. autofunction:: setRootLogger

.. autofunction:: setLogMethods

.. autofunction:: getActualLevel


Descriptors classes
-------------------

.. autoclass:: PromptAwareLoggingFilter
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: SlurpHandler
   :show-inheritance:
   :members:
   :member-order: alphabetical
