:mod:`bronx.stdtypes.date` --- Time, date and calendars functions
=================================================================

.. automodule:: bronx.stdtypes.date
   :synopsis: Time, date and calendars functions

.. moduleauthor:: The Vortex Team
.. sectionauthor:: The Vortex Team
.. versionadded:: 0.1


Main Classes
------------

.. autoclass:: Date
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: Period
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: Time
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: Month
   :show-inheritance:
   :members:
   :member-order: alphabetical


Functions that generate list of dates or times
----------------------------------------------

.. autofunction:: daterangex

.. autofunction:: timerangex

.. autofunction:: timeintrangex


Generator function for timeseries of dates
------------------------------------------

.. autofunction:: daterange


Helper functions
----------------

.. autofunction:: now

.. autofunction:: utcnow

.. autofunction:: at_hour

.. autofunction:: at_minute

.. autofunction:: at_second

.. autofunction:: lastround

.. autofunction:: synop

.. autofunction:: today

.. autofunction:: tomorrow

.. autofunction:: yesterday

.. autofunction:: easter


Utility functions
-----------------

.. autofunction:: stardates

.. autofunction:: stamp

.. autofunction:: mkisodate

.. autofunction:: guess


Utility classes
---------------

.. autoclass:: _GetattrCalculatorMixin
   :show-inheritance:
   :members: __getattr__
   :member-order: alphabetical

.. autoclass:: TimeInt
   :show-inheritance:
   :members:
   :member-order: alphabetical
