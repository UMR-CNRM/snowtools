:mod:`bronx.syntax.minieval` --- Parse, check and execute single-line Python's statements
=========================================================================================

.. automodule:: bronx.syntax.minieval
   :synopsis: Parse, check and execute single-line Python's statements

.. moduleauthor:: The Vortex Team & many contributors
.. sectionauthor:: The Vortex Team & many contributors
.. versionadded:: 1.6.1


Public object
-------------

.. autodata:: safe_eval

Exceptions
----------

.. autoclass:: SingleLineStatementError
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: SingleLineStatementParsingError
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: SingleLineStatementSecurityError
   :show-inheritance:
   :members:
   :member-order: alphabetical

.. autoclass:: SingleLineStatementEvalError
   :show-inheritance:
   :members:
   :member-order: alphabetical

Classes
-------

Main Classes
************

.. autoclass:: SingleLineStatement
   :show-inheritance:
   :members: check, __call__
   :member-order: alphabetical

AST NodeVisitor classes
***********************

.. autoclass:: SafetyCheckNodeVisitor
   :show-inheritance:
   :members:
   :member-order: alphabetical
