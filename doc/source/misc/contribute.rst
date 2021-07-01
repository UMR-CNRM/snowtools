.. _sec-contribute:

Contribute to snowtools
=======================

Developer environment
---------------------

To contribute to snowtools, you must :

* use the **git** versioning tool 
* use PyCharm as code editor

To add the git project to PyCharm directly, click on ``VCS``, then ``checkout from version control`` and then ``git``.

Code style
----------

Python code should comply with PEP8 style guide. Please check your code before committing.
In particular, tabs are prohibited and lines may not exceed 120 characters.

Code tips
---------

Shebangs
^^^^^^^^

If you are writing a script that can be executed, please add the following shebang. Note that you should not point to ``python`` as it may not be defined on all platforms, hence you have to point to ``pyhton3`` (or ``python2`` if absolutely needed).

.. code-block:: python
   
   #!/usr/bin/env python3

Moreover, **all** codes must specify the encodign, which have to be ``utf-8``:

.. code-block:: python
   
   # -*- coding: utf-8 -*-

Imports
^^^^^^^

Please avoid relative imports in snowtools. Absolute imports must start at ``snowtools`` package level. For instance, to import the ``prosimu`` class from everywhere, write:

.. code-block:: python
   
   from snowtools.utils.prosimu import prosimu


Documentation and testing
-------------------------

Each modification or code addition must be clearly documented in a docstring so that it could be included in this documentation. Please refer to :ref:`sec-doc` section for the main guidelines.

In addition, each new development should come with tests (unit tests and/or test cases). See :ref:`sec-test` section for main guidelines.
