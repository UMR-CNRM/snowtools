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

Python code should comply with PEP8 https://www.python.org/dev/peps/pep-0008 style guide. Please check your code before committing.
In particular, tabs are prohibited and lines may not exceed 120 characters.

Code tips
---------

Shebangs
^^^^^^^^

If you are writing a script that can be executed, please add the following shebang. Note that you should not point to ``python`` as it may not be defined on all platforms, hence you have to point to ``python3``.

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

See PEP 8 import section https://www.python.org/dev/peps/pep-0008/#imports for more details on correct import order and other guidelines on imports.

Of course star-imports (``from XXX import *``) are prohibited.


Documentation and testing
-------------------------

Each modification or code addition must be clearly documented in a docstring so that it could be included in this documentation. Please refer to :ref:`sec-doc` section for the main guidelines.

In addition, each new development should come with tests (unit tests and/or test cases). See :ref:`sec-test` section for main guidelines.

Git commits
-----------

For both snowtools and SURFEX projects, all developments must be done in seperated git branches with frequent commit to ensure traceability of your modifications and the ability to include in your own branch the main bugfixes and developments of the main branches.

We recoment to commit only when :

- You have a state of the code that is self-sufficient (not work in progress that could not be used yet)
- Tests are passing
- You have in your commit only changes on one topic (and not mixed changes that are unrelated)

Each commit must contain a clear message that include in few words the part of the code that is affected and the reason of the commit. The commit message is composed of a title (1st line) that summarize the changes and could be complemented by more detailed description of the changes.

Developpement branches must start from the ``dev`` branch and be named ``dev_<project>`` where ``<project>`` is the name of the project or obviously describe what is inside the branch.

Language
--------

Of course, all comments in the code, documentation and commit message have to be in english.

SURFEX
======

SURFEX developements must follow the specific coding rules of this project: http://www.cnrm.meteo.fr/surfex-lab/spip.php?rubrique93
Contributions will not be accepted if they do not follow these requirements.
