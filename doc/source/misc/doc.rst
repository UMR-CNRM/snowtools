.. _sec-doc:

Documentation
=============

The documentation is managed through `Sphinx <www.sphinx-doc.org>` documentation generator.

Where to find the documentation
-------------------------------

If you are reading these lines, you may have found the documentation. Anyway, snowtools documentation could be accessed in two ways:

* Get pre-generated documentation
* Generated through code source

Documentation access
^^^^^^^^^^^^^^^^^^^^
If you are at Meteo-France, Pre-generated documentation is accessible on http://intra.cnrm.meteo.fr/cen/snowtools/. Otherwise, please ask the snowtools team where you can access this documentation.

Generate documentation
^^^^^^^^^^^^^^^^^^^^^^
Documentation can be generated directly from source repository. It could be generated in two formats : HTML and PDF.

To do so, go to the ``doc`` folder and use ``make <format>`` where ``<format>`` could be either ``html`` or ``pdf``.
The documentation will be generated in ``doc/_build/<format>``.

Note that this generation will require some packages to be installed:

 - all snowtools dependecies (this includes a correct installation of vortex)
 - Python 3.7 or later
 - ``sphinx``
 - The ``sphinx-fortran`` extension (installable with ``pip install sphinx-fortran``)


Code documentation
------------------

Each modification or code addition must be documented through docstrings. In python code, it should be inserted between ``"""`` immediately after function or class definition. As it will be parsed by sphinx for generation documentation, you are highly encouraged to use the reST/sphinx documentation format. You can add unit tests through doctests (see the `corresponding documentation <https://www.sphinx-doc.org/en/master/usage/extensions/doctest.html>`_ for more details).

Note that functions or classes without docstring will not appear in this documentation.


Documentation format
^^^^^^^^^^^^^^^^^^^^

The docstrings will be arsed by Sphinx as reStructuredText format. More details on the format could be found on the links below:

* https://sphinx-rtd-tutorial.readthedocs.io/en/latest/docstrings.html
* https://thomas-cokelaer.info/tutorials/sphinx/docstring_python.HTML

Here is an example of documentation as it should take place in snowtools:

.. code-block:: python

   class MyClass(utils.prosimu):
       """This is a useful class for doing a lot of things
       It extends the :class:`utils.prosimu` class with
       very useful functions.
    
       :param fn: filename to read
       :type fn: path-like (str)
       :param n: an integer
       :type n: int
       """
    
       def __init__(self, fn, *args, n=0, **kwargs):
           super().__init__(fn, *args, **kwargs)
           self.n = n
    
       def useful_function(self, n):
           """
           My useful function. Do nothing.
    
           :param n: an input parameter
           :type n: int
           :return: always 0
           :rtype: int
           """
           return 0


Scripts documentation
^^^^^^^^^^^^^^^^^^^^^

Documentation for scripts is generated from ``-h`` option.

To add a documented script :

1. Add script path to doc/scripts_list.txt (file path)
2. Add an include directive where you want in the documentation pointing to ``code/autoscripts/<fn>`` where ``<fn>`` is the script file path with ``/`` replaced by ``--``

Fortran Documentation
^^^^^^^^^^^^^^^^^^^^^

Fortran documentation is done through `sphinx-fortran <https://sphinx-fortran.readthedocs.io>`_ add-on. Folder containing Fortran files are set in the ``doc/source/conf.py`` file and then it is possible to document a whole file with:

.. code-block:: rest
   
   .. f:autosrcfile:: ../path/to/file.f90

or specific elements (program, module, subroutine, function) giving their name (and possibly the module name):

.. code-block:: rest

   .. f:autoprogram:: progname

   .. f:autovariable:: modname

   .. f:autosubroutine:: [modname/]subrname

   .. f:autofunction:: [modname/]funcname

Note that elements included in programs, subroutine or functions are not documentd (e.g. subroutine defined in a ``CONAINS`` section of a program).
