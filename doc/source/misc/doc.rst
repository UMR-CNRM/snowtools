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

Note that this generation will require some additional python packages to be installed and a correct installation of vortex as some parts of snowtools are in closely linked to vortex resources.


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

1. Add script path to doc/scripts_list.txt (file path + a unique rst filename used hereafter and denoted ``<fn>``)
2. Add an include directive where you want in the documentation pointing to ``code/autoscripts/<fn>``
