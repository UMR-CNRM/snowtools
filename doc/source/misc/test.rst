.. _sec-test:

Testing
=======

A development with no test will be broken in future evolutions and is impossible to maintain. Test are therefore necessary to ensure the maintainability and perennity of the code.

Please don't forget that tests are useful in both situations:
* running the test to check that your code is not breaking something else
* adding a test to protect your developments

Running the test
----------------

To run the tests after a new developement. To do so, you can run any ``test_*`` file in the ``test`` folder or use the unittest discovery tool, running ``python3 -m unittest``.
For python2 tests, run ``python2 -m unittest discover``.

When running tests, your environment must:

 * Have a working SURFEX install (compiled, with ``OFFLINE``, ``PREP`` and ``PGD`` executable in the ``exe`` folder)
 * Have defined ``EXESURFEX`` environment variable pointing to the ``exe`` folder of your SURFEX repository
 * Have python package described on :ref:`sec-install` installed on the python version you plan to run tests.


Adding a test
-------------

In snowtools, case tests have to be added to the ``snowtools/test`` directory for each significant development.
Please note that only test files runnable with unittest are allowed to contain ``test`` in their names.
