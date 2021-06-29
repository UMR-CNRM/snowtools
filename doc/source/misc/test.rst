.. _sec-test:

Testing
=======

A developement with no test will be broken in future evolutions and is impossible to maintain. Test are therefore necessary to ensure the maintainability and perennity of the code.

In snowtools, case tests have to be added to the ``test`` directory for each significant development.

To run the tests after a new developement. To do so, you can run any ``test_*`` file in the ``test`` folder or use the unittest discovery tool, running ``pyhton3 -m unittest``.
For python2 tests, run ``python2 -m unittest discover``.
