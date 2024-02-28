.. _sec-test:

Testing
=======

A development with no test will be broken in future evolutions and is impossible to maintain. Test are therefore necessary to ensure the maintainability and perennity of the code.

Please don't forget that tests are useful in both situations:
* running the test to check that your code is not breaking something else
* adding a test to protect your developments

Running the test
----------------

To run the tests after a new developement. To do so, you can run any ``test_*`` file in the ``test`` folder or just run ``python3 -m unittest`` or ``python3 -m pytest``.

When running tests, your environment must:

 * Have a working SURFEX install (compiled, with ``OFFLINE``, ``PREP`` and ``PGD`` executable in the ``exe`` folder)
 * Have defined ``EXESURFEX`` environment variable pointing to the ``exe`` folder of your SURFEX repository
 * Have python package described on :ref:`sec-install` installed on the python version you plan to run tests.


Adding a test
-------------

In snowtools, case tests have to be added to the ``snowtools/test`` directory for each significant development.
Please note that only test files runnable with unittest are allowed to contain ``test`` in their names.

Test of assimilation part
-------------------------

The assimilation tool can be tested with :

.. code-block:: bash

   cd $SNOWTOOLS_CEN/snowtools/tests/test_soda
   ./test_soda.sh

You should see ``SODA ENDS CORRECTLY`` at the end.

Manual tests for s2m with vortex
--------------------------------

The success of the following commands must be checked before a new code release :

.. code-block:: bash

    # Reanalysis test case:
    s2m research -r alp_allslopes -b 20220801 -e 20230801 -m safran -f reanalysis2020.2 -o reanalysis_test -n snowtools_git/snowtools/DATA/OPTIONS_V8.1_NEW_OUTPUTS_NC_reanalysis.nam

    # ESCROC test case:
    s2m research -r cdp -b 1994100101 -e 2014100100 -x 2014100100 -m ESM-SnowMIP -f obs@lafaysse -o E2_test --task=escroc --escroc=E2

    # Stochastic perturbation test case:
    s2m research -r cor_flat -b 20200801 -e 20210801 -m s2m -f reanalysis2020.2 -o perturb --task='croco_perturb' --nmembers=80

    # Croco test case:
    s2m research -r postes_12_csv -b 2013080106 -e 2014063006 -x 20160801 -m safran -f forcing_20132014B_31D_11_t1500_160@fructusm -o test0l -n ~lafaysse/croco/OPTIONS_MOTHER_DEP.nam --task='croco' --croco='real' --escroc=E1notartes --nmembers=35 --nforcing=35 --conf=/home/lafaysse/croco/conf.ini -s ~lafaysse/SURFEX/cen/exe_mpi --obsxpid=obs@lafaysse --sensor=bdclim

    # Operational analysis and forecast:
    s2m oper -b YYYYMMDD03 -r alp
    s2m oper -b YYYYMMDD03 -r alp --task='forecast'

    # Building of reforecast initial conditions test case:
    s2m research -r vog3_allslopes -b 20000801 -e 20010801 -a 400 -m s2m -f reanalysis_era5.2023 -p reanalysis_era5.2023 -o initialconditions_test -n snowtools_git/snowtools/DATA/OPTIONS_V8.1_NEW_OUTPUTS_NC_reanalysis_forprep.nam

    # Reforecast test case
    s2m research -b 20000302 -e 20000327 -r vog3_allslopes -n /home/cnrm_other/cen/mrns/lafaysse/snowtools_git/snowtools/DATA/OPTIONS_reforecast.nam --task='reforecast' -m safran -f reforecast_2023 --nmembers=11 -p
