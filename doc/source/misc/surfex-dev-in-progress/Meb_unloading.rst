1-MEB Unloading documentation
============================================================

Basic information
-----------------

* **Developer name** : Axel Bouchet
* **Status of the development** : [Finished:Beta]
* **Date of start of development** : 02/2022
* **Date of end of development** : 08/2022
* **Commit and tag before development** : a3c09c5e
* **Commit and tag after development** : 676f9308 (no tag)
* **Branches on which the developpment is present** : [cen_dev/neige-foret-axel]
* **Evaluated against SURFEX test database ?** : [No]
* **New test added to database ?** :  [Yes]

Description of the development
------------------------------

This configuration was developed in order to better describe snow-vegetation interactions in temperate midlatitude climates.

It was initially produced in order to improve snow amount simulations (height and SWE) at the Col de Porte experimental site.
You can try this parameterization for any other site, even in cold climates like boreal forests (but we need more validations to be sure it makes no major degradation in this kind of vegetation).

All tests were done with the four flags at the same time: no test for separate flags

Changes in namelist
-------------------
.. code-block:: bash

   &NAM_MEB_ISBA
       LMEB_TALL_VEG        = .T.,
       LMEB_INT_PHASE_LUN   = .T.,
       LMEB_INT_UNLOAD_LUN  = .T.,
       LMEB_INT_UNLOAD_SFC  = .T.,
       
   &NAM_ISBA
       LMEB = .T.,


Other technical changes
-----------------------

Commit 676f930859 is the integration of all these developments in cen_dev branch. The step by step modifications are in neige-foret-axel branch with only these developments.

User documentation
------------------

**LMEB_TALL_VEG** (bool) : enable this key to use vegetation height as a major variable to calculate maximum snow load on trees and turbulent fluxes. It will mainly increase evaporation and sublimation mass loss on tree branches.

**LMEB_INT_PHASE_LUN** (bool) : enable this key if you want to use the [Lundquist et al., 2021] intercepted snow melt formulation. When you set this key at True, the snow intercepted by the trees (= the snow which is ON the tree branches) melts faster (4kg.m-2.K-1.jour-1) than using the classical config.

**LMEB_INT_UNLOAD_LUN** (bool) : enable this key if you want to use the snow unloading scheme of [Lundquist et al., 2021] (calibration of the [Roesh et al., 2001] scheme). This scheme is globally slowing the solid unloading, which favors snowmelt and sublimation of the intercepted snow as it stays a bit longer on the branches.

**LMEB_INT_UNLOAD_SFC** (bool) : enable this key in order to separate snow unloading from snowfalls in Crocus fresh snow incorporation. When the key is set at True, snow unloading will be included into the Crocus snowpack as “old” snow, with properties of melt forms and a density of 200kg.m-2 .

The simple way to test this development is to enable all keys.

Once again: THIS HAS NOT BEEN TESTED EXHAUSTIVELY (no test for separate flags)


