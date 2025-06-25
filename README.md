# Snowtools

`snowtools` is a series of mostly python scripts that are designed to make our life simpler in terms of pre- and post-processing of SURFEX-Crocus snow model simulations.

`snowtools` is freely distributed under CeCILL-C licence. See [LICENCE](LICENCE.txt) for details

Note that this package is only useful for people interested in using numerical codes of snowpack modelling on a Linux environment. The package does not include any meteorological or snow data.

## Installation

For installation of this package, please refer to [Install documentation](https://umr-cnrm.github.io/snowtools-doc/misc/install.html)

## SURFEX

Snowtools is often used in combination with [SURFEX](http://www.cnrm-game-meteo.fr/surfex/spip.php) to run snow cover simulations. Snowtools only contain code for post- and pre-processing and driving simulations but no the core code of the Crocus snow cover model which is inside the SURFEX repository.

General informations about snowpack modelling with SURFEX-Crocus can be found here: <http://www.cnrm-game-meteo.fr/spip.php?rubrique73>

Users interested by snow modeling with SURFEX/Crocus model must follow detailed instructions below:

* [Installation of SURFEX](https://umr-cnrm.github.io/snowtools-doc/misc/surfex-install.html)
* [Run a SURFEX-Crocus simulation](https://umr-cnrm.github.io/snowtools-doc/misc/surfex-run.html)

### First test
If you correctly installed the snowtools and SURFEX projects, you must be able to run successfully the following test case:

```bash
s2m research -f $SNOWTOOLS_CEN/snowtools/DATA/FORCING_test_base.nc -b 20100801 -e 20110801 -o output -g -s ...yoursurfexdirectory.../exe
```

:warning: If you installed SURFEX with MPIAUTO option, you have to set the environment variable NOFFLINE to 1 to make this test work (because this test have only one simulation point).

## Plot tools

As SURFEX-Crocus output files are netCDF files, the visualization of scalar variables is possible with classical visualization tools for NetCDF files, for example :

- ncview
- pyncview

However, visualization of the simulated vertical profiles of the snowpack can be difficult due to the lagrangian discretization of Crocus numerical layers and requires specific softwares. `snowtools` propose two dedicated softwares:

- `proplotter`: a graphical user interface for plotting snow profiles evolution across simulations (point or gridded simulations, one point at each time).
- `procompare`: a graphical user interface to compare two simulations

These two softwares are installed with the snowtools repository.

## Full documentation

The full documentation of the packageis available at [https://umr-cnrm.github.io/snowtools-doc/](https://umr-cnrm.github.io/snowtools-doc/)

## Assistance

For users which meet technical difficulties during the installation or the execution of the codes, we only provide support through the dedicated interface on: [https://github.com/UMR-CNRM/snowtools-tickets](https://github.com/UMR-CNRM/snowtools-tickets). We will try to answer as soon as possible although we can not guarantee a fixed response time.

Note that the access to tickets is limited to known users of the SURFEX-Crocus or snowtools. If you do not already have an access, please ask for access by email to crocus at meteo dot fr.
