Short documentation on the Vortex toolbox
=========================================

Introduction
------------

**NB** "vortex" refers to the vortex-nwp Python library (often refered to as "vortex-2")

The full vortex documentation is available here : https://vortex-nwp.readthedocs.io/en/latest/index.html.
A Meteo-France/CNRM-specific documentation is also available here : https://cnrm-gmap.gitlab.meteo.fr/vortex-cnrm-docs/.
Vortex relies heavily on the "footprints" package : http://intra.cnrm.meteo.fr/algopy/sphinx/vortex/current/technical/footprints_fr.html

The main benefit of the use of vortex in the snowtools package is to standardise data management for snowpack simulation workflows.

Vortex offers features that allow users to store and share simulation I/Os transparently and optimally by uniformly fetching and writing data to and from an archive.
It also provides high-level tools for executing sequential and parallel programs with minimal user implementation.

Installing and configuring Vortex
---------------------------------

Installing snowtools automatically installs Vortex. Please follow the snowtools installation documentation to ensure a valid installation of Vortex and associated tools.
For an independent installation, follow the Vortex documentation.

..
    Votex data management
    ----------------------
    TODO

The vortex data tree
--------------------

Introduction
^^^^^^^^^^^^

The vortex data tree refers to the tree structure used by vortex to store data.
Vortex data tree documentation : https://vortex-nwp.readthedocs.io/en/latest/user-guide/data-layout.html
The following sections provides a sumary of this more comprehensive documentation and focuses on the main differences between the standard vortex data tree and the CEN-specific data tree.

The application (*vapp*) and configuration (*vconf*) levels
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The complementary notions of application and configuration are central in vortex.
An application, identified by the *vapp* footprint (type string), is an arbitrary name describing the first level of the vortex data tree. It encompasses all data produced for a specific purpose.
A configuration, identified by the *vconf* footprint (type string) is an arbitrary name describing the second level of the vortex data tree. It corresponds to a specific declination (set of scientific and technical choices) of an application.

The experiment identifier (*xpid*) level
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The *experiment* footprint (type string), commonly refered to as *xpid*, is an arbitrary user-defined tag for identifying all simulations associated to a given experiment. The *xpid* is the third level of the vortex data tree.

.. warning::
   4-digit *xpid* are saved for operational and OLIVE experiments, so **never use 4-digits xpid values**.

.. note::

   If you inadvertently used a 4-digit *xpid*, you will get get an error, with this kind of mysterious log message:

   # [2026/04/30-21:34:10][footprints.collectors][find_best:0383][INFO]: no.1 in.5 is <class 'vortex_olive.data.providers.Olive'>
   no.1 in.5 is <class 'vortex_olive.data.providers.Olive'>
   # [2026/04/30-21:34:10][footprints.collectors][find_best:0383][INFO]: no.2 in.5 is <class 'vortex.data.providers.Vortex'>
   no.2 in.5 is <class 'vortex.data.providers.Vortex'>
   ...
   ...
   ftplib.error_perm: 553 Directory does not exist

   # [2026/04/30-19:37:03][vortex.layout.dataflow][_fatal_wrap:0248][ERROR]: Resource ???
   Resource ???
   # [2026/04/30-19:37:03][vortex.layout.dataflow][_fatal_wrap:0250][CRITICAL]: Fatal error with action get on ???
   Fatal error with action get on ???


.. _datebegin_dateend:

Notions of *datebegin* / *dateend* vs *date* / *term*
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Snowpack simulations generally cover long time periods (typically entire years) in a single "run", and input / output files also cover the entire time period.
These periods are identified by the *datebegin* and *dateend* footprints (type "Date" or "YYYYMMDDHH" string or list of the previous types [#footprint_lists]_).
This is a major difference with the NWP-driven vortex data tree, which rather uses the notions of *date* (a model run time) and *term* (a model lead time).

Consequently, the *date* footprint, which is central in vortex and constitutes the fourth level of the vortex data tree, is optional for all objects defined in the vortex-cen plugin.

The *namebuild* footprint (type string, see section :ref:`namebuild`) allows to switch from a *date* / *term* data tree to a *datebegin* / *dateend* data tree. In the later case, the *datebegin* and *dateend* information appears at the file level (see :ref:`file_level`)

.. note::

   At CEN, the *date* footprint is only used for data deriving from a real-time / operational usage (when the simulation period depends on the date of the run).

.. rubric:: Footnotes
.. [#footprint_lists] When a list is provided to a footprint entry, vortex automatically iterates over all elements of the list.

The (optional) *member* level
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

In case of an ensemble simulation, the ensemble members are identifies by the **optional** *member* footprint (type int, list or 'start-end-step' format string). If provided, this information constitutes the next level of the data tree, with a "mbXXXX" format ("mbXXX" with a standard vortex name builder).

The *block* level
^^^^^^^^^^^^^^^^^

The *block* footprint (type string) is an arbitrary name typically used to group data files that are produced by the same unit task.

The *block* corresponds to the latest level of a vortex data tree.

.. note::

   The *block* footprint can be used to define several data tree levels if it contains the "/" character.

.. _file_level:

The file level
^^^^^^^^^^^^^^

The file level (the file name) contains several information coming from the footprints of the resource itself.

The resource's *kind*
"""""""""""""""""""""

The prefix of the file name is the resource *kind* footprint (type string) or any alias deriving from it.

.. _vortex_geometries:

The resource's *geometry*
"""""""""""""""""""""""""

The second level of the file name is derived from the *geometry* footprint (string or list of str type), which is a tag identifying a geometry object describing a given discretisation of space.
Standard NWP geometries are referenced in the vortex_nwp package, and additional "CEN"-specific geometries are references in the vortex-cen plugin.

Any "new" geometry not included in these standrd geometries must be provided in a ``geometries.ini`` file stored under $HOME/.vortexrc.
The *geometry* footprint is the geometry block/entry name. For example, *geometry='GrandesRousses250m'* identifies the following geometry :

.. code-block:: ini

    [GrandesRousses250m]
    info       = 250 m lat/lon regular grid
    kind       = lonlat
    area       = GrandesRousses

The minimum information to provide for the description of a geometry is the geometry *kind* ("ustructured" for massif-like geometries or "lonlat" for 2D simulations).

.. note::

    This *geometry* information in the file name is a major difference with the naming convention used at CEN before the migration to vortex-2:
    The *geometry* used to appear at the *vconf* level and not in the filename.

The resource's *datebegin* and *dateend*
""""""""""""""""""""""""""""""""""""""""

At CEN, file names contain the period covered by the resource as defined by the *datebegin* and *dateend* footprints (see section :ref:`datebegin_dateend`).

The resource's *model*
""""""""""""""""""""""

The *model* footprints is an arbitrary name used to indicate the name of the  model who produced the data (the last one in case of a model chain [#f1]_).

.. note::
   The *model* footprint should not be confused with the (optional) *source_app* and *source_conf* footprints

**NB** The proper use of the *model*, *source_app* and *source_conf* footprints at CEN remains to be clarified.

.. rubric:: Footnotes
.. [#f1] This needs further discussions

The resource's *format*
"""""""""""""""""""""""

Finaly, the suffix of the file name is defined by the *format* footprint (type string).

Additional footprints
^^^^^^^^^^^^^^^^^^^^^

The *local* or *filename* footprint
"""""""""""""""""""""""""""""""""""

The *local* (or its alias *filename*) footprint (type string) is the name of the target file in the user's working directory.

.. note::

   In case the target resource is stored in a sub-directory of the current workdir, the value to provide to the *local* footprint should be the relative path to the file :
   *local* = 'subdir/MyFile.txt'

.. _namebuild:

The *namebuild* footprint
"""""""""""""""""""""""""

The *namebuild* footprint (type string) defines the name-builder object to be used. Setting the *namebuild* footprint to 'cen' removes the *date* level of the data tree.

The *namespace* footprint
""""""""""""""""""""""""""

The *namespace* footprint (type string) defines the server(s) where the resources will be fetched or stored.
The most common values of the *namespace* footprint at CEN are:
* 'vortex.cache.fr'   : local server only
* 'vortex.archive.fr' : Archive server only (Hendrix)
* 'vortex.multi.fr'   : local server **and** archive (default value)

The *username* footprint
""""""""""""""""""""""""

To access files from another user, simply set the *username* footprint (type string) to this user's login name.
If no *username* is given (or it set to None), then your own username is used.

.. note::

    The *username* footprint is useless in the case of a toolbox.output(...)

The *role* footprint
""""""""""""""""""""

The *role* footprint (type string) can be used to access a specific loaded Resource Handler from anywhere in the subsequent code (even outside the method / class where the Resource Handler has been defined). In particular, the *role* value is used to identify alternative resources fulfilling the same function (see :ref:`alternate`).

.. admonition:: Example

   You can access the resource handler of FORCING files retrieved by an earlier call to a vortex.input(role='Forcing,...) with the following command:

   avail_forcings = t.context.sequence.effective_inputs(role='Forcing')

The *source_app* / *source_conf* footprints
"""""""""""""""""""""""""""""""""""""""""""

The optional *source_app* / *source_conf* footprints (type string) can be used to extend the file name in the vortex data tree with additional information on the application and configuration that originally produced the resource.

The *genv*, *gvar*, *source* and *domain* footprints
""""""""""""""""""""""""""""""""""""""""""""""""""""

The *genv*, *gvar*, *source* and *domain* footprints are used to describe a static (time independent) resource stored in a User Environment (independent from the vortex data tree):
* the *genv* footprint (type string, mandatory) is the name of the UEnv (or GEnv)
* the *gvar* footprint (type string, mandatory) is the key of the target resource in the UEnv / GEnv
* the *domain* footprint (type string, optional) is an optional information on the geographical domain of the target resource. The default value is the *area* of the associated geometry object.

The *fatal* footprint
"""""""""""""""""""""

The *fatal* footprint (type bool) defines the behavior in case of failure of a vortex.input/output request (crash if *fatal* = True, continue if *fatal* = False).

The *intent* footprint
""""""""""""""""""""""

The *intent* footprint (type string) allows to set the rights to give the target file in the working directory:
* *intent* = 'in' for a read-only file
* *intent* = 'inout' for a read-write file

.. _alternate:

The *alternate* footprint
"""""""""""""""""""""""""

The *alternate* footprint (type string) is mostly used in real-time application as an alternative / rescue input when the primary input is missing.
The value of the *alternate* footprint in the description of the alternative / rescue input must match exactly the value of the *role* footprint in the description of the primary input.

The *cutoff* footprint
""""""""""""""""""""""

The *cutoff* footprint (type string) is a (confusing) NWP-derived term to distinguish data coming from an analysis (*cutoff* = "assimilation", data from past potentially benefiting from the assimilation of observations) or a forecast (*cutoff* = "production").
The *cutoff*  footprint is optional for the "cen" namebuilder, but is mandatory for the standard vortex namebuilder (it appears as a letter at the *date* level, with a format yyyymmddThhmm[**AP**]).


Get data archived with vortex at CEN
------------------------------------

As mentioned in the introduction, Vortex provides simulation IOs manipulation tools.
Getting data archived with Vortex requires to provide a valid footprint description of the target file(s).
Vortex provides the `vtx` command line to fetch (and) store data to/from the vortex data tree from a description writen in a YAML configuration file :
https://vortex-nwp.readthedocs.io/en/latest/user-guide/cli.html

An example of such a yaml configuration file to extract a specific file from the S2M reanalysis is provided : vortex_cen/conf/S2MReanalysis.yaml

Extracting this data can simply be done with :

.. code::

   vtx get S2MReanalysis.yaml

However, the use of the `vtx` tool to extract the entire S2M dataset is not straightforward due to the annual data storage convention.

More broadly, using data extractors means that the fetched files are duplicated from the vortex data tree into the user's working directory and become "wild" files.
This results in the loss of the benefits to use vortex in the first place.
In addition, the duplication of potentialy large dataset can lead to very sub-optimal data management.

Most snow-related simulation IOs are NetCDF files opened with xarray and its extensions provided by the snowtools package (see :ref:`xarray` for more information).
Among those extensions, a `open_vortex_data` wrapper is provided to fetch files from the vortex data tree and read them with xarray in a single python command.

The typical use of this wrapper would be to write the vortex/footprint description of the target data in a configuration file. For example:

.. code::ini

   # description.ini

   [DEFAULT]
   vapp=s2m
   vconf=reanalysis
   datebegin=2024080106
   dateend=2025080106
   experiment=release_2026.1
   username=vernaym
   duration=yearly
   kind=PRO
   block=pro
   geometry=cor2_flat

And call the `open_vortex_data` with the absolute or relative path to this configuration file to the "configfile" keyword argument.

.. code-block:: python

    from snowtools.utils.xarray_snowtools import open_vortex_data

    with open_vortex_data(configfile='description.ini') as ds:
        print(ds)
        # `ds` is an xarray.Dataset containing the target data
        # Do any data manipulation here :
        # * apply native xarray methods to `ds`
        # * apply xarray_snowtools_accessor methods to `ds`
        # * code block
        # * call external functions with ds as argument
        # ...

This workflow enables all file transfer and opening operations to be completed discreetly in the background, allowing you to focus on the data itself.

    

..
    Parallelisation management with vortex
    --------------------------------------
    TODO

Vortex glossary
---------------

**footprints** The manipulation of vortex objects relies heavily on the "footprints" python package. This package is designed to identify objects (python classes) from a keyword description (a python dictionary, the "footprint" of the target object) instead of a direct python import of the target class.

**Resources** refer to vortex objects encompassing the information related to a given type of file (see :ref:`file_level`).
*Flow resources* refer to all time-dependent resources  (for example "FORCING" or "PRO" files).
*Static resources* refer to time-indepent resources (for example Namelists, Executables, Digital Elevation Models,...)

**Providers** refer to vortex objects encompassing the information on the location of a given *resource* in the vortex world.

**Containers** refer to vortex objects encompassing the information on the location of a given file in the user's working directory.

**Resource Handlers** consist of an association of a provider, a resource and a container. They establish a bridge between the vortex and the user worlds.

**algo components** refer to the classes outlying the core algorithm to produce a given set of output files from a given set of input files.





