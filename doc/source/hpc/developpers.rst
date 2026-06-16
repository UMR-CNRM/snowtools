Documentation for HPC task developpers
======================================

.. _minimal_example:

Tutorial : creation of a minimal unit task
------------------------------------------

Writing a unit task
^^^^^^^^^^^^^^^^^^^

This example illustrates the creation of a "task" with :
* one input file (a FORCING file)
* a simple piece of python code (without any explicit parallelisation) extracting a sub-period forom the input forcing file
* the backup of the generated forcing file over the prescribed sub-period

1. create a class deriving from "research_task_base" (itself deriving from a "task" object of the mkjob package) :

.. code-block:: python

  # -*- coding: utf-8 -*-

  from vortex_cen.tasks.research_task_base import _CenResearchTask
  import vortex


  class ExtractSubPeriod(_CenResearchTask):

2. implement the "get_remote_inputs" method with the input file and set the "local" footprint with the desired file name on the working directory (here "FORCNG_IN.nc")

.. code-block:: python

  def get_remote_inputs(self):

      self.sh.title('Input FORCING')  # Give an explicit name to find this resource easily in the log
      input_forcing = vortex.input(
          kind        = 'MeteorologicalForcing',
          local       = 'FORCING_IN.nc',  # Name of the file in the working directory
          experiment  = self.conf.get('forcing_xpid', self.conf.xpid),
          username    = self.conf.get('forcing_user', None),  # Default set to $USER by vortex
          geometry    = self.conf.get('forcing_geometry', self.conf.geometry),
          datebegin   = self.conf.get('forcing_datebegin'),  # Must be different than self.conf.datebegin in this example
          dateend     = self.conf.get('forcing_dateend'),  # Must be different than self.conf.dateend in this example
          block       = self.conf.get('forcing_block'), 'meteo'),
          nativefmt   = 'netcdf',
          namespace   = 'vortex.multi.fr',
          namebuild   = 'flat@cen',
      ),
      print(self.ticket.prompt, 'input forcing =', input_forcing) # display the result nicely in the log
      print()

.. note::

    Naming the variable returned by the vortex.input/output command (here "input_forcing") is optional most of the time, it is used only to display the result of the command nicely in the log.

3. implement the "get_local_inputs" method with resources to get on HPC exclusively (doing nothing in this example) :

.. code-block:: python

  def get_local_inputs(self):
      pass

4. implement the "algo" method with your python script producing a "FORCING_OUT.nc" file covering a sub-period of time :

.. code-block:: python

  def algo(self):
      import xarray as xr
      from snowtools.utils import xarray_snowtools
      ds = xr.open_dataset('FORCING_IN.nc', engine='snowtools')
      shorter_forcing = ds.sel(time=slice(self.conf.datebegin, self.conf.dateend))
      out.to_netcdf("FORCING_OUT.nc", format='NETCDF4_CLASSIC')

5. implement the "launch_algo" method (to do nothing in this example) :

.. code-block:: python

  def launch_algo(self, algo):
      pass

6. implement the "put_outputs" method to archive the "FORCING_OUT.nc" file :

.. code-block:: python

  def put_outputs(self):
      self.sh.title('Output FORCING')
      output_forcing = vortex.output(
          kind        = 'MeteorologicalForcing',
          local       = 'FORCING_OUT.nc',  # Name of the file produced by the script
          experiment  = self.conf.xpid,  # By convention the output "experiment" is always self.conf.xpid
          datebegin   = self.conf.datebegin,
          dateend     = self.conf.dateend,
          geometry    = self.conf.get('forcing_geometry', self.conf.geometry),  # Must be the same as the input geometry in this example
          block       = 'meteo',  # Force output block at "meteo" (arbitrary choice)
          nativefmt   = 'netcdf',
          namespace   = 'vortex.cache.fr',  # Do not archive the output file on Hendrix in this exemple (duplicated data)
          namebuild   = 'flat@cen',
      ),
      print(self.ticket.prompt, 'Output forcing =', output_forcing)
      print()

The task is now ready, you can launch it by following the instructions of section :ref:`launch_a_unit_task`


.. _configuration_variables:

Configuration variables
^^^^^^^^^^^^^^^^^^^^^^^

Introduction
""""""""""""

As illustrated in section :ref:`minimal_example`, the IOs of a unit task are simply a sequence of call to the vortex.input() and vortex.output() methods.
These methods expect a specific "footprint" description (a dictionnary) of the target resource file. The values of these dictionnaries can either be a fixed value or a variable one.
The variable values can/must be defined at runtime by the user through the mkjob command-line variables or a configuration file. These variables are transfered to the task through to a dictionary class variable named "conf".
The value of a user-defined "varname" variable can thus be accessed in the task with self.conf.varname.

Naming convention
"""""""""""""""""

Since some footprint "keys" are common for all resources (*experiment*, *geometry*, *datebegin*, *dateend* and so on). By convention, the associated configuration variables should be named "ressource_key" for input resources, as illustrated in section :ref:`minimal_example`.

Other configuration variables
"""""""""""""""""""""""""""""

It is also possible that the python script in the "algo" method expect some user-defined variable values. The values are specific to the script and do not follow any naming convention.
For example, the "algo" of the example above could only select one specific date with a variable named "validity_time" :

.. code-block:: python

  def algo(self):
      import xarray as xr
      from snowtools.utils import xarray_snowtools
      ds = xr.open_dataset('FORCING_IN.nc', engine='snowtools')
      out = ds.sel(time=self.conf.validity_time)
      out.to_netcdf("FORCING_OUT.nc")

.. _minimal_task_doc:

Documentation of the task
^^^^^^^^^^^^^^^^^^^^^^^^^

This section illustrates how to document the minimal unit task described in section :ref:`minimal_example`.
The general documentation of the task should contain a short description of the task and the list of input and output files.

The list of mandatory and optional configuration variables must be documented with the "MANDATORY_CONFIGURATION_VARIABLES" and "OPTIONAL_CONFIGURATION_VARIABLES" class attributes.

.. note::

   A variable should be considered "mandatory" if not providing it leads the task to crash (any call to self.conf.something).
   Optional variables can have default values (for example self.conf.get('something', 'default_value')) or not (if 'something' in self.conf)


The configuration variable documentation must include an "help" message and the variable "type".
It is also possible to provide:

* a "format" in case a specific format is required
* a list of possible "choices"
* a "default" value for optional variables

The documentation of the example task of section :ref:`minimal_example` would look like :

.. code-block:: python

   class ExtractSubPeriod(_CenResearchTask):
       """
       Extract a sub period in a Forcing file

       Inputs:
       --------
       - FORCING file

       Outputs:
       ---------
       - FORCING file on a shorter period (but same geometry)
       """

       MANDATORY_CONFIGURATION_VARIABLES = [
           "xpid+help=Experiment identifier;type=str",
           "forcing_datebegin+help=Begin date of the input forcing file;type=str or Date",
           "forcing_dateend+help=End date of the input forcing file;type=str or Date",
           "datebegin+help=Begin date of the output forcing file;type= str or Date",
           "dateend+help=End date of the output forcing file;type=str or Date",
           "forcing_geometry|geometry+help=The input and output forcing geometry;type=str",  # Both "geometry" or "forcing_geometry" can be provided with the same result
       ]
       OPTIONAL_CONFIGURATION_VARIABLES = [
           "forcing_xpid+help=Experiment identifier of the input forcing;type=str;default=The current experiment identifier",
           "forcing_user+help=Name of the producer of the input forcing;type=str;default=$USER",
           "forcing_block+help=*block* level of the input forcing;type=str;default=meteo",
       ]

For more information on the documentation of configuration variables, see section :ref:`dynamic_documentation`.

.. _launch_a_unit_task:

Launching a unit task
^^^^^^^^^^^^^^^^^^^^^

In order to launch a unit task, you first need to choose the arbitrary application (*vapp*) and configuration (*vconf*) levels.

.. note::

  A same task can appear in several applications / configurations.


Then, you must include the task into a "driver" of tasks.
To do so, add the following method in the task module itself or a new module: **in a vapp/vconf/drivers/ directory structure:**

.. code-block:: python

  # vortex_cen/*vapp*/*vconf*/drivers/example.py

  from mkjob.nodes import Driver

  def setup(t, **kw):
      return Driver(
          tag    = 'example',
          ticket = t,
          nodes  = [
              ExtractSubPeriod(tag='extract_sub_period', ticket=t, **kw),
          ],
          options = kw,
      )

You also need to provide a configuration file containing the configuration variables values.
This configuration file should at least have a "DEFAULT" section, but it is recomended to add a "extract_sub_period" section ("extract_sub_period" being the "tag" of the task in the driver above).
In addition, an optional job configuration can be provided to set the job partition, walltime, number of nodes,...
This job configuration should be placed in a specific section (for example "extract_sub_period_job").

In the example of section :ref:`minimal_task_doc`, to extract the month of august 2020 from the s2m reanalysis over Corsica in the "allslopes" geometry, this configuration file ("test.ini") could look like :

.. code-block:: ini

   # vortex_cen/*vapp*/*vconf*/conf/test.ini

   [DEFAULT]

   [extract_sub_period_job]
   walltime = 0:01:00
   partition = shared
   mem = 1G

   [extract_sub_period]
   xpid = test_extract_period
   forcing_datebegin = 2020080106
   forcing_dateend = 2021080106
   forcing_geometry = release_2026.1
   forcing_user = vernaym
   forcing_geometry = cor2_allslopes
   datebegin = 2020010106
   dateend =  2020090106


**NB** the module containing the definition of the driver and the configuration file should be in a vapp/vconf/ directory structure:

.. code::

   vortex_cen/
       vapp/
           __init__.py
           vconf/
               __init__.py
               drivers/
                   __init__.py
                   exemple.py
               conf/
                   test.ini


You can now launch this new task with the following mkjob command:

.. code-block::

   mkjob -j name=extract_sub_period_job task=exemple plugin=drivers profile=rd-belenos-mt -c $SNOWTOOLS_CEN/vortex_cen/*vapp*/*vconf*/conf/test.ini

If you want to shorten this command, you can create a job directives file in a "jobs" repository (at the "conf" and "drivers" level) containing the following line:

.. code-block::

   # vortex_cen/*vapp*/*vconf*/jobs/job_directive_file

   name=new_task_job task=driver_of_new_task plugin=drivers profile=rd-belenos-mt

The following mkjob command is now equivalent to the one above :

.. code-block::

   mkjob -f $SNOWTOOLS_CEN/vortex_cen/*vapp*/*vconf*/jobs/job_directives_file -c $SNOWTOOLS_CEN/vortex_cen/*vapp*/*vconf*/conf/conf_example.ini


Adding a unit test for the task
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

To add a unit test, follow the exact same steps than the previous section (:ref:`launch_a_unit_task`) with the imposed **vapp=tests** and **vconf=testhpc**.

.. note::

   The configuration variables should be set to minimise the execution time, this is only a test !


Tutorial : creation of an algo component
----------------------------------------

In the example of the previous section, the core "algorithm" of the task is hard coded in the "algo" method.
In order to benefit from high-level tools provided by the vortex package, it is better to put this piece of python code in a specific "algo component" object.
In the vortex-cen plugin, these objects are stored under "vortex_cen/algo".
To implement a new algo component you can add it to an existing file that suits your need or create a new one.

**NB** If you add your algo compenent in a new file, make your algo component available by importing this file in the __init__.py of the "algo" folder :

.. code-block:: python

   # vortex_cen/algo/__init__.py
   ...
   from . import NewFile


.. _sequential_algo:

Creation of a sequential algo component
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you only need to execute the piece of python code sequentially, your algo component can directly inherit from the main "AlgoComponent" vortex object.
You must then choose an arbitrary class name for your algo component, and specify the associated footprint description.
First, choose an arbitrary (but not already used in any other algo component) *kind* in order to identify your algo component.
Then add the "arguments" used by your piece of python code (*datebegin* and *dateend* in the example of section :ref:`minimal_example`).
Finaly, add the code in an "execute" method.

In the example of section :ref:`minimal_example`, the algo component should look like :

.. code-block:: python

   from vortex.algo.components import AlgoComponent
   import xarray as xr
   from snowtools.utils import xarray_snowtools

   class Extract_Subperiod(AlgoComponent):  # Arbitrary class name

       _footprint = dict(
           attr = dict(
               kind = dict(
                   values = ['extract_subperiod'],  # Arbitrary 'tag' for this object (must not be use by another algo)
               ),
               # Algo component "parameters"
               datebegin = dict(
                   info = "Begin date of the period to extract",
                   type = Date,
               ),
               dateend = dict(
                   info = "End date of the period to extract",
                   type = Date,
               ),
           )
       )

   def execute(self, rh, opts):
       # Main execution method
       ds = xr.open_dataset('FORCING_IN.nc', engine='snowtools')
       shorter_forcing = ds.sel(time=slice(self.datebegin, self.dateend))
       shorter_forcing.to_netcdf('FORCING_OUT.nc', format='NETCDF4_CLASSIC')

.. note::

   It is of course also possible to call an external function in the "execute" method.
   In this case the function be coded with the assumption that the input file(s) (here 'FORCING_IN.nc') are already available in the working directory.

To use this algo compenent in the task introduced in section :ref:`minimal_example`, replace the "algo" method of the task by the following code :

.. code-block:: python

   def algo(self):
       self.algo = vortex.task(
           kind = "extract_subperiod",  # the target algo *kind*
           engine = "algo",  # the way the algo should be run
           datebegin = self.conf.datebegin,
           dateend = self.conf.dateend,
        )
        return algo

.. note::

   The *engine* footprint set the way the algo should be run : "algo" for python code, "blind" for a binary executable


Creation of a parallel algo component
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The motivation behind working with HPC is to enable high-level parallelisation of your scripts.
In the example of section :ref:`minimal_example`, you might want to deal with an ensemble of N forcing files instead of a single forcing file.
The optimal way to execute such a script is to make the N independent sub-period extractions in parallel.
To do so, you can rely on algo components using the "Taylorism" parallelisation tool.
These algo components split the execution between a set of sub-processes (called "Workers"), each doing a single independent execution of the script.
The work distribution between the workers and the monitoring of the workers state is done by the main process (called "Boss").

To implement a parallelisation based on Taylorism in the Vortex-Cen plugin, the "Boss" should inherit from the abstract `_CenTaylorRun` algo component and the associated "Workers" should inherit from `_CenTaylorVortexWorker`.

In the "execute" method of the Boss, "individual" (different for each Worker) and "common" (shared by all Workers) instructions can be provided.
The default behavior in `_CenTaylorRun` is to assign a sub-directory to each worker, in which the "common" set of instructions will be carried out.
This implies that input files to be processed in parallel should be fetched in different working subdirectories by the `get_remote_inputs` and `get_local_inputs` methods :

.. code-block:: python

  def get_remote_inputs(self):

      self.sh.title('Input FORCING')
      input_forcing = vortex.input(
          role        = 'Forcing',  # This role will be used to identify the Workers sub-directories (*role_members* footprint of the Boss)
          kind        = 'MeteorologicalForcing',
          local       = 'mb[member]/FORCING_IN.nc',  # Add a member-dependend sub-directory in the working directory
          experiment  = self.conf.get('forcing_xpid', self.conf.xpid),
          username    = self.conf.get('forcing_user', None),
          geometry    = self.conf.get('forcing_geometry', self.conf.geometry),
          datebegin   = self.conf.get('forcing_datebegin'),
          dateend     = self.conf.get('forcing_dateend'),
          block       = self.conf.get('forcing_block'), 'meteo'),
          member      = self.conf.get('forcing_member', self.conf.get('member', None)),  # "member" or "forcing_member" can be provided indistinctly but are optional
          nativefmt   = 'netcdf',
          namespace   = 'vortex.multi.fr',
          namebuild   = 'flat@cen',
      ),
      print(self.ticket.prompt, 'input forcing =', input_forcing) # display the result nicely in the log
      print()

The FORCING files are now fetched in member-dependend sub-directories (addition of "mb[member]/" in the *local* footprint), so the working directory will look like :

.. code-block::

     workdir/
         mb0001/
             FORCING_IN.nc
         mb0002/
             FORCING_IN.nc
         ...
         mbXXXX/
             FORCING_IN.nc

.. note::

   * An optional *member* footprint has been added (when no "member" or "forcing_member" is provided, the execution is similar to the "deterministic" one)
     --> Do not forget to update the documentation of configuration variables by documenting the optional "forcing_member|member" variable
   * A *role* footprint has been added, its value will be passed to the Boss trough the *role_members* footprint to identify the Workers subdirectories

The `Extract_Subperiod` Boss can now be created, and do nothing more than `_CenTaylorRun` in this example. The `Extract_Subperiod` Workers do the actual work (through the "_common" method)

.. code-block:: python

   from vortex_cen.algo.components import _CenTaylorRun, _CenTaylorVortexWorker
   import xarray as xr
   from snowtools.utils import xarray_snowtools


   # Definition of the "Boss"
   class Extract_Subperiod(_CenTaylorRun):

       _footprint = dict(
           attr = dict(
               kind = dict(
                   values = ['extract_subperiod'],  # The "sequential" algo of the previous section should be removed first
               ),
               role_members = dict(
                   info     = "Role of RH inputs to use for members definition",
               ),
               # Algo component "parameters"
               datebegin = dict(
                   info = "Begin date of the period to extract",
                   type = Date,
               ),
               dateend = dict(
                   info = "End date of the period to extract",
                   type = Date,
               ),
           )
       )


   # Definition of the "Workers"
   class Extract_Subperiod_Worker(_CenTaylorVortexWorker):

       # All footprint values of the Boss are automatically transfered to the Workers
       _footprint = dict(
           attr = dict(
               kind = dict(
                   values = ['extract_subperiod'],  # The *kind* of the Workers must be the same than the one of the Boss
               ),
               # Algo component "parameters"
               datebegin = dict(
                   info = "Begin date of the period to extract",
                   type = Date,
               ),
               dateend = dict(
                   info = "End date of the period to extract",
                   type = Date,
               ),
           )
       )

       def _commons(self, rundir, thisdir, rdict, **kwargs):
           # Comon instructions for all Workers
           ds = xr.open_dataset('FORCING_IN.nc', engine='snowtools')
           shorter_forcing = ds.sel(time=slice(self.datebegin, self.dateend))
           shorter_forcing.to_netcdf('FORCING_OUT.nc', format='NETCDF4_CLASSIC')

.. note::

   * The "sequential" algo of the previous section should be removed first
   * The *kind* of the Workers must be the same than the one of the Boss
   * All footprint values of the Boss are automatically transfered to the Workers


You can now execute your task in parallel by modifying the algo of section :ref:`sequential_algo` as follow :

.. code-block:: python

  def algo(self):
      self.algo = vortex.task(
          kind = "extract_subperiod",
          engine = "algo",  # Still run the algo as python code
          datebegin = self.conf.datebegin,
          dateend = self.conf.dateend,
          role_members = 'Forcing',  # The *role* value of the input
      )
      return algo

**NB** Of course, you also have to add the *member* footprint to the output files in your task in order to add the members subdirectories :

.. code-block:: python

  def put_outputs(self):
      self.sh.title('Output FORCING')
      output_forcing = vortex.output(
          kind        = 'MeteorologicalForcing',
          local       = 'mb[member]/FORCING_OUT.nc',  # Add member subdirectory
          experiment  = self.conf.xpid,
          datebegin   = self.conf.datebegin,
          dateend     = self.conf.dateend,
          geometry    = self.conf.get('forcing_geometry', self.conf.geometry),
          block       = 'meteo',
          member      = self.conf.get('forcing_member', self.conf.get('member', None)),  # Add member as in the input
          nativefmt   = 'netcdf',
          namespace   = 'vortex.cache.fr',
          namebuild   = 'flat@cen',
      ),
      print(self.ticket.prompt, 'Output forcing =', output_forcing)
      print()


.. _dynamic_documentation:

Dynamic documentation of unit tasks
-----------------------------------

The documenation of all configuration variables used by a given task is build dynamically by the "mkjob-help" script.
To do so, the list of the name of the configuration variables must be provided in the "MANDATORY_CONFUGURATION_VARIABLES" and "OPTIONAL_CONFIGURATION_VARIABLES" attributes of the task.
In order to inherit from the documentation of parent classes, these attributes should be extended as follows:

.. code-block:: python

   def __init__(self, **kw):

       MANDATORY_CONFIGURATION_VARIABLES = [
           "new_mandatory_var_1",
           "new_mandatory_var_2",
           ...
       ]
       OPTIONAL_CONFIGURATION_VARIABLES = [
           "new_optional_var_1",
           "new_optional_var_2",
           ...
       ]
       super().__init__(**kw)

       self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)


.. note::
   It is also possible to remove the documentation of variables from the parent class with an additional "overwrite" list:
   self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES, overwrite=[remove_var_1, remove_var_2,...])

Any configuration variable not already documented should be added to the "standard_variables" dictionary of the "vortex_cen/tasks/configuration_variables.py" file.
The minimal information to provide is an help message and the variable type (as string):

.. code-block:: python

   new_mandatory_var_1 = dict(
       help = "Help message",
       type = "'str', 'int', 'dict', 'list', 'bool'",
     )

Additional information such as the "default" value, the variable "format" and abvailable "choices" can also be provided.

.. code-block:: python

   new_mandatory_var_1 = dict(
       help = "Help message",
       type = "str",
       default = "const_cen.XX@YYYYYYYY",
       format = " <uenv_name>@<uenv_username>",
   )

In order to lighten the documentation of configuration variables associated to the footprint description of a specific resource, it is possible to use specific "meta variables".
These variables are identified with a "metavar=True" attribute in the "standard_variables" dictionnary.
They should also have a "values" containing the list of actual configuration variables associated to this "meta variable".

For example, the following "prep" meta variable includes all possible configuration variables to describe a PREP.nc file :

.. code-block:: python

   prep = dict(
       metavar = True,
       help = "Footprint description of PREP.nc file(s)",
       values = ["prep_xpid", "prep_user", "prep_vapp", "prep_vconf", "prep_vortex1", "prep_member", "prep_block"],
   ),

This allows to replace the following class documentation :

.. code-block:: python

   MANDATORY_CONFIGURATION_VARIABLES = [
       "prep_xpid",
       "prep_user",
       "prep_vapp",
       "prep_vconf",
       "prep_vortex1",
       "prep_member",
       "prep_block",
       ...
   ]

By the shorter following code :

.. code-block:: python

   MANDATORY_CONFIGURATION_VARIABLES = [
      "prep",
       ...
   ]

.. note::

   Of course, it is still possible to declare only some variables in the group as mandatory or optional.

By default, the mkjob-help script displays only the meta variable information:

.. code-block::

   prep_*               Footprint description of PREP.nc file(s)
                        actual variables : prep_xpid, prep_user, prep_vapp, prep_vconf, prep_geometry, prep_vortex1, prep_member, prep_block

But the user can choose to print all configuration variables included in the group with the "--verbose" argument :

.. code-block::

   prep_xpid            Experiment identifier of the PREP file
                        default : The simulation's *xpid*
                        type : str
   prep_user            Username of the producer of the PREP file
                        default : $USER
                        type : str
   prep_vapp            The *vapp* level of the PREP file
                        default : The simulation's *vapp*
                        type : str
   prep_vconf           The *vconf* level of the PREP file
                        default : The simulation's *vconf*
                        type : str
   prep_geometry        Geometry of the PREP.nc file. This must be a valid geometry tag in your'$HOME/.vortexrc/geometries.ini' file.
                        default : The simulation's *geometry*
                        type : str
   prep_vortex1         Set this value to 'True' if the target PREP file have been produced with a version of vortex <2
                        default : False
                        type : bool
   prep_member          The member(s) of the PREP file(s) in case they come from an ensemble (ex: SODA)
                        default : None
                        type : int, footprints.stdtypes.FPList (ex : 'first-last-step')
   prep_block           The *block* level of the PREP file(s)
                        default : prep
                        type : str

It is also possible to document equivalent configuration variables such as "uenv" and "surfex_env" (both these variables can be provided to retrieve SURFEX-related const resources).
In this case, a "uenv|surfex_uenv" variable can be used to convey this equivalency :

.. code-block:: python

   MANDATORY_CONFIGURATION_VARIABLES = [
      "uenv|surfex_uenv",
       ...
   ]

The associated documentation generated by the mkjob-help script makes the equivalency explicit:

.. code-block::

    uenv or surfex_uenv  Name of the User Environment containing constant files | Name of the User Environment containing all SURFEX constant files

It is also possible to document the action of a given configuration variable on other configuration variables.
For example, in the "surfex" driver of the Crocus/deterministic configuration, if the optional "climground" configuration variable is set to "True" a "PREP.nc" file is looked for so the "prep" description becomes a valid option.
This can be documented with the following "var:dependency1,dependency2,..." syntax:

.. code-block:: python

   OPTIONAL_CONFIGURATION_VARIABLES = [
       "climground:prep",
   ]

The mkjob-help script will add the information that a "prep" description becomes an option if the optional "climground" variable is provided with an "enforce" entry:

.. code-block::

   climground           Allow the generation of a ground initialization file by computing a climatological average of air temperature on the provided period.
                        default : False
                        type : bool
                        enforce : prep"

Finally, it is possible to change default variable attributes from the "standard_variables" dictionnary (separated by ';'), with the syntaxe "var+<var_attribute_to_modify>=<new_attribute_value>;..."
For example, the following class attributes

.. code-block:: python

   MANDATORY_CONFIGURATION_VARIABLES = [
       "forcing_geometry+help=A list of forcing geometries must be provided for this task;type=list;default=None",
   ]

states that for this given task, the value of the *forcing_geometry* configuration variable must be a list (instead of 'str or list') and that there is no default value.
This syntax can also be used to document variables not in the "standard_variables" dictionnary. In this case, at least the "help" message and the variable "type" must be provided.
