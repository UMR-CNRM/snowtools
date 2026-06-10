Documentation for HPC task developpers
======================================

..
  TODO :
  * Création d'une nouvelle tâche unitaire sans algo, de sa documentation et d'un cas test associé (ex: "extract_subperiod")
  * Création d'un driver utilisant la nouvelle tâche et du fichier de configuration associé
  * Création d'un algo sans parallélisation
  * Création d'un algo avec parallélisation

Documenting unit tasks
""""""""""""""""""""""

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

Additional information such as the "default" value and the variable "format" can also be provided.

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

   def __init__(self, **kw):

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
       OPTIONAL_CONFIGURATION_VARIABLES = [
          ...
       ]
       super().__init__(**kw)

       self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

By the shorter following code :

.. code-block:: python

   def __init__(self, **kw):

       MANDATORY_CONFIGURATION_VARIABLES = [
          "prep",
           ...
       ]
       OPTIONAL_CONFIGURATION_VARIABLES = [
          ...
       ]
       super().__init__(**kw)

       self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

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

   def __init__(self, **kw):

       MANDATORY_CONFIGURATION_VARIABLES = [
          "uenv|surfex_uenv",
           ...
       ]
       OPTIONAL_CONFIGURATION_VARIABLES = [
          ...
       ]
       super().__init__(**kw)

       self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

The associated documentation generated by the mkjob-help script makes the equivalency explicit:

.. code-block::

    uenv or surfex_uenv  Name of the User Environment containing constant files | Name of the User Environment containing all SURFEX constant files

It is also possible to document the action of a given configuration variable on other configuration variables.
For example, in the "surfex" driver of the Crocus/deterministic configuration, if the optional "climground" configuration variable is set to "True" a "PREP.nc" file is looked for so the "prep" description becomes a valid option.
This can be documented with the following "var:dependency1,dependency2,..." syntax:

.. code-block:: python

   def __init__(self, **kw):

       super().__init__(**kw)
       MANDATORY_CONFIGURATION_VARIABLES = [
       ]
       OPTIONAL_CONFIGURATION_VARIABLES = [
           "climground:prep",
       ]
       self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

The mkjob-help script will add the information that a "prep" description becomes an option if the optional "climground" variable is provided with an "enforce" entry:

.. code-block::

   climground           Allow the generation of a ground initialization file by computing a climatological average of air temperature on the provided period.
                        default : False
                        type : bool
                        enforce : prep"

Finally, it is possible to change default variable attributes from the "standard_variables" dictionnary (separated by ';'), with the syntaxe "var+<var_attribute_to_modify>=<new_attribute_value>;..."
For example, the following class attributes

.. code-block:: python

   def __init__(self, **kw):

       super().__init__(**kw)
       MANDATORY_CONFIGURATION_VARIABLES = [
           "forcing_geometry+type=list;default=None",
       ]
       OPTIONAL_CONFIGURATION_VARIABLES = [
       ]
       self.update_attributes(MANDATORY_CONFIGURATION_VARIABLES, OPTIONAL_CONFIGURATION_VARIABLES)

states that for this given task, the value of the *forcing_geometry* configuration variable must be a list (instead of 'str or list') and that there is no default value.


Tutorial (developer)
--------------------
