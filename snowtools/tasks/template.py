# -*- coding: utf-8 -*-
'''
Created on : Date
@author    : Name
'''

from vortex.layout.nodes import Driver, Task
from vortex import toolbox
from cen.layout.nodes import S2MTaskMixIn

import footprints


def setup(t, **kw):  # The Job calls this setup method defining the sequence of Tasks to be called
    return Driver(
        tag='mydriver',  # The Driver's tag is used by the job to identify the Driver
        ticket=t,
        nodes=[
            Template(  # The task's (class) name
                tag = 'template',  # The node's tag is used by the driver to identify the Task
                ticket=t,
                **kw),
            # It is possible to provide several Tasks to be called sequentially
        ],
        options=kw
    )


class Template(Task, S2MTaskMixIn):  # The Task's class inherits from the standard Vortex Task and CEN-specific methods
    '''
    Template for the definition of a new Vortex task.

    A vortex task is the sequence of actions to execute a single algo component.

    It always follow this procedure :

        1. fetch all necessary input resources (files):
            - from an archive machine --> on a transfert node ('early-fetch')
            - from the local machine --> on a compute node ('fetch')

        2. execute the algo component (an executable, a script or·a sequence of instructions)
           of inputs resources necessary to run an algo component (an executable, a script or a
           sequence of instructions) --> 'compute'

        3. save output resources (files produced or modified by the algo component)
            - to the local machine --> on a compute node ('backup')
            - to an archive machine --> on a compute node ('late-backup')
    '''

    def process(self):
        """
        Main method definig the task's sequence of actions
        """

        t = self.ticket  # Always get the Vortex ticket for fancy log outputs

        #######################################################################
        #                             Fetch steps                             #
        #######################################################################

        if 'early-fetch' in self.steps:  # Executed on a TRANSFERT NODE to fetch distant resources
            """
            1. Flow resource (resource produced by another task)
            ----------------------------------------------------
            The easiest way to define a toolbox input is to look for the corresponding toolbox.output
            in the task that produces the resource
            """
            self.sh.title('Toolbox input Forcing (a)')  # Set the title of the ressource with an explicit name
            forcing_a = toolbox.input(  # Always use an explicit variable name (avoid tbXX).
                role        = 'Forcing',  # The role can be used for parallelisation in some algo components
                kind        = 'MeteorologicalForcing',  # Look for "kind" choices in the target Vortex resources
                vapp        = self.conf.vapp,  # To be defined through s2m_command
                vconf       = '[geometry:tag]',  # Do not change the vconf (CEN's standard)
                cutoff      = 'assimilation',  # choices are 'assimilation' or 'production' (=forecast)
                filename    = 'FORCING_IN.nc',  # How to name the file in the working directory (developper's choice)
                experiment  = self.conf.forcing_xpid,  # To be defined through s2m_command
                geometry    = self.conf.geometry,  # To be defined through s2m_command
                nativefmt   = 'netcdf',  # The file format (ascii, tar,...)
                namebuild   = 'flat@cen',  # CEN's standard name builder (change only if you know what you are doing)
                model       = 'edelweiss',  # TODO
                member      = footprints.util.rangex(self.conf.members),  # Ensemble members (format 'n-N-1')
                date        = self.conf.dateend,  # To be defined through s2m_command (=dateend most of the time)
                datebegin   = self.conf.datebegin,  # To be defined through s2m_command
                dateend     = self.conf.dateend,  # To be defined through s2m_command
                namespace   = self.conf.namespace_in,  # Where is the desired file ? To be defined through s2m_command
                                                       # namespace values are:
                                                       # vortex.cache.fr --> local cache only
                                                       # vortex.archive.fr --> hendrix (or 'storage') cache only
                                                       # vortex.multi.fr --> local and hendrix/'storage' caches
                storage     = self.conf.storage,  # ex : sxcen.cnrm.meteo.fr (namespace must be cache or multi)
                block       = 'meteo',  # The additional/last directory(ies) where the file is stored
                intent      = 'in',  # The rights to give to the file : in=read-only, inout=read-write
                fatal       = False,  # Do not crash if the resource is not fetched (set True if you want to crash)
            )
            print(t.prompt, 'FORCING (a) =', forcing_a)  # Fancy print the Resource handler informations :
            #                                        * provider (where is the file ?)
            #                                        * resource (what file ?)
            #                                        * container (where does the file go ?)
            print()
            
            """
            If you are not sure to find the previous resource but want to ensure that the corresponding file is
            fetched, use the following "alternate" toolbox. It will only be used if 'forcing_a' returned False.
            """

            self.sh.title('Toolbox input Forcing (b)')  # Explicit title indicating that this is a second try
            forcing_b = toolbox.input(  # Explicit variable name (different from the previous one)
                alternate   = 'Forcing',  # The role can be used for parallelisation in some algo components
                # All other footprints are exactly the same as the ones of forcing_a, except :
                fatal       = True,  # Crash if the resource is not fetched and is compulsory for the algo component
            )
            print(t.prompt, 'FORCING (b) =', forcing_b)
            print()

        if 'fetch' in self.steps:  # Executed on a COMPUTE NODE to fetch resource already in the local cache

            """
            2. Static (date independent) resource
            -------------------------------------
            --> Use a User Environment (UEnv)
            A UEnv is a storage and versionning system for a set of static files.
            Each file is associated to an identifying key, and an ascii file (uenv) provides the link between the key
            and the actual file name. When a file is modified, its version increases but the key remane the same.
            An UEnv is thus defined by a unique combination of files with different versions and an associated uenv
            dictionnary.
            """
            self.sh.title('Toolbox input Static')
            static = toolbox.input(
                local          = 'STATIC_FILE',  # How to name the file in the working directory (developper's choice)
                nativefmt      = 'ascii',  # netcdf,tar,...
                kind           = 'TheKind',  # Look for "kind" choices in the target Vortex resources (cen/data/*)
                genv           = self.conf.uenv,  # UEnv id (syntax: uenv:id@user) --> to be defined through s2m_command
                gvar           = 'mask',  # The identifying key in the uenv dictionnary file
            ),
            print(t.prompt, 'Static input =', static)
            print()

            """
            3. Executable (if necessary)
            ----------------------------
            --> Use a UEnv
            """
            self.sh.title('Toolbox executable')  # There is only 1 possible executable for a task
            offline = toolbox.executable(
                role           = 'Binary',
                kind           = 'offline',
                local          = 'OFFLINE',
                model          = 'surfex',
                genv           = self.conf.genv,  # Common CEN uenv : 'uenv:cen.XX@CONST_CEN'
                gvar           = 'master_surfex_offline_mpi',
            )
            print(t.prompt, 'executable =', offline)
            print()

        #######################################################################
        #                            Compute step                             #
        #######################################################################

        if 'compute' in self.steps:  # Algo component (1 per task) executed on the compute node

            """
             There are different types of algo components. The main Vortex classes are defined in
             vortex/algo/components.py but you should always inherit from a CEN-specific algo component,
             which are defined in cen/alg/TODO

             I. Run a script (possibly with arguments)
             =========================================
             *Expresso* [engine='exec' or 'lauch'] (ex: TODO)

             II. Launch an executable with no parallelisation
             ================================================
             *BlindRun* [engine='blind'] (ex: TODO)

             III. Internal parallelisation (MPI)
             ===================================
             *Parallel* [engine='parallel'] (deterministic.Surfex_Parallel ; surfex_task.py (TODO : adapter))

             IV. External parallelisation (over ensemble members)
             =====================================================

                 IV.1. Launch a piece of python code in parallel
                 ------------------------------------------------
                 *TaylorRun* [engine optional ] (ensemble.PrepareForcingComponent ; surfex_task.py (TODO : adapter))

                 IV.2 Launch a script in parallel
                 ---------------------------------
                 *ParaExpresso* [engine='exec' or 'launch'] (ex: ensemble.Guess ; oper/prepsafran_prevision.py)

                 IV.3 Launch a binary executable in parallel
                 --------------------------------------------
                 *ParaBlindRund* [engine='blind'] (ex: ensemble.S2MComponent ; oper/ensemble_surfex_task.py)
            """


            self.sh.title('Toolbox algo')  # No need for a specif name since there is only one algo component
            algo = toolbox.algo(
                kind          = 'AlgoKind',  # Look for possible values under vortex/src/cen/algo/*
                # If the simulation concerns a given period :
                *datebegin    = self.conf.datebegin,  # Start of the simulation period
                *dateend      = self.conf.dateend,  #  End of the simulaiton period
                # 
                *engine       = '...',  # Set the type of alg component (possible values listed above)
                # In case of an ensemble simulation
                *members      = self.conf.members,
                *ntasks       = self.conf.ntasks,
                # TODO : généraliser l'usage du *role_member* plutot que de créer une méthode dans les algos Vortex ?
                *role_members = '...',  # Role of the resource to be used for the parallelisation (ex: 'Forcing')

            )
            # * optional footprints
            print(t.prompt, 'algo =', algo)
            print()
            algo.run()

        #######################################################################
        #                               Backup                                #
        #######################################################################

        if 'backup' in self.steps or 'late-backup' in self.steps:

            # TODO : différencier le membre 0 (ANTILOPE Post-traité)
            # des autres membres (avec perturbation)
            # Ou bien conserver ANTILOPE PT comme le membre 0 et adapter plutot les tâches aval ?
            self.sh.title('Toolbox ouput FORCING')
            forcing = toolbox.output(
                role        = 'Forcing file',
                kind        = 'MeteorologicalForcing',
                vapp        = self.conf.vapp,
                vconf       = '[geometry:tag]',
                cutoff      = 'assimilation',
                filename    = 'mb[member]/FORCING_OUT.nc',
                experiment  = self.conf.xpid,
                geometry    = self.conf.geometry,
                nativefmt   = 'netcdf',
                namebuild   = 'flat@cen',
                model       = 'edelweiss',
                date        = self.conf.dateend,
                datebegin   = self.conf.datebegin,
                dateend     = self.conf.dateend,
                namespace   = self.conf.namespace_out,
                storage     = self.conf.storage,
                member      = footprints.util.rangex(self.conf.members),
                block       = 'meteo',
            )
            print(t.prompt, 'FORCING =', forcing)
            print()
