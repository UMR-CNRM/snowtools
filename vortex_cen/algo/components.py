# -*- coding: utf-8 -*-
"""
components.py
-------------

Abstract algo components classes for multiprocessing simulations with an external parallelisation (i.e not MPI).
The parallelization is typically over the simulation members or independent sub-periods.
All processes share the same executable (if any) but at least partially different IO environments or
script arguments.

These algo components rely on the `taylorism` package. The main process (the `Boss`) acts as a task scheduler to
to allocate tasks to a set of independent processes (the `Workers`) working simultaneously.

Two abstract "Boss" classes are defined here are:

    * :class:`_CenParaBlindRun`: to launch an executable multiple times in parallel
    * :class:`_CenTaylorRun`: to launch a piece of Python code in parallel on several processes

A third abstract "Boss" class is available in the main vortex-nwp package:

    * :class:`ParaExpresso`: to launch a script multiple times in parallel

These abstract classes provide high-level tools such as the identification of the list of workers through
the list of sub-directories (via the "role" of the main input resource varying from one simulation to another).

Each "Boss" is associated to a "Worker", providing the instructions for each individual task / process.
Each "Worker" works in its specific sub-directory, which must be filled with the appropriate IOs (when fetching the
inputs at the "Task" level).
Some IOs can be shared by several or all workers. These IOs must be stored in the main working directory, and a link
to these files can be created in the Worker's working directory by the Worker itself.

Two abstract "Worker" classes are defined here are:

    * :class:`_CenWorkerBlindRun`: launch an executable without MPI parallelization
    * :class:`_CenTaylorVortexWorker`: launch a piece of Python code

A third abstract "Worker" class is available in the main vortex-nwp package:

    * :class:`VortexWorkerBlindRun`: to launch a script multiple times in parallel

Here is the full inheritance diagram of the available meta classes :

.. inheritance-diagram:: vortex_cen.algo.components
   :private-bases:
   :parts: 1

.. autoclass:: _CenParaBlindRun
   :no-members:
   :show-inheritance:

.. autoclass:: _CenTaylorRun
   :no-members:
   :show-inheritance:

.. autoclass:: _CenMixIn
   :no-members:
   :show-inheritance:

.. autoclass:: _CenWorkerBlindRun
   :no-members:
   :show-inheritance:

.. autoclass:: _CenTaylorVortexWorker
   :no-members:
   :show-inheritance:

"""

from bronx.fancies import loggers
from vortex.algo.components import ParaBlindRun, TaylorRun
from vortex.tools.parallelism import VortexWorkerBlindRun, TaylorVortexWorker

logger = loggers.getLogger(__name__)


class _CenMixIn(object):

    def vortex_task(self, **kwargs):
        # TODO : find a better name for this method ?
        """
        Main method, the first that is executed at the initialization of the
        worker.

        Its purpose is to set the worker's specific sub-environment (move to
        the potential corresponding sub-directory) and to return the output
        state of the worker (stored in the `rdict` dictionary) to the main Algo
        Component.

        Any overloading of this method or any sub-method called must return
        such a dictionary storing potential execution errors (in the `rc` entry
        of the dictionary). As a consequence, any part of code within the worker
        that might raise a python Exception must be encapsulated in a
        try/except instruction in order to catch such exception, put it into
        the `rdict` output and return it to the main Algo Component where all
        exceptions should be managed properly.

        :note: Any un-catched exception will cause a (clean) shutdown of the
               :mod:`taylorism` system (e.g. All the other workers will be killed
               and the main Algo Component will exit).
        """

        rdict = dict(rc=True)
        rundir = self.system.getcwd()
        # Retrieve the list of available forcings now because it will no longer be available in the
        # worker's subcontexts.
        # TODO : do that in a specific "preprocess" method ?
        self.avail_forcings = [x.rh for x in self.context.sequence.effective_inputs(role='Forcing')]
        if self.subdir is not self.system.path.dirname(rundir):
            thisdir = self.system.path.join(rundir, self.subdir)
            with self.system.cdcontext(self.subdir, create=True):
                rdict = self._commons(rundir, thisdir, rdict, **kwargs)
        else:
            thisdir = rundir
            rdict = self._commons(rundir, thisdir, rdict, **kwargs)

        return rdict

    def _commons(self, rundir, thisdir, rdict):
        # TODO : find a better name for this method ? At least remove the "_"...
        """
        Abstract method called by the main **vortex_task** method to set up the
        worker's environment (links to common files, name of execution listings,
        ...) and launch the executable, call the method that launches it or
        apply algo instructions.
        """
        raise NotImplementedError

    def exists(self, target):
        """Check if a target file exists."""
        if self.system.path.islink(target) or self.system.path.isfile(target):
            return True
        else:
            return False

    def mv_if_exists(self, local, dest):
        """Move a file if it exists (intended to deal with output files)."""
        if self.system.path.isfile(local):
            self.system.mv(local, dest)

    def copy_if_exists(self, local, dest):
        """Copy a file if it exists (intended to deal with input files)."""
        if self.system.path.isfile(local):
            self.system.cp(local, dest)

    def link_in(self, local, dest):
        """Link a file (the target is cleaned first)."""
        self.system.remove(dest)
        if self.system.path.isfile(local):
            self.system.symlink(local, dest)

    def link_ifnotprovided(self, local, dest):
        """Link a file if the target does not already exist."""
        if not self.system.path.islink(dest) and not self.system.path.isfile(dest):
            if self.system.path.isfile(local):
                self.system.symlink(local, dest)

    def copy_ifnotprovided(self, local, dest):
        """Link a file if the target does not already exist."""
        if not self.system.path.islink(dest) and not self.system.path.isfile(dest):
            if self.system.path.isfile(local):
                self.system.cp(local, dest)

    def postfix(self):
        self.system.subtitle('{:s} : directory listing (post-run)'.format(self.kind))


class _CenWorkerBlindRun(_CenMixIn, VortexWorkerBlindRun):
    """
    This abstract worker is designed to drive the launch of any script or executable
    without MPI parallelization (deterministic or ensemble-like simulations) in
    association with an Algo Component inheriting from an :class:`_CenParaBlindRun`
    or :class:`ParaExpresso` Algo Component.

    A single worker is thus a deterministic execution of a given binary or script with a
    specific IO environment and/or a specific set of argument.
    """

    _abstract = True
    _footprint = dict(
        info = 'Worker designed to run a specific member of an ensemble of simulations associated to a script or'
               'an executable without MPI parallelization.',
        attr = dict(
            subdir = dict(
                info = 'work in this particular subdirectory',
                optional = True
            ),
            deterministic = dict(
                type     = bool,
                default  = True,
                optional = True,
            ),
            reprod_info = dict(
                info     = "Informations that must be stored in output files for reproductibility",
                type     = dict,
                optional = True,
                default  = dict(),
            ),
        )
    )


class _CenTaylorVortexWorker(_CenMixIn, TaylorVortexWorker):
    """
    This abstract worker is designed to drive the launch a python program.
    The parallelisation is typically over the simulation members or
    independent sub-periods.

    A single worker is thus a deterministic execution of a list of python commands in a
    specific environment that can be run in parallel with other workers.
    """

    _abstract = True
    _footprint = dict(
        info = 'Worker designed to run a specific member of an ensemble of simulations NOT associated'
               'to an executable',
        attr = dict(
            subdir = dict(
                info = 'work in this particular subdirectory',
                optional = False
            ),
        )
    )


class _CenParaBlindRun(ParaBlindRun):
    """
    This abstract algo component defines common methods for simulations based on an executable without
    MPI parallelization.
    The parallelization is typically over the simulation members or independent sub-periods, and all processes
    share the same executable but different IO environments.
    """

    _abstract = True
    _footprint = dict(
        info = 'AlgoComponent that runs several executions of an executable in parallel.',
        attr = dict(
            engine = dict(
                # TODO : modification majeure, à discuter avec Matthieu
                # Engine should stay 'blind' to indicate that the core of the algo component is
                # an executable (thus the Vortex object is 'blind' to the execution)
                # This footprint should be used to chose between *_CenParaBlindRun* algo components
                # and *_CenTaylorRun* algo components
                values   = ['blind', 's2m']  # s2m for backward compatibility. TODO : remove the 's2m' engine
            ),
        )
    )

    def prepare(self, rh, opts):
        """Set some variables according to target definition."""
        super().prepare(rh, opts)
        self.env.DR_HOOK_NOT_MPI = 1

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super()._default_common_instructions(rh, opts)
        for attribute in self.footprint_attributes:
            ddict[attribute] = getattr(self, attribute)
        return ddict

    def postfix(self, rh, opts):
        pass


class _CenTaylorRun(TaylorRun):
    """
    TaylorRun derived algo components are not (necessarily) associated to an executable and can simply launch
    a piece of python code.
    """

    _abstract = True
    _footprint = dict(
        info = 'AlgoComponent that runs several executions in parallel.',
        attr = dict(
            engine = dict(
                info     = 'The way the executable should be run.',
                values   = ['algo', ],
                default  = 'algo',
            ),
            # TODO : find a more explicit name for role_member ?
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                type     = str,
            ),
            ntasks = dict(
                info        = 'The maximum number of parallel tasks',
                # WARNING : do not confuse this algo-specific argument with
                # the "ntasks" argument of the SBATCH configuration (setting the number
                # of tasks per node in case "exclusive=False")
                type        = int,
                access      = 'rwx',  # Make footprint writable
                default     = None,
                optional    = True
            ),
        )
    )

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)
        self._add_instructions(common_i, dict(subdir=self.subdirs))
        self._default_post_execute(rh, opts)

    def _default_pre_execute(self, rh, opts):
        """Various initialisations. In particular it creates the task scheduler (Boss)."""
        self.subdirs = self.get_subdirs(rh, opts)
        # WARNING : overwriting the *ntasks* footprint value can have side effects.
        # TODO : add a security to ensure that this value is lower than the number of thread available ?
        # if self.ntasks is None:
        #     self.ntasks = len(self.subdirs)
        super()._default_pre_execute(rh, opts)

    def get_subdirs(self, rh, opts):
        """
        Get the different member's subdirectories.

        One member is associated to each 'effective input' (inputs that where
        actually retrieved during the fetch step) Section with a role matching
        the one defined by the **role_members** footprint.

        WARNING : the use of a footprint attribute instead of a class method to define
        the role to use for members identification is a significant difference with other
        "_CenTaylorRun" derived algo components.
        """
        avail_members = self.context.sequence.effective_inputs(role=self.role_members)

        print('----------------------------------------------------------------------')
        print('List of Workers :')
        print('-----------------')
        if len(avail_members) > 0:
            subdirs = list()
            # Retrive the subdirectory asociated to each identified RH
            for am in avail_members:
                if am.rh.container.dirname not in subdirs:
                    subdirs.append(am.rh.container.dirname)
                    print('* ', am.rh.container.dirname)
        else:
            subdirs = ['.']
            print('* .')
        print('----------------------------------------------------------------------')
        # logger.info('Workers : \n' + '\n'.join(subdirs))

        return subdirs

    def prepare(self, rh, opts):
        """Set some variables according to target definition."""
        super().prepare(rh, opts)
        self.env.DR_HOOK_NOT_MPI = 1

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super()._default_common_instructions(rh, opts)
        for attribute in self.footprint_attributes:
            ddict[attribute] = getattr(self, attribute)
        return ddict

    def postfix(self, rh, opts):
        pass
