"""
Algo Components for ensemble S2M simulations.
"""

from bronx.fancies import loggers
from vortex.algo.components import ParaBlindRun, TaylorRun
from vortex.tools.parallelism import VortexWorkerBlindRun, TaylorVortexWorker

logger = loggers.getLogger(__name__)


class _S2MWorkerMixIn(object):

    def vortex_task(self, **kwargs):
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
        if self.subdir is not self.system.path.dirname(rundir):
            thisdir = self.system.path.join(rundir, self.subdir)
            with self.system.cdcontext(self.subdir, create=True):
                rdict = self._commons(rundir, thisdir, rdict, **kwargs)
        else:
            thisdir = rundir
            rdict = self._commons(rundir, thisdir, rdict, **kwargs)

        return rdict

    def _commons(self, rundir, thisdir, rdict):
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


class _CENWorkerBlindRun(_S2MWorkerMixIn, VortexWorkerBlindRun):
    """
    This abstract worker is designed to drive the launch of any S2M executable
    without MPI parallelization (deterministic or ensemble-like simulations) in
    association with an Algo Component inheriting from an :class:`_CENParaBlindRun`
    or :class:`Guess` Algo Component.

    A single worker is thus a deterministic execution of a given binary with a
    specific environment that can be run in parallel with other workers
    (executions of the same binary with different environments).
    """

    _abstract = True
    _footprint = dict(
        info = 'Worker designed to run a specific member of S2M ensemble experiment associated to an executable'
               'without MPI parallelization.',
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


class _CENTaylorVortexWorker(_S2MWorkerMixIn, TaylorVortexWorker):
    """
    This abstract worker is designed to drive the launch of any S2M task not associated
    to an executable with MPI parallelization (deterministic or ensemble-like simulations) in
    association with an Algo Component inheriting from an :class:`_CENTaylorRun`

    A single worker is thus a deterministic execution of a list of python commands in a
    specific environment that can be run in parallel with other workers.
    """

    _abstract = True
    _footprint = dict(
        info = 'Worker designed to run a specific member of S2M ensemble experiment NOT associated to an executable'
               'with MPI parallelization.',
        attr = dict(
            subdir = dict(
                info = 'work in this particular subdirectory',
                optional = False
            ),
        )
    )


class _CENParaBlindRun(ParaBlindRun):
    """
    This abstract algo component defines common methods for all CEN ensemble simulations
    with a parallelisation over ensemble members and no MPI parallelization.
    Algo components deriving from ParaBlindRun (and thus from this class) are associated to an executable.
    """

    _abstract = True
    _footprint = dict(
        info = 'AlgoComponent that runs several executions of a CEN model in parallel.',
        attr = dict(
            engine = dict(
                # TODO : modification majeure, à discuter avec Matthieu
                # Engine should stay 'blind' to indicate that the core of the algo component is
                # an executable (thus the Vortex object is 'blind' to the execution)
                # This footprint should be used to chose between *_CENParaBlindRun* algo components
                # and *_CENTaylorRun* algo components
                values   = ['blind', 's2m']  # s2m for backward compatibility
            ),
            metadata = dict(
                values   = ['StandardSAFRAN', 'StandardPROSNOW'],
                optional = True,
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


class _CENTaylorRun(TaylorRun):
    """
    TaylorRun derived algo components are not (necessarily) associated to an executable and can simply launch
    a piece of python code.
    """

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
        )
    )

    def execute(self, rh, opts):
        """Loop on the various initial conditions provided."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)
        subdirs = self.get_subdirs(rh, opts)
        self._add_instructions(common_i, dict(subdir=subdirs))
        self._default_post_execute(rh, opts)

    def get_subdirs(self, rh, opts):
        """
        Get the different member's subdirectories.

        One member is associated to each 'effective input' (inputs that where
        actually retrieved during the fetch step) Section with a role matching
        the one defined by the **role_members** footprint.

        WARNING : the use of a footprint attribute instead of a class method to define
        the role to use for members identification is a significant difference with other
        "_CENTaylorRun" derived algo components.
        """
        avail_members = self.context.sequence.effective_inputs(role=self.role_members)

        if len(avail_members) > 0:
            subdirs = list()
            # Retrive the subdirectory asociated to each identified RH
            for am in avail_members:
                if am.rh.container.dirname not in subdirs:
                    subdirs.append(am.rh.container.dirname)
        else:
            subdirs = ['.']

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
