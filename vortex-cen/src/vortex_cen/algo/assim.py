"""
Algo Components for ensemble S2M simulations.
"""

from bronx.fancies import loggers
from bronx.stdtypes.date import Date
from bronx.syntax.externalcode import ExternalCodeImportChecker

import footprints
from vortex.algo.components import Parallel, AlgoComponent, TaylorRun
from vortex.syntax.stdattrs import a_date
from cen.algo.ensemble import PrepareForcingWorker
from cen.algo.ensemble import _CENTaylorRun, _CENTaylorVortexWorker

logger = loggers.getLogger(__name__)

echecker = ExternalCodeImportChecker('snowtools')
with echecker:
    from snowtools.tools.update_namelist import update_namelist_object_nmembers
    from snowtools.tools.perturb_forcing import forcinput_perturb
    from snowtools.utils.resources import get_file_period, save_file_period
    from snowtools.scripts.post_processing import croco_postprocess as cpp


class SodaWorker(Parallel):
    """
    worker for a SODA run (designed for Particle filtering for snow)
    @author: B. Cluzet 2018-05-24
    """
    _footprint = dict(
        info = 'AlgoComponent that runs domain-parallelized soda',
        attr = dict(
            kind = dict(
                values = ['s2m_soda']
            ),
            binary = dict(
                values = ['SODA'],
                optional = False
            ),
            dateassim=dict(
                type = Date,
                optional = False
            ),
        )
    )

    @property
    def mb_sections(self):
        return sorted(
            self.context.sequence.effective_inputs(role=('SnowpackInit', )),
            key=lambda s: s.rh.provider.member)

    def prepare(self, rh, opts):
        super().prepare(rh, opts)
        mb_sections = self.mb_sections
        # symbolic links for each prep from each member dir to the soda dir
        for jj, mb_s in enumerate(mb_sections, start=1):
            self.system.symlink(mb_s.rh.container.localpath(),
                                'PREP_' + self.dateassim.ymdHh + '_PF_ENS' + str(jj) + '.nc')
        # symbolic link from a virtual PREP.nc to the first member (for SODA date-reading reasons)
        self.system.symlink(mb_sections[0].rh.container.localpath(), 'PREP.nc')

    def postfix(self, rh, opts):
        super().postfix(rh, opts)
        # rename ((and mix)) surfout files for next offline assim
        # rename background preps
        # delete soda symbolic links
        mb_sections = self.mb_sections
        self.system.remove('PREP.nc')
        for jj, mb_s in enumerate(mb_sections, start=1):
            dir_it = mb_s.rh.container.dirname
            self.system.remove('PREP_' + self.dateassim.ymdHh + '_PF_ENS' + str(jj) + '.nc')
            if self.system.path.isfile(self.system.path.join(dir_it, 'PREP.nc')):
                self.system.remove(self.system.path.join(dir_it, 'PREP.nc'))
            my_base, my_ext = self.system.path.splitext(mb_s.rh.container.localpath())
            # Save background:
            self.system.mv(my_base + my_ext, my_base + '_bg' + my_ext)
            # Save analysis:
            self.system.mv("SURFOUT" + str(jj) + ".nc", my_base + '.nc')
            if dir_it == 'mb{:04d}'.format(mb_s.rh.provider.member):
                # useful only for old task/offline case
                self.system.symlink(my_base + '.nc', dir_it + '/PREP.nc')

        # rename particle file
        for fprefix in ('PART', 'BG_CORR', 'IMASK', 'ALPHA'):
            if self.system.path.isfile(fprefix):
                self.system.mv(fprefix, fprefix + '_' + self.dateassim.ymdh + '.txt')


@echecker.disabled_if_unavailable
class SodaPreProcess(AlgoComponent):
    """Prepare SODA namelist according to configuration file"""

    _footprint = dict(
        attr = dict(
            kind = dict(
                values = ['soda_preprocess']),
            engine = dict(
                optional     = True,
                default   = 'algo'
            ),
            members=dict(
                info="The members that will be processed",
                type=footprints.FPList,
            ),
        )
    )

    def find_namelists(self, opts=None):
        """Find any namelists candidates in actual context inputs."""
        namcandidates = [x.rh for x in self.context.sequence.effective_inputs(kind='namelist')]
        self.system.subtitle('Namelist candidates')
        for nam in namcandidates:
            nam.quickview()

        return namcandidates

    def execute(self, rh, opts):

        # Modification of the namelist
        for namelist in self.find_namelists():
            # Update the contents of the namelist (number of members)
            # Location taken in the FORCING file.
            newcontent = update_namelist_object_nmembers(
                namelist.contents,
                nmembers=len(self.members)
            )
            newnam = footprints.proxy.container(filename=namelist.container.basename)
            newcontent.rewrite(newnam)
            newnam.close()


@echecker.disabled_if_unavailable
class PerturbForcingWorker(PrepareForcingWorker):
    """
    Worker that applies stochastic perturbations to a time series of forcing files
    (worker for 1 member).
    """

    _footprint = dict(
        info = 'Apply stochastic perturbations to a forcing file',
        attr = dict(
            kind = dict(
                values = ['perturbforcing']
            ),
            geometry_out = dict(
                info="The resource's massif geometry.",
                type=str,
                optional = True,
                default = None
            )
        )
    )

    def _prepare_forcing_task(self, rundir, thisdir, rdict):

        need_other_forcing = True
        datebegin_this_run = self.datebegin

        while need_other_forcing:

            forcingdir = self.forcingdir(rundir, thisdir)

            # Get the first file covering part of the whole simulation period
            dateforcbegin, dateforcend = get_file_period("FORCING", forcingdir,
                                                         datebegin_this_run, self.dateend)

            self.system.mv("FORCING.nc", "FORCING_OLD.nc")
            forcinput_perturb("FORCING_OLD.nc", "FORCING.nc", **self.reprod_info)

            dateend_this_run = min(self.dateend, dateforcend)

            # Prepare next iteration if needed
            datebegin_this_run = dateend_this_run
            need_other_forcing = dateend_this_run < self.dateend

            save_file_period(thisdir, "FORCING", dateforcbegin, dateforcend)

        return rdict


@echecker.disabled_if_unavailable
class PerturbForcingComponent(TaylorRun):
    """
    Algo compent that creates an ensemble of forcing files by stochastic perturbations
    of a time series of deterministic input forcing files
    (worker for 1 member).
    Can not inherit from ensemble.PrepareForcingComponent because datebegin and dateend are dates, not lists.
    Can not inherit from ensemble.S2MComponent because there is not any binary to run
    (inheritance from TaylorRun, not ParaBlindRun)
    """
    _footprint = dict(
        info = 'AlgoComponent that build an ensemble of perturbed forcings from deterministic forcing files',
        attr = dict(
            kind = dict(
                values = ['perturbforcing']
            ),
            engine=dict(
                values=['s2m']
            ),
            datebegin = a_date,
            dateend   = a_date,
            members = dict(
                info = "The list of members for output",
                type = footprints.stdtypes.FPList,
            ),
            reprod_info=dict(
                info="Informations that must be stored in output files for reproductibility",
                type=dict,
                optional=True,
                default=dict(),
            )
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

    def execute(self, rh, opts):
        """Loop on the output members requested to apply stochastic perturbations."""
        self._default_pre_execute(rh, opts)
        # Update the common instructions
        common_i = self._default_common_instructions(rh, opts)
        # Contrary to mother class, datebegin and dateend are not used for parallelization.
        subdirs = self.get_subdirs(rh, opts)
        self._add_instructions(common_i, dict(subdir=subdirs))
        self._default_post_execute(rh, opts)

    def get_subdirs(self, rh, opts):
        """
        In this algo component, the number of members is defined by the user,
        as there is only 1 single deterministic input
        """
        return ['mb{:04d}'.format(member) for member in self.members]


@echecker.disabled_if_unavailable
class CrocOPostProcess(_CENTaylorRun):

    _footprint = dict(
        info = 'AlgoComponent for SODA post_processing (merge PRO files)',
        attr = dict(
            datebegin = a_date,
            dateend   = a_date,
            kind  = dict(
                values     = ['croco_postprocess'],
            ),
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                values   = ['SnowpackSimulation'],
            ),
        ),
    )


@echecker.disabled_if_unavailable
class CrocOPostProcess_Worker(_CENTaylorVortexWorker):
    """
    Worker associated to the `SodaPostProcess` algo component.
    Each worker concatenate the different PRO files covering the subperiods between assimilation dates
    generated by the crocO task into a single PRO file covering the period *datebegin* --> *dateend*
    """

    _footprint = dict(
        info = 'AlgoComponent for snow cover duration diagnotics computation from SURFEX-Crocus simulations',
        attr = dict(
            datebegin = a_date,
            dateend   = a_date,
            kind  = dict(
                values     = ['croco_postprocess'],
            ),
        )
    )

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        """
        Method called by the main **vortex_task** method of the **_S2MWorkerMixIn** class
        """
        # Launch "core" algo
        cpp.execute(self.datebegin, self.dateend)
