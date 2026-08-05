"""
assim.py
--------

Algo Components for the exectution of SODA-related tasks within a snow data assimilation workflow.

.. inheritance-diagram:: vortex_cen.algo.assim
   :top-classes: vortex_cen.algo.components._CenParaBlindRun, vortex_cen.algo.components._CenTaylorRun,
                 vortex_cen.algo.components._CenTaylorVortexWorker, vortex_cen.algo.components._CenWorkerBlindRun,
                 vortex.algo.components.AlgoComponent, vortex.algo.components.Parallel, vortex.algo.components.TaylorRun
   :private-bases:
   :parts: 1

.. autoclass:: Soda
   :no-members:
   :show-inheritance:

.. autoclass:: SodaPreProcess
   :no-members:
   :show-inheritance:

.. autoclass:: PerturbForcingComponent
   :no-members:
   :show-inheritance:

.. autoclass:: PerturbForcingWorker
   :no-members:
   :show-inheritance:

.. autoclass:: CrocOPostProcess
   :no-members:
   :show-inheritance:

.. autoclass:: CrocOPostProcessWorker
   :no-members:
   :show-inheritance:

"""

from bronx.fancies import loggers
from bronx.stdtypes.date import Date
from bronx.syntax.externalcode import ExternalCodeImportChecker

import footprints
from vortex.algo.components import Parallel, AlgoComponent
from vortex.syntax.stdattrs import a_date
from vortex_cen.algo.components import _CenTaylorRun, _CenTaylorVortexWorker

logger = loggers.getLogger(__name__)

echecker = ExternalCodeImportChecker('snowtools')
with echecker:
    from snowtools.tools.update_namelist import update_namelist_object_nmembers
    from snowtools.tools.perturb_forcing import forcinput_perturb
    from snowtools.scripts.post_processing import croco_postprocess as cpp


class Soda(Parallel):
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
            nmembers=dict(
                info="The number of members that will be processed",
                type=int,
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
                nmembers=self.nmembers
            )
            newnam = footprints.proxy.container(filename='OPTIONS_OUT.nam')
            newcontent.rewrite(newnam)
            newnam.close()


@echecker.disabled_if_unavailable
class PerturbForcingWorker(_CenTaylorVortexWorker):
    """
    Worker that applies stochastic perturbations to a FORCING file.
    """

    _footprint = dict(
        info = 'Apply stochastic perturbations to a forcing file',
        attr = dict(
            kind = dict(
                values = ['perturbforcing']
            ),
            reprod_info=dict(
                info="Informations that must be stored in output files for reproductibility",
                type=dict,
                optional=True,
                default=dict(),
            )
        )
    )

    def _commons(self, rundir, thisdir, rdict):

        self.link_in("../FORCING.nc", "FORCING_IN.nc")
        forcinput_perturb("FORCING_IN.nc", "FORCING.nc", **self.reprod_info)

        return rdict


@echecker.disabled_if_unavailable
class PerturbForcingComponent(_CenTaylorRun):
    """
    Algo compent that creates an ensemble of forcing files by stochastic perturbations
    of a time series of deterministic input forcing files. Each worker deals with one single FORCING file
    as input (parallelisation over the different sub-periods) and one single FORCING file as output
    (parallelisation over the ensemble members).
    """
    _footprint = dict(
        info = 'AlgoComponent that build an ensemble of perturbed forcings from deterministic forcing files',
        attr = dict(
            kind = dict(
                values = ['perturbforcing']
            ),
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

    def get_subdirs(self, rh, opts):
        """
        In this algo component, the members/workers are a combination of the input FORCING file
        and output ensemble members.

        Input tree:
        -----------
        workdir
        |-- datebegin_subperiod1]/FORCING.nc
        |-- subperiod1_subperiod2]/FORCING.nc
        ...
        |-- subperiodK_dateend]/FORCING.nc

        Output tree
        -----------
        workdir
        |--[datebegin_subperiod1]
            |-- FORCING.nc          (unperturbed input FORCING)
            |-- mb0001/FORCING.nc   (perturbed FORCING)
            |-- mb0002/FORCING.nc   (perturbed FORCING)
            ...
            |-- mb000N/FORCING.nc   perturbed FORCING)
        |--[subperiod1_subperiod2]
            |-- FORCING.nc          (unperturbed input FORCING)
            |-- mb0001/FORCING.nc   (perturbed FORCING)
            |-- mb0002/FORCING.nc   (perturbed FORCING)
            ...
            |-- mb000N/FORCING.nc   perturbed FORCING)
        ...
        |--[subperiodK_dateend]
            |-- FORCING.nc          (unperturbed input FORCING)
            |-- mb0001/FORCING.nc   (perturbed FORCING)
            |-- mb0002/FORCING.nc   (perturbed FORCING)
            ...
            |-- mb000N/FORCING.nc   perturbed FORCING)
        """

        subdirs = super().get_subdirs(rh, opts)
        subdirs = [f'{subdir}/mb{member:04d}' for member in self.members for subdir in subdirs]

        return subdirs


@echecker.disabled_if_unavailable
class CrocOPostProcess(_CenTaylorRun):

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
class CrocOPostProcessWorker(_CenTaylorVortexWorker):
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
        Method called by the main **vortex_task** method of the **_CenMixIn** class
        """
        # Launch "core" algo
        cpp.execute(self.datebegin, self.dateend)
