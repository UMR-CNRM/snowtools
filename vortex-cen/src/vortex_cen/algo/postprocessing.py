"""
Algo Components for S2M post processing.
"""

from bronx.fancies import loggers
from bronx.syntax.externalcode import ExternalCodeImportChecker
import footprints
from footprints.stdtypes import FPList

from vortex.algo.components import AlgoComponent, TaylorRun
from vortex.tools.parallelism import TaylorVortexWorker
from cen.algo.ensemble import _CENTaylorRun, _CENTaylorVortexWorker
from vortex.syntax.stdattrs import a_date

logger = loggers.getLogger(__name__)

echecker = ExternalCodeImportChecker('snowtools')
with echecker:
    from snowtools.plots.pearps2m.postprocess import EnsemblePostproc, EnsembleHydro
    from snowtools.tools.hydro import hydro
    from snowtools.scripts.post_processing import extract_dates
    from snowtools.scripts.post_processing import compute_scd as scd


@echecker.disabled_if_unavailable
class S2m_ensemble_postprocessing(AlgoComponent):
    """S2M ensemble forecast postprocessing.

    Current use : Get ensemble deciles of "fresh snow" (12-hourly and daily accumulation for the Bulletin 4 saisons)
    """
    _footprint = [
        dict(
            info = 'Algo component for post-processing of s2m ensemble simulations',
            attr = dict(
                kind = dict(
                    values = ['s2m_postproc']
                ),
                varnames = dict(
                    info = "Variable names to be post-processed",
                    type = FPList,
                ),
                engine = dict(
                    optional    = True,
                    default     = 's2m',
                    values      = ['s2m']
                ),
            ),
        ),
    ]

    def execute(self, rh, opts):
        # get input resources
        avail_forecasts = self.context.sequence.effective_inputs(role="CrocusForecast")
        # get list of file names
        listforcing = [am.rh.container.filename for am in avail_forecasts]
        # init ensemble postprocessing object
        ens = EnsemblePostproc(self.varnames, listforcing)
        # do postprocessing
        ens.postprocess()

        outfile = 'PRO_post_{0}_{1}.nc'.format(avail_forecasts[0].rh.resource.datebegin.ymdh,
                                               avail_forecasts[0].rh.resource.dateend.ymdh)
        self.system.mv('PRO_post.nc', outfile)

        #: variables always written to the output file


@echecker.disabled_if_unavailable
class HydroWorker(TaylorVortexWorker):
    """Algo that computes hydrological aggregated diagnostics after a surfex run for 1 member"""
    _footprint = [
        dict(
            info = 'Algo component for post-processing of s2m ensemble simulations',
            attr = dict(
                kind = dict(
                    values = ['s2m_hydro']
                ),
                varnames = dict(
                    info = "Variable names to be post-processed",
                    type = FPList,
                ),
                forcing = dict(
                    info = 'Meteorological forcing file',
                    type = str,
                    optional = False
                ),
                pro=dict(
                    info='Surfex PRO file',
                    type = str,
                    optional = False
                ),
                areas=dict(
                    info='areas file',
                    type=str,
                    optional=False
                ),
                engine = dict(
                    optional    = True,
                    default     = 's2m',
                    values      = ['s2m']
                ),
                reprod_info=dict(
                    info="Informations that must be stored in output files for reproductibility",
                    type=dict,
                    optional=True,
                    default=dict(),
                )
            ),
        ),
    ]

    def vortex_task(self, **kwargs):

        rdict = dict(rc=True)

        # Make the output file depending on date and member
        hydrooutput = self.system.path.join(self.system.path.dirname(self.pro), 'HYDRO.nc')

        with hydro([self.forcing, self.pro], self.areas, hydrooutput) as h:
            h.integration(self.varnames, var_sca='WSN_T_ISBA')

        return rdict


@echecker.disabled_if_unavailable
class HydroComponent(TaylorRun):
    """Algo that computes hydrological aggregated diagnostics after a surfex run for an ensemble"""
    _footprint = [
        dict(
            info = 'Algo component for post-processing of s2m ensemble simulations',
            attr = dict(
                kind = dict(
                    values = ['s2m_hydro']
                ),
                varnames = dict(
                    info = "Variable names to be post-processed",
                    type = FPList,
                ),
                engine = dict(
                    optional    = True,
                    default     = 's2m',
                    values      = ['s2m']
                ),
                reprod_info=dict(
                    info="Informations that must be stored in output files for reproductibility",
                    type=dict,
                    optional=True,
                    default=dict(),
                )
            ),
        ),
    ]

    def _default_common_instructions(self, rh, opts):
        """Create a common instruction dictionary that will be used by the workers."""
        ddict = super()._default_common_instructions(rh, opts)
        for attribute in self.footprint_attributes:
            ddict[attribute] = getattr(self, attribute)

        # Get areas file
        avail_areas = self.context.sequence.effective_inputs(role='HydroAreas')
        ddict['areas'] = avail_areas[0].rh.container.filename

        return ddict

    def execute(self, rh, opts):
        self._default_pre_execute(rh, opts)
        common_i = self._default_common_instructions(rh, opts)
        # Update the common instructions
        common_i.update(dict(someattribute='Toto', ))

        # Identify available forcing and pro files
        avail_forcing = self.context.sequence.effective_inputs(role='SafranForecast')
        avail_pro = self.context.sequence.effective_inputs(role='CrocusForecast')

        members_forcing = [forcing.rh.provider.member for forcing in avail_forcing]
        members_pro = [pro.rh.provider.member for pro in avail_pro]
        members_common = [m for m in members_forcing if m in members_pro]

        avail_forcing_common = [f.rh.container.filename for f in avail_forcing
                if f.rh.provider.member in members_common]
        avail_pro_common = [p.rh.container.filename for p in avail_pro if p.rh.provider.member in members_common]

        # Give some instructions to the boss
        self._add_instructions(common_i, dict(forcing=avail_forcing_common, pro=avail_pro_common))

        self._default_post_execute(rh, opts)

        # Final synthesis
        listhydro = [self.system.path.join(self.system.path.dirname(pro), 'HYDRO.nc') for pro in avail_pro_common]

        ens = EnsembleHydro(self.varnames, listhydro)
        ens.postprocess()

        outfile = 'HYDRO_{0}_{1}.nc'.format(avail_pro[0].rh.resource.datebegin.ymdh,
                avail_pro[0].rh.resource.dateend.ymdh)
        self.system.mv('PRO_post.nc', outfile)


@echecker.disabled_if_unavailable
class ExtractDates(_CENTaylorRun):

    _footprint = dict(
        info = 'AlgoComponent to extract a list of dates from an ensemble of PRO files',
        attr = dict(
            datebegin = a_date,
            dateend   = a_date,
            extract_dates = dict(
                info = "List of dates to extract from the PRO(s) file(s)",
                type = footprints.stdtypes.FPList,
            ),
            kind  = dict(
                values     = ['extract_dates'],
            ),
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                values   = ['SnowpackSimulation'],
            ),
        ),
    )


@echecker.disabled_if_unavailable
class ExtractDates_Worker(_CENTaylorVortexWorker):
    """
    """

    _footprint = dict(
        info = 'Worker that extracts a list of dates from a specific PRO file',
        attr = dict(
            datebegin = a_date,
            dateend   = a_date,
            extract_dates = dict(
                info = "List of dates to extract from the PRO(s) file(s)",
                type = footprints.stdtypes.FPList,
            ),
            kind  = dict(
                values     = ['extract_dates'],
            ),
        )
    )

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        """
        Method called by the main **vortex_task** method of the **_S2MWorkerMixIn** class
        """
        subdir = ''  # TODO : subdir should become optional in the extract_dates.execute method
        # Launch "core" algo
        extract_dates.execute(subdir, self.extract_dates)


@echecker.disabled_if_unavailable
class SnowCoverDuration(_CENTaylorRun):

    _footprint = dict(
        info = 'AlgoComponent for snow cover duration diagnotics computation from SURFEX-Crocus simulations',
        attr = dict(
            datebegin = a_date,
            dateend   = a_date,
            kind  = dict(
                values     = ['scd'],
            ),
            role_members = dict(
                info     = "Role of RH inputs to use for members definition",
                values   = ['SnowpackSimulation'],
            ),
        ),
    )


@echecker.disabled_if_unavailable
class SnowCoverDuration_Worker(_CENTaylorVortexWorker):
    """
    Worker associated to the `SnowCoverDuration` algo component and calling various
    snowtools methods to compute snow cover duration diagnostics of SURFEX-Crocus simulations :
    * SMOD (Snow Melt Out Date)
    * SCD (Snow Cover Duration)
    * SOD (Snow onset date)
    * SD (Total number of days with snow)
    """

    _footprint = dict(
        info = 'AlgoComponent for snow cover duration diagnotics computation from SURFEX-Crocus simulations',
        attr = dict(
            datebegin = a_date,
            dateend   = a_date,
            kind  = dict(
                values     = ['scd'],
            ),
        )
    )

    def _commons(self, rundir, thisdir, rdict, **kwargs):
        """
        Method called by the main **vortex_task** method of the **_S2MWorkerMixIn** class
        """
        # Launch "core" algo
        scd.execute()
