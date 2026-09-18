# -*- coding: utf-8 -*-
"""
prosnow.py
----------

Algo Components for the PROSNOW project

.. inheritance-diagram:: vortex_cen.algo.prosnow
   :top-classes: vortex_cen.algo.components._CenParaBlindRun, vortex_cen.algo.components._CenTaylorRun,
                 vortex_cen.algo.components._CenTaylorVortexWorker, vortex_cen.algo.components._CenWorkerBlindRun
                 vortex.algo.components.AlgoComponent, vortex.algo.components.Parallel, vortex.algo.components.TaylorRun
   :private-bases:
   :parts: 1

.. autoclass:: PrepareForcingComponentForecast
   :no-members:
   :show-inheritance:

.. autoclass:: ExtractForcingWorker
   :no-members:
   :show-inheritance:

.. autoclass:: ExtractForcingWorkerEnsembleForecast
   :no-members:
   :show-inheritance:

.. autoclass:: ExtractForcingWorkerLTForecast
   :no-members:
   :show-inheritance:

.. autoclass:: Prosnow_Parallel
   :no-members:
   :show-inheritance:
"""

from bronx.syntax.externalcode import ExternalCodeImportChecker

from vortex_cen.algo.ensemble import PrepareForcingComponent, PrepareForcingWorker
from vortex_cen.algo.deterministic import Surfex_Parallel

echecker = ExternalCodeImportChecker('snowtools')
with echecker:
    from snowtools.tools.change_forcing import forcinput_extract, forcinput_changedates
    from snowtools.tools.change_prep import prep_tomodify
    from snowtools.tools.update_namelist import update_namelist_var


@echecker.disabled_if_unavailable
class PrepareForcingComponentForecast(PrepareForcingComponent):
    """
    It adapts forcing files to a ski resort geometry (several members in parallel).

    This class was implemented by C. Carmagnola in May 2019 (PROSNOW project).
    """

    _footprint = dict(
        info = 'AlgoComponent that runs several executions in parallel',
        attr = dict(
            kind = dict(
                values = ['extractforcing_STforecast', 'extractforcing_LTforecast']
            )
        )
    )

    def _default_common_instructions(self, rh, opts):

        ddict = super(PrepareForcingComponent, self)._default_common_instructions(rh, opts)
        for attribute in self.footprint_attributes:
            if attribute in ['datebegin', 'dateend']:
                ddict[attribute] = getattr(self, attribute)[0][0]
            else:
                ddict[attribute] = getattr(self, attribute)

        return ddict

    def execute(self, rh, opts):

        self._default_pre_execute(rh, opts)
        common_i = self._default_common_instructions(rh, opts)
        subdirs = self.get_subdirs(rh, opts)
        self._add_instructions(common_i, dict(subdir=subdirs))
        self._default_post_execute(rh, opts)

    def get_subdirs(self, rh, opts):

        avail_members = self.context.sequence.effective_inputs(role=self.role_ref_namebuilder())
        subdirs = list()
        for am in avail_members:
            if am.rh.container.dirname not in subdirs:
                subdirs.append(am.rh.container.dirname)

        return subdirs

    def role_ref_namebuilder(self):
        return 'Forcing'


@echecker.disabled_if_unavailable
class ExtractForcingWorker(PrepareForcingWorker):
    """
    It adapts forcing files to a ski resort geometry (worker for 1 member).

    This class was implemented by C. Carmagnola in May 2019 (PROSNOW project).
    """

    _footprint = dict(
        info = 'Prepare forcing for PROSNOW simulations - deterministic case',
        attr = dict(
            kind = dict(
                values = ['extractforcing']
            ),
        )
    )

    def _prepare_forcing_innertask(self, rundir, thisdir, dir_file_1, rdict):
        return super()._prepare_forcing_task(rundir, thisdir, rdict)

    def _prepare_forcing_task(self, rundir, thisdir, rdict):
        datebegin_str = self.datebegin.strftime('%Y%m%d%H')
        dateend_str = self.dateend.strftime('%Y%m%d%H')

        dir_file_1 = self.forcingdir(rundir, thisdir) + '/FORCING_' + datebegin_str + '_' + dateend_str + '.nc'
        dir_file_2 = self.forcingdir(rundir, thisdir) + '/FORCING_out_' + datebegin_str + '_' + dateend_str + '.nc'
        dir_file_3 = self.forcingdir(rundir, thisdir) + '/FORCING_in_' + datebegin_str + '_' + dateend_str + '.nc'
        dir_file_4 = rundir + '/SRU.txt'

        rdict = self._prepare_forcing_innertask(rundir, thisdir, dir_file_1, rdict)

        # Extraction of SRU geometry
        forcinput_extract(dir_file_1, dir_file_2, dir_file_4)
        self.system.mv(dir_file_1, dir_file_3)
        self.system.mv(dir_file_2, dir_file_1)

        # ------------------- #

        return rdict


@echecker.disabled_if_unavailable
class ExtractForcingWorkerEnsembleForecast(ExtractForcingWorker):
    """
    It adapts forcing files to a ski resort geometry (worker for 1 member)
    with specific adaptations for short term forecast

    This class was implemented by C. Carmagnola in May 2019 (PROSNOW project).
    """

    _footprint = dict(
        info = 'Prepare forcing for PROSNOW simulations - ensemble forecast',
        attr = dict(
            kind = dict(
                values = ['extractforcing_STforecast']
            )
        )
    )

    def forcingdir(self, rundir, thisdir):
        return thisdir


@echecker.disabled_if_unavailable
class ExtractForcingWorkerLTForecast(ExtractForcingWorkerEnsembleForecast):
    """
    It adapts forcing files to a ski resort geometry (worker for 1 member)
    with specific adaptations for seasonal forecasts

    This class was implemented by C. Carmagnola in May 2019 (PROSNOW project).
    """

    _footprint = dict(
        info = 'Prepare forcing for PROSNOW simulations - LT forecast',
        attr = dict(
            kind = dict(
                values = ['extractforcing_LTforecast']
            ),
        )
    )

    def _prepare_forcing_innertask(self, rundir, thisdir, dir_file_1, rdict):
        # Change dates of the climatology to the current season
        forcinput_changedates(dir_file_1, dir_file_1, self.datebegin.nivologyseason_begin)
        return rdict


@echecker.disabled_if_unavailable
class Prosnow_Parallel(Surfex_Parallel):
    """
    It adds snow management specificities by ski resorts to standard SURFEX-Crocus algo components.

    This class was implemented by C. Carmagnola in April 2019 (PROSNOW project).
    """

    _footprint = dict(
        info = 'AlgoComponent designed to run SURFEX experiments over large domains with MPI parallelization.',
        attr = dict(
            insert_data = dict(
                values = ['prosnow_insert_data', ],
                type = str,
            )
        )
    )

    def prosnow_modify_namelist(self):
        new_nam = update_namelist_var("OPTIONS_unmodified.nam", "water.txt")
        return new_nam

    def prosnow_modify_prep(self):
        dateend_str = self.dateend.strftime('%Y%m%d%H')
        my_name_OBS = 'OBS_' + dateend_str + '.nc'
        my_name_PREP = 'PREP_' + dateend_str + '.nc'

        old_prep = prep_tomodify(my_name_PREP)
        new_prep = old_prep.insert_snow_depth('SRU.txt', 'snow.txt', my_name_OBS, 'prep_fillup_50.nc',
                                              'prep_fillup_5.nc', 'variables', my_name_PREP)

        return new_prep

    def execute(self, rh, opts):

        # Insert water consumption in namelist (before running surfex)
        self.prosnow_modify_namelist()

        # Call execute of Surfex_Parallel
        # Note that modify_namelist and modify_prep methods of the mother class
        # still have to be called in the following instruction
        super().execute(rh, opts)

        # Insert snow height in prep (after running surfex)
        self.prosnow_modify_prep()
